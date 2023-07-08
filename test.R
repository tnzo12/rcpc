# fitting

name <- c("Hour", "Min", "CrCL", "WT", "MDV", "CMT", "DV", "condi", "Route", "EVID", "AMT", "RATE", "ADDL", "II", "TIME", "ID", "CRPZERO", "PCA", "SS")
row1 <- c(0, 0, NA, NA, 1, 1, NA, "est", "IV", 1, 60, 60, 6, 6, 0, 1234, 10, 32, 0)
row2 <- c(2, 0, 32, 15, 0, 1, 30, "est", NA, 0, NA, NA, NA, NA, 1, 1234, 10, 32, 0)
row3 <- c(2, 0, 32, 15, 0, 2, 10, "est", NA, 0, NA, NA, NA, NA, 0, 1234, 10, 32, 0)

df <- rbind(row1, row2, row3) %>% data.frame %>% mutate_all(as.numeric)
names(df) <- name

fit <- nlmixr(f, data=df, est="focei", control=foceiControl(maxOuterIterations = 0)) %>% rename(Time = TIME)

class(fit$eta)

eta_table <- fit$eta %>% slice(n()) %>% select(-ID)
fit$eta

library(data.table)


ev <- eventTable() %>%
  add.sampling(0:200) %>%
  add.dosing(dose=100, nbr.doses = 6, dosing.interval = 6) %>% 
  add.dosing(start.time=120, dose=100, nbr.doses = 6, dosing.interval = 6) %>% 
  add.dosing(start.time=120, dose=100) 
ev$CRPZERO <- 10
ev$PCA <- 32
ev$WT <- 1.5

ev_noiiv <- dplyr::bind_cols(ev,eta_table)

sim_res <- rxSolve(object = fit,
                       events=ev,
                       nSub = 50, # how many simulations will be generated
                       seed = 1234) %>% data.table() %>% mutate(condi=ifelse(time<80, 'est', 'sim' )) %>% rename(Time = time)

sim_res_noiiv <- rxSolve(object = fit,
                         events=ev_noiiv,
                         nSub = 1, # how many simulations will be generated
                         seed = 1234) %>% data.table() %>% mutate(condi=ifelse(time<80, 'est', 'sim' )) %>% rename(Time = time,
                                                                                                                   Estimated = pk)


# quantile ----------------------------
sim_r <- data.table::melt(sim_res,
                          id.vars = c("Time","condi"),
                          measure.vars = c(if(exists(quote(pk))){pk}else{NULL}, if(exists(quote(pd))){pd}else{NULL})) %>%
  rename(Estimated = value)

auc <- function(x,time){ # linear-log trapezoidal method
  dplyr::if_else(
    (x < dplyr::lead(x)), # ifelse
    (shift(time, type = "lead")- time) * (x + shift(x, type = "lead")) / 2, # up: linear trap.
    (shift(time, type = "lead")- time) * (x - shift(x, type = "lead")) / (log(x) - log(shift(x, type = "lead"))) # down: logarithmic trap.
  )
}

sim_q <- sim_r[, .(P05 = quantile(Estimated,0.05),
                   P25 = quantile(Estimated,0.25),
                   P50 = quantile(Estimated,0.50),
                   P75 = quantile(Estimated,0.75),
                   P95 = quantile(Estimated,0.95)), by=.(Time, condi, variable)] %>% 
  .[,c("P50_AUC") := lapply(.SD, auc, Time), .SDcols = c("P50"), by=variable]


# dose point in event table
dosep <- ev %>% filter(!is.na(amt)) %>%
  rowwise() %>% 
  summarize(x = list(seq(from=time, to=(time + (addl+1)*ii), by = ii))) %>% 
  unlist() %>% unname()

sim_res_noiiv <- sim_res_noiiv %>% data.table() %>% 
  .[Time %in% dosep, dosed := 1] %>% # check where dose was given
  .[is.na(dosed), dosed := 0] %>%
  .[, ind_auc := lapply(.SD, auc, Time), .SDcols = "Estimated"] %>% 
  .[, dose_divide := cumsum(dosed == 1)] %>% 
  .[, auc_divide := cumsum(ind_auc), by=dose_divide] %>% 
  setnafill(cols = "auc_divide", fill=0) %>% 
  .[, tad := (Time - first(Time)), by=dose_divide] # time after dose
#  .[, auc_divide := if_else(last(auc_divide)==auc_divide, auc_divide, NA), by=dose_divide]

subplot(plot_ly(data=sim_res_noiiv, showlegend = FALSE) %>%
          add_bars(x=~Time, y=~auc_divide, name="AUC_tau", color=I("#6633FF"), width=1, showlegend = FALSE) %>%
          plotly::layout(xaxis = list(title = "xlabel",
                                      color = "#999999"),
                         yaxis = list(title = "ylabel",
                                      color = "#999999"),
                         plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
                         autosize = TRUE),
        pkd_plot(sim_q, sim_res_noiiv, fit, pk_color, pk_obs, pk_x_label, pk_y_label),
        nrows = 2, heights = c(0.1, 0.9), shareX = TRUE)

class(sim_q)



# parame vis --------------------------
sim_p <- dplyr::bind_rows(sim_res, sim_res_noiiv) %>% 
  .[, sim.id, mget(est_eta)] %>%
  .[,lapply(.SD, mean),by=.(sim.id)] %>%
  .[,paste0(est_eta,".z"):=lapply(.SD,scale), .SDcols = est_eta] %>% 
  setnames(old = est_eta, new = paste0(est_eta,".v")) %>% 
  data.table::melt(id.vars = 'sim.id',
                   measure.vars = patterns("v$","z$"), # column names ends with v and z
                   variable.name= "Param",
                   value.name = c("Value","Z.score")) %>% 
  .[,`:=`(Value=round(Value,3), Z.score=round(Z.score,3))] %>% # rounding 
  .[,Param:=as.factor(est_eta)[Param]]

# visual parameter diagnostic plot
plot_ly(data = sim_p, x = ~Param, y = ~Z.score, color = ~Param, opacity = 0.6,
        type = "box", showlegend=FALSE, boxpoints = "all", jitter = 0.25, text = ~paste("<b>Value: </b>",Value),
        boxmean = TRUE) %>%
  add_markers(data = sim_p %>% filter(is.na(sim.id)),
              y = ~Z.score, x = ~Param, color = ~Param, text = ~paste("<b>Value: </b>",Value),
              marker = list(symbol = "arrow-bar-down", size = 10), 
              inherit = FALSE, showlegend=FALSE) %>%
  layout(
    xaxis = list(
      title = "Parameters",
      color = "#999999"),
    yaxis = list(
      title = "Z-scores for simulated individuals",
      color = "#999999",
      showgrid = TRUE,
      zeroline = FALSE),
    plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)" 
  )

# parameter diff table

sim_p2 <- sim_p[,`:=`(Ind = last(Value), Median = median(Value), Diff = last(Value) - median(Value)), by=Param] %>% # last value = no iiv sim.id (of NA)
  unique(by="Param") %>%
  select(-c(sim.id, Value, Z.score)) %>%
  .[,`Change(%)` := Diff/Median*100] # changes in percent



# event table
ev <- et() %>%
  et(seq(0, 24, length.out=100)) %>%
  et(amt=c(10000,2), addl=c(1,NA), ii=c(12,6), ss=c(1,1), time=c(5,3), rate=10000) %>% 
  merge(data.frame(data=1), all=TRUE)

str(ev)

ev <- ev %>% 
  filter(!is.na(amt)) %>% 
  rowwise() %>% 
  mutate(addl = ifelse('addl' %in% names(.), addl, NA)) %>% 
  mutate(to = time + addl*ii)
  summarize_at(x = list(seq(from=time, to=(time + addl*ii), by = ii)))


  
hist_data <- rbind(
  c(19453.00,	0.00,	0.00,	0.00,	NA,	NA,	0.00,	0.00,	2.00,	'est',	NA,	NA,	NA,	NA,	NA,	NA,	0.00,	201950471,	0.00),
  c(19453.00,	0.00,	0.00,	NA,	NA,	NA,	1.00,	1.00,	1.00,	'est',	'IV',	10.00,	6.00,	6.00,	0.00,	10.00,	0.00,	201950471,	0.00),
  c(19453.00,	1.00,	0.00,	15.00,	32.00,	1.00,	0.00,	0.00,	1.00,	'est',	NA,	NA,	NA,	NA,	NA,	NA,	1.00,	201950471,	0.00),
  c(19455.00,	0.00,	0.00,	NA,	NA,	NA,	1.00,	1.00,	1.00,	'sim',	'IV',	10.00,	0.00,	0.00,	0.00,	0.00,	48.00,	201950471,	0.00),
  c(19455.00,	0.00,	0.00,	NA,	NA,	NA,	1.00,	1.00,	1.00,	'sim',	'IV',	15.00,	0.00,	0.00,	0.00,	0.00,	54.00,	201950471,	0.00)
) %>% data.frame()
names(hist_data) <- c('Date','Hour','Min','DV','PCA','WT','MDV','EVID','CMT','condi','Route','AMT','ADDL','II','SS','RATE','TIME','ID','CRPZERO')

hist_data <- hist_data %>% mutate_at(vars("TIME","ADDL","II","RATE","SS","AMT","EVID"), as.numeric)

est_hist <- subset(hist_data, condi=='est') %>% replace(is.na(.), 0) %>% tail(1)

est_endtime <- est_hist[1,"TIME"] + (est_hist[1,"ADDL"] * est_hist[1,"II"]) + 24 # set endtime

amt_data <- subset(hist_data, !is.na(hist_data$AMT))

ev <- et() %>% 
  et(seq(from=0, to=as.numeric(max( # follows the bigger record between simulation and estimation dosing history
    hist_data$TIME[nrow(hist_data)] + 5*24,
    est_endtime
  )), by=0.25)) %>% # by 0.25 hour = 15 min, tracking for additional 48 hours
  bind_rows(
    amt_data %>% select(ID, TIME, CMT, AMT, RATE, II, ADDL, EVID, SS) %>% mutate(ID = 1) %>% rename_all(tolower)
  ) %>% arrange(time)

ev %>% filter(!is.na(amt))

amt_data$TIME



# scenario-based simulations
sce_doser <- c(.25, .5, .75, 1, 1.25, 1.5, 2)
sce_tau <- c(3, 4, 6, 8, 12, 24, 48)

df$condi <- c('sim', 'est', 'est') # re

cov_data <- subset(df, MDV==0) %>%
  rename(time = TIME) %>%
  mutate(evid = MDV,
         amt = 0) %>% dplyr::select(time, evid, any_of(mod_cov), CRPZERO)

sim_lastr <- df %>% data.table() %>% # f_data in application
  setnames(., tolower(names(.))) %>%
  .[!is.na(amt),] %>% 
  .[condi=='sim',] %>%
  .[.N] %>%
  select(any_of(c("time","amt","cmt","rate","ii","evid","ss")))
sim_lastr
evt <- et(seq(from=0,to=24, by=0.5))

sce_et_gen <- function(doser, tau){
  
  evt %>% 
    merge(sim_lastr %>% mutate(time=0,
                               ss=1,
                               amt=amt*doser,
                               rate=rate*doser,
                               ii=tau)
          , # last row
          all=TRUE) %>% 
    merge(cov_data, all=TRUE) %>% # merge (outer join)
    tidyr::fill(any_of(mod_cov), .direction = "downup") %>% # to fill NAs in the event table
    tidyr::fill("CRPZERO", .direction = "downup") %>% 
    dplyr::bind_cols(eta_table)
}

combs <- expand.grid(doser=sce_doser, tau=sce_tau) %>%
  mutate(id = row_number())

#combs_exam <- expand.grid(doser=sce_doser, tau=sce_tau) %>% mutate(id = row_number())
#exam <- combs %>% data.table() %>% 
#  .[, sce_et_gen(doser, tau), by=id] %>% system.time()
#sce_grid <- combs %>% 
#  data.table() %>% 
#  furrr::future_pmap(sce_et_gen) %>%
#  rbindlist() %>% 
#  mutate(id = rep(1:nrow(combs), each=nrow(evt)+1)) %>% system.time()
#exam
#sce_grid
sce_grid <- combs %>% 
  data.table() %>% 
  .[, sce_et_gen(doser, tau), by=id] %>% 
  #furrr::future_pmap(sce_et_gen) %>% 
  #rbindlist() %>% 
  #mutate(id = rep(1:nrow(combs), each=nrow(evt)+1)) %>%  # dosing record
  rxSolve(object=fit, events=., nSub=1) %>%
  data.table() %>% 
  .[,ind_auc := lapply(.SD, auc, time), .SDcols="ipredSim", by=id] %>% 
  .[,cum_auc := cumsum(ind_auc), by=id] %>% 
  .[,c_peak := max(ipredSim), by=id] %>% 
  .[,c_trou := .SD[time==sim_lastr$ii, ipredSim], by=id] %>% 
  .[,auc_tau := .SD[time==sim_lastr$ii, cum_auc], by=id] %>% 
  .[,auc_24 := auc_tau*24/sim_lastr$ii, by=id] %>% 
  .[,.SD[1], by=id]

sce_res <- left_join(combs, sce_grid, by="id") %>% mutate(dose=doser*sim_lastr$amt)

plot_ly(type=NULL) %>% 
  layout(xaxis = list(tickmode = "array", tickvals = sce_doser*sim_lastr$amt, title = "dosing amount", type="category", showgrid=FALSE),
         yaxis = list(tickmode = "array", tickvals = sce_tau, title = "dosing interval", type="category", showgrid=FALSE),
         plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
         autosize = TRUE) %>% 
  plotly::add_heatmap(data=sce_res, x=~dose, y=~tau, z=~auc_24, showlegend=FALSE, opacity = 0.8, showscale=FALSE) %>% 
  plotly::add_text(data=sce_res, x=~dose, y=~tau, type="scatter", text=~round(auc_24,2), textfont=list(color='#999999'))

class(sim_res)


hm_plot(sce_res, "#6699CC", "#FF6666", "dosing amount (AUC 24)", "dosing interval","auc_24")


sce_et_gen_iiv <- function(doser, tau){
  
  evt %>% 
    merge(sim_lastr %>% mutate(time=0,
                               ss=1,
                               amt=amt*doser,
                               rate=rate*doser,
                               ii=tau)
          , # last row
          all=TRUE) %>% 
    merge(cov_data, all=TRUE) %>% # merge (outer join)
    tidyr::fill(any_of(mod_cov), .direction = "downup") %>% # to fill NAs in the event table
    tidyr::fill("CRPZERO", .direction = "downup")
}

sim_res_p <- sce_et_gen_iiv(1,sim_lastr$ii) %>%
  rxSolve(object=fit, events=., nSub=50) %>% 
  data.table() %>% 
  .[,ind_auc := lapply(.SD, auc, time), .SDcols="ipredSim", by=sim.id] %>% 
  .[,cum_auc := cumsum(ind_auc), by=sim.id] %>% 
  .[,c_peak := max(ipredSim), by=sim.id] %>% 
  .[,c_trou := .SD[time==sim_lastr$ii, ipredSim], by=sim.id] %>% 
  .[,auc_tau := .SD[time==sim_lastr$ii, cum_auc], by=sim.id] %>% 
  .[,auc_24 := auc_tau*24/sim_lastr$ii, by=sim.id] %>% 
  .[,.SD[1], by=sim.id]

sim_res_p %>% filter(c_trou>15, c_peak<50)


plot_ly(
  data = sim_res_p,
  x = ~sim.id,
  y = ~auc_24
) %>% add_trace(type="bar",
                marker = list(color = ifelse(sim_res_p$auc_24>800, "#FF9900", "#999999")))
