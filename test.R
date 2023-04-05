# fitting

name <- c("Hour", "Min", "CrCL", "WT", "MDV", "CMT", "DV", "condi", "Route", "EVID", "AMT", "RATE", "ADDL", "II", "TIME", "ID", "CRPZERO", "PCA")
row1 <- c(0, 0, NA, NA, 1, 1, NA, "est", "IV", 1, 60, 60, 6, 6, 0, 1234, 10, 32)
row2 <- c(2, 0, 32, 15, 0, 1, 30, "est", NA, 0, NA, NA, NA, NA, 1, 1234, 10, 32)
row3 <- c(2, 0, 32, 15, 0, 2, 10, "est", NA, 0, NA, NA, NA, NA, 0, 1234, 10, 32)

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
  setnafill(cols = "auc_divide", fill=0)
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



