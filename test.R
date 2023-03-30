name <- c("Hour", "Min", "CrCL", "WT", "MDV", "CMT", "DV", "condi", "Route", "EVID", "AMT", "RATE", "ADDL", "II", "TIME", "ID", "CRPZERO", "PCA")
row1 <- c(0, 0, NA, NA, 1, 1, NA, "est", "IV", 1, 60, 60, 6, 6, 0, 1234, 10, 32)
row2 <- c(2, 0, 32, 15, 0, 1, 30, "est", NA, 0, NA, NA, NA, NA, 1, 1234, 10, 32)
row3 <- c(2, 0, 32, 15, 0, 2, 10, "est", NA, 0, NA, NA, NA, NA, 0, 1234, 10, 32)

df <- rbind(row1, row2, row3) %>% data.frame %>% mutate_all(as.numeric)
names(df) <- name

fit <- nlmixr(f, data=df, est="focei", control=foceiControl(maxOuterIterations = 0))

class(fit$eta)

eta_table <- fit$eta %>% slice(n()) %>% select(-ID)
fit$eta

library(data.table)


ev <- eventTable() %>% add.sampling(0:200) %>% add.dosing(dose=100)
ev$CRPZERO <- 10
ev$PCA <- 32
ev$WT <- 1.5

sim_res <- rxSolve(object = fit,
                       events=ev,
                       nSub = 100, # how many simulations will be generated
                       seed = 1234) %>% data.table() %>% mutate(condi=ifelse(time<80, 'est', 'sim' )) %>% rename(Time = time)
sim_r <- data.table::melt(sim_res, id.vars = c("Time","condi"), measure.vars = c(if(exists(quote(pk))){pk}else{NULL}, if(exists(quote(pd))){pd}else{NULL}))


sim_r <- sim_r %>% rename(Estimated = value)
sim_q <- sim_r[, .(p05 = quantile(Estimated,0.05),
                     p25 = quantile(Estimated,0.25),
                     p50 = quantile(Estimated,0.50),
                     p75 = quantile(Estimated,0.75),
                     p95 = quantile(Estimated,0.95)), by=.(Time, condi, variable)] %>% data.frame()
class(sim_q)

sim_q$variable

sim_q %>% if_any(cp, crp)

# drug network




m1 <-RxODE({
  KA=2.94E-01;
  CL=1.86E+01;
  V2=4.02E+01;
  Q=1.05E+01;
  V3=2.97E+02;
  Kin=1;
  Kout=1;
  EC50=200;
  ## Added modeled bioavaiblity, duration and rate
  fdepot = 1;
  durDepot = 8;
  rateDepot = 1250;
  C2 = centr/V2;
  C3 = peri/V3;
  d/dt(depot) =-KA*depot;
  f(depot) = fdepot
  dur(depot) = durDepot
  rate(depot) = rateDepot
  d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3;
  d/dt(peri)  =                    Q*C2 - Q*C3;
  d/dt(eff)  = Kin - Kout*(1-C2/(EC50+C2))*eff;
  eff(0) = 1
})

ev <- et(timeUnits="hr") %>%
  et(amt=10000, ii=12, ss=1) %>%
  et(seq(0, 240, length.out=1000)) %>% 
  mutate(addl)


ev
et(et, amt=10000, ii=12, ss=1)

ev <- eventTable()
ev <- ev$add.dosing(dose=10000, dosing.interval = 12, ss=0) %>% 
  et(seq(0, 240, length.out=1000))
rxSolve(m1, ev) %>% plot(C2)

