name <- c("Hour", "Min", "CrCL", "WT", "MDV", "CMT", "DV", "condi", "Route", "EVID", "AMT", "RATE", "ADDL", "II", "TIME", "ID")
row1 <- c(0, 0, NA, NA, 1, 1, NA, "est", "IV", 1, 60, 60, 1, 6, 0, 1234)
row2 <- c(1, 0, 32, 15, 0, 1, 15, "est", NA, 0, NA, NA, NA, NA, 0, 1234)

df <- rbind(row1, row2) %>% data.frame %>% mutate_all(as.numeric)
names(df) <- name

fit <- nlmixr(f, data=df, est="focei", control=foceiControl(maxOuterIterations = 0))

