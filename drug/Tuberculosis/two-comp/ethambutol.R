# Ethambutol population pk model

# PK model description ----------------------------------------------
des_intro <- "Ethambutol po model for patients with pulmonary tuberculosis"
des_notes <- c("- measurement time is recommended to be matched with the first dose",
               "<br>",
               "- Detailed tracking of WT and HIV is recommend to reflect physiological changes in clearance and volume of distribution")
des_comp <- "transit, absorption, center, peripheral"
des_cov <- "Weight, HIV status" # Strict 

des_params <- c("- v: volume of distritubtion (Ethambutol)","<br>",
                "- cl: clearance (Ethambutol)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("WT", "HIV")
mod_cov_abbr <- c("Weight", "HIV status")

mod_route <- c("PO")

# plot option -------------------------------------------------------
pk <- "ipred"
pk_obs <- "SDC"
pk_color <- '#FF6666'
pk_x_label <- "Time (hours)"
pk_y_label <- "Ethambutol Conc. (mg/L)"

# model scheme image ------------------------------------------------
scheme_image <- "https://els-jbs-prod-cdn.jbs.elsevierhealth.com/cms/attachment/e9f4a16a-a113-4770-86c4-614b4895f30c/fx1.jpg"


# Compartment designation -------------------------------------------
mod_comp <- c(
  PO = 1,
  SDC = 3,
  NONE = 10
)


# Inter-individually Variable parameters ----------------------------
est_eta <-c('L/hr'='cl', 
            '1/hr'='ka', 
            'hr'='MTT')

sd_eta <- sqrt(c(0.1965, 0.5675, 0.702)) # put sd^2 value in this vector

# Model file for estimation -----------------------------------------

f <- function() {
  ini({
    theta1 <- c(log(40.9))     # 1. CL
    theta2 <- c(log(139))      # 2. V2
    theta3 <- c(log(0.505))    # 3. Ka
    theta4 <- c(0.314)         # 4. Prop. error
    theta5 <- c(0.12)          # 5. Add. error
    theta6 <- c(log(1110))          # 6. V3
    theta7 <- c(log(28.5))          # 7. Q
    theta8 <- c(log(0.359))         # 8. MTT
    theta9 <- c(0.155)        # 9. F
    eta1 ~ c(0.0408)          # 1. Cl
    eta2 ~ c(0.3660)          # 2. Ka
    eta3 ~ c(0.7020)           # 3. MTT
  })
  model({
    if(HIV==1){F = 1-theta9;} 
    else {F=1; }
    cl <- (WT/50)**0.75 * exp(theta1 + eta1)
    v2 <- (WT/50) * exp(theta2)
    ka <- exp(theta3 + eta2)
    v3 <- (WT/50) * exp(theta6)
    Q <- (WT/50)**0.75 * exp(theta7)
    MTT <- exp(theta8+eta3)
    ktr <- 1/MTT
    k34 <- Q/v2
    k43 <- Q/v3
    k30 <- cl/v2
    d/dt(transit) = -ktr* transit
    d/dt(abs) = ktr * transit - ka * abs
    d/dt(center) = F * ka * abs - k34 * center + k43 * periph - k30 * center
    d/dt(periph) = k34 * center - k43 * periph
    cp = center / v2
    cp ~ add(theta5)+prop(theta4)
  })
} 
