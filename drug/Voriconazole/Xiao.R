# Voriconazole population pk model

# PK model description ----------------------------------------------
des_intro <- "model of intravenous voriconazole (VRC) in critically ill patients with liver dysfunction"
des_notes <- c("- measurement time is recommended to be matched with the first dose",
               "<br>",
               "- Detailed tracking of CLCR, Child-Pugh class and weight is recommend to reflect physiological changes in clearance and volume of distribution")
des_comp <- "center, peri"
des_cov <- "Child-Pugh class, weight" # Strict 

des_params <- c("- V: volume of distritubtion (Voriconazole)","<br>",
                "- Cl: clearance (Voriconazole)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("CPugh", "WT")
mod_lcov = c("CPugh") # covariates with dropdown list
mod_lcov_value <- list(CPugh = c('A'= 1, 'B'= 1, 'C'= 0))
mod_cov_abbr <- c("Child Pugh class", "weight")

mod_route <- c("IV")

# plot option -------------------------------------------------------
pk <- "cp"
pk_obs <- "SDC"
pk_color <- '#FF6666'
pk_x_label <- "Time (hours)"
pk_y_label <- "Voriconazole Conc. (mg/L)"

pd <- NA
pd_obs <- NA
pd_color <- NA
pd_x_label <- NA
pd_y_label <- NA

# model scheme image ------------------------------------------------
scheme_image <- "https://els-jbs-prod-cdn.jbs.elsevierhealth.com/cms/attachment/e9f4a16a-a113-4770-86c4-614b4895f30c/fx1.jpg"

# Compartment designation -------------------------------------------
mod_comp <- c(
  IV = 1,
  SDC = 1,
  NONE = 10
)

# Inter-individually Variable parameters ----------------------------
est_eta <-c('L/h'='cl',
            'L'='v1',
            'L'='v2',
            'L/h'='q')


sd_eta <- sqrt(c(0.5355, 0.5684, 0.5834, 0.4756)) # put sd^2 value in this vector

# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(1.29)       # CL
    theta2 <- c(51.64)        # V1
    theta3 <- c(110.89)       # V2
    theta4 <- c(36.45)       # Q
    theta5 <- c(0.055)            # BW effect on theta2
    theta6 <- c(0.159)            # BW effect on theta3
    theta7 <- c(0.59)        # Decrease ratio in theta1 for CPugh-A/B
    theta8 <- c(0.09)        # Standard deviation of residual variability.
    
    eta1 ~ c(0.5355)         # Cl
    eta2 ~ c(0.5684)        # V1
    eta3 ~ c(0.5834)         # v2
    eta4 ~ c(0.4756)         # q
  })
  model({
    cl <- theta1 * exp(theta7 * CPugh) * exp(eta1)
    v1 <- theta2 * ((WT/64)^theta5) * exp(eta2)
    v2 <- theta3 * ((WT/64)^theta6) * exp(eta3)
    q <- theta4 * exp(eta4)
    k12 = q/v1
    k21 = q/v2
    ke = cl/v1
    d/dt(center) = - k12 * center + k21 * peri - ke * center
    d/dt(peri) = k12 * center - k21 * peri
    cp = center / v1
    cp ~ prop(theta8)
  })
}

