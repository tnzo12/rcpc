# Voriconazole population pk model
# Author: Jo Seo yoon

# PK model description ----------------------------------------------
des_intro <- "Voriconazole IV model for patients with talaromycosis."
des_notes <- c("- measurement time is recommended to be matched with the first dose",
               "<br>",
               "- Detailed tracking of CLCR is recommended to reflect physiological changes in clearance")
des_comp <- "central"
des_cov <- "CLCR" # Strict 

des_params <- c("- V: volume of distritubtion (Vancomycin)","<br>",
                "- Cl: clearance (Vancomycin)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("CRP")
mod_lcov = NULL # covariates with dropdown list
mod_lcov_value <- NULL
mod_cov_abbr <- c("C-reactive Protein")

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
est_eta <-c('L/h'='cl')

sd_eta <- sqrt(c(0.278)) # put sd^2 value in this vector

# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(log(4.34))    # CL
    theta2 <- c(-0.135)    # CRP on CL
    theta3 <- c(log(97.4))         # V
    theta4 <- c(1.1)               # ka
    theta5 <- c(0.262)             # Proportional error
    
    eta1 ~ c(0.834)         # CL
    eta2 ~ c(0.521)         # V
  })
  model({
    cl <- exp(theta1 + eta1)*exp(theta2*(CRP/43.6))*exp(1.01)
    v <- exp(theta3 + eta2) *exp(0.0973)
    ka <- theta4
    
    ke = cl/v
    d/dt(center) = - ke * center
    cp = center / v
    cp ~ prop(theta5)
  })
}
