# Pyrazinamide population pk model

# PK model description ----------------------------------------------
des_intro <- "Vancomycin iv model for Chinese adult patients"
des_notes <- c("- measurement time is recommended to be matched with the first dose",
               "<br>",
               "- Detailed tracking of CLCR and age is recommend to reflect physiological changes in clearance and volume of distribution")
des_comp <- "central"
des_cov <- "CLCR, AGE" # Strict 

des_params <- c("- V: volume of distritubtion (Vancomycin)","<br>",
                "- Cl: clearance (Vancomycin)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("CLCR", "AGE")
mod_lcov = NULL # covariates with dropdown list
mod_lcov_value <- NULL
mod_cov_abbr <- c("CLCr", "Age")

mod_route <- c("IV")

# plot option -------------------------------------------------------
pk <- "cp"
pk_obs <- "SDC"
pk_color <- '#FF6666'
pk_x_label <- "Time (hours)"
pk_y_label <- "Vancomycin Conc. (mg/L)"

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
            'L'='v')

sd_eta <- sqrt(c(0.100, 0.080)) # put sd^2 value in this vector

# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(log(2.829))       # CL
    theta2 <- c(log(52.14))       # V
    theta3 <- c(0.00842)     # CRcl on CL
    theta4 <- c(0.8143)      # Age on CL
    theta5 <- c(2.647)            # Additive error
    theta6 <- c(0.2679)           # Proportional error
    
    eta1 ~ c(0.100)         # Cl
    eta2 ~ c(0.080)         # V
  })
  model({
    cl <- exp(theta1 + eta1) * (1 + theta3 * (CLCR-80)) * (75/AGE)**theta4
    v <- exp(theta2 + eta2) 
    ke = cl/v
    d/dt(center) = - ke * center
    cp = center / v
    cp ~ add(theta5)+prop(theta6)
  })
}

