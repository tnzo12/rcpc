# Vancomycin population pk model

# PK model description ----------------------------------------------
des_intro <- "Vancomycin IV model for adult Chinese PCM patients"
des_notes <- c("All patients received an IV infusion of VCM over 60 min. VCM trough serum samples were obtained at steady state.",
               "<br>",
               "CLcr significantly influenced VCM CL, where as gender, age, BW, co-therapy and laboratory test results had no effect.")
des_comp <- "central"
des_cov <- "CLCR" # Strict 

des_params <- c("- V: volume of distritubtion (Vancomycin)","<br>",
                "- Cl: clearance (Vancomycin)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("CLCR")
mod_lcov = NULL # covariates with dropdown list
mod_lcov_value <- NULL
mod_cov_abbr <- c("Creatinine Clearance")

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

sd_eta <- sqrt(c(0.092)) # put sd^2 value in this vector

# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(7.56)    # CL
    theta2 <- c(0.89)    # CLCR
    theta3 <- c(101)     # V / FIX
    theta4 <- c(exp(0.04))       # Exponential error
    eta1 ~ c(0.092)      # CL
  })
  model({
    cl <- theta1 * ((CLCR/104.71)**theta2) * exp(eta1)
    v <- theta3
    #ke <- theta1 / theta3
    ke = cl/v
    d/dt(center) = - ke * center
    cp = center / v
    cp ~ prop(theta4)
  })
}
