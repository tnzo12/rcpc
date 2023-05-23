# Vancomycin population pk model
# Author: Sung Taewook
# PK model description ----------------------------------------------
des_intro <- "Vancomycin IV model for critically ill adult septic patients"
des_notes <- c("- measurement time is recommended to be matched with the first dose",
               "<br>",
               "- Detailed tracking of CLCR is recommend to reflect physiological changes in clearance")
des_comp <- "central"
des_cov <- "CLCR" # Strict 

des_params <- c("- V: volume of distritubtion (Vancomycin)","<br>",
                "- Cl: clearance (Vancomycin)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("CLCR","WT")
mod_lcov = NULL # covariates with dropdown list
mod_lcov_value <- NULL
mod_cov_abbr <- c("Creatinine Clearance","Body Weight")

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
  
  sd_eta <- sqrt(c(0.278)) # put sd^2 value in this vector
  
  # Model file for estimation -----------------------------------------
  f <- function() {
    ini({
      theta1 <- c(log(0.024))    # CL
      theta2 <- c(0.511)    # V
      theta3 <- c(0.112)         # Proportional error
      eta1 ~ c(0.278)         # CL
      })
    model({
      cl <- exp(theta1 + eta1)*CLCR + 1.93
      v <- theta2 * WT 
      #ke <- theta1 / theta2
      ke = cl/v
      d/dt(center) = - ke * center
      cp = center / v
      cp ~ prop(theta3)
    })
  }
  