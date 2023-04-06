# Phenobarbital population pk model

# PK model description ----------------------------------------------
des_intro <- "Phenobarbital IV model for patients who were placed on the drug for the prevention of intraventricular hemorrahge."
des_notes <- c("The data collected from each individual included the drug dosing history (dose, route, and time of administration)",
               "<br>",
               "A one-compartment open model with intravenous bolus administration and firstorder elimination is used to describe the pharmacokinetics of phénobarbital")
des_comp <- c("central")
des_cov <- c("APGAR", "WT") # Strict 

des_params <- c("- V: volume of distritubtion (Phenobarbital)","<br>",
                "- Cl: clearance (Phenobarbital)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("APGAR","WT")
mod_cov_abbr <- c("Apgar score at 5 minute, either 1(<5) or zero","Body Weight")

mod_route <- c("IV")

# plot option -------------------------------------------------------
pk <- "cp"
pk_obs <- "SDC"
pk_color <- '#FF6666'
pk_x_label <- "Time (hours)"
pk_y_label <- "Phenobarbital Conc. (mg/L)"

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

sd_eta <- sqrt(c(0.035, 0.025)) # put sd^2 value in this vector

# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(0.0047)        # CL
    theta2 <- c(0.96)          # V
    theta3 <- c(0.135)         # APGAR
    theta4 <- c(0)             # Additive error
    theta5 <- c(0.107)         # Proportional error
    
    eta1 ~ c(0.035)
    eta2 ~ c(0.025)
  })
  model({
    VCOV <- 1 + theta3 * APGAR
    
    TVCL <- theta1 * WT
    
    TVV  <- theta2 * WT * VCOV
    
    cl  <- TVCL * exp(eta1)
    v   <- TVV  * exp(eta2)
    
    #ke <- theta1 / theta2
    ke = cl/v
 
    d/dt(center) = - ke * center
    
    cp = center / v
    cp ~ prop(theta5) + add(theta4)
  })
}
