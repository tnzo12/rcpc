# Vancomycin population pk model

# PK model description ----------------------------------------------
des_intro <- "Vancomycin IV model for Thai patients"
des_notes <- c("- complete data regarding dosage regimens, serum drug concentration, precise timing of dose administration and blood sampling over the entire course of vancomycin therapy",
               "<br>",
               "- Detailed tracking of CLCR is recommend to reflect physiological changes in clearance. Detailed tracking of Age is recommended to reflect physiological changes in Volume")
des_comp <- c("central", "peripheral")
des_cov <- c("CLCR", "Age") # Strict 

des_params <- c("- V: volume of distritubtion (Vancomycin)","<br>",
                "- Cl: clearance (Vancomycin)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("CLCR","AGE")
mod_cov_abbr <- c("Creatinine Clearance","Age")

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
            'L'='v1' ,
            'L'= 'v2' ,
            'L/h' = 'q')

sd_eta <- sqrt(c(0.36, 0.21, 0.40, 0.57)) # put sd^2 value in this vector



# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(0.044)    # CL
    theta2 <- c(0.542)    # V1
    theta3 <- c(6.950)    # Q
    theta4 <- c(44.20)    # V2
    theta5 <- c(4.51)          # Additive error
    
    eta1 ~ c(0.36)         # CL
    eta2 ~ c(0.21)         # V1
    eta3 ~ c(0.40)         # Q
    eta4 ~ c(0.57)         # V2
    
  })
  model({
    cl <- theta1 * exp(eta1) * CLCR
    v1 <- theta2 * exp(eta2) * AGE 
    v2 <- theta4 * exp(eta4)
    q <- theta3 * exp(eta3)
    
    #ke <- theta1 / theta2
    ke = cl/v1
    k12 = q/v1
    k21 = q/v2
    
    d/dt(center) = -k12 * center + k21 * peri - ke * center
    d/dt(peri) = k12 * center - k21 * peri
    
    cp = center / v1
    cp ~ add(theta5)
  })
}
