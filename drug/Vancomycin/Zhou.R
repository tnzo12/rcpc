# Vancomycin population pk model

# PK model description ----------------------------------------------
des_intro <- "Vancomycin IV model for Chinese geriatric patients (age≥65 years) with pulmonary infections and to explore the clinical application of this information for vancomycin dose individualization"
des_notes <- c("The vancomycin dosing regimen consisted of 500 mg (every 6 h, 8 h, 12 h, 24 h, 48 h) and 1000 mg (every 8 h, 12 h), for 1.5–2 h intravenous infusion.",
               "<br>",
               "CLcr significantly influenced VCM CL")
des_comp <- "central"
des_cov <- "CLCR" # Strict 

des_params <- c("- V: volume of distritubtion (Vancomycin)","<br>",
                "- Cl: clearance (Vancomycin)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("CLCR")
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

sd_eta1 <- sqrt(c(0.174)) # put sd^2 value in this vector
sd_eta2 <- sqrt(c(0.339))

# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(log(2.45))      # CL
    theta2 <- c(log(154))       # V
    theta3 <- c(0.542)     # CLCR on Cl
    theta4 <- c(0.0657)    # Proportional error
    
    eta1 ~ c(0.174)      # CL
    eta2 ~ c(0.339)      # V
  })
  model({
    cl <- exp(theta1 + eta1) * (CLCR/56.28)**theta3
    v <- exp(theta2 + eta2)
    
    #ke <- theta1 / theta3
    ke = cl/v
    d/dt(center) = - ke * center
    cp = center / v
    cp ~ prop(theta4)
  })
}
