# Pyrazinamide population pk model

# PK model description ----------------------------------------------
des_intro <- c("Vancomycin iv model for Chinese adult in-house patients", "<br>",
               "model is built with patients who had clinical evidence and had a vancomycin treatment period of no less than 5 days", "<br>")
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
mod_cov_abbr <- c("Creatinin clearance", "Age(Data were collected from patients aged 49-74 years)")

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

sd_eta <- sqrt(c(0.2385, 0.1859)) # put sd^2 value in this vector

# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(log(3.84))   # CL
    theta2 <- c(log(45.1))   # V
    theta3 <- c(0.0559)      # Proportional error
    eta1 ~ c(0.2385)         # Cl
    eta2 ~ c(0.1859)         # V
  })
  model({
    cl <- (CLCR/86)**0.519 * exp(theta1 + eta1)
    v <- (AGE/61)**0.452 * exp(theta2 + eta2) 
    ke = cl/v
    d/dt(center) = - ke * center
    cp = center / v
    cp ~ prop(theta3)
  })
}

