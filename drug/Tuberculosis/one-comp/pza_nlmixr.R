# Pyrazinamide population pk model

# PK model description ----------------------------------------------
des_intro <- "Pyrazinamide po model for patients with pulmonary tuberculosis"
des_notes <- c("- measurement time is recommended to be matched with the first dose",
               "<br>",
               "- Detailed tracking of WT and SEX is recommend to reflect physiological changes in clearance and volume of distribution")
des_comp <- "depot, central"
des_cov <- "Weight, Sex" # Strict 

des_params <- c("- V: volume of distritubtion (Pyrazinamide)","<br>",
                "- Cl: clearance (Pyrazinamide)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("WT", "SEX")
mod_cov_abbr <- c("Weight")

mod_route <- c("PO")

# plot option -------------------------------------------------------
pk <- "cp"
pk_obs <- "SDC"
pk_color <- '#FF6666'
pk_x_label <- "Time (hours)"
pk_y_label <- "Pyrazinamide Conc. (mg/L)"

# model scheme image ------------------------------------------------
scheme_image <- "https://els-jbs-prod-cdn.jbs.elsevierhealth.com/cms/attachment/e9f4a16a-a113-4770-86c4-614b4895f30c/fx1.jpg"

# Compartment designation -------------------------------------------
mod_comp <- c(
  PO = 1,
  SDC = 2,
  NONE = 10
)

# Inter-individually Variable parameters ----------------------------
est_eta <-c('L/h'='cl',
            'L'='v',
            '1/hr'='ka')

sd_eta <- sqrt(c(0.0515, 0.0119, 1.7647)) # put sd^2 value in this vector

# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(log(5.06))    # CL
    theta2 <- c(log(46.5))    # V
    theta3 <- c(log(3.63))    # Ka
    theta4 <- c(0.94)         # Additive error
    theta5 <- c(0.01)         # Proportional error
    eta1 ~ c(0.0515)         # Cl
    eta2 ~ c(0.0119)         # V
    eta3 ~ c(1.7647)         # Ka
  })
  model({
    cl <- (WT/70)**0.75 * exp(theta1 + eta1)
    v <- 1.16**SEX * (WT/70) * exp(theta1 + eta2) 
    ka <- exp(theta3 + eta3)   
    #ke <- theta1 / theta2
    ke = cl/v
    d/dt(depot) = - ka * depot
    d/dt(center) = ka * depot - ke * center
    cp = center / theta2
    cp ~ add(theta4)+prop(theta5)
  })
}
