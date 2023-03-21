# Rifampin population pk model

# PK model description ----------------------------------------------
des_intro <- "Rifampin po model for patients with pulmonary tuberculosis"
des_notes <- c("- measurement time is recommended to be matched with the first dose",
               "<br>",
               "- Detailed tracking of AGE is recommend to reflect physiological changes in clearance and volume of distribution")
des_comp <- "depot, central"
des_cov <- "Age" # Strict 

des_params <- c("- V: volume of distritubtion (isoniazid)","<br>",
                "- Cl: clearance (isoniazid)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("AGE","WT")
mod_cov_abbr <- c("Age by year","Weight")

mod_route <- c("PO")

# plot option -------------------------------------------------------
pk <- "ipred"
pk_obs <- "SDC"
pk_color <- '#FF6666'
pk_x_label <- "Time (hours)"
pk_y_label <- "Rifampin Conc.(mg/L)"

# model scheme image ------------------------------------------------
scheme_image <- "https://els-jbs-prod-cdn.jbs.elsevierhealth.com/cms/attachment/e9f4a16a-a113-4770-86c4-614b4895f30c/fx1.jpg"

# Compartment designation -------------------------------------------
mod_comp <- c(
  PO = 1,
  SDC = 2,
  NONE = 10
)

# Inter-individually Variable parameters ----------------------------
est_eta <-c('L/hr'='cl',
            'L'='v')

sd_eta <- sqrt(c(0.1965, 0.5675)) # put sd^2 value in this vector

# Model file for estimation -----------------------------------------

f <- function() {
  ini({
    theta1 <- c(log(23.9))    # 1. CL
    theta2 <- c(0.517)        # 2. AGE
    theta3 <- c(log(44.6))    # 3. V
    theta4 <- c(log(0.236))   # 4. Ka
    theta5 <- c(0.517)        # 5. f(bioavailability)
    theta6 <- c(0.207)        # 6. Proportional error
    eta1 ~ c(0.1965)         # 1. Cl
    eta2 ~ c(0.5675)         # 2. V
  })
  model({
    if(AGE>=14){F = 1 } 
    else {F = theta5 }
    cl <- (WT/70)**0.75 * (AGE/24.856)**theta2 * exp(theta1 + eta1) 
    v  <- (WT/70) * exp(theta3 + eta2) 
    ka <- exp(theta4) 
    ke = cl/v
    d/dt(depot) = -ka * depot
    d/dt(center) = F * ka * depot - ke * center
    cp = center / v
    cp ~ prop(theta6)
  })
} 
