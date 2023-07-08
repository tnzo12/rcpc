# phenobarbital population pk model

# PK model description ----------------------------------------------
des_intro <- "Phenobarbital IV/po model for neonates and infants"
des_notes <- c("- measurement time is recommended to be matched with the first dose",
               "<br>",
               "- Detailed tracking of age is recommend to reflect physiological changes in clearance and volume of distribution")
des_comp <- "central"
des_cov <- "weight"

des_params <- c("- V: volume of distritubtion (Phenobarbital)","<br>",
                "- Cl: clearance (Phenobarbital)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("WT")
mod_lcov = NULL # covariates with dropdown list
mod_lcov_value <- NULL
mod_cov_abbr <- c("weight")

mod_route <- c("IV", "PO")

# plot option -------------------------------------------------------
pk <- "cp"
pk_obs <- "SDC"
pk_color <- '#FF6666'
pk_x_label <- "Time (hours)"
pk_y_label <- "Phenobarbital Conc. (mg/L)"

pd <- NA
pd_obs <- NA
pd_color <- NA
pd_x_label <- NA
pd_y_label <- NA

# model scheme image ------------------------------------------------
scheme_image <- "https://els-jbs-prod-cdn.jbs.elsevierhealth.com/cms/attachment/e9f4a16a-a113-4770-86c4-614b4895f30c/fx1.jpg"

# Compartment designation -------------------------------------------
mod_comp <- c(
  PO = 1, IV=2,
  SDC = 2,
  NONE = 10
)

# Inter-individually Variable parameters ----------------------------
est_eta <-c('L/h'='cl',
            'L'='v',
            'unitless'='fdepot')

sd_eta <- sqrt(c(0.027, 0.219, 0.144)) # put sd^2 value in this vector

# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(log(0.372))  # CL
    theta2 <- c(log(62.5))   # V
    theta3 <- c(log(0.89))   # F
    theta4 <- c(0.8)         # KA
    theta5 <- c(0.265)       # Additive error
    eta1 ~ c(0.027)          # Cl
    eta2 ~ c(0.219)          # V
    eta3 ~ c(0.144)          # F
  })
  
  model({
    # ---- FIXED AND RANDOM EFFECTS ----
    fdepot <- exp(theta3 + eta3)
    
    cl <-  (WT/70)**0.75 * exp(theta1 + eta1)
    v <- (WT/70) * exp(theta2 + eta2) 
    ke = cl/v
    
    ka <- theta4
    
    d/dt(depot) = - ka * depot
    d/dt(center) = ka * depot - ke * center
    cp = center / v
    cp ~ add(theta5)
  })
}

