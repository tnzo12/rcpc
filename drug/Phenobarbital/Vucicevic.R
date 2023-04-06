# phenobarbital population pk model

# PK model description ----------------------------------------------
des_intro <- "Phenobarbital po model for adults"
des_notes <- c("- measurement time is recommended to be matched with the first dose",
               "<br>",
               "- Detailed tracking of age is recommend to reflect physiological changes in clearance and volume of distribution")
des_comp <- "depot, central"
des_cov <- "Post natal age(days), birthweight(kg), actual bodyweight(kg)"

des_params <- c("- V: volume of distritubtion (Phenobarbital)","<br>",
                "- Cl: clearance (Phenobarbital)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("DVPA", "WT")
mod_cov_abbr <- c("median of daily dose of valporic acid", "Weight(kg)")

mod_route <- c("PO")

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
  PO = 1,
  SDC = 2,
  NONE = 10
)

# Inter-individually Variable parameters ----------------------------
est_eta <-c('L/h'='cl',
            'L'='v',
            '1/h'='ka',
            '1/h'='ke')

sd_eta <- sqrt(0.199) # put sd^2 value in this vector

# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(log(0.314))     # CL
    theta2 <- c(-0.248)         # CL, DVPA
    theta3 <- c(3)              # ka
    theta4 <- c(0.147)          # Proportional error

    eta1 ~ c(0.199)             # Cl
  })
  
  model({
    # ---- FIXED AND RANDOM EFFECTS ----
    cl <- (1 + theta2 * (DVPA/1000)) * exp(theta1 + eta1)
    v <- 0.6 * WT
    ke = cl/v
    
    ka = theta3
    
    d/dt(depot) = - ka * depot
    d/dt(center) = ka * depot - ke * center
    cp = center / v
    cp ~ prop(theta4)
  })
}

