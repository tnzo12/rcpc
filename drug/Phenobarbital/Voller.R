# phenobarbital population pk model

# PK model description ----------------------------------------------
des_intro <- "Phenobarbital IV/PO model for neonates"
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

mod_cov <- c("PNA", "BBW", "WT")
mod_cov_abbr <- c("Post natal age(days)", "birthweight(kg)", "actual bodyweight(kg)")

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
            '1/h'='ka',
            '1/h'='ke')

sd_eta <- sqrt(c(0.086, 0.049)) # put sd^2 value in this vector

# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(log(0.0091))  # CL
    theta2 <- c(log(2.38))    # V
    theta3 <- c(0.594)        # F
    theta4 <- c(50)           # KA
    theta5 <- c(0.0533)       # CL- PNA
    theta6 <- c(0.369)        # CL- WT
    theta7 <- c(0.309)        # V - ABW
    theta8 <- c(0.0258)       # Proportional error

    eta1 ~ c(0.086)           # Cl
    eta2 ~ c(0.049)           # V
  })
  
  model({
    # ---- FIXED AND RANDOM EFFECTS ----
    fdepot <- theta3
    cl <- (1+theta5*(PNA-4.5)) * (1+theta6*(BBW-2.6)) * exp(theta1 + eta1)
    v <- (1+theta7*(WT-2.7)) * exp(theta2 + eta2) 
    ke = cl/v
    
    ka <- theta4
    d/dt(depot) = - ka * depot
    d/dt(center) = ka * depot - ke * center
    cp = center / v
    cp ~ prop(theta8)
  })
}

