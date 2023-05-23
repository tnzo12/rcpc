# Phenobarbital population pk model

# PK model description ----------------------------------------------
des_intro <- "Phenobarbital Oral model for neonates who were treated for seizures with phenobarbital"
des_notes <- c("- Two groups of infants. One is who get the therapeutic hypothermia and the other is not. The PK of the phenobarbital depends on whether TH is performed or not.",
               "<br>",
               "-  one-compartment linear PK model with an exponential error model for interindividual variability of CL and combined proportional and additive error model for the residual variability.")
des_comp <- c("central", "depot")
des_cov <- c("WT", "PNA") # Strict 

des_params <- c("- V: volume of distritubtion (Phenobarbital)","<br>",
                "- Cl: clearance (Phenobarbital)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("WT","PNA")
mod_lcov = NULL # covariates with dropdown list
mod_lcov_value <- NULL
mod_cov_abbr <- c("Body Weight","Post natal age")

mod_route <- c("Oral")

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
  Oral = 1,
  SDC = 2,
  NONE = 10
)

# Inter-individually Variable parameters ----------------------------
est_eta <-c('L/h'='cl',
            'L'='v')

sd_eta <- sqrt(c(0.175)) # put sd^2 value in this vector

# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(0.672)         # CL
    theta2 <- c(64.9)          # V
    theta3 <- c(22.1)          # PNAC50
    theta4 <- c(6.12)          # Additive error
    theta5 <- c(0.197)         # Proportional error
    
    eta1 ~ c(0.175)        
  })
  model({
    PNAC50 <- theta3
    CLCOV <- PNA/(PNAC50 + PNA)
    TVCL <- theta1 * (WT/70)**(0.75) * CLCOV
    cl <- TVCL * exp(eta1)
    v <- theta2 * (WT/70)
    
    #ke <- theta1 / theta2
    ke = cl/v
    ka = 50                 # FIX
    d/dt(center) = ka * depot - ke * center
    d/dt(depot) = -ka * depot
    cp = center / v
    cp ~ prop(theta5) + add(theta4)
  })
}
