# Phenobarbital population pk model

# PK model description ----------------------------------------------
des_intro <- "Phenobarbital Oral model for neonates"
des_notes <- c("A one-compartment pharmacokinetic model with first-order elimination was used.",
               "<br>",
               "Covariates screened were current total bodyweight (TBW), gestational age, postnatal age (PNA), post-conceptional age, gender and neonates-infants clearance factor (serum concentration of phenobarbital; Conc).")
des_comp <- c("central", "depot")
des_cov <- c("CrCL","WT", "PNA") # Strict 

des_params <- c("- V: volume of distritubtion (Phenobarbital)","<br>",
                "- Cl: clearance (Phenobarbital)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("WT","PNA")
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

sd_eta <- sqrt(c(0.0676, 0.375)) # put sd^2 value in this vector

# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(5.95)         # CL-WT
    theta2 <- c(1.01)          # V-WT
    theta3 <- c(0.483)          # F
    theta4 <- c(1.41)          # CL-PNA
    theta5 <- c(-0.221)        # DV EXP
    theta6 <- c(0)             # Additive Error
    theta7 <- c(0.474)         # Proportional error
    
    eta1 ~ c(0.0676)
    eta2 ~ c(0.375)
  })
  model({
    CLWTKG <- theta1 * WT
    CLPNA  <- theta4 * PNA
    CLCOV  <- CLWTKG + CLPNA
    
    if (DV>=50) {
    TVCL <- CLCOV * (DV**theta5) / 1000
    }
    else{
    
    TVCL <- CLCOV /1000 
    }
    
    TVV  <- theta2* WT
    
    cl <- TVCL * (1+ eta1)
    v <- TVV  * (1+ eta2)
    
    F1   <- theta3
    #ke <- theta1 / theta2
    ke = cl/v
    ka = 50                 # FIX
    d/dt(center) = ka * depot - ke * center
    d/dt(depot) = -ka * depot
    cp = center / v
    cp ~ prop(theta7) + add(theta6)
  })
}
