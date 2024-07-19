# Voriconazole population pk model

# PK model description ----------------------------------------------
des_intro <- c("Voriconazole PO model in patients with liver dysfunction.", 
               "<br>", 
               "Data were obtained from 51 patients", 
               "<br>",
               " Patients with mild to moderate liver dysfunction received 400mg twice daily for oral ")
des_notes <- c("- measurement time is recommended to be matched with the first dose",
               "<br>",
               "- Detailed tracking of PLT, WT and TBIL  is recommend to reflect physiological changes in clearance and volume of distribution")
des_comp <- "depot, center"
des_cov <- "PLT, WT, TBIL" # Strict 

des_params <- c("- V: volume of distritubtion (Voriconazole)","<br>",
                "- Cl: clearance (Voriconazole)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("PLT", "WT", "TBIL")
mod_lcov = NULL # covariates with dropdown list
mod_lcov_value <- NULL
mod_cov_abbr <- c("Platelets", "Weight", "Total Bilirubin")

mod_route <- c("PO")

# plot option -------------------------------------------------------
pk <- "cp"
pk_obs <- "SDC"
pk_color <- '#FF6666'
pk_x_label <- "Time (hours)"
pk_y_label <- "Voriconazole Conc. (mg/L)"

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
  NONE = 10)

# Inter-individually Variable parameters ----------------------------
est_eta <-c('L/hr'='cl',
            'L'='v')


sd_eta <- sqrt(c(1.716, 1.602)) # put sd^2 value in this vector

# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(log(0.88))           # CL
    theta2 <- c(log(148.8))          # V
    theta3 <- c(1.1)                 # Ka
    #theta4 <- c(0.884)               # F
    theta5 <- c(0.32)                # PLT on CL
    theta6 <- c(-0.57)               # TBIL on CL
    theta7 <- c(1.43)               # WT on V
    theta8 <- c(0.421)               # Proportional error
   
    eta1 ~ c(0.407)                  # CL
    eta2 ~ c(0.337)                  # V
  })
  model({
  
    ka <- theta3
    
    cl <-  (exp(theta1+eta1)) * ((PLT/65) ^ theta5) * ((TBIL/314)^theta6)
    v <- (exp(theta2 + eta2)) * ((WT/58) ^ theta7)
    ke = cl/v
    
    
    d/dt(depot) = - ka * depot
    d/dt(center) = ka * depot - ke * center
    
    cp = center / v
    cp ~ prop(theta8)
  })
}
