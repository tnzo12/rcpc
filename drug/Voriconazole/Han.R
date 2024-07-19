# Voriconazole population pk model

# PK model description ----------------------------------------------
des_intro <- c("Voriconazole po model for Liver transplant recipients.", 
               "<br>", 
               "Data were obtained from 13 patients", 
               "<br>",
               "All study participants received voriconazole twice daily as part  of prophylactic regimen(Vfend 200mg tablets) ")
des_notes <- c("- measurement time is recommended to be matched with the first dose",
               "<br>",
               "- Detailed tracking of POT and INR is recommend to reflect physiological changes in clearance and volume of distribution")
des_comp <- "depot, center"
des_cov <- "POT, INR" # Strict 

des_params <- c("- V: volume of distritubtion (Voriconazole)","<br>",
                "- Cl: clearance (Voriconazole)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("POT", "INR")
mod_lcov = NULL # covariates with dropdown list
mod_lcov_value <- NULL
mod_cov_abbr <- c("Post Operative Time(h)", "International Normalized Ratio")

mod_route <- c("PO")

# plot option -------------------------------------------------------
pk <- "cp"
pk_obs <- "SDC"
pk_color <- '#FF6666'
pk_x_label <- "Time (hours)"
pk_y_label <- "Voriconazole Conc. (ug/L)"

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


sd_eta <- sqrt(c(1.989, 2.108, 2.242, 2.044)) # put sd^2 value in this vector

# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(log(10.6))       # CL/F
    theta2 <- c(log(776))        # V/F
    theta3 <- c(log(316))            # Ka
    theta4 <- c(0.817)     # tlag 
    theta5 <- c(-1.3)      # 
    theta6 <- c(0.084)     #
    theta7 <- c(-3.92)     #
    theta8 <- c(-1.51)     #
    theta9 <- c(10.9)      #
    theta10 <- c(0.43)     # Proportional error
    theta11 <- c(0.3)      # Additive error
    
    
    
    eta1 ~ c(0.643)          # CL/F
    eta2 ~ c(0.781)          # V/F
    eta3 ~ c(0.961)          # Ka
    eta4 ~ c(0.709)          # tlag
  })
  model({
    tlag <- exp(theta4 + eta4) * ((theta6)^(POT/86.77))
    ka <- exp(theta3 + eta3) * ((POT/86.77)^theta9)
    
    cl <-  (exp(theta1+eta1)-theta7 * (INR-1.29)/0.17) * ((POT/86.77)^theta8) 
    v <- exp(theta2 + eta2) * exp(theta5 * (POT/86.77))
    ke = cl/v
    
    
    d/dt(depot) = - ka * depot
    d/dt(center) = ka * depot - ke * center
   
    lag(depot) = exp(tlag)
    
    cp = center / v
    cp ~ prop(theta10) + add(theta11)
  })
}
