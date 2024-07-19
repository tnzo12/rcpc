# Voriconazole population pk model
# If절 사용. Value 그래프 없어서 그래프 그리기 힘듬. 실패


# PK model description ----------------------------------------------
des_intro <- c("Voriconazole po model for Renal transplant recipients.", 
               "<br>", 
               "Data were obtained from 105 patients. Dosing regimes were administered differently depending on the type of CYP2C19. ", 
               "<br>",
               " Patients who are CYP2C19 poor metabolizer received 250mg twice daily for oral. ")
des_notes <- c("- measurement time is recommended to be matched with the first dose",
               "<br>",
               "- Detailed tracking of CYP2C19, WT and POT  is recommend to reflect physiological changes in clearance and volume of distribution",
               "<br>",
               "- POT is Post Operative Time. POT1 < 1 month, POT2 1~6 months, POT3 6~12 monts, POT4 > 1 year.")

des_comp <- "depot, center"
des_cov <- "CYP2C19, WT" # Strict 

des_params <- c("- V: volume of distritubtion (Voriconazole)","<br>",
                "- Cl: clearance (Voriconazole)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("CYP2C19", "WT")
mod_lcov = c("CYP2C19") # covariates with dropdown list
mod_lcov_value <- list(CYP2C19 = c('PM'="PM", 'IM'="IM", 'EM'="EM"))

mod_cov_abbr <- c("Genotyping of CYP2C19", "Weight")

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


sd_eta <- sqrt(c(0.39, 0.42)) # put sd^2 value in this vector

# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(log(169.27))           # V
    theta2 <- c(log(2.88))          # CL
    #theta3 <- c(0.58)                 # F
    theta4 <- c(1.30)               # 
    theta5 <- c(0.45)                #
    theta6 <- c(0.80)               # 
    #theta7 <- c(0.43)               # 
    #theta8 <- c(0.57)               # 
    #theta9 <- c(0.57)               #
    theta10 <- c(0.57)              # Proportional Error
    
    eta1 ~ c(0.39)                  # CL
    eta2 ~ c(0.42)                  # V
    #eta3 ~ c(0.22)                  # F
  })
  model({
    ka <- 1.1
    
    if (CYP2C19 == "PM") {
    cl <- exp(theta1 + eta1)
    }
    else if (CYP2C19 == "IM") {
    cl <- exp(theta1 + eta1) * exp(theta5)
    }
    else { 
    cl <- exp(theta1 + eta1) * exp(theta6)
    }
    
    #cl <-  (exp(theta1+eta1)) * exp(CYP2C19) * exp(theta5) * exp(theta6)
    v <- (exp(theta2 + eta2)) * ((WT/56.1) ^ theta4)
    ke = cl/v
    
    
    d/dt(depot) = - ka * depot
    d/dt(center) = ka * depot - ke * center
    
    cp = center / v
    cp ~ prop(theta10)
  })
}
