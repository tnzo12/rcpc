# Tacrolimus population pk model
# v 값이 논문 상에는 27.2로 나오는데 extravalidation 논문에서는 17.2 * WT 로 나와있음


# PK model description ----------------------------------------------
des_intro <- "Patients received oral tacrolimus as part of a ‘‘triple’’ or ‘‘double’’ immunosuppressive regimen, including azathioprine and/or corticosteroids."
des_notes <- c("Tacrolimus trough concentrations were measured daily (before the morning dose) in the 1st month, twice weekly in the 3rd month, and weekly in the 12th month after transplantation.",
               "<br>",
               "Patients over 20kg got an initial oral dose of tacrolimus of 0.1mg/kg. Otherwise, patients got 0.15mg/kg.")
des_comp <- c("central", "depot")
des_cov <- c("Age","BSA","WT","BIL") # Strict 

des_params <- c("- V: volume of distritubtion (Tacrolimus)","<br>",
                "- Cl: clearance (Tacrolimus)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("WT","POD")
mod_lcov = NULL # covariates with dropdown list
mod_lcov_value <- NULL
mod_cov_abbr <- c("Body surface area","Postoperative days")

mod_route <- c("Oral")

# plot option -------------------------------------------------------
pk <- "cp"
pk_obs <- "SDC"
pk_color <- '#FF6666'
pk_x_label <- "Time (hours)"
pk_y_label <- "Tacrolimus Conc. (mg/L)"


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

sd_eta <- sqrt(c(0.6,0.51,0.77,0.28)) # put sd^2 value in this vector

# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(1.37)         # CL/Fmax
    theta2 <- c(0.148)        # CL/F50
    theta3 <- c(5.38)         # Tcl/F50
    theta4 <- c(17.2)         # V/F
    theta5 <- c(1.63)         # Additive error
    theta6 <- c(0.292)        # Proportional error
    
    theta7 <- c(4.48)         # Ka
    theta8 <- c(3.78)         # Gamma
    
    eta1 ~ c(0.6)           # CL/Fmax
    eta2 ~ c(0.51)          # CL/F50
    eta3 ~ c(0.77)          # V/F
    # eta4 ~ c(0.28)        # eps
  })
  model({
    ka <- theta7
    
    
    TVCLFmax <- theta1
    TVCLF0 <- theta2
    TVTCL <- theta3
    TVV <- theta4 * WT
    
    CLFmax <- TVCLFmax * exp(eta1)
    CLF0 <- TVCLF0 * exp(eta2)
    
    cl <- (CLF0 + (((CLFmax * (POD**theta8))) / ((TVTCL ** theta8) + (POD**theta8)))) * (WT**0.75)
    v <- TVV  * exp(eta3)
    
    #ke <- theta1 / theta2
    ke = cl/v
    ka = theta7  # FIX
    
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - ke * center
    cp = center / v
    cp ~ prop(theta6) + add(theta5)
  })
}
