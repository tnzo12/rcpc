# Tacrolimus population pk model
# 환자 그룹별로 dosing 다르게 들어감. 





# PK model description ----------------------------------------------
des_intro <- "Tacrolimus Oral centeral model for paediatric patients who underwent orthotopic liver transplantation"
des_notes <- c("The group of patients was divided by 2 group. Each of groups got different Tacrolimus regimen.",
               "<br>",
               "For the 4 patients group, postoperative immunosuppressive therapy consisted of a combination of steroids and tacrolimus.")
des_comp <- c("central", "depot")
des_cov <- c("Age","BSA","WT","BIL") # Strict 

des_params <- c("- V: volume of distritubtion (Tacrolimus)","<br>",
                "- Cl: clearance (Tacrolimus)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("AGE","BSA","WT","BIL")
mod_lcov = NULL # covariates with dropdown list
mod_lcov_value <- NULL
mod_cov_abbr <- c("Age","Body surface area","Body weight","Bilirubin")

mod_route <- c("Oral")

# plot option -------------------------------------------------------
pk <- "cp"
pk_obs <- "SDC"
pk_color <- '#FF6666'
pk_x_label <- "Time (hours)"
pk_y_label <- "Tacrolimus Conc. (ug/L)"

pd <- NA
pd_obs <- NA
pd_color <- NA
pd_x_label <- NA
pd_y_label <- NA

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

sd_eta <- sqrt(c(0.112,0.109,0.0579)) # put sd^2 value in this vector

# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(log(1.46))         # CL
    theta2 <- c(log(39.1))         # V
    theta3 <- c(log(0.197))        # F (Bioavailability)
    
    theta4 <- c(0.339)        # Age on CL
    theta5 <- c(4.57)         # BSA on V
    theta6 <- c(0.0887)       # WT on F
    theta7 <- c(1.61)         # BIL on F
    
    theta8 <- c(4.5)          # Ka
    theta9 <- c(5.79)         # Additive error
    
    eta1 ~ c(0.112)           # CL
    eta2 ~ c(0.109)           # V
    eta3 ~ c(0.0579)          # F
  })
  model({
    # TVCL <- theta1 * (1 + theta4 * (AGE-2.25))
    # TVV <- theta2 * (1 + theta5 * (BSA-0.49))
    # WTVF <- theta3 * (1 + theta6 * (WT-11.4))
    
    
    if (BIL>=200) {
    BILF <- theta7 
    }   
    else{
    BILF <- 1 
    }
    
    BA <- exp(theta3 + eta3)  * (1 + theta6 * (WT-11.4)) * BILF
    cl <- exp(theta1 + eta1) * (1 + theta4 * (AGE-2.25))
    v <- exp(theta2 + eta2) * (1 + theta5 * (BSA-0.49))
    # BA <- TVF * (1 + eta3)
    
    #ke <- theta1 / theta2
    ke = cl/v
    ka = theta8  # FIX
    
    d/dt(depot) = -ka * depot
    f(depot) = BA
    
    d/dt(center) = ka * depot - ke * center
    cp = center / v
    cp ~ add(theta9)
  })
}
