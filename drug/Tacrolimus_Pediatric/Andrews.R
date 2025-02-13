# Tacrolimus population pk model
# mu-referencing 안되는 parameter :  cl / 그래프 모양 이상하긴 함





# PK model description ----------------------------------------------
des_intro <- "A total of 722 blood samples were collected from 46 children treated with tacrolimus over the first 6 weeks after renal transplantation. patients aged younger than 18 years, who received a 476 L. M. Andrews et al.kidney from an ABO compatible living or a deceased donor, and were treated with tacrolimus as part of their initial immune suppressive regimen. All clinical values were collected from 24 h before transplantation until 6 weeks post-transplantation. All children received an initial tacrolimus dose of 0.3 mg/kg/day divided into two doses every 12 h"
des_notes <- c("Living donor compared with a deceased donor.",
               "<br>",
               "All patients were treated according to the TWIST protocol with basiliximab, tacrolimus, mycophenolic acid, and a 5-day course of glucocorticoids",
               "<br>",
               "Higher tacrolimus CL in kidneys from a deceased donor is probably caused by other unknown parameters that could not be tested as covariates and therefore cannot be corrected for.")
des_comp <- c("depot","central", "peripheral")
des_cov <- c("CYP3A5","WT","eGFR","Donor Living","HCT") # Limustin : 1 / unknown formula : 0 

des_params <- c("- V: volume of distritubtion (Tacrolimus)","<br>",
                "- Cl: clearance (Tacrolimus)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("CYP3A5","WT","DL","HCT","eGFR")
mod_lcov = c("CYP3A5","DL") # covariates with dropdown list
mod_lcov_value <- list(CYP3A5 = c('1*1*'=0,'1*3*'=1,'3*3*'=2), DL = c('YES' =1, 'NO' =0))

mod_cov_abbr <- c("Genotype of CYP3A5","Body weight","Donor Living", "Hematocrit","eGFR")

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
            'L'='v1' ,
            'L'= 'v2' ,
            'L/h' = 'q')

sd_eta <- sqrt(c(1.23, 0.25, 0.624, 0.7, 0.18, 0.34)) # put sd^2 value in this vector



# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(0.37)    # Tlag
    theta2 <- c(log(0.56))    # Ka
    theta3 <- c(log(50.5))    # CL/F
    theta4 <- c(log(206))     # V1/F
    theta5 <- c(114)     # Q/F
    theta6 <- c(log(1520))    # V2/F
    #theta7 <- c(1.04)    # CYP3A5 effect on CL (3*/3*)
    #theta8 <- c(1.98)    # CYP3A5 effect on CL (others)
    theta9 <- c(0.19)    # eGFR
    theta10 <- c(log(0.74))   # Donor living
    theta11 <- c(-0.44)  # HCT
    theta12 <- c(0.28)    # Additive error
    theta13 <- c(0.21)    # Proportional error
    
    eta1 ~ c(1.23)         # Ka IIV
    eta2 ~ c(0.25)         # CL/F IIV
    eta3 ~ c(0.624)        # V1/F IIV
    eta4 ~ c(0.7)          # V2/F IIV
    #eta5 ~ c(0.18)         # CL/F IOV
    #eta6 ~ c(0.34)         # V2/F IOV
    
  })
  model({
    tlag <- theta1
    
    ka <- exp(theta2 + eta1)
    
    # WHTVCL <- theta3 * ((WT/70)**0.75) * CYP3A5 * exp(theta10 * DL) * ((eGFR/69)**theta9) # CYP3A5가 *3/*3이면 0, 아니면 1 / DL이 living이면 1, 아니면 0
    #if (HCT<0.3) { TVCL <- WHTVCL * ((HCT/0.3)**theta11) }
    # else{TVCL <- WHTVCL}
    
    if (HCT < 0.3) {
      TVHCT <- ((HCT/0.3)**theta11)
    }
    else {
      TVHCT <- 1
    }
    
    cl <- exp(theta3 + eta2) * ((WT/70)**0.75) * CYP3A5 * exp(theta10 * DL) * ((eGFR/69)**theta9) * TVHCT
    
    
    
    # cl <- TVCL * exp(eta2)
    
    v1 <- exp(theta4 + eta3)
    v2 <- exp(theta6 + eta4)
    q <- theta5 
    
    
    #ke <- theta1 / theta2
    ke = cl/v1
    k12 = q/v1
    k21 = q/v2
    
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot -k12 * center + k21 * peri - ke * center
    d/dt(peri) = k12 * center - k21 * peri
    
    lag(depot) = exp(tlag)
    
    cp = center / v1
    cp ~ prop(theta13) + add(theta12) # Combined error
  })
}
