# Tacrolimus population pk model

# PK model description ----------------------------------------------
des_intro <- "Tacrolimus Oral model for paediatric renal transplant recipients"
des_notes <- c("blood samples were collected from 46 children treated with tacrolimus over the first 6 weeks after renal transplantation.",
               "<br>",
               " All clinical values were collected from 24 h before transplantation until 6 weeks post-transplantation")
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
pk_y_label <- "Tacrolimus Conc. (mg/L)"

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
    theta2 <- c(0.56)    # Ka
    theta3 <- c(50.5)    # CL/F
    theta4 <- c(206)     # V1/F
    theta5 <- c(114)     # Q/F
    theta6 <- c(1520)    # V2/F
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
    eta5 ~ c(0.18)         # CL/F IOV
    eta6 ~ c(0.34)         # V2/F IOV
    
  })
  model({
    tlag <- theta1
    TVKA <- theta2
    ka <- TVKA * exp(eta1)
    
    WHTVCL <- theta3 * ((WT/70)**0.75) * CYP3A5 * exp(theta10 * DL) * ((eGFR/69)**theta9) # CYP3A5가 *3/*3이면 0, 아니면 1 / DL이 living이면 1, 아니면 0
    if (HCT<0.3) {
      TVCL <- WHTVCL * ((HCT/0.3)**theta11) 
    }
    else{
      
      TVCL <- WHTVCL 
    }
    cl <- TVCL * exp(eta2 + eta5)
    TVV1 <- theta4
    v1 <- TVV1 * exp(eta3)
    TVV2 <- theta6
    v2 <- TVV2 * exp(eta4 + eta6)
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
