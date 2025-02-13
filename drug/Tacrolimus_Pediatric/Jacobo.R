# Tacrolimus population pk model

# PK model description ----------------------------------------------
des_intro <- "Tacrolimus Oral model for paediatric renal transplant recipients"
des_notes <- c("Most patients received a standardized immunosuppressive regimen including tacrolimus, mycophenolate mofetil and prednisone (n = 44), whereas 9 received tacrolimus and mycophenolate mofetil only.",
               "<br>",
               " The twice daily tacrolimus dose (every 12 h) adjustment was performed trying to maintain blood concentrations between 5 and 10 ng/ml")
des_comp <- c("depot","central", "peripheral")
des_cov <- c("CYP3A5","FORM") # Limustin : 1 / unknown formula : 0 

des_params <- c("- V: volume of distritubtion (Tacrolimus)","<br>",
                "- Cl: clearance (Tacrolimus)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("CYP3A5","FORM")
mod_lcov = c("CYP3A5","FORM") # covariates with dropdown list
mod_lcov_value <- list(CYP3A5 = c('1*3*'=1,'1*1*'=0), FORM = c('Limustin' =1, 'Unknown' =0))
mod_cov_abbr <- c("Genotype of CYP3A5","Formulation of drug")

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

sd_eta <- sqrt(c(0.36, 0.60, 0.37, 0.34)) # put sd^2 value in this vector



# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(log(0.52))    # Ka
    theta13 <- c(-0.76)    # formulation on Ka (Limustin)
    theta14 <- c(-0.51)    # formulation on Ka (Unknown Formulation)
    
    theta2 <- c(log(24.16))     # V/F (Central)
   
    theta3 <- c(11.98)    # CL/F
    theta8 <- c(0.5)      # CYP3A5
    theta9 <- c(0.93)     # CYP3A5
    
    theta4 <- c(32.49)    # Q/F
    
    theta5 <- c(383.5)     # Vt/F (Peripheral)
    
    theta6 <- c(0.39)     # Tlag
   
    theta10 <- c(-0.3)    # FDTOT (effect of the tacrolimus total dose on F)
    theta11 <- c(log(0.47))   # FFOR (Limustin)
    theta15 <- c(0.12)    # Additive error
    
    eta1 ~ c(log(0.36))         # Ka
    eta2 ~ c(log(0.60))         # Vc/F
    eta3 ~ c(0.37)         # F
    # eta4 ~ c(0.34)         # RE
    
  })
  model({
    # TVKA <- theta1 * ((1+ (theta13*FORM) + (theta14 * (1-FORM))))
    ka <- exp(theta1 + eta1) * ((1+ (theta13*FORM) + (theta14 * (1-FORM))))
    
    #FDTOT <- exp((theta10)*(DOSE/1000 - 2))   # Dose가 걸려있어서 3000ng 투여한다는 가정으로 수식 작성
    #FFOR <- theta11
    #TVF <- 100 * FDTOT * FFOR
    #BA <- TVF * exp(eta3)
    BA <- exp((theta10)*(DOSE/1000 - 2) + eta3 + theta11) * 100 
    
    
    
    TVCL <- theta3 * (1+ ((theta8*CYP3A5) + (theta9 * (1-CYP3A5)))) 
    cl <- TVCL / BA
    
    # TVVc <- theta2
    # Vc <- TVVc / BA
    v1 <- exp(theta2 + eta2) / BA
    
    q <- theta4
    v2 <- theta5
    tlag <- theta6 

    #ke <- theta1 / theta2
    ke = cl/v1
    k12 = q/v1
    k21 = q/v2
    
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot -k12 * center + k21 * peri - ke * center
    d/dt(peri) = k12 * center - k21 * peri
    
    lag(depot) = exp(tlag)
    
    cp = center / v1
    cp ~ add(theta15) # Additive error
  })
}
