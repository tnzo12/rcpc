# Tacrolimus population pk model

# PK model description ----------------------------------------------
des_intro <- "Tacrolimus Oral model for all children under 18 years who received a de novo liver transplantation"
des_notes <- c("Tac doses and biological data were collected daily from the day of transplantation up until day 14 and then at day 30, 60 and 90.",
               "<br>",
               " Patients were given oral Tac (Prograf®) from the day of transplantation, with basiliximab (Simulect®) intravenous induction on day 0 and day 4 ")
des_comp <- c("depot","central", "peripheral")
des_cov <- c("POD","AGE","DCYP3A5","FZLDne","WT","RCYP3A5","RABCB1","DCYP3A4","FZLDe") 

des_params <- c("- V: volume of distritubtion (Tacrolimus)","<br>",
                "- Cl: clearance (Tacrolimus)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("POD","AGE","DCYP3A5","FZLDne","WT","RCYP3A5","RABCB1","DCYP3A4","FZLDe")
mod_lcov = c("DCYP3A5","FZLDne","RCYP3A5","RABCB1","DCYP3A4","FZLDe") # covariates with dropdown list
mod_lcov_value <- list(DCYP3A5 = c('1*1*'=1,'1*3*'=1,'3*3*'=0), FZLDne = c('YES' =1, 'NO' =0), RCYP3A5 = c('1*1*'=1,'1*3*'=1,'3*3*'=0), RABCB1 = c('MUTATION' =1, 'NOT MUTATION' =0), DCYP3A4 = c('CYP3A4*1/*22'=1, 'CYP3A4*22/*22'=1, 'Other'=0), FZLDe = c('YES'=1, 'NO'=0))
mod_cov_abbr <- c("Postoperative days","Age", "Donor's Genotype of CYP3A5","Fluconazole patients use","Body weight","Recipient's Genotype of CYP3A5","Recipient's Genotype of ACB1","Donor's Genotype of CYP3A4","Fluconazole donor use")

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

sd_eta <- sqrt(c(0.27,0.36)) # put sd^2 value in this vector



# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(0.01)    # CL-DIG (Intestinal clearance)
    theta2 <- c(1.17)    # RCYP3A5 expression on CL-DIG
    theta3 <- c(0.98)    # RABCB 2677 GT/GA or TT/AA on CL-DIG
    # theta4 <- c(6)       # T50
    # theta5 <- c(10.9)    # CL-HEP (Hepatic clearance)
    theta6 <- c(4.5)     # Ka
    theta7 <- c(79)      # V1 (Central)
    theta8 <- c(100)     # V2 (Peripheral)
    theta9 <- c(105)     # Q
    theta10 <- c(0.46)   # WT on V1
    theta11 <- c(0.16)   # AGE on CL
    theta12 <- c(1.3)    # DCYP3A5 expression on CL-HEP
    theta13 <- c(0.71)   # DCYP3A4*22 on CL-HEP
    theta14 <- c(0.7)    # DCYP3A5 expression 
    theta15 <- c(0.4)    # DCYP3A5 non-expression
    
    theta16 <- c(0.1)    # Proportional error
    theta17 <- c(2.38)   # Additive error
    
    eta1 ~ c(0.27)         # CL(total)
    eta2 ~ c(0.36)         # V1

    
  })
  model({
    TVCL <- (theta1 * (theta2 ** RCYP3A5) * (theta3 ** RABCB1)) + (theta6 * ((AGE / 13)**theta11) * (theta12 ** DCYP3A5) * (theta13 ** DCYP3A4) * (theta14 ** FZLDe) * (theta15 ** FZLDne) * (POD/144 + POD))
    cl <- TVCL * exp(eta1)
    
    TVV1 <- theta7 * ((WT/10)**theta10)
    v1 <- TVV1 * exp(eta2)
    v2 <- theta8        # FIX
    q <- theta9 
    ka <- theta6
    
    #ke <- theta1 / theta2
    ke = cl/v1
    k12 = q/v1
    k21 = q/v2
    
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot -k12 * center + k21 * peri - ke * center
    d/dt(peri) = k12 * center - k21 * peri
    
    cp = center / v1
    cp ~ prop(theta16) + add(theta17) # Combined error
  })
}
