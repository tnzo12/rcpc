# Tacrolimus population pk model

# PK model description ----------------------------------------------
des_intro <- " As there are no pharmacokinetic data available in pediatric kidney transplant recipients, the aims of this study were to develop a population pharmacokinetic model of tacrolimusPR in pediatric and adolescent kidney transplant recipients and to identify covariates that have a significant impacts on tacrolimusPR pharmacokinetics, including CYP3A5 polymorphism."
des_notes <- c("Mycophenolate mofetil, MMF and mycophenolic acid, MPA doses were adjusted to maintain AUC between 30 and 60 h*mg/L",
               "<br>",
               "A limited number of blood samples were obtained before, 1, 2, 3, 6, 12, 16 and 24 hours after drug intake")
des_comp <- c("central",",","depot")
des_cov <- c("Body weight","Genotype of CYP3A5") # Strict 

des_params <- c("- V: volume of distritubtion (Tacrolimus)","<br>",
                "- Cl: clearance (Tacrolimus)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("WT", "CYP3A5")
mod_lcov = c("CYP3A5")        # covariates with dropdown list
mod_lcov_value <- list(CYP3A5 = c('1*3*'=1,'3*3*'=0))
mod_cov_abbr <- c("Body weight", "Genotype of CYP3A5")

mod_route <- c("Oral")

# plot option -------------------------------------------------------
pk <- "cp"
pk_obs <- "SDC"
pk_color <- '#FF6666'
pk_x_label <- "Time (hours)"
pk_y_label <- "Tacrolimus Conc. (ug/L)"


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

sd_eta <- sqrt(c(1.09,0.5,0.34)) # put sd^2 value in this vector

# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(log(1100))          # V/F
    theta2 <- c(log(30.6))          # CL/F
    theta3 <- c(1.66)          # CYP3A5
    theta4 <- c(22.1)          # Exponential error
    
    theta5 <- c(0.872)         # Lag-time
    theta6 <- c(log(8.34))          # Ka
    
    eta1 ~ c(1.09)              # Ka
    eta2 ~ c(0.5)             # V/F
    eta3 ~ c(0.34)             # CL/F
  })
  model({
    Tlag <- theta5
    TVKA <- theta6
    TVV <- theta1 * (WT/70)
    TVCL <- theta2 * ((WT/70)**0.75) * (theta3**CYP3A5)
  
    #cl <- TVCL + exp(eta3)
    #v <- TVV + exp(eta2)
    #ka <- TVKA + exp(eta1)
    
    cl <- exp(theta2 + eta3) * ((WT/70)**0.75) * (theta3**CYP3A5)
    v <- exp(theta1 + eta2) * (WT/70)
    ka <- exp(theta6 + eta1)

    #ke <- theta1 / theta2
    ke = cl/v
    
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - ke * center
    
    lag(depot) = exp(Tlag)
    
    cp = center / v
    cp ~ prop(theta4)
  })
}
