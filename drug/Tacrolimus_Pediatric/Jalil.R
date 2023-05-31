# Tacrolimus population pk model
# TPT를 POD로 통일함 (days단위)

# PK model description ----------------------------------------------
des_intro <- "Tacrolimus Oral model for 43 children who underwent liver transplantation"
des_notes <- c("Tacrolimus pre-dose steady-state blood concentrations collected approximately 12 h after a tacrolimus dose were recorded retrospectively for all samples taken over 1 year post-transplantation.",
               "<br>",
               "The immunosuppressive protocol consisted mainly of tacrolimus in combination with low dose steroids, with tacrolimus being administered orally in the form of capsules or as an oral suspension.")
des_comp <- c("central", "depot")
des_cov <- c("WT","CYP3A5","POD") # Strict 

des_params <- c("- V: volume of distritubtion (Tacrolimus)","<br>",
                "- Cl: clearance (Tacrolimus)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("WT","CYP3A5","POD")
mod_lcov = c("CYP3A5") # covariates with dropdown list
mod_lcov_value <- list(CYP3A5 = c('1*1*'= 1,'1*3*'=1,'3*3*'=0))
mod_cov_abbr <- c("Body weight","Genotype of CYP3A5","Postoperative days")

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

sd_eta <- sqrt(c(0.16)) # put sd^2 value in this vector

# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(log(12.92))        # CL/F
    theta2 <- c(-0.00158)     # POD
    theta3 <- c(0.4282)       # CYP3A5 effect on clearance
    
    theta4 <- c(0.125)        # Proportional error
   
    theta5 <- c(4.5)          # Ka
    theta6 <- c(30)           # V/F (FIX)
    
    eta1 ~ c(0.16)           # CL/F
    
  })
  model({
    ka <- theta5
    
    # TVCL <- theta1 * ((WT/13.2)**0.75) * exp(theta2 * POD) * exp(CYP3A5 * (theta3))
    cl <- exp(theta1 + eta1) * ((WT/13.2)**0.75) * exp(theta2 * POD) * exp(CYP3A5 * (theta3))
    v <- theta6
    # cl <- theta1 * exp(eta1)
    
    #ke <- theta1 / theta2
    ke = cl/v
    
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - ke * center
    cp = center / v
    cp ~ prop(theta4) 
  })
}
