# Tacrolimus population pk model

# PK model description ----------------------------------------------
des_intro <- c("Tacrolimus po model for adult kidney transplant recipients", "<br>", "Data were obtained from 161 patients treated at a single center, from 0 to 95 POD (median value:9 days)")
des_notes <- c("- measurement time is recommended to be matched with the first dose",
               "<br>",
               "- Detailed tracking of DOT and AST is recommend to reflect physiological changes in clearance and volume of distribution")
des_comp <- "depot, center"
des_cov <- "CYP3A5, CYP3A4, HCT, POD, WT" # Strict 

des_params <- c("- V: volume of distritubtion (Tacrolimus)","<br>",
                "- Cl: clearance (Tacrolimus)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration(ng/ml)")

mod_cov <- c("CYP3A4", "CYP3A5","HCT", "POD", "WT")
mod_lcov = c("CYP3A4", "CYP3A5") # covariates with dropdown list
mod_lcov_value <- list(CYP3A4=c('*1G/*1G'=0, '*1/*1G'=0, '*1/*1'=1),
                       CYP3A5=c('*3/*3'=0, '*1/*3'=1, '*1/*1'=1))
mod_cov_abbr <- c("CYP3A4*1G(also called CYP3A4*18B) indicates G2030A polymorphism","CYP3A5*3 indicates A6986G polymorphism", "hematocrit(%)","post operation days", "weight(kg)")

mod_route <- c("PO")

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
    PO = 1,
    SDC = 2,
    NONE = 10)
  
  # Inter-individually Variable parameters ----------------------------
  est_eta <-c('L/hr'='cl',
              'L'='v')
  
  
  sd_eta <- sqrt(c(0.1784, 0.2693)) # put sd^2 value in this vector
  
  # Model file for estimation -----------------------------------------
  f <- function() {
    ini({
      theta1 <- log(26.6)       # CL/F
      theta2 <- log(1020)        # V/F
      theta3 <- c(1.47)            # Additive error
      theta4 <- c(0.0384)          # Proportional error 
      
      eta1 ~ c(0.1784)          # CL/F
      eta2 ~ c(0.2693)          # V/F
    })
    
    model({
      ka <- 3.09

      cl <- exp(theta1 + eta1) * (HCT/27.0)**-0.45 * (1.21* (CYP3A5*(1-CYP3A4)) + 0.982* (CYP3A5*CYP3A4) + 0.77* (1-CYP3A5)*(1-CYP3A4) + 0.577 * ((1-CYP3A5)*CYP3A4))
      v <- exp(theta2 + eta2)
      ke = cl/v
      
      d/dt(depot) = - ka * depot
      d/dt(center) = ka * depot - ke * center
      cp = center / v
      cp ~ add(theta3) + prop(theta4)
    })
  }
  