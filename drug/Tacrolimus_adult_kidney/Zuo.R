# Tacrolimus population pk model

# PK model description ----------------------------------------------
des_intro <- "Tacrolimus po model for adult kidney transplant recipients"
des_notes <- c("- measurement time is recommended to be matched with the first dose",
               "<br>",
               "- Detailed tracking of DOT and AST is recommend to reflect physiological changes in clearance and volume of distribution")
des_comp <- "depot, center"
des_cov <- "CYP3A5_11, CYP3A5_1g, CYP3A5_gg, CYP3A4_33, CYP3A4_13, CYP3A4_11, HCT, POD, WT" # Strict 

des_params <- c("- V: volume of distritubtion (Tacrolimus)","<br>",
                "- Cl: clearance (Tacrolimus)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("CYP3A4_11","CYP3A4_1g","CYP3A4_gg", "CYP3A5_33","CYP3A5_13","CYP3A5_11","HCT", "POD", "WT")
mod_lcov = NULL # covariates with dropdown list
mod_lcov_value <- NULL
mod_cov_abbr <- c("1 for mutation, otherwise no","1 for mutation, otherwise no","1 for mutation, otherwise no","1 for mutation, otherwise no","1 for mutation, otherwise no","1 for mutation, otherwise no","hematocrit","post operation days", "weight")

mod_route <- c("PO")

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
      theta1 <- c(log(26.6))       # CL/F
      theta2 <- c(log(1020))        # V/F
      theta3 <- c(1.47)            # Additive error
      theta4 <- c(0.0384)          # Proportional error 
      
      eta1 ~ c(0.1784)          # CL/F
      eta2 ~ c(0.2693)          # V/F
    })
    
    model({
      ka <- 3.09

      cl <- exp(theta1 + eta1) * (HCT/27.0)**-0.45 * (1.21*(CYP3A5_11+CYP3A5_13)*(CYP3A4_1g+CYP3A4_gg) + 0.982*(CYP3A5_11+CYP3A5_13)*CYP3A4_11 + 0.77 * CYP3A5_33*(CYP3A4_1g+CYP3A4_gg) + 0.577 * CYP3A5_33*CYP3A4_11)
      v <- exp(theta2 + eta2)
      ke = cl/v
      
      d/dt(depot) = - ka * depot
      d/dt(center) = ka * depot - ke * center
      cp = center / v
      cp ~ add(theta3) + prop(theta4)
    })
  }
  