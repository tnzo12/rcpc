# Tacrolimus population pk model

# PK model description ----------------------------------------------
des_intro <- "Tacrolimus po model for adult kidney transplant recipients"
des_notes <- c("- measurement time is recommended to be matched with the first dose",
               "<br>",
               "- Detailed tracking of DOT and AST is recommend to reflect physiological changes in clearance and volume of distribution")
des_comp <- "depot, center, peri"
des_cov <- "CYP3A5_33, CYP3A5_66, CYP3A5_77, CYP3A5_36, CYP3A5_37, CYP3A5_67, CYP3A5_13, CYP3A5_16,  CYP3A5_17, CYP3A5_11, WT" # Strict 

des_params <- c("- V: volume of distritubtion (Tacrolimus)","<br>",
                "- Cl: clearance (Tacrolimus)","<br>",
                "- Q: Inter-compartmental clearance","<br>")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("CYP3A5_33", "CYP3A5_66", "CYP3A5_77", "CYP3A5_36", "CYP3A5_37", "CYP3A5_67","CYP3A5_13","CYP3A5_16","CYP3A5_17","CYP3A5_11", "WT")
mod_cov_abbr <- c("1 for mutation, otherwise no", "1 for mutation, otherwise no", "1 for mutation, otherwise no", "1 for mutation, otherwise no","1 for mutation, otherwise no","1 for mutation, otherwise no","1 for mutation, otherwise no","1 for mutation, otherwise no","1 for mutation, otherwise no","1 for mutation, otherwise no","Weight")

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
  est_eta <-c('L/h'='cl',
              'L' = 'v1',
              'L/h'='q',
              '1/h'='ka')
  
  
  sd_eta <- sqrt(c(0.1283, 0.4626, 0.2120, 0.3931)) # put sd^2 value in this vector
  
  # Model file for estimation -----------------------------------------
  f <- function() {
    ini({
      
      theta1 <- c(log(19.7))       # CL/F
      theta2 <- c(log(234))        # Vc/F
      theta3 <- c(log(403))        # Vp/F
      theta4 <- c(log(52.6))       # Q
      theta5 <- c(log(4.21))       # ka
      theta6 <- c(0.0080)          # Proportional error 
      
      eta1 ~ c(0.1283)          # CL/F
      eta2 ~ c(0.4626)          # Vc/F
      eta3 ~ c(0.2120)          # Q/F
      eta4 ~ c(0.3931)          # Ka
    })
    
    model({

      cl <- exp(theta1 + eta1) * (1.45**(CYP3A5_13 * CYP3A5_16 * CYP3A5_17) * (2.25**(CYP3A5_11)) * (1**(CYP3A5_33 *CYP3A5_66*CYP3A5_77*CYP3A5_36*CYP3A5_37*CYP3A5_67)))
      v1 <- exp(theta2 + eta2) * (WT/85.9)
      v2 <- exp(theta3)
      ke = cl/v1
      
      q <- exp(theta4 + eta3)
      k12 = q/v1
      k21 = q/v2
    
      ka <- exp(theta5 + eta4)
      
      d/dt(depot) = - ka * depot
      d/dt(center) = ka * depot - k12 * center + k21 * peri - ke * center
      d/dt(peri) = k12 * center - k21 * peri
      alag(depot) = 0.828
      
      cp = center / v1
      cp ~ prop(theta6)
    })
  }
  