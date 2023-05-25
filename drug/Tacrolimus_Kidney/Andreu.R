# Tacrolimus population pk model

# PK model description ----------------------------------------------
des_intro <-  c("Tacrolimus po model for adult kidney transplant recipients.", 
                "<br>", 
                "Data from 304 patients were collected from week 1 to month 12 after transplantation.", 
                "<br>",
                "All study participants received tacrolimus twice daily in combination with mycophenolate mofetil.")

des_notes <- c("- measurement time is recommended to be matched with the first dose",
               "<br>",
               "- Detailed tracking of DOT and AST is recommend to reflect physiological changes in clearance and volume of distribution")
des_comp <- "depot, center, peri"
des_cov <- "CYP3A5, CYP3A4, AGE" # Strict 

des_params <- c("- V: volume of distritubtion (Tacrolimus)","<br>",
                "- Cl: clearance (Tacrolimus)","<br>",
                "- Q: Inter-compartmental clearance","<br>")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("CYP3A5", "CYP3A4", "AGE")
mod_lcov = c("CYP3A5", "CYP3A4") # covariates with dropdown list
mod_lcov_value <- list(CYP3A5 = c('*1/*1'=0, '*1/*3'=1, '*3/*3'=2),
                       CYP3A4 = c('*1/*1'=0, '*1/*22'=1, '*22/*22'=1))

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
              'L'='v1')
  
  
  sd_eta <- sqrt(c(0.0744, 0.1051)) # put sd^2 value in this vector
  
  # Model file for estimation -----------------------------------------
  f <- function() {
    ini({
      
      theta1 <- c(log(20.5))       # CL/F_EM
      theta2 <- c(log(12.5))       # CL/F_IM
      theta3 <- c(log(9.1))        # CL/F_PM
      theta4 <- c(log(5.02))       # Vc/F
      theta5 <- c(log(526))        # Vp/F
      theta6 <- c(log(4.2))        # Q
      theta7 <- c(log(0.183))      # Ka
      theta8 <- c(log(0.243))      # Tlag
      theta9 <- c(0.0606)          # Proportional error 
      
      eta1 ~ c(0.0744)          # CL/F_IIV
      eta2 ~ c(0.1051)          # CL/F_IOV
    })
    
    model({
      ka <- exp(theta7)
      q <- exp(theta6)
    
      if(CYP3A4 == 0 & CYP3A5 == 0 & AGE < 63){tvcl <- theta1}
      if(CYP3A4 == 0 & CYP3A5 == 0 & AGE >= 63){tvcl <- theta1-0.205}
      if(CYP3A4 == 0 & CYP3A5 == 1 & AGE < 63){tvcl <- theta1}
      if(CYP3A4 == 0 & CYP3A5 == 1 & AGE >= 63){tvcl <- theta1-0.205}
      if(CYP3A4 == 0 & CYP3A5 == 2 & AGE < 63){tvcl <- theta2}
      if(CYP3A4 == 0 & CYP3A5 == 2 & AGE >= 63){tvcl <- theta2-0.205}
      if(CYP3A4 == 1 & CYP3A5 == 0 & AGE < 63){tvcl <- theta2}
      if(CYP3A4 == 1 & CYP3A5 == 0 & AGE >= 63){tvcl <- theta2-0.205}
      if(CYP3A4 == 1 & CYP3A5 == 1 & AGE < 63){tvcl <- theta2}
      if(CYP3A4 == 1 & CYP3A5 == 1 & AGE >= 63){tvcl <- theta2-0.205}
      if(CYP3A4 == 1 & CYP3A5 == 2 & AGE < 63){tvcl <- theta3}
      if(CYP3A4 == 1 & CYP3A5 == 2 & AGE >= 63){tvcl <- theta3-0.205}
      
      cl <- exp(tvcl+eta1+eta2)
      
      v1 <- exp(theta4)
      v2 <- exp(th)
      ke = cl/v1
      k12 = q/v1 
      k21 = q/v2
      
      d/dt(depot) = - ka * depot
      d/dt(center) = ka * depot - k12 * center + k21 * peri - ke * center
      d/dt(peri) = k12 * center - k21 * peri
      alag(depot) = exp(theta8)
      
      cp = center / v1
      cp ~ prop(theta5)
    })
  }
  