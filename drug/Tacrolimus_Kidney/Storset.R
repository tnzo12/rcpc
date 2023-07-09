# Tacrolimus population pk model

# PK model description ----------------------------------------------
des_intro <- "Tacrolimus po model for adult kidney transplant recipients"
des_notes <- c("- measurement time is recommended to be matched with the first dose",
               "<br>",
               "- Detailed tracking of DOT and AST is recommend to reflect physiological changes in clearance and volume of distribution")
des_comp <- "depot, center, peri"
des_cov <- "CYP3A5, FFM, Prednisolone, POD" # Strict 

des_params <- c("- V: volume of distritubtion (Tacrolimus)","<br>",
                "- Cl: clearance (Tacrolimus)","<br>",
                "- Q: Inter-compartmental clearance","<br>")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("CYP3A5", "FFM", "Prednisolone", "POD")
mod_lcov = c("CYP3A5") # covariates with dropdown list
mod_lcov_value <- list(CYP3A5 = c('*1/*1'=1, '*1/*3'=1, '*3/*3'=0))
mod_cov_abbr <- c("Mutation on cytochrome P450 3A5", "free fat mass(kg)", "Prednisolone dose(mg/day)", "POD")

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
  est_eta <-c('L/h'='cl',
              'L'='v1',
              'L/h'='q',
              'L/h'='ka')
  
  
  sd_eta <- sqrt(c(0.1484, 0.2558, 0.3342, 0.2813, 0.0515, 0.8919)) # put sd^2 value in this vector
  
  # Model file for estimation -----------------------------------------
  f <- function() {
    ini({
      theta1 <- log(1.01)       # Ka
      theta2 <- log(811)        # Cl
      theta3 <- log(6290)       # Vc/F
      theta4 <- c(32100)      # Vp/F
      theta5 <- log(1200)       # Q
      theta6 <- log(1)          # BA
      theta7 <- c(0.0219)          # Proportional error 
      
      eta1 ~ c(0.1484)          # IIV_CL
      eta2 ~ c(0.2558)          # IIV V1/F
      eta3 ~ c(0.3342)          # IIV Q/F
      eta4 ~ c(0.2813)          # IIV Fday2
      eta5 ~ c(0.0515)          # IOV F
      eta6 ~ c(0.8919)          # IOV Ka
    })
    
    model({
      ka <- exp(theta1+eta6)
      cl <- exp(theta2+eta1) * (FFM/60)**0.75 * (1.3 * CYP3A5 + (1-CYP3A5))
      v1 <- exp(theta3+eta2)
      v2 <- theta4
      q <- exp(theta5+eta3)
      
      ke = cl/v1
      k12 = q/v1
      k21 = q/v2
      
      BA <- exp(theta6 + eta4) * (1 - (0.67 * Prednisolone)/(Prednisolone+35)) * 0.82**(CYP3A5)
      if (POD <= 0) {BA <- exp(theta6 + eta5) * (1 - (0.67 * Prednisolone)/(Prednisolone+35)) * 2.68 * 0.82**(CYP3A5)}
      
      d/dt(depot) = - ka * depot
      d/dt(center) = ka * depot - k12 * center + k21 * peri - ke * center
      d/dt(peri) = k12 * center - k21 * peri
      f(depot) = BA
      
      cp = center / v1
      cp ~ prop(theta7)
    })
  }
  