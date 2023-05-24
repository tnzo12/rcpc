# PK model description ----------------------------------------------
des_intro <- "Tacrolimus po model for adult kidney transplant recipients"
des_notes <- c("- measurement time is recommended to be matched with the first dose",
               "<br>",
               "- Detailed tracking of DOT and AST is recommend to reflect physiological changes in clearance and volume of distribution")
des_comp <- "depot, center, peri"
des_cov <- "CYP3A5, WT, rGT, HCT" # Strict 

des_params <- c("- V: volume of distritubtion (Tacrolimus)","<br>",
                "- Cl: clearance (Tacrolimus)","<br>",
                "- Q: Inter-compartmental clearance","<br>")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("CYP3A5", "WT", "rGT", "HCT")
mod_lcov = c("CYP3A5") # covariates with dropdown list
mod_lcov_value <- list(CYP3A5 = c('*1/*1'=1, '*1/*3'=1, '*3/*3'=0))
mod_cov_abbr <- c("1 for mutation, otherwise no", "weight", "r-glutamyl transpeptidase", "Hematocrit")

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
  est_eta <-c('L/hr'='ka',
              'L/hr'='cl',
              'L'='v1')
  
  
  sd_eta <- sqrt(c(0.2899, 0.1844, 0.0606, 1.3584)) # put sd^2 value in this vector
  
  # Model file for estimation -----------------------------------------
  f <- function() {
    ini({
      theta1 <- c(log(0.23))   # Ka
      theta2 <- c(log(35))     # cl
      theta3 <- c(log(12))     # V1
      theta4 <- c(log(109))    # V2
      theta5 <- c(log(68))     # Q
      theta6 <- c(0.0431)      # Proportional error 
      
      eta1 ~ c(0.2899)          # Ka
      eta2 ~ c(0.1844)          # CL_IIV
      eta3 ~ c(0.0606)          # CL_IOV
      eta4 ~ c(1.3584)          # V1
    })
    model({
      ka <- exp(theta1 + eta1)
      
      tvcl <-  exp(theta2) * (1+(0.45*(CYP3A5) + 0.41*(1-CYP3A5))) * (WT/70)**0.75 * (rGT/13)**-0.21 * (HCT/0.34)**-0.59
      cl <- tvcl * exp(eta2 + eta3)
      v1 = exp(theta3+eta4) * (WT/70)
      v2 = exp(theta4) * (WT/70)
      q = exp(theta5) * (WT/70)**0.75

      k12 = q/v1
      k21 = q/v2
      ke = cl/v1
      
      d/dt(depot) = - ka * depot
      d/dt(center) = ka * depot - k12 * center + k21 * depot - ke * center
      d/dt(peri) = k12 * center - k21 * depot
      alag(depot) = 0.47
      
      cp = center / v1
      cp ~ prop(theta6)
    })
  }