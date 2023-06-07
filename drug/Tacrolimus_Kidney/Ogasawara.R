# Tacrolimus population pk model

# PK model description ----------------------------------------------
des_intro <- c("Tacrolimus po model for adult kidney transplant recipients.", 
               "<br>", 
               "Data were obtained from 102 (74 male and 28 female) patients, age ranging from 18 to 74", 
               "<br>",
               "All study participants received triple immunosuppressive drug regimen")
des_notes <- c("- measurement time is recommended to be matched with the first dose",
               "<br>",
               "- Detailed tracking of age, diabetes, MRP2 and CYP3A5 polymorphism is recommend to reflect physiological changes in clearance")
des_comp <- "depot, center, peri"
des_cov <- "MRP2, CYP3A5, AGE, diabetes" # Strict 

des_params <- c("- V: volume of distritubtion (Tacrolimus)","<br>",
                "- Cl: clearance (Tacrolimus)","<br>",
                "- Q: Inter-compartmental clearance","<br>")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("MRP2", "CYP3A5", "AGE", "diabetes")
mod_lcov = c("MRP2", "CYP3A5", "diabetes") # covariates with dropdown list
mod_lcov_value <- list(MRP2=c('H2/H2'=0, 'H1/H2'=1, 'H1/H1'=1),
                       CYP3A5=c('*3/*3'=0, '*1/*3'=1, '*1/*1'=1),
                       diabetes=c('yes'=1, 'no'=0))
mod_cov_abbr <- c("H1 is wild type and H2 is 1249G>A polymorphism", "*3 indicates 6986A>G(rs776746)", "Age(years)", "Whether patient has diabetes")

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
  
  
  sd_eta <- sqrt(c(0.1762, 1.2426)) # put sd^2 value in this vector
  
  # Model file for estimation -----------------------------------------
  f <- function() {
    ini({
      ka <- 0.544
      
      theta1 <- log(20.7)       # CL/F
      theta2 <- log(234)       # Vc/F
      theta3 <- c(1319)       # Vp/F
      theta4 <- c(0.183)      # tlag
      theta5 <- c(0.0332)          # Proportional error 
      
      eta1 ~ c(0.1762)          # CL/F
      eta2 ~ c(1.2426)          # Vc/F
    })
    
    model({
      ka <- 0.544
      q <- 70.7
      cl <- exp(theta1 + eta1) * (AGE/50)**-0.78 * 2.03**CYP3A5 * 1.4**MRP2
      v1 <- exp(theta2 + eta2)
      v2 <- theta3
      ke = cl/v1
      k12 = q/v1 
      k21 = q/v2
      
      d/dt(depot) = - ka * depot
      d/dt(center) = ka * depot - k12 * center + k21 * peri - ke * center
      d/dt(peri) = k12 * center - k21 * peri
      alag(depot) = theta4 * (2.6**diabetes)
      
      cp = center / v1
      cp ~ prop(theta5)
    })
  }
  