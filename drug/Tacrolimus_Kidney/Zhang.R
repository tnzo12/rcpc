# Tacrolimus population pk model

# PK model description ----------------------------------------------
des_intro <-  c("Tacrolimus po model for adult kidney transplant recipients.", 
                "<br>", 
                "Data were obtained from 83 Chinese patients, median value of POD was 9 days", 
                "<br>",
                "All study participants received tacrolimus twice daily starting from the third day after operation, as part of their triple immunosuppressive drug regimen")

des_notes <- c("- measurement time is recommended to be matched with the first dose",
               "<br>",
               "- Detailed tracking of CYP3A5, HCT and POD is recommend to reflect physiological changes in clearance and volume of distribution")
des_comp <- "depot, center"
des_cov <- "CYP3A5, HCT, POD" # Strict 

des_params <- c("- V: volume of distritubtion (Tacrolimus)","<br>",
                "- Cl: clearance (Tacrolimus)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("CYP3A5","HCT", "POD")
mod_lcov = c("CYP3A5") # covariates with dropdown list
mod_lcov_value <- list(CYP3A5 = c('*1/*1'=0, '*1/*3'=0, '*3/*3'=1))
mod_cov_abbr <- c("*3 indicates genetic polymorphism in the introm 3(6986G>A), and *1 indicates wild-type", "hematocrit(%)", "post operational days")

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
  
  
  sd_eta <- sqrt(c(0.2215, 0.2128)) # put sd^2 value in this vector
  
  # Model file for estimation -----------------------------------------
  f <- function() {
    ini({
      theta1 <- c(log(22.4))       # CL/F
      theta2 <- c(log(179))        # V/F
      theta3 <- c(2.33)            # Additive error
      
      eta1 ~ c(0.2231)          # CL/F
      eta2 ~ c(0.3110)          # V/F
    })
    
    model({
      ka <- 4.5
      
      cl <- exp(theta1 + eta1) * exp(-0.0526 * (83/POD)) * (39.1/HCT)**0.548 * exp(-0.32*CYP3A5) 
      v <- exp(theta2 + eta2) * POD**0.842
      ke = cl/v
      
      d/dt(depot) = - ka * depot
      d/dt(center) = ka * depot - ke * center
      cp = center / v
      cp ~ add(theta3)
    })
  }
  