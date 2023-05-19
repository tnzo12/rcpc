# Pyrazinamide population pk model

# PK model description ----------------------------------------------
des_intro <- "Tacrolimus po and iv model for adult kidney transplant recipients"
des_notes <- c("- measurement time is recommended to be matched with the first dose",
               "<br>",
               "- Detailed tracking of DOT and AST is recommend to reflect physiological changes in clearance and volume of distribution")
des_comp <- "depot, center"
des_cov <- "POD, CS" # Strict 

des_params <- c("- V: volume of distritubtion (Tacrolimus)","<br>",
                "- Cl: clearance (Tacrolimus)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("POD", "CS")
mod_cov_abbr <- c("Post operation days", "Amount of concominnant prednisolone")

mod_route <- c("IV", "PO")

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
    IV = 2,
    PO = 1,
    SDC = 2,
    NONE = 10)
  
  # Inter-individually Variable parameters ----------------------------
  est_eta <-c('L/hr'='cl',
              'L'='v')
  
  
  sd_eta <- sqrt(c(0.0917, 0.4849, 0.0975)) # put sd^2 value in this vector
  
  # Model file for estimation -----------------------------------------
  f <- function() {
    ini({
      theta1 <- c(log(1.81))       # CL
      theta2 <- c(log(98.4))       # V
      theta3 <- c(log(0.137))      # F
      theta4 <- c(0.96)           # Additive error error
      theta5 <- c(0.0340)        # Proportional error error
       
      eta1 ~ c(0.0917)          # CL
      eta2 ~ c(0.4849)          # V
      eta3 ~ c(0.0975)          # F
    })
    model({
      ka <- 4.5

      if(CS>=25) {CS2 <- 1} else {CS2 <- 0}
      cl <- exp(theta1 + eta1) * (1+(POD**2.54/(POD**2.54+3.81**2.54))) * 1.575**CS2
      v <- exp(theta2 + eta2)
      ke = cl/v
      
      BA = exp(theta3 + eta3)
      
      d/dt(depot) = - ka * depot
      f(depot) = BA
      d/dt(center) = ka * depot - ke * center
      cp = center / v
      cp ~ add(theta4) + prop(theta5)
    })
  }
  
  