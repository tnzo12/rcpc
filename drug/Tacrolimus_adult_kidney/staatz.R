# Pyrazinamide population pk model

# PK model description ----------------------------------------------
des_intro <- "Tacrolimus po model for adult kidney transplant recipients"
des_notes <- c("- measurement time is recommended to be matched with the first dose",
               "<br>",
               "- Detailed tracking of DOT and AST is recommend to reflect physiological changes in clearance and volume of distribution")
des_comp <- "depot, center"
des_cov <- "DOT, AST" # Strict 

des_params <- c("- V: volume of distritubtion (Tacrolimus)","<br>",
                "- Cl: clearance (Tacrolimus)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("DOT","AST")
mod_lcov = NULL # covariates with dropdown list
mod_lcov_value <- NULL
mod_cov_abbr <- c("Days of therapy", "Aspartate aminotransferase")

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
  
  
sd_eta <- sqrt(c(0.16245, 0.8029)) # put sd^2 value in this vector
  
# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(23.6)       # CL/F
    theta2 <- c(31.9)       # DOT on CL/F
    theta3 <- c(76.7)       # AST on CL/F
    theta4 <- c(log(1070))  # V/F
    theta5 <- c(3.7)        # Additive error error
    
    eta1 ~ c(0.16245)         # CL/F
    eta2 ~ c(0.8029)          # V/F
    })
    model({
      ka <- 4.48
      cl <- (theta1 + theta2/DOT + theta3/AST) * exp(eta1)
      v <- exp(theta4 + eta2)
      ke = cl/v
      
      d/dt(depot) = - ka * depot
      d/dt(center) = ka * depot - ke * center
      cp = center / v
      cp ~ add(theta5)
    })
  }
  