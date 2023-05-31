# Tacrolimus population pk model

# PK model description ----------------------------------------------
des_intro <- c("Tacrolimus po model for adult kidney transplant recipients", "<br>", "This model was built based on de novo Korean patients", "<br>", " Follow-up period lasted for 400 days after transplantation.")
des_notes <- c("- measurement time is recommended to be matched with the first dose",
               "<br>",
               "- Detailed tracking of CYP3A5, HCT, POD and WT is recommend to reflect physiological changes in clearance and volume of distribution")
des_comp <- "depot, center"
des_cov <- "CYP3A5, HCT, POD, WT" # Strict 

des_params <- c("- V: volume of distritubtion (Tacrolimus)","<br>",
                "- Cl: clearance (Tacrolimus)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("CYP3A5","HCT", "POD", "WT")
mod_lcov = c("CYP3A5")
mod_lcov_value <- list(CYP3A5 = c('*1/*1'=0.2225, '*1/*3'=0.17, '*3/*3'=0.0525))
mod_cov_abbr <- c("CYP3A5*3 allele indicates genetic variation in intron 3 (A6986G)", "hematocrit(%)", "post operational days", "weight(kg)")

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
      theta1 <- log(22.9)       # CL/F
      theta2 <- log(716)        # V/F
      theta3 <- c(0.1484)          # Proportional error 
      
      eta1 ~ c(0.2215)         # CL/F
      eta2 ~ c(0.2128)          # V/F
    })
    model({
      ka <- 4.5
      if(HCT <=33) {HCT2 <- 0.297} else {HCT2 <- 0.117}

      cl <- exp(theta1 + eta1) * exp(CYP3A5) * exp(HCT2) * (POD**-0.00762)
      v <- exp(theta2 + eta2) * exp(0.355 * WT / 59.025)
      ke = cl/v
      
      d/dt(depot) = - ka * depot
      d/dt(center) = ka * depot - ke * center
      cp = center / v
      cp ~ prop(theta3)
    })
  }
  