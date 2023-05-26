# Default model configuration on initial page

# PK model description ----------------------------------------------
des_intro <- "Tacrolimus model for adult patients with liver transplantation"
des_notes <- c("-total bilirubin (TBL) represents hepatic function",
               "<br>",
               "-serum creatinine (Scr) represents renal function")
des_comp <- "depot, center"
des_cov <- "POD, TBIL, SCr, HW, BW" # Strict 

des_params <- c("- Cl: clearance (tacrolimus)","<br>",
                "- V: Volume of distritubtion(tacrolimus)","<br>",
                "- F: Bioavailability(tacrolimus)","<br>")
# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("POD", "TBIL", "SCr", "HW", "BW")
mod_lcov <- NULL
mod_lcov_value <- NULL
mod_cov_abbr <- c("Postoperative days", "Total bilirubin(mg/dL)", "Serum creatinine(mg/dL)", "Grafted hepatic weight(g)", "Bodyweight(kg)")

mod_route <- c("PO")

# plot option -------------------------------------------------------
pk <- "cp"
pk_obs <- "SDC"
pk_color <- '#FF6666'
  pk_x_label <- "Time (hours)"
  pk_y_label <- "Tacrolimus conc. (mg/L)"
  
  pd <- NA
  pd_obs <- NA
  pd_color <- NA
  pd_x_label <- NA
  pd_y_label <- NA
  
  # model scheme image ------------------------------------------------
  scheme_image <- "http://www.turkupetcentre.net/petanalysis/pic/pk-2cm.svg"
  
  
  # Compartment designation -------------------------------------------
  mod_comp <- c(
    PO = 1,
    SDC = 2,
    NONE = 10
  )
  
  
  # Inter-individually Variable parameters ----------------------------
  est_eta <-c('unitless' = 'ba',
              'L/h'='cl',
              'L/kg'='v'
  )
  
  sd_eta <- sqrt(c(0.5069, 0.36, 0.1253)) # put sd^2 value in this vector
  
  
  
  # Model file for estimation
  f <- function(){
    ini({
      # thetas
      theta1 <- c(0.743)      # CL (L/h)
      theta2 <- c(1.64)       # volume of distribution (L/kg)
      theta3 <- c(0.0732)     # BA (bioavailability)
      theta4 <- c(0.792)      # theta on HF
      theta5 <- c(0.0157)     # theta on POD (L/h/day)
      theta6 <- c(0.810)      # theta on RF
      theta7 <- c(2.75)       # Additive residual error  
      
      # ETAs
      eta1 ~ c(0.5069)      # F
      eta2 ~ c(0.36)        # cl
      eta3 ~ c(0.1253)      # v
    })
    
    model({
      
      if(TBIL>2.5){HF <- 1} else {HF <- 0 }
      if(SCr>1){RF <- 1} else {RF <- 0 } 
      tcl <- (theta1 + theta5 * POD) * theta4^HF *theta6^RF * HW/600
      cl <- tcl * exp(eta2)
      
      tv <- theta2 * BW
      v <- tv * exp(eta3)
      
      tba <- theta3
      ba <- tba * exp(eta1)
      
      ke = cl/v
      
      d/dt(center) = - ke * center
      f(center) = ba
      
      cp = center / v
      cp ~ add(theta7)
      
    })
  }
  