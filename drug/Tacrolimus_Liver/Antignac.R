# Default model configuration on initial page

# PK model description ----------------------------------------------
des_intro <- "Tacrolimus model (Antignac et al.) 37 adult patients with liver transplantation"
des_notes <- c("-TCL50 is the time needed to obtatin 50% of maximum CL/F",
               "<br>",
               "-PK model was well described by one-compartment model with 1st order absorption and elimination ")
des_comp <- "depot, center"
des_cov <- "POD, ALB, AST" # Strict 

des_params <- c("- Cl: clearance (tacrolimus)","<br>",
                "- V: Volume of distritubtion(tacrolimus)","<br>")
# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("POD", "ALB", "AST")
mod_cov_abbr <- c("Postoperative days", "Albumin(g/L)", "AST(U/L)")

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
  est_eta <-c('L/h'='cl',
              'day'='TCL50',
              'L'='v'
  )
  
  sd_eta <- sqrt(c(0.174, 0.1046, 0.2152)) # put sd^2 value in this vector
  
  
  
  # Model file for estimation
  f <- function(){
    ini({
      # thetas
      theta1 <- c(36)            # CLmax (L/h)
      theta2 <- c(1870)          # V (L)
      theta3 <- c(6.3)           # TCL50 (day)
      theta4 <- c(4.9)           # sigmoidicity coefficient
      theta5 <- c(0.28)          # theta on AST
      theta6 <- c(0.64)          # theta on Albumin
      theta7 <- c(3.07)          # Additive Residual Error
      
      # ETAs
      eta1 + eta3 ~ c(0.174,
                      0.55, 0.2152)       # IIV CL/Fmax
      eta2 ~ c(0.1046)                    # IIV TCL50
      
      
      
    })
    
    model({
      ka <- 4.48   #fixed
      
      tclmax <- theta1 * (ALB / 38)**theta6
      clmax <- tclmax * exp(eta1)
      
      ttcl50 <- theta3 * (AST / 38)**theta5
      tcl50 <- ttcl50 * exp(eta2)
      
      cl <- (clmax * POD**theta4) / (tcl50**theta4 + POD**theta4)
      
      tv <- theta2
      v <- tv * exp(eta3)
      
      ke = cl/v
      
      d/dt(depot) = -ka * depot
      d/dt(center) = ka * depot - ke * center
      
      cp = center / v
      cp ~ add(theta7)
      
    })
  }
  