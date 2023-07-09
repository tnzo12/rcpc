# Default model configuration on initial page

# PK model description ----------------------------------------------
des_intro <- "Tacrolimus model for adult patients with liver transplantation"
des_notes <- c("-Two compartmental model with lagtime",
               "<br>",
               "- Note 2 ")
des_comp <- "depot, cent, per"
des_cov <- "ALT" # Strict 

des_params <- c("- Cl: clearance (tacrolimus)","<br>",
                "- V: Volume of distritubtion(tacrolimus)","<br>")
# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("ALT")
mod_lcov <- NULL
mod_lcov_value <- NULL
mod_cov_abbr <- c("Alanine aminotransferase (U/L)")

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
              'L'='vc',
              'L/h'='q',
              'L'='vp',
              '1/h'='ka'
  )
  
  sd_eta <- sqrt(c(0.1965, 0.2839, 0.1919, 0.6282, 0)) # put sd^2 value in this vector
  
  
  
  # Model file for estimation
  f <- function(){
    ini({
      # thetas
      theta1 <- c(32.8)           # CL/F (L/h)
      theta2 <- log(22.7)           # Vc/F (L)
      theta3 <- log(76.3)           # Q/F (L/h)
      theta4 <- log(916)            # Vp/F (L)
      theta5 <- log(0.419)          # Ka (1/h)
      theta6 <- c(0.404)          # ALAG1 (h)
      theta7 <- c(0.562)          # CL subpop
      theta8 <- c(-0.0237)        # CL ALT
      theta9 <- c(0.398)          # prop RE
      
      # ETAs
      eta1 ~ c(0.1965)       # IIV CL/F
      eta2 ~ c(0.2839)       # IIV Vc/F
      eta3 ~ c(0.1919)       # IIV Q/F
      eta4 ~ c(0.6282)       # IIV Vp/F
      eta5 ~ c(0)            # IIV Ka
      
      
    })
    
    model({
      tcl <- theta1 * theta7 * exp(ALT*theta8/40)
      cl <- tcl * exp(eta1)
      
      vc <- exp(theta2 + eta2)
      
      q <- exp(theta3 + eta3)
      
      vp <- exp(theta4 + eta4)
      
      kcp <- q/vc
      kpc <- q/vp
      
      ka <- exp(theta5 + eta5)
     
      ke = cl/vc
      
      d/dt(depot) = - ka * depot
      d/dt(cent) = ka * depot - ke * cent - kcp * cent + kpc * per
      d/dt(per) = kcp * cent - kpc * per
      
      alag(depot) = theta6
      
      cp = cent / vc
      cp ~ prop(theta9)
      
    })
  }
  