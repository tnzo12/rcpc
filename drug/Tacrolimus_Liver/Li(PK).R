# Default model configuration on initial page

# PK model description ----------------------------------------------
des_intro <- "Tacrolimus model for adult patients with liver transplantation"
des_notes <- c("-Total bilirubin conc. was only covariate for this model",
               "<br>",
               "- Note 2 ")
des_comp <- "depot, center"
des_cov <- "TBIL" # Strict 

des_params <- c("- Cl: clearance (tacrolimus)","<br>",
                "- V: Volume of distritubtion(tacrolimus)","<br>")
# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("TBIL")
mod_cov_abbr <- c("Total bilirubin(umol/L)" )

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
              'L'='v'
  )
  
  sd_eta <- sqrt(c(0.1526, 0.3217)) # put sd^2 value in this vector
  
  
  
  # Model file for estimation
  f <- function(){
    ini({
      # thetas
      theta1 <- c(22.1)            # CL/F (L/h)
      theta2 <- c(653)             # Vc/F (L)
      theta3 <- c(2.17)            # TBIL
      theta4 <- c(2.83)            # add error
      
      
      # ETAs
      eta1 ~ c(0.1526)       # IIV CL
      eta2 ~ c(0.3217)       # IIV V
    })
    
    model({
      ka <- 4.48    # fixed
      if(TBIL<=25.7){TBIL <- 0
      } else if(TBIL<51.4){TBIL <- 1
      } else if(TBIL<77.1){TBIL <- 2
      } else if(TBIL<128.5){TBIL <- 3
      } else {TBIL <-4}
      tcl <- theta1 - theta3 * TBIL 
      cl <- tcl * exp(eta1)
      
      tv <- theta2
      v <- tv * exp(eta2)
      
      ke = cl/v
      
      d/dt(depot) = - ka * depot
      d/dt(cent) = ka * depot - ke * cent 
      
      cp = cent / v
      cp ~ add(theta4)
      
    })
  }
  