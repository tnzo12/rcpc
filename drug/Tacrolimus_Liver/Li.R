# Default model configuration on initial page

# PK model description ----------------------------------------------
des_intro <- "Tacrolimus model for adult patients with liver transplantation"
des_notes <- c("-Total bilirubin and CYP3A5 genotype is covariate in this model",
               "<br>",
               "-Donor and Recipient's CYP3A5 genotype need to be selected")
des_comp <- "depot, center"
des_cov <- "TBIL, CYPD, CYPR" # Strict 

des_params <- c("- Cl: clearance (tacrolimus)","<br>",
                "- V: Volume of distritubtion(tacrolimus)","<br>")
# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("TBIL", "CYPR", "CYPD")
mod_lcov <- c("CYPR", "CYPD")
mod_lcov_value <- list(
  CYPR = c('*3*3'=1,'others'=0),
  CYPD = c('*3*3'=1,'others'=0)
)
mod_cov_abbr <- c("Total bilirubin(umol/L)", "Recipient CYP3A5 *3 Genotype", "Donor CYP3A5 *3 Genotype" )

mod_route <- c("PO")

# plot option -------------------------------------------------------
pk <- "cp"
pk_obs <- "SDC"
pk_color <- '#FF6666'
  pk_x_label <- "Time (hours)"
  pk_y_label <- "Tacrolimus conc. (ug/L)"
  
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
  
  sd_eta <- sqrt(c(0.0929, 0.2643)) # put sd^2 value in this vector
  
  
  
  # Model file for estimation
  f <- function(){
    ini({
      # thetas
      theta1 <- c(15.9)            # CL/F (L/h)
      theta2 <- log(620)           # Vc/F (L)
      theta3 <- c(1.88)            # TBIL
      theta4 <- c(7.65)            # CYPD
      theta5 <- c(7)               # CYPR
      theta6 <- c(2.81)            # add error
      
      
      # ETAs
      eta1 ~ c(0.0929)       # IIV CL
      eta2 ~ c(0.2643)       # IIV V
      
      
    })
    
    model({
      ka <- 4.48    # fixed
      
      if(TBIL<=25.7){TBIL <- 0
      } else if(TBIL<51.4){TBIL <- 1
      } else if(TBIL<77.1){TBIL <- 2
      } else if(TBIL<128.5){TBIL <- 3
      } else {TBIL <-4}
      tcl <- theta1 - theta3 * TBIL + theta4 * (1-CYPD) + theta5 * (1-CYPR)
      cl <- tcl * exp(eta1)
      
      v <- exp(theta2 + eta2)
      
      ke = cl/v
      
      d/dt(depot) = - ka * depot
      d/dt(cent) = ka * depot - ke * cent 
      
      cp = cent / v
      cp ~ add(theta6)
      
    })
  }
  