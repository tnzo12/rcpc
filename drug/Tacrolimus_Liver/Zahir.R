# Default model configuration on initial page

# PK model description ----------------------------------------------
des_intro <- "Tacrolimus model for adult patients with liver transplantation"
des_notes <- c("-Co-administered drug (diltiazem and fluconazole) choose: Yes/No",
               "<br>",
               "- Note 2 ")
des_comp <- "depot, center"
des_cov <- "HCT, ALB, DIL, FLU" # Strict 

des_params <- c("- Cl: clearance (tacrolimus)","<br>",
                "- V: Volume of distritubtion(tacrolimus)","<br>")
# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("HCT", "ALB", "DIL", "FLU")
mod_lcov <- c("DIL","FLU")
mod_lcov_value <- list(
  DIL = c('Yes'=1,'No'=0), 
  FLU = c('Yes'=1,'No'=0)
  )
mod_cov_abbr <- c("Hematocrit(%)", "Albumin(g/dL)", "Diltiazem coadministration", "Fluconazole coadministration")

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
  est_eta <-c('L/h'='cl'
  )
  
  sd_eta <- sqrt(c(0.0952)) # put sd^2 value in this vector
  
  
  
  # Model file for estimation
  f <- function(){
    ini({
      # thetas
      theta1 <- c(21.3)            # CL/F (L/h)
      theta2 <- c(314)             # V/F (L)
      theta3 <- c(9.8)             # theta on hematocrit 
      theta4 <- c(3.4)             # theta on albumin
      theta5 <- c(2.1)             # theta on diltiazem coadministration
      theta6 <- c(7.4)             # theta on fluconazole coadministration
      theta7 <- c(0, 0.243)        # residual random error  
      
      # ETAs
      eta1 ~ c(0.0952)      # IIV CL/F
      
    })
    
    model({
      ka <- 4.5   #fixed
      
      if(HCT < 35){HCT <- 0} else {HCT <- 1 }
      if(ALB < 3.5){ALB <- 0} else {ALB <- 1 }
  
      tcl <- theta1 + theta3 * (1-HCT) + theta4 * (1-ALB) - theta5 * DIL - theta6 * FLU
      cl <- tcl * exp(eta1)
      
      v <- theta2
      
      ke = cl/v
      
      d/dt(depot) = -ka * depot
      d/dt(center) = ka * depot - ke * center
      
      cp = center / v
      cp ~ prop(theta7)
      
    })
  }
  