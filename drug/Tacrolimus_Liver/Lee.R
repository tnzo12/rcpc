# Default model configuration on initial page

# PK model description ----------------------------------------------
des_intro <- "Tacrolimus model for adult patients with liver transplantation"
des_notes <- c("-Note1",
               "<br>",
               "- Note 2 ")
des_comp <- "depot, center"
des_cov <- "POD, TBIL, INR, GRWR, BW" # Strict 

des_params <- c("- Cl: clearance (tacrolimus)","<br>",
                "- V: Volume of distritubtion(tacrolimus)","<br>")
# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("POD", "TBIL", "INR", "GRWR", "BW")
mod_lcov <- NULL
mod_lcov_value <- NULL
mod_cov_abbr <- c("Postoperative days", "Total bilirubin(mg/dL)", "International Normalized Ratio", "Graft to recipient weight ratio(%)", "Bodyweight(kg)")

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
  
  sd_eta <- sqrt(c(0.13, 0.46)) # put sd^2 value in this vector
  
  
  
  # Model file for estimation
  f <- function(){
    ini({
      # thetas
      theta1 <- c(0.36)          # CL/F (L/kg/hr)
      theta2 <- log(568)         # V/F (L)
      theta3 <- c(2.01)          # Factor for LPOD on CL/F
      theta4 <- c(-0.23)         # Power for TBL level >1.2 mg/dL
      theta5 <- c(0.49)          # Factor for EPOD on CL/F
      theta6 <- c(0.75)          # Factor for INR >1.4 on CL/F
      theta7 <- c(0.86)          # Factor for GRWR <= 1.25% on CL/F
      theta8 <- c(3.14)          # residual variability 
      
      # ETAs
      eta1 ~ c(0.13)       # IIV CL/F
      eta2 ~ c(0.46)       # IIV V/F
      
      
    })
    
    model({
      
      if(POD>35){L=1} else {L=0} #late operative days (theta3)
      if(POD<=3){PODF<-1} else {PODF<-0} #early operative days (theta5)
      if(TBIL<=1.2){TBILF<-0} else {TBILF<-1} #theta4
      if(INR>1.4){INRF<-1} else {INRF<-0} #theta 6
      if(GRWR<=1.25){GRWRF<-1} else {GRWRF<-0} #theta 7
      
      
      tcl <- (theta1 + (theta3/POD)*L) * ((TBIL**TBILF)**theta4) * (theta5**PODF) * (theta6**INRF) * (theta7**GRWRF) * BW
      cl <- tcl * exp(eta1)
      
      v <- exp (theta2 + eta2)
      
      
      ke = cl/v
      
      
      d/dt(center) = - ke * center
      
      cp = center / v
      cp ~ add(theta8)
      
    })
  }
  