# Default model configuration on initial page

# PK model description ----------------------------------------------
des_intro <- "Tacrolimus model for adult patients with liver transplantation"
des_notes <- c("- Clearance depends on CYP3A5 genotype of donors and recipients",
               "<br>",
               "- Note2")
des_comp <- "depot, trans1, trans2, trans3, cent, per"
des_cov <- "CYPD, CYPR" # Strict 

des_params <- c("- Cl: clearance (tacrolimus)","<br>",
                "- V: Volume of distritubtion(tacrolimus)","<br>")
# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("CYPD", "CYPR")
mod_lcov <- c("CYPD", "CYPR")
mod_lcov_value <- list(
  CYPD= c('*1 carrier'=1,'*1 non-carrier'=0),
  CYPR= c('*1 carrier'=1,'*1 non-carrier'=0)
)
mod_cov_abbr <- c("Donor's CYP3A5*1 Genotype", "Recipient's CYP3A5*1 Genotype" )

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
              '1/h' = 'ka'
  )
  
  sd_eta <- sqrt(c(0.1682, 0.5566, 0.3607)) # put sd^2 value in this vector
  
  
  
  # Model file for estimation
  f <- function(){
    ini({
      # thetas
      theta1 <- c(4.21)            # CL if both R and D noncarrier
      theta2 <- c(5.60)            # CL if either one is carrier
      theta3 <- c(7.20)            # CL if both R and D carrier
      theta4 <- c(88.3)            # Vc
      theta5 <- c(145)             # vp
      theta6 <- c(14)              # Q
      theta7 <- c(3.76)            # Ka
      theta8 <- c(0.13)            # prop RE
      
      
      # ETAs
      eta1 ~ c(0.1682)       # IIV CL
      eta2 ~ c(0.5566)       # IIV Vc
      eta3 ~ c(0.3607)       # IIV Ka
    })
    
    model({
      
      tcl <- theta2
      if (CYPD==0 & CYPR==0) {tcl <- theta1}
      if (CYPD==1 & CYPR==1) {tcl <- theta3}
      
      cl <- tcl * exp(eta1)
      
      ka <- theta7 * exp(eta3)
      
      vc <- theta4 * exp(eta2)
      vp <- theta5
      
      q <- theta6
      
      kcp <- q/vc
      kpc <- q/vp
      
      ke = cl/vc
      
      d/dt(depot) = - ka * depot
      f(depot) = 0.23    # fixed
      d/dt(trans1) = ka * depot - ka * trans1
      d/dt(trans2) = ka * trans1 - ka * trans2
      d/dt(trans3) = ka * trans2 - ka * trans3
      d/dt(cent) = ka * trans3 - ke * cent - kcp * cent + kpc * per
      d/dt(per) = kcp * cent - kpc * per
      
      
      cp = cent / vc
      cp ~ prop(theta8)
      
    })
  }
  
  