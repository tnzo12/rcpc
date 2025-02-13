# Pyrazinamide population pk model

# PK model description ----------------------------------------------
des_intro <- c("Tacrolimus po model for adult kidney transplant recipients", "<br>", "This model was built based on 70 patients who were given oral tacrolimus with methylprednisoloine and mycophenolic acid")
des_notes <- c("- measurement time is recommended to be matched with the first dose",
               "<br>",
               "- Detailed tracking of HCT, CYP3A5, ABCB1 is recommend to reflect physiological changes in clearance")
des_comp <- "depot, center, peri"
des_cov <- "HCT, CYP3A5, ABCB1" # Strict 

des_params <- c("- V: volume of distritubtion (Tacrolimus)","<br>",
                "- Cl: clearance (Tacrolimus)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("HCT", "CYP3A5", "ABCB1")
mod_lcov <- c("CYP3A5", "ABCB1")
mod_lcov_value <- list(CYP3A5 = c('*1/*1'=1, '*1/*3'=1, '*3/*3'=0),
                       ABCB1 = c('genetic polymorphism'=1, 'else'=0))
mod_cov_abbr <- c("Haematocrit level", "*1 stands for CYP3A5 intron 36986 G>A status", "Select ABCB1 polymorphism: ABCB1 exon 12 1236 C>T status, ABCB1 exon 21 2677 G>T/A status, ABCB1 exon 26 3435 C>T status")

mod_route <- c("PO")

# plot option -------------------------------------------------------
pk <- "cp"
pk_obs <- "SDC"
pk_color <- '#FF6666'
  pk_x_label <- "Time (hours)"
  pk_y_label <- "Tacrolimus Conc. (ug/L)"
  
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
  est_eta <-c('1/hr'='ka',
              'L/hr'='cl',
              'L'='v1',
              'L'='v2')

  sd_eta <- sqrt(c(0.6032, 0.0974,0.2642,0.2073,0.3163)) # put sd^2 value in this vector
  
  # Model file for estimation -----------------------------------------
  f <- function() {
    ini({
      theta1 <- log(0.45)       # ka
      theta2 <- c(16.3)           # Cl
      theta3 <- log(86.4)       # Vc
      theta4 <- log(1115)       # Vp
      theta5 <- log(0.1)        # Tlag
      theta6 <- c(0.88)            # Additive error
      theta7 <- c(0.0167)          # Proportional error
      
      eta1 ~ c(0.6032)          # ka
      eta2 ~ c(0.0974)          # Cl
      eta3 ~ c(0.2642)          # Vc
      eta4 ~ c(0.2073)          # Vp
      eta5 ~ c(0.3163)          # Tlag
      
    })
    model({
      ka <- exp(theta1 + eta1)
      cl <- (theta2 + (20.6*HCT/21) + 15.4**CYP3A5 + 7.6**ABCB1) * exp(eta2)
      q <- 58.2
      v1 <- exp(theta3 + eta3)
      v2 <- exp(theta4 + eta4)
      
      ke = cl/v1
      k12 = q/v1 
      k21 = q/v2
      
      d/dt(depot) = - ka * depot
      d/dt(center) = ka * depot - k12 * center + k21 * peri - ke * center
      d/dt(peri) = k12 * center - k21 * peri
      alag(depot) = exp(theta5 + eta5)
      
      cp = center / v1
      cp ~ add(theta6) + prop(theta7)
    })
  }
  