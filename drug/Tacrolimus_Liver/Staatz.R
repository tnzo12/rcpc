# Default model configuration on initial page

# PK model description ----------------------------------------------
des_intro <- "Tacrolimus model for adult patients with liver transplantation"
des_notes <- c("- Some notes for the model will appear here",
               "<br>",
               "- ex) Characteristics of covariates")
des_comp <- "depot, center"
des_cov <- "AST, WT" # Strict 

des_params <- c("- Vd: Volume of distritubtion(tacrolimus)","<br>",
                "- Cl: clearance (tacrolimus)","<br>")
# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")
mod_cov <- c("AST", "WT")
mod_cov_abbr <- c("Aspartate aminotransferase(U/L)", "Weight(kg)")
mod_lcov = NULL # covariates with dropdown list
mod_lcov_value <- NULL
mod_cov_abbr <- c("Aspartate aminotransferase", "Weight")
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

sd_eta <- sqrt(c(0.1849, 0.8649)) # put sd^2 value in this vector



# Model file for estimation
f <- function(){
  ini({
    # thetas
    theta1 <- c(29.6)     # CL/F < 70
    theta2 <- c(24)     # CL/F >= 70
    theta3 <- c(601)     # Vd/F
    theta4  <- c(3.3)  # Additive Residual Error
    
    # ETAs
    eta1 ~ c(0.1849)      # CL/F
    eta2 ~  c(0.8649)     # Vd/F
  })
    
  model({
   ka <- 4.48     # Fixed
   
   if(AST>=70){tcl <- theta2 } else { tcl <- theta1 } 
   cl <- tcl * exp(eta1)
   
   tv <- theta3 * (WT/72.1)
   v <- tv * exp(eta2)
   
   ke = cl/v
   
   d/dt(depot) = -ka * depot
   d/dt(center) = ka * depot - ke * center
   
   cp = center / v
   cp ~ add(theta4)
     
  })
}
