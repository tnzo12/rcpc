# Default model configuration on initial page

# PK model description ----------------------------------------------
des_intro <- "One compartment model for IV administration of Vancomycin"
des_notes <- c("- Inter-individual variabilities to Vd, Cl","<br>",
               "- No covariates","<br>",
               "- No PK for PO admin is supported")
des_comp <- "Compartment information"
des_cov <- "Covariate information" # Strict 

des_params <- c("- v: Central volume of distritubtion","<br>",
                "- cl: Clearance (vancomycin)","<br>")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- NULL
mod_lcov = NULL # covariates with dropdown list
mod_lcov_value <- NULL
mod_cov_abbr <- NULL

mod_route <- c("IV")

# plot option -------------------------------------------------------
pk <- "cp"
pk_obs <- "SDC"
pk_color <- '#FF6666'
pk_x_label <- "Time (hours)"
pk_y_label <- "Vancomycin Conc. (mg/L)"

pd <- NA
pd_obs <- NA
pd_color <- NA
pd_x_label <- NA
pd_y_label <- NA

# model scheme image ------------------------------------------------
scheme_image <- "one_cmt.png"


# Compartment designation -------------------------------------------
mod_comp <- c(
  IV = 1,
  SDC = 1
)


# Inter-individually Variable parameters ----------------------------
est_eta <-c('L'='v',
            'L/hr'='cl'
)

sd_eta <- sqrt(c(0.229, 0.605)) # pu omega(sd^2) value in this vector



# Model file for estimation
f <- function(){
  ini({
    # thetas
    theta1 <- c(0.387) # Add err PK
    theta2 <- c(log(50.9)) # Vd
    theta3 <- c(log(3.42)) # Cl
    
    # ETAs
    eta1 + eta2 ~ c(0.194,          
                    -0.0159, 0.306) # Vd, Cl
  })
  model({
   
    # central compartmental params
    v <- exp(theta2 + eta1)
    cl <- exp(theta3 + eta2)
 
    # algebraic expressions
    k = cl/v
    cp = cent/v
    # differential equations
    d/dt(cent) = -k*cent
    
    # error model
    cp ~ add(theta1) | centr
  })
}
