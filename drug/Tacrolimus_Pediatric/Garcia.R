# Tacrolimus population pk model
# theta 값에 v 넣어줘야 하는데 논문에 volume of distribution에 관한 theta값이 없음



# PK model description ----------------------------------------------
des_intro <- "Tacrolimus Oral centeral model for paediatric patients who were on treatment with tacrolimus as conversion therapy were chosen for this retrospective study."
des_notes <- c("The indications for conversion from cyclosporin to tacrolimus were chronic rejection (7 patients), acute rejection (3 patients), and cyclosporin toxicity (8 patients).",
               "<br>",
               "Cyclosporin was discontinued for 24 hours before patients were converted to tacrolimus therapy.")
des_comp <- c("central", "depot")
des_cov <- c("WT","ALT","TIME","BIL") # Strict 

des_params <- c("- V: volume of distritubtion (Tacrolimus)","<br>",
                "- Cl: clearance (Tacrolimus)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("WT","ALT","TIME","BIL")
mod_lcov = NULL # covariates with dropdown list
mod_lcov_value <- NULL
mod_cov_abbr <- c("Body weight","Alanine aminotransferase","Time after initiation of treatment","Bilirubin")

mod_route <- c("Oral")

# plot option -------------------------------------------------------
pk <- "cp"
pk_obs <- "SDC"
pk_color <- '#FF6666'
pk_x_label <- "Time (hours)"
pk_y_label <- "Tacrolimus Conc. (mg/L)"


# model scheme image ------------------------------------------------
scheme_image <- "https://els-jbs-prod-cdn.jbs.elsevierhealth.com/cms/attachment/e9f4a16a-a113-4770-86c4-614b4895f30c/fx1.jpg"

# Compartment designation -------------------------------------------
mod_comp <- c(
  Oral = 1,
  SDC = 2,
  NONE = 10
)

# Inter-individually Variable parameters ----------------------------
est_eta <-c('L/h'='cl',
            'L'='v')

sd_eta <- sqrt(c(0.059)) # put sd^2 value in this vector

# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(10.4)         # CL
    theta2 <- c(-0.00032)     # Time
    theta3 <- c(-0.057)       # BIL
    theta4 <- c(0.079)        # ALT
    theta5 <- c(0.20)         # BA
    theta6 <- c(1.0)          # Ka
    theta7 <- c(0.087)        # Proportional error

    eta1 ~ c(0.059)           # CL
   
  })
  model({
    TVCL <- theta1 
    TVTIME <- theta2 
    TVBIL <- theta3
    TVALT <- theta4
    BA <- theta5 
    
    cl <- (TVCL* (1 + eta1))* ((WT/70)**0.75) * exp(theta2 * TIME) * exp(theta3 * BIL) * (1 - theta4 * ALT)
    v <- 
   
    
    #ke <- theta1 / theta2
    ke = cl/v
    ka = theta6  # FIX
    
    d/dt(depot) = -ka * depot
    f(depot) = BA
    
    d/dt(center) = ka * depot - ke * center
    cp = center / v
    cp ~ prop(theta7)
  })
}
