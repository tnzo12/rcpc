# Tacrolimus population pk model

# PK model description ----------------------------------------------
des_intro <- "Tacrolimus Oral model for the paediatric liver transplant population"
des_notes <- c("Dosage was adjusted by the
liver transplant team to keep tacrolimus Ctrough within the suggested target range of 5–15 ng ml−1 according to time post-transplantation and concomitant immunosuppression",
               "<br>",
               " All PK profiles were obtained at least 3 days after receiving the same dose of tacrolimus.")
des_comp <- c("depot","central", "peripheral")
des_cov <- c("WT") 

des_params <- c("- V: volume of distritubtion (Tacrolimus)","<br>",
                "- Cl: clearance (Tacrolimus)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("WT")
mod_lcov = NULL # covariates with dropdown list
mod_lcov_value <- NULL
mod_cov_abbr <- c("Body weight")

mod_route <- c("Oral")

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
  Oral = 1,
  SDC = 2,
  NONE = 10
)

# Inter-individually Variable parameters ----------------------------
est_eta <-c('L/h'='cl',
            'L'='v1',
            'L/h'='q')

sd_eta <- sqrt(c(0.52,0.98,0.73)) # put sd^2 value in this vector



# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(log(12.1))      # CL/F
    theta2 <- c(log(31.3))      # V1/F
    theta3 <- c(log(30.7))      # Q2/F
    theta4 <- c(290)       # V2/F (FIX)
    theta5 <- c(0.342)     # Ka
    #theta6 <- c(0.433)     # Tlag
    theta7 <- c(0.2)       # Proportional error
    
    eta1 ~ c(0.52)         # CL/F
    eta2 ~ c(0.98)         # V1/F
    eta3 ~ c(0.73)         # Q/F
    
    
  })
  model({
    # TVCL <- theta1 * ((WT/20)**0.75)
    # TVV1 <- theta2 * (WT/20)
    TVV2 <- theta4 * (WT/20)
    # TVQ <- theta3 * ((WT/20)**0.75)
    
    ka <- theta5 * ((WT/20)**0.75)
    #tlag <- theta6
    
    cl <- exp(theta1 + eta1) * ((WT/20)**0.75)
    v1 <- exp(theta2 + eta2) * (WT/20)
    q <- exp(theta3 + eta3) * ((WT/20)**0.75)
    v2 <- TVV2
    
    #ke <- theta1 / theta2
    ke = cl/v1
    k12 = q/v1
    k21 = q/v2
    
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot -k12 * center + k21 * peri - ke * center
    d/dt(peri) = k12 * center - k21 * peri
    
    #lag(depot) = exp(tlag)
    
    cp = center / v1
    cp ~ prop(theta7)  # Proportional error
  })
}
