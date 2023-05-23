# Pyrazinamide population pk model

# PK model description ----------------------------------------------
des_intro <- "Vancomycin iv model for patients with open heart surgery"
des_notes <- c("- measurement time is recommended to be matched with the first dose",
               "<br>",
               "- Detailed tracking of CLCR, ALB and weight is recommend to reflect physiological changes in clearance and volume of distribution")
des_comp <- "center, peri"
des_cov <- "CLCR, ALB, weight" # Strict 

des_params <- c("- V: volume of distritubtion (Vancomycin)","<br>",
                "- Cl: clearance (Vancomycin)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("CLCR","ALB", "WT")
mod_lcov = NULL # covariates with dropdown list
mod_lcov_value <- NULL
mod_cov_abbr <- c("Creatinine CL", "Albumin concentration", "weight")

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
scheme_image <- "https://els-jbs-prod-cdn.jbs.elsevierhealth.com/cms/attachment/e9f4a16a-a113-4770-86c4-614b4895f30c/fx1.jpg"

# Compartment designation -------------------------------------------
mod_comp <- c(
  IV = 1,
  SDC = 1,
  NONE = 10
)

# Inter-individually Variable parameters ----------------------------
est_eta <-c('L/h'='cl',
            'L'='v1',
            'L'='v2',
            'L/h'='q')


sd_eta <- sqrt(c(0.0476, 0.004, 0.3481, 0.2882)) # put sd^2 value in this vector

# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(log(6.13))       # CL
    theta2 <- c(log(40))        # V1
    theta3 <- c(log(3.88))       # V2
    theta4 <- c(log(0.22))       # Q
    theta5 <- c(0.055)            # Additive error
    theta6 <- c(0.159)            # Proportional error
    
    eta1 ~ c(0.0476)         # Cl
    eta2 ~ c(0.004)          # V1
    eta3 ~ c(0.3481)         # v2
    eta4 ~ c(0.2882)         # q
  })
  model({
    cl <- (CLCR/83.5)**0.514 * (ALB/35.5)**0.854 * exp(theta1+eta1)
    v1 <- (WT/79.6)**0.466  * exp(theta2+eta2)
    v2 <- exp(theta3+eta3)
    q <- exp(theta4+eta4)
    k12 = q/v1
    k21 = q/v2
    ke = cl/v1
    d/dt(center) = - k12 * center + k21 * peri - ke * center
    d/dt(peri) = k12 * center - k21 * peri
    cp = center / v1
    cp ~ add(theta5)+prop(theta6)
  })
}

