# voriconazole population pk model
# Michaelis-Menten 식 활용한 Time에 따른 V 계산, 실패.

# PK model description ----------------------------------------------
des_intro <- "Voriconazole iv model for patients with Invasive Aspergillosis"
des_notes <- c("- measurement time is recommended to be matched with the first dose",
               "<br>",
               "- Detailed tracking of CLCR, SEX and weight is recommend to reflect physiological changes in clearance and volume of distribution")
des_comp <- "center, peri"
des_cov <- "SEX, weight" # Strict 

des_params <- c("- V: volume of distritubtion (Voriconazole)","<br>",
                "- Cl: clearance (Voriconazole)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("SEX", "WT")
mod_lcov = c("SEX") # covariates with dropdown list
mod_lcov_value <- list(SEX = c('Male'= 1, 'Female'= 0))
mod_cov_abbr <- c("SEX", "weight")

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
    theta1 <- c(1.15)         # Km
    theta2 <- c(113)        # Vmax,1
    theta3 <- c(1.50)         # Vmax,inh
    theta4 <- c(2.42)         # T 50
    theta5 <- c(5.30)          # CL
    theta6 <- c(77.6)          # V2 Central
    theta7 <- c(89.5)          # V3 Peripheral
    theta8 <- c(15.9)          # Q
    theta9 <- c(1.2)           # Ka
    theta10 <- c(1)            # Alag
    theta11 <- c(12.8)         # Rate
    theta12 <- c(7.28)             # Residual error IV
    theta13 <- c(7.810)             # Residual error Oral
    
    
    eta1 ~ c(1.91)         # Km - Vmax,1
    eta2 ~ c(0.583)        # Vmax, scale
    eta3 ~ c(0.634)        # CL
    eta4 ~ c(0.139)        # V2 Central
    eta5 ~ c(0.831)        # V3 Peripheral
    eta6 ~ c(0.459)        # Q
    eta7 ~ c(0.713)        # F1
    eta8 ~ c(0.411)        # BC-F
    eta9 ~ c(0.910)        # R2
    
  })
  model({
    tlag
    ka = 
    
    cl <- (exp(theta1 + eta1)) * (WT/70)^0.75
    v1 <- theta4 * WT/70
    v2 <- theta6 * (WT/70)^0.75
    q <- theta5 * (WT/70)^0.75
    ke = cl/v1
    k12 = q/v1
    k21 = q/v2
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot -k12 * center + k21 * peri - ke * center
    d/dt(peri) = k12 * center - k21 * peri
    
    lag(depot) = exp(tlag)
    
    cp = center / v1
    cp ~ prop(theta8)
  })
}

