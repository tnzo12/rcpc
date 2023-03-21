# Isoniazid population pk model

# PK model description ----------------------------------------------
des_intro <- "Isoniazid po model for patients with pulmonary tuberculosis"
des_notes <- c("- measurement time is recommended to be matched with the first dose",
               "<br>",
               "- Detailed tracking of NAT2 and WT is recommend to reflect physiological changes in clearance and volume of distribution")
des_comp <- "depot, central, peripheral"
des_cov <- "polymorphisms in NAT2, Weight" # Strict 

des_params <- c("- V: volume of distritubtion (isoniazid)","<br>",
                "- Cl: clearance (isoniazid)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("NAT2", "WT")
mod_cov_abbr <- c("NAT2 genotype", "Weight")
mod_route <- c("PO")

# plot option -------------------------------------------------------
pk <- "ipred"
pk_obs <- "SDC"
pk_color <- '#FF6666'
pk_x_label <- "Time (hours)"
pk_y_label <- "Isoniazid Conc. (mg/L)"

# model scheme image ------------------------------------------------
scheme_image <- "https://els-jbs-prod-cdn.jbs.elsevierhealth.com/cms/attachment/e9f4a16a-a113-4770-86c4-614b4895f30c/fx1.jpg"


# Compartment designation -------------------------------------------
mod_comp <- c(
  PO = 1,
  SDC = 2,
  NONE = 10
)


# Inter-individually Variable parameters ----------------------------
est_eta <-c('L/hr'='cl',
            'L'='Q',
            '1/hr'='ka')

sd_eta <- sqrt(c(0.061, 0.334, 0.061)) # put sd^2 value in this vector


# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(log(31.4))   # 1. CL
    theta2 <- c(log(21.1))   # 2. v2
    theta3 <- c(log(27.7))   # 3. v3
    theta4 <- c(log(43.7))   # 4. Q
    theta5 <- c(log(1.70))   # 5. Ka
    theta6 <- c(log(0.930))  # 6. Weight
    theta7 <- c(log(0.378))  # 7. F for slow acetylator
    theta8 <- c(log(1.36))   # 8. F for fast acetylator
    theta9 <- c(0.061)       # 9. Proportional error
    eta1 ~ c(0.061)         # Cl
    eta2 ~ c(0.334)         # Q
    eta3 ~ c(0.446)         # Ka
  })
  model({
    if(NAT2==0){F = theta7;}
    else if(NAT2==1){F=1;}
    else {F=theta8; }
    cl <- F * (WT/58)**theta6 * exp(theta1 + eta1)
    v2 <- exp(theta2)
    v3 <- exp(theta3)
    Q  <- exp(theta4 + eta2)
    ka <- exp(theta5 + eta3)
    ke = cl/v2
    k12 <- Q / v2 
    k21 <- Q / v3
    d/dt(depot) = - ka * depot
    d/dt(center) = Q * depot - k12 * center + k21 * periph - ke * center
    d/dt(periph) = k12 * center - k21 * periph
    cp = center / v2
    cp ~ prop(theta9)
  })
} 
