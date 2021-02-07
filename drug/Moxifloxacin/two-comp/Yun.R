# MDR-TB-Moxiflxacin model (Preparing publication, Yun, Savic et al., 2021) =============

# PK model description ----------------------------------------------
des_intro <- "Moxifloxacin two-compartment model for MDR-TB patients"
des_notes <- c("- Moxifloxacin for MDR-TB patietns",
               "<br>",
               "- Covariate: SEX would be effected by Vcentral")
des_comp <- "GI, Vc, Vp"
des_cov <- "SEX" # Strict 

des_params <- c("- Ka: Absorption rate constant","<br>",
                "- Vc: Central volume of distritubtion","<br>",
                "- Cl: clearance (vancomycin)","<br>",
                "- Q: Inter-compartmental clearance","<br>",
                "- Vp: Peripheral volume of distribution","<br>",
                "- VSEX: Covariate coefficient for V-SEX")
# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("SEX")
mod_cov_abbr <- c("Gender (0 for women and 1 for men)")

mod_route <- c("IV", "PO")

# plot option -------------------------------------------------------
pk <- "ipred"
pk_obs <- "SDC"
pk_color <- '#FF6666'
pk_x_label <- "Time (hours)"
pk_y_label <- "Moxifloxacin Conc. (mg/L)"

pd <- NA
pd_obs <- NA
pd_color <- NA
pd_x_label <- NA
pd_y_label <- NA

# model scheme image ------------------------------------------------
scheme_image <- "http://www.turkupetcentre.net/petanalysis/pic/pk-2cm.svg"


# Compartment designation -------------------------------------------
mod_comp <- c(
  IV = 2,
  PO = 1,
  SDC = 2
)


# Inter-individually Variable parameters ----------------------------
est_eta <-c('L'='Vc',
            'L/hr'='Cl',
            'L/hr'='Q'
            )

sd_eta <- sqrt(c(0.229, 0.605, 1.27)) # put sd^2 value in this vector

# Model file for estimation -----------------------------------------
f <- function(){
  ini({
    # thetas
    theta1  <- c(0.488)     # Propotional err for PK
    theta2  <- c(log(1.15)) # lKA
    theta3  <- c(log(157))  # lV
    theta4  <- c(log(8.39)) # lCL
    theta5  <- c(log(3.63)) # lQ
    theta6  <- c(log(12400))# lVP
    theta7  <- c(-0.15)     # VSEX1
    # ETAs
    eta1 + eta2 + eta3 ~ c(0.229,
                           0.335, 0.605,
                           -0.056, -0.234, 1.27) # V-CL-Q-ETA
  })
  model({
    # Covariate params
    VSEX1 <- theta7
    # central compartmental params
    KA <- exp(theta2)
    Vc <- exp(theta3 + eta1)*(1+VSEX1*SEX)
    Cl <- exp(theta4 + eta2)
    Q <- exp(theta5 + eta3)
    Vp <- exp(theta6)
    # algebraic expressions
    K = Cl/Vc
    K23 = Q/Vc
    K32 = Q/Vp
    cp = centr/Vc
    # differential equations
    d/dt(GI) = -KA*GI
    d/dt(centr) = KA*GI - K*centr - K23*centr + K32*peri
    d/dt(peri) = K23*centr - K32*peri
    # error model
    cp ~ prop(theta1) | centr
  })
}