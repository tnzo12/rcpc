# Default model configuration on initial page

# PK model description ----------------------------------------------
des_intro <- "Description of chosen model will appear here"
des_notes <- c("- Some notes for the model will appear here",
               "<br>",
               "- ex) Characteristics of covariates")
des_comp <- "Compartment information"
des_cov <- "Covariate information" # Strict 

des_params <- c("- Ka: Absorption rate constant","<br>",
                "- Vc: Central volume of distritubtion","<br>",
                "- Cl: clearance (vancomycin)","<br>",
                "- Q: Inter-compartmental clearance","<br>",
                "- Vp: Peripheral volume of distribution","<br>",
                "- VSEX: Covariate coefficient for V-SEX")
# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("Cov1", "Cov2", "Cov3")
mod_lcov = c("Cov3") # covariates with dropdown list
mod_lcov_value <- list(Cov3 = c(Stat1=1, Stat2=2, Stat3=3))
mod_cov_abbr <- c("Covariate1", "Covariate2", "Covariate3")

mod_route <- c("IV", "PO")

# plot option -------------------------------------------------------
pk <- NA
pk_obs <- NA
pk_color <- NA
pk_x_label <- NA
pk_y_label <- NA

pd <- NA
pd_obs <- NA
pd_color <- NA
pd_x_label <- NA
pd_y_label <- NA

# model scheme image ------------------------------------------------
scheme_image <- "two_cmt.png"


# Compartment designation -------------------------------------------
mod_comp <- c(
  IV = 2,
  PO = 1,
  SDC = 2
)


# Inter-individually Variable parameters ----------------------------
est_eta <-c('L'='V',
            'L/hr'='Cl',
            'L/hr'='Q'
)

sd_eta <- sqrt(c(0.229, 0.605, 1.27)) # put sd^2 value in this vector



# Model file for estimation
f <- function(){
  ini({
    # thetas
    theta1  <- c(0.387)     # Add err PK
    theta2  <- c(log(50.9))      # Vd
    theta3  <- c(log(3.42))      # Cl
    theta4  <- c(31.2)      # TM50
    theta5  <- c(3.68)      # Hill
    theta6  <- c(1.46)      # K growth
    theta7  <- c(0.187)     # K death
    theta8  <- c(1.52)      # Emax bact
    theta9  <- c(0.304)     # EC50 bact
    theta10 <- c(4.99)     # Gamma bact
    theta11 <- c(0.162)    # Add err PD
    theta12 <- c(0.274)    # Prop err PD
    theta13 <- c(log(0.276))    # base
    theta14 <- c(log(0.0431))   # Kout
    theta15 <- c(1.22)     # EC50/10^3
    theta16 <- c(0.134)    # Emax
    # ETAs
    eta1 ~ c(1.33)                     # Base
    eta2 + eta3 ~ c(0.194,          
                    -0.0159, 0.306) # Vd, Cl
    eta4 + eta5 ~ c(0.521,          
                    -0.435, 0.83)   # Emax, Kout
  })
  model({
    # maturation params
    TM50 <- theta4
    HILL <- theta5
    # central compartmental params
    V <- (WT/70)*exp(theta2 + eta2)
    Cl <- ((WT/70)**0.75) * (PCA**HILL) / ((PCA**HILL)+(TM50**HILL))*exp(theta3 + eta3)
    # bacterial params
    KGROWTH <- theta6
    KDEATH <- theta7
    EMAXBACT <- theta8
    EC50BACT <- theta9
    GAMMABACT <- theta10
    BACINIT <- 10**6
    # crp level params
    BASE <- exp(theta13 + eta1)
    KOUT <- exp(theta14 + eta5)
    KIN <- BASE*KOUT
    EC50 <- theta15*1000
    EMAX <- theta16*(1+eta4)
    # initial conditions
    bact(0) <- BACINIT
    crp(0) <- CRPZERO
    # algebraic expressions
    K = Cl/V
    cp = centr/V
    # differential equations
    d/dt(centr) = -K*centr
    d/dt(crp)   = KIN + EMAX*bact/(EC50 + bact) - KOUT*crp
    d/dt(bact)  = KGROWTH*bact - KDEATH*bact - EMAXBACT*(centr/V)**GAMMABACT/(EC50BACT**GAMMABACT+(centr/V)**GAMMABACT)*bact
    # error model
    cp ~ add(theta1) | centr
    crp ~ prop(theta12) + add(theta11) | crp
  })
}
