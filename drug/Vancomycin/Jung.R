# Vancomycin,Bacteria,CRP model (Jung, Goo et al.,2021) =============

# PK model description ----------------------------------------------
des_intro <- "Vancomycin IV model for pediatric patients with sepsis by MRSA"
des_notes <- c("- CRP measurement time is recommended to be matched with the first dose",
               "<br>",
               "- Detailed tracking of PCA and WT is recommend to reflect physiological changes in clearance and volume of distribution")
des_comp <- "Central, Bacterial count, C-Reactive protein"
des_cov <- "Postconceptual age, Weight" # Strict 

des_params <- c("- Base: decides the baseline CRP level in individual","<br>",
                "- V: volume of distritubtion (vancomycin)","<br>",
                "- Cl: clearance (vancomycin)","<br>",
                "- Emax: decides the efficacy of vancomycin towards CRP level normalization","<br>",
                "- Kout: basal CRP normalization rate in infected individual")

# observation value -------------------------------------------------
mod_obs <- c("SDC", "CRP") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration", "C-reactive protein")

mod_cov <- c("PCA", "WT")
mod_lcov = NULL # covariates with dropdown list
mod_lcov_value <- NULL
mod_cov_abbr <- c("Postconceptual age", "Weight")

mod_route <- c("IV", "PO", "SC")

# plot option -------------------------------------------------------
pk <- "cp"
pk_obs <- "SDC"
pk_color <- '#FF6666'
pk_x_label <- "Time (hours)"
pk_y_label <- "Vancomycin Conc. (mg/L)"

pd <- "crp"
pd_obs <- "CRP"
pd_color <- '#FFCC66'
pd_x_label <- "Time (hours)"
pd_y_label <- "C-Reactive Protein Conc. (mg/L)"

# model scheme image ------------------------------------------------
scheme_image <- "https://els-jbs-prod-cdn.jbs.elsevierhealth.com/cms/attachment/e9f4a16a-a113-4770-86c4-614b4895f30c/fx1.jpg"


# Compartment designation -------------------------------------------
mod_comp <- c(
  IV = 1,
  PO = 3,
  SDC = 1,
  CRP = 2,
  NONE = 10
)


# Inter-individually Variable parameters ----------------------------
est_eta <-c('mcg/mL'='BASE',
            'L'='V',
            'L/hr'='Cl',
            '-'='EMAX',
            '1/hr'='KOUT')

sd_eta <- sqrt(c(1.33, 0.194, 0.306, 0.521, 0.83)) # put sd^2 value in this vector


# Model file for estimation -----------------------------------------
f <- function(){
  ini({
    # thetas
    theta1  <- c(0.387)     # Add err PK
    theta2  <- c(log(50.9))      # V
    theta3  <- c(log(3.42))      # Cl
    theta4  <- c(31.2)      # TM50
    theta5  <- c(3.68)      # Hill
    theta6  <- fix(1.46)      # K growth
    theta7  <- fix(0.187)     # K death
    theta8  <- fix(1.52)      # Emax bact
    theta9  <- fix(0.304)     # EC50 bact
    theta10 <- fix(4.99)     # Gamma bact
    theta11 <- c(0.162)    # Add err PD
    theta12 <- c(0.274)    # Prop err PD
    theta13 <- fix(log(0.276))    # base
    theta14 <- fix(log(0.0431))   # Kout
    theta15 <- fix(1.22)     # EC50/10^3
    theta16 <- fix(0.134)    # Emax
    # ETAs
    eta1 ~ fix(1.33)                     # Base
    eta2 + eta3 ~ fix(0.194,          
                    -0.0159, 0.306) # Vd, Cl
    eta4 + eta5 ~ fix(0.521,          
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
    EMAX <- theta16*(1 + eta4)
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
