# phenobarbital population pk model

# PK model description ----------------------------------------------
des_intro <- "Phenobarbital IV/enteral model for pediatric patients(<19 years)"
des_notes <- c("- measurement time is recommended to be matched with the first dose",
               "<br>",
               "- Detailed tracking of age is recommend to reflect physiological changes in clearance and volume of distribution")
des_comp <- "central"
des_cov <- "Fat free mass(kg), Postmenstrual age(wk), age(y), phenytoin (yes/no), midazolam(yes/no), pantoprazole(yes/no), SCR" # Strict 

des_params <- c("- V: volume of distritubtion (Phenobarbital)","<br>",
                "- Cl: clearance (Phenobarbital)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("FFM", "PNA", "AGE", "PHENY", "MIDAZ", "PANTOP", "SCR")
mod_cov_abbr <- c("Fat free mass(kg)", "Postmenstrual age(wk)", "age(y)", "phenytoin (yes/no)", "midazolam(yes/no)", "pantoprazole(yes/no)", "SCR")

mod_route <- c("IV", "enteral")

# plot option -------------------------------------------------------
pk <- "cp"
pk_obs <- "SDC"
pk_color <- '#FF6666'
pk_x_label <- "Time (hours)"
pk_y_label <- "Phenobarbital Conc. (mg/L)"

pd <- NA
pd_obs <- NA
pd_color <- NA
pd_x_label <- NA
pd_y_label <- NA

# model scheme image ------------------------------------------------
scheme_image <- "https://els-jbs-prod-cdn.jbs.elsevierhealth.com/cms/attachment/e9f4a16a-a113-4770-86c4-614b4895f30c/fx1.jpg"

# Compartment designation -------------------------------------------
mod_comp <- c(
  enteral = 1,
  IV=2,
  SDC = 2,
  NONE = 10
)

# Inter-individually Variable parameters ----------------------------
est_eta <-c('L/h'='cl',
            'L'='v',
            '1/h'='ka',
            '1/h'='ke')

sd_eta <- sqrt(c(0.181, 0.105)) # put sd^2 value in this vector

# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(log(0.372))  # CL
    theta2 <- c(log(62.5))   # V
    theta3 <- c(0.89)        # F
    theta4 <- c(0.8)         # KAFIX
    theta5 <- c(0.265)       # CL- SCR
    theta6 <- c(-4.22)       # CL- PNA
    theta7 <- c(0.596)       # CL- PHENY
    theta8 <- c(0.761)       # CL- MIDAZ
    theta9 <- c(1.25)        # CL- PANTOP
    theta10 <- c(0.981)      # V - AGEYRS
    theta11 <- c(0.148)      # Proportional error
    eta1 ~ c(0.181)           # Cl
    eta2 ~ c(0.105)           # V
  })
  
  model({

    fdepot <- theta3
    
    cl <-  ((FFM/70)**0.75) * (0.4/SCR)**theta5 * (1/(1+((PNA/7 + 40)/41)**theta6)) * theta7**PHENY * theta8**MIDAZ * theta9**PANTOP * exp(theta1 + eta1)
    v <- (FFM/70) * theta10**log(AGE/0.04384) * exp(theta2 + eta2) 
    ke = cl/v
    
    ka <- theta4
    
    d/dt(depot) = - ka * depot
    d/dt(center) = ka * depot - ke * center
    cp = center / v
    cp ~ prop(theta11)
  })
}

