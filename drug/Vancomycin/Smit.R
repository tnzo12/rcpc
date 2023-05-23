# Vancomycin population pk model

# PK model description ----------------------------------------------
des_intro <- "Vancomycin IV model for obese and non-obese patients."
des_notes <- c("a single intravenous infusion of vancomycin (obese patients: 12.5 mg kg−1, maximum 2500 mg; nonobese 1000 mg as fixed dose, all infused in 10 mg min−1)",
               "<br>",
               "Blood samples were collected 0.25, 0.5, 1, 1.5, 2, 3, 4, 6 and 12 h after end of infusion.")
des_comp <- c("central", "peripheral1", "peripheral2")
des_cov <- c("WT", "AGE") # Strict 

des_params <- c("- V: volume of distritubtion (Vancomycin)","<br>",
                "- Cl: clearance (Vancomycin)")

# observation value -------------------------------------------------
mod_obs <- c("SDC") # {**should be matched with compartment order in model equation}
mod_obs_abbr <- c("Serum drug concentration")

mod_cov <- c("AGE","WT")
mod_lcov = NULL # covariates with dropdown list
mod_lcov_value <- NULL
mod_cov_abbr <- c("Age","Body Weight")

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
            'L'='v3',
            'L/h'='q1',
            'L/h'='q2')

sd_eta <- sqrt(c(0.003,0.187)) # put sd^2 value in this vector


# Model file for estimation -----------------------------------------
f <- function() {
  ini({
    theta1 <- c(5.72)    # CL
    theta2 <- c(16.7)    # V1
    theta3 <- c(6.98)    # V2
    theta4 <- c(19.5)    # V3
    theta5 <- c(15.8)    # Q1
    theta6 <- c(5.21)    # Q2
    
    theta8 <- c(0.535)   # CL THETA 1
    theta9 <- c(0.0136)  # V1, V2 THETA2
    theta10 <- c(1.07)   # Additive error
    
    eta1 ~ c(0.003)
    # eta2 <- c(0.059)
    eta3 ~ c(0.187)
    
  })
  model({
    cl <- theta1 * exp(eta1)  * ((WT/70)**theta8)
    v1 <- theta2 *  exp(eta3) * (1 + theta9 * (AGE-36.5))
    v2 <- theta3 * (WT/70) * (1 + theta9 * (AGE-36.5))
    v3 <- theta4
    q1 <- theta5
    q2 <- theta6
    
    #ke <- theta1 / theta2
    ke = cl/ v1
    k12 = q1/v1 # cent to peri 1
    k21 = q1/v2 # peri 1 to cent
    k13 = q2/v1 # cent to peri 2
    k31 = q2/v3 # peri 2 to cent
    
    d/dt(center) = - k12 * center + k21 * peri1 - k13 * center + k31 * peri2 - ke * center
    d/dt(peri1) = k12 * center - k21 * peri1
    d/dt(peri2) = k13 * center - k31 * peri2
    
    cp = center / v1
    cp ~ add(theta10)
  })
}
