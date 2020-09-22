# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Shiny ui (dashboard) libraries
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinyWidgets)
# Shiny server libraries
library(devtools)
library(rsconnect)
library(RxODE)
library(nlmixr)

library(reticulate)
library(dplyr)
library(ggplot2)
library(plotly)
# Data management
library(rhandsontable)

# System configuration ==========================
virtualenv_create(envname = "python_environment", python='python3.5') # python version setting
virtualenv_install("python_environment", packages = c('sympy')) # Install required packages
use_virtualenv("python_environment", required = FALSE) # Using virtual environment

# UI ============================================
ui <- dashboardPage(
  # ui header
  dashboardHeader(
    # changing logo
    title = shinyDashboardLogo(
      theme = "grey_dark",
      boldText = "CPC",
      mainText = "engine",
      badgeText = " alpha 0.5.1"
    )
    
    
  ),
  dashboardSidebar(
    # changing theme
    shinyDashboardThemes(
      theme = "grey_dark"
    ),
    # sidebar menu. icons available on https://fontawesome.com/icons?from=io
    sidebarMenu(
      menuItem("Demographic / History", tabName = "demo", icon = icon("address-card")),
      #menuItem("Medication History", tabName = "his", icon = icon("calendar")),
      menuItem("Parameter Estimation", tabName = 'est', icon = icon("dashboard")),
      menuItem("Simulation", tabName = "sim", icon = icon('chart-area')),
      menuItem("System Configuration", tabName = 'sys', icon = icon('cog'))
    )
  ),
  dashboardBody(
    tags$style(HTML("hr {border-top: 1px dashed #666666;}
                    ")),
    helpText("Loading the model..."), # Console-like status bar
    
    tabItems(
      # Patient information tab content
      tabItem(tabName = 'demo',
              fluidRow(
                box(title='Identification', width=4,
                    numericInput("ID","Patient Number",value=201950471),
                    textInput("FN","First Name (given)",value=NA),
                    textInput("LN","Last Name (family)",value=NA)),
                box(title='Demographics', width=4,
                    numericInput("WT","Weight (kg)",value=1.5),
                    numericInput("PCA","PCA (weeks)",value=32),
                    helpText("- Weight, PCA should be their first records"),
                    helpText('- ID, Weight, PCA input mendatory')),
                box(title='Infection state', width=4,
                    pickerInput("BACT","Bacterial Strain",choices = c("S.Aureus","Enterococci","P.Aeruginosa","A.Baumannii"),
                                multiple=TRUE, selected="S.Aureus",
                                choicesOpt = list(
                                  content = sprintf("<span class='label label-%s'>%s</span",
                                                    c("danger","success","warning","primary"), # number of the items should be the same under
                                                    paste('',c("S.Aureus","Enterococci","P.Aeruginosa","A.Baumannii")))
                                )),
                    numericInput("MIC","MIC value (mcg/mL)", value = 0.304),
                    numericInput("CFU","Colony-forming Unit", value = 1)
                ),
                box(title='Dosing History', width=12,
                    helpText("ID, Weight information, PCA value automatically filled in, possible to edit value if needed.",
                             "Right-click to delete/insert row", "Double-click on a cell to edit"),
                    rHandsontableOutput("doseh"),
                    hr(),
                    helpText("- H: Hour, M: Min. Should fill in"),
                    helpText("- AMT: mg vancomycin"),
                    helpText("- DUR: Duration (hours)")
                    ),
                box(title='Observation History', width=12,
                    helpText("ID, Weight information, PCA value automatically filled in, possible to edit value if needed.",
                             "Right-click to delete/insert row", "Double-click on a cell to edit"),
                    rHandsontableOutput("obsh"),
                    hr(),
                    helpText("- TYPE: SDC=Serum Drug Concentration (mg/L), CRP=CRP level (mg/L)"),
                    helpText("- WT: Weight (kg)"),
                    helpText("- PCA: Post-conceptional age (weeks)")
                    )
              )
              ), # demo tab closing
      tabItem(tabName = 'his',
              fluidRow(

              )), # history tab closing
      tabItem(tabName = 'est',
              fluidRow(
                box(title='Estimate Parameters',
                    helpText("Estimate the individual's volume of distribution / clearance by dosing and observation records."),
                    helpText("Results is plotted on each point of the patient's observations"),
                    br(),
                    actionButton("estbut", "RUN"),
                    br(),
                    br()
                ),
                box(title='Estimation Results'),
                valueBoxOutput('vde', width=3), # info box output: vd
                valueBoxOutput('cle', width=3),
                box(title='Plots',width=12,
                    helpText("- Observation is plotted as dots, Model Prediction is drawn as line"),
                    helpText("- Model-described residual errors plotted as vertical lines"),
                    plotlyOutput("sim_graph0"),
                    plotlyOutput("sim_bact0"))
              )), # estimation tab closing
      tabItem(tabName = 'sim',
              fluidRow(
                box(title='Dosing Options',
                    numericInput("MPK","Miligram Per Kilogram",value=NA),
                    numericInput("DURA","Infusion Duration (hours)", value=NA),
                    numericInput("INTER","Dosing Interval (hours)", value=NA),
                    numericInput("NUMB", "Number of doses", value=20)),
                box(title='Simulation Result',
                    htmlOutput("SAMT"),
                    tableOutput("SIMT"),
                    helpText("Model based steady state prediction. steady state time is set when 1% of concentration difference is observed")),
                box(title='Simulation Plot',width=12,
                    plotlyOutput("sim_graph1"),
                    plotlyOutput("sim_bact1")),
                box(title="Sim_res", Width=12, tableOutput("SIMT2"))
              )),
      tabItem(tabName = 'sys',
              fluidRow(
                box(title='History Table (NONMEM style table)', width=12,
                    tableOutput("dataf")),
                box(title ='Estimation Table (NONMEM style table)', width=12,
                    tableOutput("datae")),
                box(title = 'combined monod equation constant',
                    tableOutput('monod'))
              ))
    ),
    
    
  )
)


# Server ========================================
server <- function(input, output) {
  kgrow <- reactive({
    strain = input$BACT
    d <- as.data.frame(strain) # "S.Aureus","Enterococci","P.Aeruginosa","A.Baumannii"
    d$rate <- ifelse(d$strain=="S.Aureus",1.46,
                  ifelse(d$strain=="Enterococci",1.03,
                         ifelse(d$strain=="P.Aeruginosa",0.47,
                                ifelse(d$strain=="A.Baumannii",1.03,0))))
    d$w <- ifelse(d$strain=="S.Aureus",11,
                  ifelse(d$strain=="Enterococci",12,
                         ifelse(d$strain=="P.Aeruginosa",15,
                                ifelse(d$strain=="A.Baumannii",2,0))))
    d$wtd.rate <- d$rate * d$w
    sum(d$wtd.rate)/sum(d$w)
    # 
  })
  output$monod <- renderTable({
    kgrow()
  })
  # estimation ==================================
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
  
  # fitting in FOCEi
  fit.s <- eventReactive(input$estbut,{
    subset(
      nlmixr(object=f,data=data(), est="focei",
             control=foceiControl(covMethod="r,s",
                                  interaction=TRUE,
                                  maxOuterIterations = 0,
                                  #maxInnerIterations = 0,
                                  iter.max=0)
      ),
      select = -c(CPRED,CRES,CWRES,eta1,eta2,eta3,eta4,eta5, TM50, HILL, KGROWTH, KDEATH,
                  EMAXBACT, EC50BACT, GAMMABACT, KIN, EC50)
    )

  })
  output$vde <- renderValueBox({
    valueBox(
      "Volume of Dist.", value=round(fit.s()$V[nrow(fit.s())],2), icon=icon("inbox"), color="olive"
    )
  })
  output$cle <- renderValueBox({
    valueBox(
      "Clearance", value=round(fit.s()$Cl[nrow(fit.s())],2),icon=icon("level-down-alt"), color="yellow"
    )
  })
  
  output$datae <- renderTable(fit.s())
  # Simulation ==================================
  # inter-individual variability variables removed(ETAs)
  # Simulation reflects the last recorded PK, CRP, Bacterial counts
  # Estimated Parameter; BASEe, Ve, Cle, EMAXe, KOUTe should be provided in estimation
  
  # Residual variability visualized
  
  # Bacterial counts, CRP can be selected to be visualized or not
  
  
  # initial condition of dosing history and obersvation history
  df_doseh_ini <- data.frame(DATE=Sys.Date(), H=0, M=0, AMT=0, DUR=0)
  df_obsh_ini <- data.frame(DATE=Sys.Date(), H=c(0,0), M=c(0,0), TYPE=c("SDC","CRP"), VAL=c(0,0))
  
  # plotting onto UI with initial condition, and interaction-ready-state
  output$doseh <- renderRHandsontable({
    rhandsontable(df_doseh_ini, rowHeaders = NULL)
  })
  output$obsh <- renderRHandsontable({
    rhandsontable(df_obsh_ini, rowHeaders = NULL) %>% 
      hot_col(col = "TYPE", type = "dropdown", source = c("SDC","CRP"))
  })  
  
  # receives changed information on UI
  # ID=input$ID, WT=input$WT, PCA=input$PCA is combined with the information provided prior
  
  # translating histories into NONMEM-like dataset
  data <- reactive({
    doseh <- hot_to_r(input$doseh)
    doseh$MDV <- 1
    doseh$CMT <- 1
    doseh$AMT <- doseh$AMT
    doseh$RATE <- ifelse(doseh$DUR!=0, doseh$AMT/doseh$DUR, 0)
    
    obsh <- hot_to_r(input$obsh)
    obsh$MDV <- 0
    obsh$CMT <- ifelse(obsh$TYPE=="SDC", 1, 2)
    obsh$DV <- obsh$VAL
    
    dataf <- dplyr::bind_rows(obsh, doseh) # dosing, observation data merging
    # time ordering
    dataf <- dataf[order(dataf$DATE, dataf$H, dataf$M),]
    dataf$TIME <- difftime(paste0(dataf$DATE," ",dataf$H,":",dataf$M), # until
                           paste0(dataf[1,"DATE"]," ",dataf[1,"H"],":",dataf[1,"M"]), # from
                           units='hours')
    dataf$CRPZERO <- subset(dataf, TYPE=="CRP")[1,"VAL"]
    dataf$ID <- input$ID
    dataf$WT <- input$WT
    dataf$PCA <- input$PCA + dataf$TIME/168
    
    #subset(dataf, select= -c(DATE, H, M, TYPE))
    dataf[c('ID','TIME','AMT','RATE','DV','MDV','CMT','WT','PCA','CRPZERO')]
    # data.frame(ID=NA, TIME=NA, AMT=NA, RATE=NA, DV=NA, MDV=NA, CMT=NA, WT=NA, PCA=NA, CRPZERO=NA)
  })
  output$dataf <- renderTable({ data() })
  
  output$SAMT <- renderPrint({
    fit.s <- fit.s()
    sim_res <- sim_res1()
    
    mpk = input$MPK # Administered amount mg/kg
    infusion_time = ifelse(!is.na(input$DURA),input$DURA, 0) # Infusion Duration
    inter = input$INTER # Time gap between doses
    numb = input$NUMB # number of doses
    
    sim_res_trp <- sim_res[sim_res$time%%(round(inter/0.25)*0.25)==0,]
    sim_res_trp$dif <- c(0, diff(sim_res_trp$cp))
    sim_res_trp <- sim_res_trp[-1,]
    
    ss50 <- subset(sim_res_trp, dif/cp<0.5)$time[1] # 50% trough concentration difference
    ss99 <- subset(sim_res_trp, dif/cp<0.01)$time[1] # 1% trough concentration difference
    
    sim_res_ss <- data.frame(Trough = mean(sim_res[sim_res$time==ss99 + ifelse(infusion_time==0, -0.25, 0), "cp"]),
                             Average = mean(sim_res[sim_res$time>=ss99 & sim_res$time<=ss99+(round(inter/0.25)*0.25), "cp"]),
                             Peak = mean(sim_res[sim_res$time==ss99+(round(infusion_time/0.25)*0.25), "cp"]))

    
    output$SIMT <- renderTable({sim_res_ss})
    output$SIMT2 <- renderTable({sim_res_trp})
    cat(
      "<b> - Administered dose :</b>",
      input$MPK*input$WT, "mg","<br>",
      "<hr>",
      "<b> - Steady state :</b>",
      "  Takes", ss99, 'hours',
      "<br>","<br>"
    )
  })
  
  mod <- RxODE({
    # maturation params
    TM50 <- theta4
    HILL <- theta5
    # central compartmental params
    V <- Ve
    Cl <- Cle
    # bacterial params
    KGROWTH <- theta6
    KDEATH <- theta7
    EMAXBACT <- theta8
    EC50BACT <- theta9
    GAMMABACT <- theta10
    # crp level params
    BASE <- BASEe
    KOUT <- KOUTe
    KIN <- BASE*KOUT
    EC50 <- theta15*1000
    EMAX <- EMAXe
    # initial conditions
    centr(0) <- CON*V # DV * Estimaed Vd...!!!
    bact(0) <- BACINIT
    crp(0) <- CRPZERO
    # algebraic expressions
    K = Cl/V
    cp = centr/V
    # differential equations
    d/dt(centr) = -K*centr
    d/dt(crp)   = KIN + EMAX*bact/(EC50 + bact) - KOUT*crp
    d/dt(bact)  = KGROWTH*bact - KDEATH*bact - EMAXBACT*(centr/V)**GAMMABACT/(EC50BACT**GAMMABACT+(centr/V)**GAMMABACT)*bact
  })
  
  theta_s0 <- reactive({ # Dose simulation before estimation
    fit.s <- fit.s()
    data <- data() # User defined time-schedule
    
    theta <- c(
      CON = 0,
      
      CRPZERO = data$CRPZERO[1], # Last recorded CRP
      PCA = input$PCA, # Last recorded PCA
      WT = input$WT, # Last recorded WT
      BACINIT = 10**6,
      
      theta1  = 0.387,     # Add err PK
      Ve = (input$WT/70)*50.9,
      Cle = ((input$WT/70)**0.75) * (input$PCA**3.68) / ((input$PCA**3.68)+(31.2**3.68))*3.42,
      theta4  = 31.2,      # TM50
      theta5  = 3.68,      # Hill
      theta6  = 1.46,      # K growth
      theta7  = 0.187,     # K death
      theta8  = 1.52,      # Emax bact
      theta9  = input$MIC,     # EC50 bact
      theta10 = 4.99,     # Gamma bact
      theta11 = 0.162,    # Add err PD
      theta12 = 0.274,    # Prop err PD
      BASEe = 0.276,    # base     
      KOUTe = 0.0431,   # Kout
      theta15 = 1.22,     # EC50/10^3
      EMAXe = 0.134    # Emax
    )
  })
  
  theta_s1 <- reactive({
    fit.s <- fit.s()
    theta <- c(
      CON = fit.s$cp[nrow(fit.s)], # Last recorded concentration # DF
      
      CRPZERO = fit.s$crp[nrow(fit.s)], # Last recorded CRP
      PCA = fit.s$PCA[nrow(fit.s)], # Last recorded PCA
      WT = fit.s$WT[nrow(fit.s)], # Last recorded WT
      BACINIT = fit.s$bact[nrow(fit.s)],
      
      theta1  = 0.387,     # Add err PK
      Ve  = fit.s$V[nrow(fit.s)],      # Vd
      Cle  = fit.s$Cl[nrow(fit.s)],      # Cl
      theta4  = 31.2,      # TM50
      theta5  = 3.68,      # Hill
      theta6  = 1.46,      # K growth
      theta7  = 0.187,     # K death
      theta8  = 1.52,      # Emax bact
      theta9  = input$MIC,     # EC50 bact
      theta10 = 4.99,     # Gamma bact
      theta11 = 0.162,    # Add err PD
      theta12 = 0.274,    # Prop err PD
      BASEe = fit.s$BASE[nrow(fit.s)],    # base
      KOUTe = fit.s$KOUT[nrow(fit.s)],   # Kout
      theta15 = 1.22,     # EC50/10^3
      EMAXe = fit.s$EMAX[nrow(fit.s)]    # Emax
    )
  })
  
  sim_res0 <- reactive({ # Dose simulation before estimation
    
    theta <- theta_s0()
    data <-  data() # completed history table
    data_amt <- subset(data, !is.na(data$AMT))
    
    ev <- eventTable(amount.units="mg") %>%
      add.sampling(seq(from=0, to=as.numeric(data$TIME[nrow(data)]+24), by=1/4)) # by 0.25 hour = 15 min
    # add dosing event (reading the history table written)
    for (i in 1:nrow(data_amt)){
      ev$add.dosing(dose = data_amt[i,"AMT"], rate=data_amt[i,"RATE"], nbr.doses = 1, start.time = data_amt[i,"TIME"])  
    }
    sim_res <- rxSolve(mod, theta, ev, nsub=1)
  })
  
  sim_res1 <- reactive({
    
    theta <- theta_s1()

    mpk = input$MPK # Administered amount mg/kg
    infusion_time = input$DURA # Infusion Duration
    inter = input$INTER # Time gap between doses
    numb = input$NUMB # number of doses
    
    ev <- eventTable(amount.units="mg") %>%
      add.dosing(dose = theta["WT"]*mpk, rate=theta["WT"]*mpk/infusion_time, nbr.doses = numb, dosing.interval = inter) %>%
      add.sampling(seq(from=0,to=inter*numb+12,by=1/4)) # by 0.25 hour = 15 min
    
    sim_res <- rxSolve(mod, theta, ev, nsub=1)
  })
  
  # visualization ===============================
  
  output$sim_graph0 <- renderPlotly({
    data <- data()
    sim_res <- sim_res0()
    fit.s <- fit.s()
    ggp <- ggplot(sim_res) +
      geom_area(fill='#66FF99', aes(x=time, y=cp), alpha=0.25) +
      geom_line(color='#66FF99', aes(x=time, y=cp), size=0.4) +
      xlab("Time (hours)") +
      ylab("Concentration (mg/L)") +
      scale_x_continuous(breaks=seq(0,as.numeric(data$TIME[nrow(data)]+24),12), minor_breaks=seq(6,186,12)) +
      scale_y_continuous(breaks=seq(0,150,5)) +
      geom_line(color='#FFCC00', aes(x=time, y=crp), linetype='dotted') +
      
      geom_point(data=fit.s[fit.s$CMT=='crp',], color = '#FFCC00', aes(x=TIME,y=DV)) +
      geom_errorbar(data=fit.s[fit.s$CMT=='crp',], color = '#FFCC00', aes(x=TIME, y=DV, ymax=DV+IRES, ymin=DV-IRES),width=0.3) +
      
      geom_point(data=fit.s[fit.s$CMT=='centr',], color = '#66FF99', aes(x=TIME,y=DV)) +
      geom_errorbar(data=fit.s[fit.s$CMT=='centr',], color = '#66FF99', aes(x=TIME, y=DV, ymax=DV+IRES, ymin=DV-IRES),width=0.3) +
      # ggplot theme setting
      theme(legend.position='none',
            plot.background = element_rect(fill='transparent',colour=NA),
            panel.background = element_rect(fill='transparent',colour=NA),
            panel.grid.major = element_line(colour='grey50', size=0.05),
            panel.grid.minor = element_line(colour='grey50', size=0.05),
            axis.title.x = element_text(colour='grey50'),
            axis.title.y = element_text(colour='grey50'),
            axis.text = element_text(colour='grey50')
      )
    ggplotly(ggp)
  })
  
  output$sim_bact0 <- renderPlotly({
    data <- data()
    sim_res <- sim_res0()
    fit.s <- fit.s()
    sim_res$bact <- sim_res$bact/10**6
    ggp <- ggplot(sim_res) +
      geom_area(fill='#9933FF', aes(x=time, y=bact), alpha=0.25) +
      geom_line(color='#9933FF', aes(x=time, y=bact), size=0.4) +
      xlab("Time (hours)") +
      ylab("Relative Bacterial amount") +
      
      # ggplot theme setting
      theme(legend.position='none',
            plot.background = element_rect(fill='transparent',colour=NA),
            panel.background = element_rect(fill='transparent',colour=NA),
            panel.grid.major = element_line(colour='grey50', size=0.05),
            panel.grid.minor = element_line(colour='grey50', size=0.05),
            axis.title.x = element_text(colour='grey50'),
            axis.title.y = element_text(colour='grey50'),
            axis.text = element_text(colour='grey50')
      )
    ggplotly(ggp)
  })
  
  output$sim_graph1 <- renderPlotly({
    sim_res <- sim_res1()
    fit.s <- fit.s()
    ggp <- ggplot(sim_res, aes(x=time, y=cp)) +
      geom_area(fill='#F8766D', alpha=0.25) +
      geom_line(color='#F8766D', size=0.4) +
      #geom_point(data = subset(sim_res,time%%as.numeric(input$INTER)==0), color='#F8766D') + # trough Conc.
      #geom_point(data = subset(sim_res,time%%as.numeric(input$INTER)==as.numeric(input$DURA)), color='#F8766D') + # peak conc.
      xlab("Time (hours)") +
      ylab("Concentration (mg/L)") +
      scale_x_continuous(breaks=seq(0,input$INTER*input$NUMB+12,12), minor_breaks=seq(6,186,12)) +
      scale_y_continuous(breaks=seq(0,150,5)) +
      
      geom_line(color='#FFCC00', aes(x=time, y=crp), linetype='dotted') +
      
      
      #geom_label(aes(0,fit.s$cp[nrow(fit.s)]),label=round(fit.s$cp[nrow(fit.s)],3), nudge_x = 2.5, colour='white',fill="#F8766D") +
      
      # ggplot theme setting
      theme(legend.position='none',
            plot.background = element_rect(fill='transparent',colour=NA),
            panel.background = element_rect(fill='transparent',colour=NA),
            panel.grid.major = element_line(colour='grey50', size=0.05),
            panel.grid.minor = element_line(colour='grey50', size=0.05),
            axis.title.x = element_text(colour='grey50'),
            axis.title.y = element_text(colour='grey50'),
            axis.text = element_text(colour='grey50')
      )
    ggplotly(ggp)
  })
  
  output$sim_bact1 <- renderPlotly({
    data <- data()
    sim_res <- sim_res1()
    fit.s <- fit.s()
    sim_res$bact <- sim_res$bact/10**6
    ggp <- ggplot(sim_res) +
      geom_area(fill='#9933FF', aes(x=time, y=bact), alpha=0.25) +
      geom_line(color='#9933FF', aes(x=time, y=bact), size=0.4) +
      xlab("Time (hours)") +
      ylab("Relative Bacterial amount") +
      scale_y_continuous(breaks=seq(0,5,1)) +
      
      # ggplot theme setting
      theme(legend.position='none',
            plot.background = element_rect(fill='transparent',colour=NA),
            panel.background = element_rect(fill='transparent',colour=NA),
            panel.grid.major = element_line(colour='grey50', size=0.05),
            panel.grid.minor = element_line(colour='grey50', size=0.05),
            axis.title.x = element_text(colour='grey50'),
            axis.title.y = element_text(colour='grey50'),
            axis.text = element_text(colour='grey50')
      )
    ggplotly(ggp)
  })
  
} # Server end

# Run the application 
shinyApp(ui = ui, server = server)
