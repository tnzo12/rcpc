#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# UI side libraries -------------------
library(shiny)
library(bs4Dash)
library(networkD3)
library(rhandsontable)
library(formattable)
library(plotly)
library(shinyWidgets)
library(shinycssloaders)

# Server side libraries ---------------
library(shiny)
library(devtools)
library(rxode2)
library(nlmixr2)
library(symengine)

library(dplyr)
library(tidyr)
library(shinyjs)

# Data management
library(rhandsontable)
library(formattable)
library(kableExtra)
library(knitr)

# Visualization
library(networkD3)
library(ggplot2)
library(plotly)

# Addition
library(n1qn1)
library(PreciseSums)

# Shiny modules
source("1.mods.R", local=TRUE)
source("2.des.R", local=TRUE)

# Rhandsontable css theme
# https://handsontable.com/docs/3.0.0/tutorial-styling.html
css <- "
.handsontable tbody th.ht__highlight,
.handsontable thead th.ht__highlight {
  background-color: #ffb347;
}

.handsontable.ht__selection--columns thead th.ht__highlight,
.handsontable.ht__selection--rows tbody th.ht__highlight {
  background-color: #ffb347;
  color: #FFF;
}

.wtBorder {
  background-color: #ffb347!important;
}

.handsontable td.area {
  background: -moz-linear-gradient(top,  rgba(181,209,255,0.34) 0%, rgba(181,209,255,0.34) 100%); /* FF3.6+ */
  background: -webkit-gradient(linear, left top, left bottom, color-stop(0%,rgba(181,209,255,0.34)), color-stop(100%,rgba(181,209,255,0.34))); /* Chrome,Safari4+ */
  background: -webkit-linear-gradient(top,  rgba(181,209,255,0.34) 0%,rgba(181,209,255,0.34) 100%); /* Chrome10+,Safari5.1+ */
  background: -o-linear-gradient(top,  rgba(181,209,255,0.34) 0%,rgba(181,209,255,0.34) 100%); /* Opera 11.10+ */
  background: -ms-linear-gradient(top,  rgba(181,209,255,0.34) 0%,rgba(181,209,255,0.34) 100%); /* IE10+ */
  background: linear-gradient(to bottom,  rgba(181,209,255,0.34) 0%,rgba(181,209,255,0.34) 100%); /* W3C */
  filter: progid:DXImageTransform.Microsoft.gradient( startColorstr='#57b5d1ff', endColorstr='#57b5d1ff',GradientType=0 ); /* IE6-9 */
  background-color: #673800;
}


.htContextMenu table tbody tr td {
  background: #7a8188;
  color: white;
  border-width: 0;
  border-radius: 0px;
  cursor: pointer;
  overflow: hidden;
  white-space: nowrap;
  text-overflow: ellipsis;
}

.htContextMenu table tbody tr td.current,
.htContextMenu table tbody tr td.zeroclipboard-is-hover {
  background: #59626A;
}

.handsontable th {
  background-color: #7a8188;
  color: white;
  font-size: 14px;
  border-radius: 3px;
}

.handsontable td {
  background-color: #59626A;
  color: white;
  font-size: 14px;
  border-radius: 3px;
}

.handsontableInput {
  border:none;
  outline-width: 0;
  margin: 0;
  font-family: inherit;
  line-height: 21px;
  font-size: inherit;
  resize: none;
  box-shadow: 0 0 0 2px #ffb347 inset;
  /*below are needed to overwrite stuff added by jQuery UI Bootstrap theme*/
  display: inline-block;
  color: white;
  border-radius: 4px;
  background-color: transparent;
}

"

ui <- dashboardPage(
  
  fullscreen = TRUE,
  
  dashboardHeader(
    title = HTML("<span style='color:grey'>
          R CPC v0.5.0
               </span>"),
    skin ='light',
    fixed = TRUE,
    compact = TRUE,
    actionButton(
      label = "Run Model",
      icon = icon('sync-alt'),
      inputId = "run_button",
      # icon: https://fontawesome.com/icons?from=io
    ),
    
    HTML("&nbsp;"), # spacing
    
    dropdown(
      label = "Estimation Settings",
      icon = icon('cog'),
      numericInput(
        inputId = "vpc_opt",
        label = HTML("<span style='color:grey'><i>
          Virtual Individuals
               </i></span>"),
        value = 50,
        min = 0,
        max = 2000,
        step = 1
      ),
      numericInput(
        inputId = "vpc_seed",
        label = HTML("<span style='color:grey'><i>
          Seed (iteration)
               </i></span>"),
        value = 1,
        min = NA,
        max = NA,
        step = 1
      ),
      numericInput(
        inputId = "step_size",
        label = HTML("<span style='color:grey'><i>
          Integration Step Size
               </i></span>"),
        value = 1/4,
        min = 0,
        max = 5,
        step = 1/4
      ),
      HTML("<hr size='1px', style='color:#e0e0e0;border-style:dashed'>"), # horizental line
      HTML("<span style='color:grey'><i>
      [Manipulate estimation specs] <br>
      *Decreasing the number of virtual individual helps the performance of the application. Individuals lessor then 20 is not recommended <br>
      *Running the model in same seed number reproduces identical iteration results <br>
      *Integration step size decides the resolution of the results. Higher resolution (lower step size) can give precise results in minutes-controlled situation,
      but computational burden steeply increases. In caeses with over 2 weeks of observation step size higher then 0.5 (hour) is recommened
               </i></span>")
      
      
    )
    
  ),
  
  dashboardSidebar(
    
    h6("[Institude]"),
    skin = 'light', # Theme
    width = '300px',
    sidebarUserPanel(
      'Chunnam National University', image = "https://res-3.cloudinary.com/crunchbase-production/image/upload/c_lpad,h_170,w_170,f_auto,b_white,q_auto:eco/oathengskxv2qsas7epw"
    ),
    h6("[User]"),
    sidebarUserPanel(
      'uzis', image = "https://www.flaticon.com/svg/static/icons/svg/3824/3824068.svg"
    ),
    
    elevation = 4,
    expandOnHover = FALSE,
    minified = FALSE,
    collapsed = TRUE
    
  ),
  
  dashboardBody(
    tags$head(
      
    ),
    fluidRow(
      column(
        width=12,
        box(
          width=12,
          title = "INFORMATION", # =====================================================
          collapsed = TRUE,
          background = 'gray',
          gradient = TRUE,
          elevation = 2,
          "Information about the patient should put"
          
        )    
      )
    ),
    fluidRow(
      box(
        width=6,
        title = "Patient Info",
        elevation = 3,
        
        HTML("<span style='color:grey'><i>
          *The information below is only used to identify patients
               </i></span>"),
        HTML("<hr size='1px', style='color:#e0e0e0;border-style:dashed'>"), # horizental line
        div(style="display: inline-block;vertical-align:top; width: 100%;",
            numericInput(
              inputId = "ID",
              label = "Patient number",
              value = 201950471,
              min = NA,
              max = NA,
              step = 1
            )),
        div(style="display: inline-block;vertical-align:top; width: 100%;",
            textInput(
              inputId = 'pat_name',
              label = 'Patient Name',
              value = NULL,
            )),
        div(style="display: inline-block;vertical-align:top; width: 100%;",
            radioGroupButtons(
              inputId = "SEX",
              label = "Sex",
              choices = c('Male','Female'),
              justified = TRUE,
              individual = TRUE,
              checkIcon = list(
                yes = tags$i(class = "fa fa-circle", 
                             style = "color: slategray"),
                no = tags$i(class = "fa fa-circle-o", 
                            style = "color: slategray"))
            )),
        HTML("<span style='color:grey'><i>
          Necessary input values are made to be represented in combine with history table below
               </i></span>")
        
      ),
      tabBox(
        id = 'calcul_tab',
        width=6,
        elevation = 3,
        type = 'tabs',
        status = 'gray',
        solidHeader = TRUE,
        
        tabPanel(
          title = '[Calculator]',
          HTML("<span style='color:grey'><i>
          *This tab is for various calculation related to the physical dimensions of the patient's
              <hr size='1px', style='color:#e0e0e0;border-style:dashed'>
          - 'Ages' tab: Calculate the age in days, weeks, hours if two dates are set <br><br>
          - 'Weights' tab: Calculate the kinds of weight metrics <br><br>
          - 'BSA' tab: Calculate body surface area <br><br>
                   Not all the model requires such information and calculated values are not recommended being rounded for precise model estimation if the selected model requires<br><br>
               </i></span>"),
        ),
        tabPanel(
          title = 'Ages',
          div(style="display: inline-block;vertical-align:top; width: 45%;",
              dateInput(inputId = "date_start", label = "Starting Date", value = Sys.Date(), format = 'mm-dd-yyyy')),
          div(style="display: inline-block;vertical-align:top; width: 45%;",
              dateInput(inputId = "date_end", label = "Ending Date", value = Sys.Date(), format = 'mm-dd-yyyy')),
          htmlOutput("age_cal")
        ),
        tabPanel(
          title = 'Weights'
        ),
        tabPanel(
          title = 'BSA'
        )
      ),
      
      box(
        width=5,
        title = "Model Selection",
        elevation = 3,
        
        selectInput(
          
          inputId = 'drug_selection',
          label = 'Select drug',
          choices = c(list.files("./drug/"))
          
        ),
        HTML("<span style='color:grey'><i>
          [Click on a node to select model]
               </i></span>"),
        
        mods_ui("mod_netwk"),
        
        # Selected model
        
        verbatimTextOutput('drug'),
        des_model_ui("des_model")
        #htmlOutput('des_model')
      ),
      
      column(
        width=7,
        box(
          width=12,
          title = "Model Scheme",
          elevation = 3,
          HTML("<span style='color:grey'><i>
          *Scheme of the selected model
               </i></span>"),
          
          uiOutput(outputId = "scheme")
          
          
        ),
        box(
          width=12,
          title = "Notes",
          elevation = 3,
          HTML("<span style='color:grey'><i>
          *Abbreviations used in the selected model
               </i></span>"),
          br(),br(),
          
          des_abbr_ui("des_abbr"),
          des_notes_ui("des_notes")
        )
      ),
      
      
      box(
        width=8,
        title = "Patient History",
        elevation = 3,
        HTML("<span style='color:grey'><i>
          *Dosing History
               </i></span>"),
        br(),
        tags$head(tags$style(HTML(css))),
        rHandsontableOutput("doseh"),
        
        HTML("<span style='color:grey'><i>
          *Observation History
               </i></span>"),
        br(),
        rHandsontableOutput("obsh")),
      
      box(
        width=4,
        title = "History Information",
        elevation = 3,
        
        HTML("<span style='color:grey'><i>
          - Needed covariate information about selected model will be appeared in the observation history table <br><br>
          - Covariate value doesn't needed to be in every time points, but at least a single point of covariate value should be put in somewhere
               </i></span>")
      )
      
    ),
    fluidRow(
      column(
        width=12,
        box(
          width=12,
          title = "ESTIMATION", # ======================================================
          collapsed = TRUE,
          background = 'gray',
          gradient = TRUE,
          elevation = 2,
          "Estimate the model parameter based on the patient information input"
          
        )    
      )
    ),
    fluidRow(
      box(
        width=12,
        title = "Plot information",
        elevation = 3,
        maximizable = TRUE,
        
        htmlOutput("pkpd_des")
      ),
      box(
        width=6,
        title = "Pharmacokinetic Profiles",
        elevation = 3,
        maximizable = TRUE,
        HTML("<span style='color:grey'><i>
          *Time-Conc. plot of the patient given by the selected model
               </i></span>"),
        withSpinner(plotlyOutput("pk_est_plot"),
                    type = getOption("spinner.type", default = 8),
                    color = getOption("spinner.color", default = 'lightgrey'))
        
      ),
      box(
        width=6,
        title = "Pharmcodynamic Profiles",
        elevation = 3,
        maximizable = TRUE,
        HTML("<span style='color:grey'><i>
          *Time-Effect plot of the patient given by the selected model
               </i></span>"),
        withSpinner(plotlyOutput("pd_est_plot"),
                    type = getOption("spinner.type", default = 8),
                    color = getOption("spinner.color", default = 'lightgrey'))
        
      )
    ),
    fluidRow(
      box(
        width=6,
        maximizable = TRUE,
        title = "Parameter Variation",
        elevation = 3,
        HTML("<span style='color:grey'><i>
          *Each parameter's variation is represented in relative scale<br>
          *Parameters not being plotted follows the population value
               </i></span>"),
        withSpinner(plotlyOutput("param_vis"),
                    type = getOption("spinner.type", default = 8),
                    color = getOption("spinner.color", default = 'lightgrey'))
        
        
      ),
      box(
        width=6,
        title = "Estimation Result",
        elevation = 3,
        HTML("<span style='color:grey'><i>
          *Estimation results presented in a table
               </i></span>"),
        br(),br(),
        
        formattableOutput("param_table"),
        
        htmlOutput("des_params")
      )
      
      
      
    ),
    fluidRow(
      
      box(
        collapsed = TRUE,
        width=12,
        title = "Additonal Diagnosis",
        elevation = 3,
        HTML("<span style='color:grey'><i>
        [Goodness of Fit]
               </i></span>"),
        withSpinner(plotlyOutput("gof"),
                    type = getOption("spinner.type", default = 8),
                    color = getOption("spinner.color", default = 'lightgrey')),
        htmlOutput('gof_des'),
        br(),
        HTML("<span style='color:grey'><i>
        [Fitted Table]
               </i></span>"),
        tableOutput('fit_table')
        
        
        
      )
    ),
    
    
    
    fluidRow(
      column(
        width=12,
        box(
          width=12,
          title = "SIMULATION", # ======================================================
          collapsed = TRUE,
          background = 'gray',
          gradient = TRUE,
          elevation = 2,
          "Simulate dosings with estimation results"
          
        )    
      )
    ),
    fluidRow(
      box(
        width=8,
        title = "Simulation Options",
        elevation = 3,
        HTML("<span style='color:grey'><i>
          *Simulation History
               </i></span>"),
        rHandsontableOutput("sim_doseh"),
        br(),
        HTML("<span style='color:grey'><i>
          *Observation Period (days)
               </i></span>"),
        numericInput(inputId = 'sim_obs_period',
                     label = NULL,
                     value = 5,
                     min = 0,
                     max = 60,
                     step = 1)
      ),
      
      box(
        width=4,
        title = "Simulation Diagnosis",
        elevation = 3,
        
        HTML("<span style='color:grey'><i>
          *Simulation based steady state diagnosis
               </i></span>"),
        br(),br(),
        tableOutput('sim_conc_sum'),
        htmlOutput('sim_time_sum')
        
        
      ),
      column(
        width=12,
        box(
          width=12,
          title = "Simulation Plot",
          elevation = 3,
          HTML("<span style='color:grey'><i>
          *Simulated Time-Conc. (upper) and Time-Effect (lower) plots of the patient given by the selected model
               </i></span>"),
          withSpinner(plotlyOutput("pk_sim_plot"),
                      type = getOption("spinner.type", default = 8),
                      color = getOption("spinner.color", default = 'lightgrey')),
          withSpinner(plotlyOutput("pd_sim_plot"),
                      type = getOption("spinner.type", default = 8),
                      color = getOption("spinner.color", default = 'lightgrey'))
          
        )
      ),
      box(
        width=12,
        title = "Probability analysis",
        elevation = 3,
        
        HTML("<span style='color:grey'><i>
          Functions will be updated <br><br>
          - Diagnosis method selection <br>
          - Prediction Given by residuals
               </i></span>")
      ),
      box(
        width=12,
        collapsed = TRUE,
        title = "hist data()",
        elevation = 3,
        tableOutput("data_arr")
      ),
      box(
        width=12,
        collapsed = TRUE,
        title = "simulation without IIV",
        elevation = 3,
        tableOutput("data_arr2")
      ),
      box(
        width=12,
        collapsed = TRUE,
        title = "simulation with IIV",
        elevation = 3,
        tableOutput("data_arr7")
      ),
      box(
        width=12,
        collapsed = TRUE,
        title = 'obshis',
        elevation = 3,
        tableOutput("obshis")
      ),
      box(
        width=12,
        collapsed = TRUE,
        title = 'fit.s',
        elevation = 3,
        tableOutput("data_arr5")
      ),
      box(
        width=12,
        collapsed = TRUE,
        title = 'ev',
        elevation = 3,
        tableOutput("data_arr6")
      )
      
      
      
    )
    
    
    # fluid ends
  )
  
)

  

# Server ========================================
server <- function(input, output, session) {
  
  drug_selection <- shiny::reactive({ input$drug_selection }) # selected drug in picker
  model <- shiny::reactive({ input$model }) # selected model in network
  
  # model module server
  mods_server("mod_netwk", drug_selection, model) # select drug/model -> force network vis
  des_server("des_model", mod_env, model)
  des_server("des_notes", mod_env, model)
  des_server("des_abbr", mod_env, model)  # Load selected model environment
  
  mod_env <- reactive({
    drug_selection <- drug_selection()
    model <- model()
    
    ifelse(
      sum(paste0(
        drug_selection,"/",list.files(paste0("./drug/", drug_selection), # Specified drug name should be put
                                      pattern = paste0(model,".R"), recursive = TRUE) # Finding specified model name
      ) %in% list.files("./drug/", pattern = ".R", recursive = TRUE) ) == 1 # TRUE when only 1 model is returned
      ,
      
      # then
      source(
        paste0(
          "./drug/",drug_selection,"/",list.files(paste0("./drug/", drug_selection), # Specified drug name should be put
                                                  pattern = paste0(model, ".R"), recursive = TRUE) # Finding specified name
        )
      ),
      
      # else
      source("./default.R") # Default state (:no model loaded)
    )  
    
  }) # Loading model environment ends
  
  output$pat_info <- renderPrint({
    cat(
      '<span style="color:grey">',
      'PCA: Postconceptual age',"<br>",
      '</span>'
    )
  })
  
  output$age_cal <- renderPrint({
    cat(
      '<span style="color:grey">',
      "Calculated Time",
      "<hr size='1px', style='color:#e0e0e0;border-style:solid'>",
      "<i>- days:</i>",difftime(input$date_end, input$date_start, units = 'days'),
      "<hr size='1px', style='color:#e0e0e0;border-style:dashed'>",
      "<i>- weeks:</i>",difftime(input$date_end, input$date_start, units = 'weeks'),
      "<hr size='1px', style='color:#e0e0e0;border-style:dashed'>",
      "<i>- hours:</i>",difftime(input$date_end, input$date_start, units = 'hours'),
      '</span>'
    )
  })
  
  
  # Model based input table
  
  # Generating model specific table
  input_table <- reactive({
    mod_env()
    
    # Dosing initial state: list[[1]]
    df_doseh_ini <- data.frame(Date=Sys.Date(),
                               Hour=0,
                               Min=0,
                               Route=mod_route,
                               Amt=as.numeric(NA),
                               Dur=as.numeric(NA),
                               Rep=as.numeric(NA),
                               Inter=as.numeric(NA),
                               stringsAsFactors = FALSE)
    # Observation initial state: list[[2]]
    df_obsh_ini <- data.frame(Date=Sys.Date(),
                              Hour=0,
                              Min=0,
                              Type=mod_obs,
                              Val=as.numeric(NA),
                              stringsAsFactors = FALSE)
    for (i in 1:length(mod_cov) ){
      df_obsh_ini[ mod_cov[i] ] <- as.numeric(NA)
    }
    # Dosing initial state: list[[3]]
    df_sim_doseh_ini <- data.frame(Date=Sys.Date()+1,
                                   Hour=0,
                                   Min=0,
                                   Route=mod_route,
                                   Amt=as.numeric(NA),
                                   Dur=as.numeric(NA),
                                   Rep=as.numeric(NA),
                                   Inter=as.numeric(NA),
                                   stringsAsFactors = FALSE)
    
    list(df_doseh_ini, # [[1]]
         df_obsh_ini, # [[2]]
         df_sim_doseh_ini) # [[3]]
  })
  
  
  # Uploading generated (updated by node selection) table
  output$doseh <- renderRHandsontable({ # needed administration routes will appear in this table
    mod_env()
    rhandsontable(input_table()[[1]], rowHeaders = NULL, stretchH = "all") %>% 
      hot_col(col = "Route", type = "dropdown", source = mod_route) %>% 
      hot_col(col = "Hour", default = 0) %>% 
      hot_col(col = "Min", default = 0) %>% 
      #hot_cols(renderer=renderer, halign = "htCenter")
      hot_cols(halign = "htCenter")
    
    
    
  })
  
  output$obsh <- renderRHandsontable({ # needed observation types will appear in this table
    mod_env()
    rhandsontable(input_table()[[2]], rowHeaders = NULL,stretchH = "all") %>% 
      hot_col(col = "Type", type = "dropdown", source = mod_obs) %>% 
      hot_col(col = "Hour", default = 0) %>% 
      hot_col(col = "Min", default = 0) %>% 
      #hot_cols(renderer=renderer, halign = "htCenter")
      hot_cols(halign = "htCenter")
    
  })
  
  output$sim_doseh <- renderRHandsontable({ # needed administration routes will appear in this table
    mod_env()
    rhandsontable(input_table()[[3]], rowHeaders = NULL, stretchH = "all") %>% 
      hot_col(col = "Route", type = "dropdown", source = mod_route) %>% 
      hot_col(col = "Hour", default = 0) %>% 
      hot_col(col = "Min", default = 0) %>% 
      #hot_cols(renderer=renderer, halign = "htCenter")
      hot_cols(halign = "htCenter")
    
  })
  
  # Get inputs form buttons: estimation and simulation
  
  
  
  # Download table input from UI -> Processing ======================
  # Data processing method
  hist_data <- eventReactive(input$run_button, {
    mod_env() # load model's environment
    
    # Loading dosing history from ui input
    doseh <- rbind(
      cbind(hot_to_r(input$doseh),condi='est'),
      cbind(hot_to_r(input$sim_doseh),condi='sim')
    )
    doseh <- doseh %>%
      tidyr::fill_(fill_cols = c('Date', 'Route'), .direction = "down") %>%
      mutate_at(vars('Hour', 'Min', 'Amt', 'Dur', 'Inter'), ~replace_na(., 0)) %>% 
      mutate_at(vars('Rep'), ~replace_na(., 1))
    doseh <- doseh[!(doseh$Amt == 0), ] # get rid of the unused dosing (dosing amount of 0)
    
    # NONMEM like data processing
    doseh$MDV <- 1
    doseh$EVID <- 1
    doseh$CMT <- mod_comp[doseh$Route] # model dependent
    doseh$AMT <- doseh$Amt
    doseh$RATE <- ifelse(doseh$Dur!=0, doseh$Amt/doseh$Dur, 0)
    doseh$ADDL <- doseh$Rep
    doseh$II <- doseh$Inter
    
    # Loading observation history from ui input
    obsh <- hot_to_r(input$obsh)
    
    obsh$MDV <- 0
    doseh$EVID <- 1
    obsh$CMT <- mod_comp[obsh$Type]
    obsh <- obsh %>%
      tidyr::fill_(fill_cols = c('Date', 'Type'), .direction = "down") %>%  # to fill NAs in the f_data
      mutate_at(vars('Hour', 'Min', 'Val'), ~replace_na(., 0))
    obsh$DV <- obsh$Val
    obsh$condi <- 'est'
    
    
    
    
    output$obshis <- renderTable({obsh})
    
    f_data <- dplyr::bind_rows(obsh, doseh) # dosing, observation data merging
    # Time ordering
    f_data <- f_data[order(f_data$Date, f_data$Hour, f_data$Min), ]
    f_data$TIME <- difftime(paste0(f_data$Date," ",f_data$Hour,":",f_data$Min), # until
                            paste0(f_data[1,"Date"]," ",f_data[1,"Hour"],":",f_data[1,"Min"]), # from
                            units='hours')
    # Patient info
    f_data$ID <- input$ID
    
    
    # Only for model Jung et al. ---------------------------------------------
    f_data$CRPZERO <- subset(f_data, Type=="CRP")[1,"DV"]
    # put any logical equations about covariates in models
    # -------------------------------------------------------------------------
    
    # Table output (sorted history data)
    f_data <- subset(f_data, select= -c(Amt, Dur, Rep, Type, Inter, Val))
    
    
    # Have to generate duplicate of individual in order to make nlmixr work (have to correct later)**
    f_data2 <- f_data
    f_data2$ID <- 0
    f_data <- rbind(f_data, f_data2)
    output$data_arr4 <- renderTable({ f_data })
    f_data
  })
  
  
  sim_start_time <- reactive({
    sim_hist <- subset(hist_data(), condi=='sim' & ID==input$ID)
    sim_hist[is.na(sim_hist)] <- 0
    sim_hist <- head(sim_hist, 1)
    sim_start_time <- sim_hist[1,"TIME"]
    sim_start_time
  })
  
  
  # Estimation tab ==================================================
  
  # model scheme rendering from website 
  output$scheme <- renderUI({
    mod_env() # load model's environment
    tags$img(src = scheme_image)
  })
  
  
  
  # starts fitting when estimation button is pressed 
  fit.s <- eventReactive(input$run_button, {
    fit.s <- subset(
      
      nlmixr(
        object = f,
        data = subset(hist_data(), condi=='est'),
        est="focei",
        control=foceiControl(maxOuterIterations = 0) # post-hoc method, https://github.com/nlmixrdevelopment/nlmixr/issues/32
        
      ), ID==input$ID
      
    )
    ifelse(is.null(fit.s$CMT), # condition
           #fit.s$CMT <- mod_comp[pk_obs], # TRUE (=CMT is null)
           fit.s$CMT <- pk_obs, # TRUE (=CMT is null)
           fit.s$CMT <- mod_obs[as.numeric(fit.s$CMT)] ) # FALSE (=CMT is not null)
    
    fit.s
  })
  
  
  output$data_arr5 <- renderTable({ fit.s() })
  
  
  # estimation table for plot =======================================
  est_table <- reactive({ # Dose simulation on simulation box
    fit.s <- fit.s()
    hist_data <-  hist_data() # completed history table
    hist_data <- subset(hist_data, ID==input$ID)
    
    amt_data <- subset(hist_data, !is.na(hist_data$AMT))
    cov_data <- subset(hist_data, MDV==0)
    
    est_hist <- subset(hist_data(), condi=='est' & ID==input$ID)
    est_hist[is.na(est_hist)] <- 0
    est_hist <- tail(est_hist, 1)
    est_endtime <- est_hist[1,"TIME"] + est_hist[1,"ADDL"] * est_hist[1,"II"] + 24
    
    ev <- eventTable() %>% 
      add.sampling(seq(from=0, to=as.numeric(max( # follows the bigger record between simulation and estimation dosing history
        hist_data$TIME[nrow(hist_data)]+input$sim_obs_period*24,
        est_endtime
      )), by=input$step_size)) # by 0.25 hour = 15 min, tracking for additional 48 hours
    
    # add dosing event (reading the history table written)
    for (i in 1:nrow(amt_data)){
      ev$add.dosing(dose = amt_data[i,"AMT"],
                    rate=amt_data[i,"RATE"],
                    nbr.doses = amt_data[i,"ADDL"],
                    dosing.interval = amt_data[i,"II"],
                    start.time = amt_data[i,"TIME"])  
    }
    
    # merging cov data
    names(cov_data)[names(cov_data)=="TIME"] <- "time"
    cov_data$evid <- cov_data$MDV # should be corrected if there's any issues with EVID varying model
    cov_data$amt <- 0
    
    cov_data <- subset(cov_data, select = c("time",
                                            "evid",
                                            mod_cov, # gathering model covariates
                                            "CRPZERO")) # {only for the model Jung et al.}
    
    ev <- merge(ev, cov_data, all=TRUE) # merge method: 'outer join'
    ev <- ev %>% tidyr::fill_(fill_cols=mod_cov, .direction = "downup") %>% # to fill NAs in the event table
      tidyr::fill_(fill_cols = "CRPZERO", .direction = "downup")
    ev # ev table will be transferred into 'eta' version or 'no-eta' version
    
    output$data_arr6 <- renderTable({ ev })
    
    # ETA definition / Simulation ---------------------------------------------
    
    # Visual parametric diagnosis table (df_iiv), list[[3]]
    eta_table <- fit.s$eta %>% slice(n()) %>% select(-ID) # fit result > last time eta estimates
    df_iiv <- NULL
    
    for (i in 1:ncol(eta_table)){
      
      # options for normal distribution
      iiv_range = seq(from=-(6*sd_eta[i]), to=(6*sd_eta[i]), length=3) # numeric range (6 sigma = 99.999% range)
      iiv_density = dnorm(iiv_range, mean=0, sd=sd_eta[i]) # probability density
      
      df_params <- data.frame(
        iiv_range,
        iiv_density,
        eta_group = paste0('eta',i), # grouping by eta
        est_dif = eta_table[1,i],
        param = unname(est_eta)[i],
        unit = names(est_eta)[i]
      )
      
      df_iiv <- rbind(df_iiv, df_params)
    }
    df_iiv <- df_iiv %>%
      group_by(param) %>% 
      mutate(max_range = max(iiv_range)) %>% 
      mutate(Percentage = est_dif/max_range*100) %>% 
      mutate(iiv_range_percent = scales::rescale(iiv_range, to=c(-100,100)))
    
    
    fit_est <- data.frame("Estimated" = t(tail(fit.s[,est_eta], 1)))
    fit_est <- tibble::rownames_to_column(fit_est, "param")
    
    
    df_iiv <- merge(df_iiv, fit_est)
    
    # 'iiv' version for VPC(visual predictive check), 'no-iiv' version for simple simulation
    # estimated eta
    ev_noiiv <- cbind(ev, eta_table) # 'fixed' eta from final output
    # randomized eta  
    ev_iiv <- ev # final output. no corrections made from original event table
    
    #output$data_arr4 <- renderTable({
    #  ev_noiiv
    #})
    
    hist_dose <- hist_data %>% filter(is.na(DV))
    # simulation without IIV: list[[1]]
    sim_res_noiiv <- rxSolve(object = fit.s,
                             events=ev_noiiv,
                             nSub = 1)
    
    sim_res_noiiv$date <- hist_dose[1,"Date"] + sim_res_noiiv$time%/%24
    sim_res_noiiv[sim_res_noiiv$time %% 24 != 0,"date"] <- NA
    sim_res_noiiv[2,"date"] <- NA #  /////////////////// Caution about this code, may cause some trouble ///////////////////
    sim_res_noiiv$date <- format(sim_res_noiiv$date, "%m/%d")
    
    
    sim_start_time <- sim_start_time()
    
    sim_res_noiiv$condi <- ifelse(sim_res_noiiv$time<as.numeric(sim_start_time),
                                  'est',
                                  'sim')
    sim_res_noiiv$condi[is.na(sim_res_noiiv$condi)] <- 'est'
    
    # simulation with IIV: list[[2]]
    sim_res_iiv <- rxSolve(object = fit.s,
                           events=ev_iiv,
                           nSub = input$vpc_opt, # how many simulations will be generated
                           seed = input$vpc_seed)
    
    sim_res_iiv$condi <- ifelse(sim_res_iiv$time<as.numeric(sim_start_time),
                                'est',
                                'sim')
    sim_res_iiv$condi[is.na(sim_res_iiv$condi)] <- 'est'
    
    list(sim_res_noiiv, sim_res_iiv, df_iiv) # [[1]]: no iiv simtab, [[2]] iiv simtab, [[3]] eta vis
    
  })
  
  
  
  
  output$param_vis <- renderPlotly({
    df_iiv <- est_table()[[3]]
    
    # plot
    ggp <- ggplot(df_iiv,aes(x=param, y=iiv_range_percent, width=iiv_density, fill=param)) +
      geom_violin(aes(color=param),trim=FALSE, alpha=0.5) +
      geom_point(aes(y=Percentage, color=param), alpha=0.15, shape=16, size=3) +
      xlab("Parameter") +
      ylab("Variability (%)") +
      theme(legend.position='none',
            plot.background = element_rect(fill='transparent',colour=NA),
            panel.background = element_rect(fill='transparent',colour=NA),
            panel.grid.major = element_line(colour='grey70', size=0.05),
            panel.grid.minor = element_line(colour='grey70', size=0.05),
            axis.title.x = element_text(colour='grey70'),
            axis.title.y = element_text(colour='grey70'),
            axis.text = element_text(colour='grey70'),
            axis.ticks = element_line(colour='transparent', size=0.05)
      )
    
    ggplotly(ggp, tooltip = c("Percentage"))
    
  })
  
  
  
  output$param_table <- renderFormattable({
    df_iiv <- est_table()[[3]]
    
    df_iiv_sub <- unique(subset(df_iiv, select= -c(iiv_range, iiv_density, max_range, iiv_range_percent)))
    df_iiv_sub <- df_iiv_sub[, c('param','Estimated','unit','Percentage')]
    
    colnames(df_iiv_sub) = c('Params','Estimated','Units','%') # rename header
    rownames(df_iiv_sub) <- NULL
    
    df_iiv_sub$Estimated <- round(df_iiv_sub$Estimated,2) # rounding down to 2 digits
    df_iiv_sub$'%' <- round(df_iiv_sub$'%',2) # rounding down to 2 digits
    
    formattable(
      df_iiv_sub,
      align = c("c"),
      list(
        `%` = formatter(
          "span",
          style = x ~ formattable::style(
            color = ifelse(x > 0, 'MediumSeaGreen', ifelse(x < 0, 'Salmon', "gray")))
        )
      )
    )
    
  })
  output$des_params <- renderPrint({
    mod_env()
    cat(
      '<span style="color:grey">',
      "<i>",
      des_params,
      "</i>",
      "</span>"
    )
    
  })
  
  
  
  
  
  output$pk_est_plot <- renderPlotly({
    mod_env() # load selected model's environment
    if(is.na(pk)){ # check if there's pk designated in the model document
      ggp <- ggplot() +
        geom_text(color = 'gray65',
                  aes(x=0, y=0.1,  label="No pharmacokinetic analysis supported"), size=3.5) +
        geom_text(color = 'gray65',
                  aes(x=0, y=-0.1,  label="Explore another model to configure"), size=3.5) +
        ylim(-1,1) +
        
        theme(legend.position='none',
              plot.background = element_rect(fill='transparent',colour=NA),
              panel.background = element_rect(fill='transparent',colour=NA),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              panel.grid.major = element_line(colour='grey70', size=0.05),
              panel.grid.minor = element_line(colour='grey70', size=0.05),
              axis.text = element_text(colour='grey70'),
              axis.ticks = element_line(colour='transparent', size=0.05)
        )
    }else{
      
      # pk observation period
      est_hist <- subset(hist_data(), condi=='est' & ID==input$ID)
      est_hist[is.na(est_hist)] <- 0
      est_hist <- tail(est_hist, 1)
      est_endtime <- est_hist[1,"TIME"] + est_hist[1,"ADDL"] * est_hist[1,"II"] + 48
      
      fit.s <- fit.s()
      names(fit.s)[names(fit.s)=="TIME"] <- "Time" # renaming time
      
      sim_res_noiiv <- est_table()[[1]] %>% filter(time<=est_endtime)
      sim_res_iiv <- est_table()[[2]] %>% filter(time<=est_endtime)
      
      names(sim_res_noiiv)[names(sim_res_noiiv)=="time"] <- "Time" # renaming time
      names(sim_res_noiiv)[names(sim_res_noiiv)==pk] <- "Estimated" # renaming the model-predicted value
      
      names(sim_res_iiv)[names(sim_res_iiv)=="time"] <- "Time" # renaming time
      names(sim_res_iiv)[names(sim_res_iiv)==pk] <- "Estimated" # renaming the model-predicted value
      
      sim_res_iiv <- sim_res_iiv %>% 
        group_by(Time) %>% 
        summarize(
          P5 = quantile(Estimated, probs = 0.05),
          P25 = quantile(Estimated, probs = 0.25),
          P50 = quantile(Estimated, probs = 0.5),
          P75 = quantile(Estimated, probs = 0.75),
          P95 = quantile(Estimated, probs = 0.95),
          condi = condi
        )
      #sim_res_iiv <- subset(sim_res_iiv, sim.id==1)
      
      
      
      
      # plot
      ggp <- ggplot(sim_res_noiiv) +
        
        scale_fill_manual(values=c(pk_color, '#999999')) +
        scale_color_manual(values=c(pk_color, '#999999')) +
        
        geom_ribbon(data=sim_res_iiv, aes(x=Time, ymin=P5, ymax=P95, fill=condi), alpha=0.15) +
        geom_ribbon(data=sim_res_iiv, aes(x=Time, ymin=P25, ymax=P75, fill=condi), alpha=0.25) +
        geom_line(data=sim_res_iiv, aes(x=Time, y=P50, color=condi), size=0.6, alpha=0.4, linetype="dashed") +
        
        geom_line(aes(x=Time, y=Estimated, color=condi), size=0.6) +
        
        
        geom_point(data = base::subset(fit.s, CMT==pk_obs),
                   color = pk_color,
                   aes(x=Time, y=DV)) +
        
        geom_errorbar(data = base::subset(fit.s, CMT==pk_obs),
                      color = pk_color,
                      aes(x=Time, y=DV, ymax=DV+IRES, ymin=DV-IRES),width=1) +
        
        
        geom_text(color = 'gray65',
                  aes(x=Time, y=Estimated, label=date), size=3.5) +
        
        
        
        xlab(pk_x_label) +
        ylab(pk_y_label) +
        scale_x_continuous(breaks=seq(0,720,12), # from 0 ~ 720 by 12
                           minor_breaks=seq(6,720,12)) + # from 6 ~ 720 by 12
        scale_y_continuous(breaks=seq(0,150,5)) +
        
        # ggplot theme setting
        theme(legend.position='none',
              plot.background = element_rect(fill='transparent',colour=NA),
              panel.background = element_rect(fill='transparent',colour=NA),
              panel.grid.major = element_line(colour='grey70', size=0.05),
              panel.grid.minor = element_line(colour='grey70', size=0.05),
              axis.title.x = element_text(colour='grey70'),
              axis.title.y = element_text(colour='grey70'),
              axis.text = element_text(colour='grey70'),
              axis.ticks = element_line(colour='transparent', size=0.05)
        )
    }
    ggplotly(ggp)
  })
  
  output$pkpd_des <- renderPrint({
    cat(
      '<span style="color:grey">',
      "<i>",
      "- Solid line: Individual prediction (bright), Population prediction (dark)",
      "<br>",
      "- Light shade: 5-95th pencentile prediction of the model from generated virtual individuals",
      "<br>",
      "- Dark shade: 25-75th pencentile prediction of the model from generated virtual individuals",
      "</i>",
      '</span>' 
    )
  })
  
  
  
  
  
  
  output$pd_est_plot <- renderPlotly({
    mod_env() # load selected model's environment
    if(is.na(pd)){ # check if there's pd designated in the model document
      ggp <- ggplot() +
        geom_text(color = 'gray65',
                  aes(x=0, y=0.1,  label="No pharmacodynamic analysis supported"), size=3.5) +
        geom_text(color = 'gray65',
                  aes(x=0, y=-0.1,  label="Explore another model to configure"), size=3.5) +
        ylim(-1,1) +
        
        theme(legend.position='none',
              plot.background = element_rect(fill='transparent',colour=NA),
              panel.background = element_rect(fill='transparent',colour=NA),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              panel.grid.major = element_line(colour='grey70', size=0.05),
              panel.grid.minor = element_line(colour='grey70', size=0.05),
              axis.text = element_text(colour='grey70'),
              axis.ticks = element_line(colour='transparent', size=0.05)
        )
    }else{
      
      # pd observation period
      est_hist <- subset(hist_data(), condi=='est' & ID==input$ID)
      est_hist[is.na(est_hist)] <- 0
      est_hist <- tail(est_hist, 1)
      est_endtime <- est_hist[1,"TIME"] + est_hist[1,"ADDL"] * est_hist[1,"II"] + 48
      
      fit.s <- fit.s()
      names(fit.s)[names(fit.s)=="TIME"] <- "Time" # renaming time
      
      sim_res_noiiv <- est_table()[[1]] %>% filter(time<=est_endtime)
      sim_res_iiv <- est_table()[[2]] %>% filter(time<=est_endtime)
      
      
      names(sim_res_noiiv)[names(sim_res_noiiv)=="time"] <- "Time" # renaming time
      names(sim_res_noiiv)[names(sim_res_noiiv)==pd] <- "Estimated" # renaming the model-predicted value
      
      names(sim_res_iiv)[names(sim_res_iiv)=="time"] <- "Time" # renaming time
      names(sim_res_iiv)[names(sim_res_iiv)==pd] <- "Estimated" # renaming the model-predicted value
      
      sim_res_iiv <- sim_res_iiv %>% 
        group_by(Time) %>% 
        summarize(
          P5 = quantile(Estimated, probs = 0.05),
          P25 = quantile(Estimated, probs = 0.25),
          P50 = quantile(Estimated, probs = 0.5),
          P75 = quantile(Estimated, probs = 0.75),
          P95 = quantile(Estimated, probs = 0.95),
          condi = condi
        )
      #sim_res_iiv <- subset(sim_res_iiv, sim.id==1)
      
      
      #fit.s$CMT <- as.numeric(fit.s$CMT) # Compartment numbering in fitted table
      
      # plot
      ggp <- ggplot(sim_res_noiiv) +
        
        geom_ribbon(data=sim_res_iiv, aes(x=Time, ymin=P5, ymax=P95), alpha=0.15, fill=pd_color) +
        geom_ribbon(data=sim_res_iiv, aes(x=Time, ymin=P25, ymax=P75), alpha=0.25, fill=pd_color) +
        geom_line(data=sim_res_iiv, aes(x=Time, y=P50), size=0.6, alpha=0.4, color=pd_color, linetype="dashed") +
        
        
        geom_line(color=pd_color, aes(x=Time, y=Estimated), size=0.6) +
        
        
        geom_point(data = subset(fit.s, CMT==pd_obs),
                   color = pd_color,
                   aes(x=Time, y=DV)) +
        
        geom_errorbar(data = subset(fit.s, CMT==pd_obs),
                      color = pd_color,
                      aes(x=Time, y=DV, ymax=DV+IRES, ymin=DV-IRES),width=1) +
        
        geom_text(color = 'gray65',
                  aes(x=Time, y=Estimated, label=date), size=3.5) +
        
        
        
        xlab(pd_x_label) +
        ylab(pd_y_label) +
        scale_x_continuous(breaks=seq(0,720,12), # from 0 ~ 720 by 12
                           minor_breaks=seq(6,720,12)) + # from 6 ~ 720 by 12
        scale_y_continuous(breaks=seq(0,150,5)) +
        
        # ggplot theme setting
        theme(legend.position='none',
              plot.background = element_rect(fill='transparent',colour=NA),
              panel.background = element_rect(fill='transparent',colour=NA),
              panel.grid.major = element_line(colour='grey70', size=0.05),
              panel.grid.minor = element_line(colour='grey70', size=0.05),
              axis.title.x = element_text(colour='grey70'),
              axis.title.y = element_text(colour='grey70'),
              axis.text = element_text(colour='grey70'),
              axis.ticks = element_line(colour='transparent', size=0.05)
        )
      
    }
    ggplotly(ggp)
  })
  
  
  
  
  output$gof <- renderPlotly({
    fit.s <- fit.s()
    fit.s$CMT <- factor(fit.s$CMT, levels=mod_obs)
    #fit.s$CMT <- mod_obs[as.numeric(fit.s$CMT)] 
    #fit.s$CMT <- factor(fit.s$CMT, levels=mod_obs)
    
    fit.s <- subset(fit.s, select = c("CMT", "TIME", "DV", "PRED", "RES", "WRES", "IPRED", "IRES", "IWRES", mod_cov))
    
    output$fit_table <- renderTable({ fit.s })
    
    ggp <- ggplot(fit.s) +
      #geom_smooth(aes(x = IPRED, y = DV), method='glm', color='slategray', size=0.8, alpha=0.1, se=FALSE) +
      coord_equal(ratio = 1) +
      geom_point(aes(x = IPRED, y = DV, alpha=TIME, color=CMT), size=3) + 
      # ggplot theme setting
      geom_abline(slope = 1, intercept= 0, alpha= 0.3, linetype='dotted', color='grey70' ) +
      xlim(0, ceiling(max(fit.s$DV, fit.s$IPRED))) +
      ylim(0, ceiling(max(fit.s$DV, fit.s$IPRED))) +
      
      
      facet_grid(.~CMT) +
      scale_alpha(range = c(0.4, 1)) +
      scale_color_manual(values=c(pk_color, pd_color)) +
      
      theme(legend.position='none',
            plot.background = element_rect(fill='transparent',colour=NA),
            panel.background = element_rect(fill='transparent',colour=NA),
            panel.grid.major = element_line(colour='grey70', size=0.05),
            panel.grid.minor = element_line(colour='grey70', size=0.05),
            axis.title.x = element_text(colour='grey70'),
            axis.title.y = element_text(colour='grey70'),
            axis.text = element_text(colour='grey70'),
            axis.ticks = element_line(colour='transparent', size=0.05),
            strip.background =element_rect(fill="slategray"),
            strip.text = element_text(colour = 'white')
      )
    
    ggplotly(ggp, tooltip = c("IPRED, DV, TIME")) %>% subplot()
  })
  
  output$gof_des <- renderPrint({
    cat(
      '<span style="color:grey">',
      "<i>",
      '- IPRED: individual prediction', '<br>',
      '- DV: observation', '<br>',
      "* Observation's time point is represented by each point's opacity",'<br>',
      "</i>",
      '</span>' 
    )
  })
  
  
  # Simulation plots ================================================
  
  output$pk_sim_plot <- renderPlotly({
    mod_env() # load selected model's environment
    if(is.na(pk)){ # check if there's pk designated in the model document
      ggp <- ggplot() +
        geom_text(color = 'gray65',
                  aes(x=0, y=0.1,  label="No pharmacokinetic analysis supported"), size=3.5) +
        geom_text(color = 'gray65',
                  aes(x=0, y=-0.1,  label="Explore another model to configure"), size=3.5) +
        ylim(-1,1) +
        
        theme(legend.position='none',
              plot.background = element_rect(fill='transparent',colour=NA),
              panel.background = element_rect(fill='transparent',colour=NA),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              panel.grid.major = element_line(colour='grey70', size=0.05),
              panel.grid.minor = element_line(colour='grey70', size=0.05),
              axis.text = element_text(colour='grey70'),
              axis.ticks = element_line(colour='transparent', size=0.05)
        )
    }else{
      
      # pk observation period
      
      sim_start_time <- sim_start_time()
      
      sim_res_noiiv <- est_table()[[1]]# %>% filter(time>=sim_start_time-12) %>% mutate(time = time - sim_start_time)
      sim_res_iiv <- est_table()[[2]]# %>% filter(time>=sim_start_time-12) %>% mutate(time = time - sim_start_time)
      
      names(sim_res_noiiv)[names(sim_res_noiiv)=="time"] <- "Time" # renaming time
      names(sim_res_noiiv)[names(sim_res_noiiv)==pk] <- "Estimated" # renaming the model-predicted value
      
      names(sim_res_iiv)[names(sim_res_iiv)=="time"] <- "Time" # renaming time
      names(sim_res_iiv)[names(sim_res_iiv)==pk] <- "Estimated" # renaming the model-predicted value
      
      sim_res_iiv <- sim_res_iiv %>% 
        group_by(Time) %>% 
        summarize(
          P5 = quantile(Estimated, probs = 0.05),
          P25 = quantile(Estimated, probs = 0.25),
          P50 = quantile(Estimated, probs = 0.5),
          P75 = quantile(Estimated, probs = 0.75),
          P95 = quantile(Estimated, probs = 0.95),
          condi = condi
        )
      
      
      
      # plot
      ggp <- ggplot(sim_res_noiiv) +
        
        scale_fill_manual(values=c(pk_color, '#66CCCC')) +
        scale_color_manual(values=c(pk_color, '#66CCCC')) +
        
        geom_ribbon(data=sim_res_iiv, aes(x=Time, ymin=P5, ymax=P95, fill=condi), alpha=0.15) +
        geom_ribbon(data=sim_res_iiv, aes(x=Time, ymin=P25, ymax=P75, fill=condi), alpha=0.25) +
        geom_line(data=sim_res_iiv, aes(x=Time, y=P50, color=condi), size=0.6, alpha=0.4) +
        
        geom_line(aes(x=Time, y=Estimated, color=condi), size=0.6) +
        
        geom_text(color = 'gray65',
                  aes(x=Time, y=Estimated, label=date), size=3.5) +
        
        
        xlab(pk_x_label) +
        ylab(pk_y_label) +
        scale_x_continuous(breaks=seq(0,720,12), # from 0 ~ 720 by 12
                           minor_breaks=seq(6,720,12)) + # from 6 ~ 720 by 12
        scale_y_continuous(breaks=seq(0,150,5)) +
        
        # ggplot theme setting
        theme(legend.position='none',
              plot.background = element_rect(fill='transparent',colour=NA),
              panel.background = element_rect(fill='transparent',colour=NA),
              panel.grid.major = element_line(colour='grey70', size=0.05),
              panel.grid.minor = element_line(colour='grey70', size=0.05),
              axis.title.x = element_text(colour='grey70'),
              axis.title.y = element_text(colour='grey70'),
              axis.text = element_text(colour='grey70'),
              axis.ticks = element_line(colour='transparent', size=0.05)
        )
    }
    ggplotly(ggp)
  })
  
  
  
  
  
  output$pd_sim_plot <- renderPlotly({
    mod_env() # load selected model's environment
    if(is.na(pd)){ # check if there's pd designated in the model document
      ggp <- ggplot() +
        geom_text(color = 'gray65',
                  aes(x=0, y=0.1,  label="No pharmacodynamic analysis supported"), size=3.5) +
        geom_text(color = 'gray65',
                  aes(x=0, y=-0.1,  label="Explore another model to configure"), size=3.5) +
        ylim(-1,1) +
        
        theme(legend.position='none',
              plot.background = element_rect(fill='transparent',colour=NA),
              panel.background = element_rect(fill='transparent',colour=NA),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              panel.grid.major = element_line(colour='grey70', size=0.05),
              panel.grid.minor = element_line(colour='grey70', size=0.05),
              axis.text = element_text(colour='grey70'),
              axis.ticks = element_line(colour='transparent', size=0.05)
        )
    }else{
      
      
      # pd observation period
      
      sim_start_time <- sim_start_time()
      
      sim_res_noiiv <- est_table()[[1]]# %>% filter(time>=sim_start_time-12) %>% mutate(time = time - sim_start_time)
      sim_res_iiv <- est_table()[[2]]# %>% filter(time>=sim_start_time-12) %>% mutate(time = time - sim_start_time)
      
      
      
      names(sim_res_noiiv)[names(sim_res_noiiv)=="time"] <- "Time" # renaming time
      names(sim_res_noiiv)[names(sim_res_noiiv)==pd] <- "Estimated" # renaming the model-predicted value
      
      names(sim_res_iiv)[names(sim_res_iiv)=="time"] <- "Time" # renaming time
      names(sim_res_iiv)[names(sim_res_iiv)==pd] <- "Estimated" # renaming the model-predicted value
      
      sim_res_iiv <- sim_res_iiv %>% 
        group_by(Time) %>% 
        mutate(
          P5 = quantile(Estimated, probs = 0.05),
          P25 = quantile(Estimated, probs = 0.25),
          P50 = quantile(Estimated, probs = 0.5),
          P75 = quantile(Estimated, probs = 0.75),
          P95 = quantile(Estimated, probs = 0.95)
        )
      sim_res_iiv <- subset(sim_res_iiv, sim.id==1)
      
      
      # plot
      ggp <- ggplot(sim_res_noiiv) +
        
        geom_ribbon(data=sim_res_iiv, aes(x=Time, ymin=P5, ymax=P95), alpha=0.15, fill=pd_color) +
        geom_ribbon(data=sim_res_iiv, aes(x=Time, ymin=P25, ymax=P75), alpha=0.25, fill=pd_color) +
        geom_line(data=sim_res_iiv, aes(x=Time, y=P50), size=0.6, alpha=0.4, color=pd_color) +
        
        
        geom_line(color=pd_color, aes(x=Time, y=Estimated), size=0.6) +
        
        geom_text(color = 'gray65',
                  aes(x=Time, y=Estimated, label=date), size=3.5) +
        
        
        
        xlab(pd_x_label) +
        ylab(pd_y_label) +
        scale_x_continuous(breaks=seq(0,720,12), # from 0 ~ 720 by 12
                           minor_breaks=seq(6,720,12)) + # from 6 ~ 720 by 12
        scale_y_continuous(breaks=seq(0,150,5)) +
        
        # ggplot theme setting
        theme(legend.position='none',
              plot.background = element_rect(fill='transparent',colour=NA),
              panel.background = element_rect(fill='transparent',colour=NA),
              panel.grid.major = element_line(colour='grey70', size=0.05),
              panel.grid.minor = element_line(colour='grey70', size=0.05),
              axis.title.x = element_text(colour='grey70'),
              axis.title.y = element_text(colour='grey70'),
              axis.text = element_text(colour='grey70'),
              axis.ticks = element_line(colour='transparent', size=0.05)
              
              
        )
    }
    ggplotly(ggp)
  })
  
  
  # Simulation result table
  sim_summary <- reactive({
    
    sim_start_time <- sim_start_time()
    
    
    sim_res_noiiv <- est_table()[[1]]
    
    
    # if it is intermittent dosing (comparison between peaks)
    sim_res_peak <- sim_res_noiiv %>%
      subset(diff(sign(diff(c(Inf, sim_res_noiiv$ipred, Inf)))) == -2, select = c(time,ipred)) %>% 
      mutate(dif=c(NA,diff(ipred))) %>%
      mutate(dif_ratio = abs(dif / ipred)) %>% 
      subset(dif_ratio<=0.01 & dif_ratio>=0) %>% 
      filter(time>sim_start_time)
    
    
    sim_res_trough <- sim_res_noiiv %>%
      subset(diff(sign(diff(c(Inf, sim_res_noiiv$ipred, Inf)))) == 2, select = c(time,ipred)) %>% 
      mutate(dif=c(NA,diff(ipred))) %>%
      mutate(dif_ratio = abs(dif / ipred)) %>% 
      slice(2:n()) %>% 
      slice(1:n()-1) %>% 
      subset(dif_ratio<=0.01 & dif_ratio>=0) %>% 
      filter(time>sim_start_time)
    
    
    # if it is continuous infusion (comparison between prior point)
    sim_res_inf <- sim_res_noiiv %>%
      mutate(dif = c(NA,diff(sim_res_noiiv$ipred))) %>% 
      mutate(dif_ratio = abs(dif / ipred)) %>% 
      subset(dif_ratio<=0.001 & dif_ratio>=0) %>% 
      filter(time>sim_start_time)
    
    
    # concentration table: list[[1]]
    sim_conc_sum <- data.frame(
      Trough = sim_res_trough$ipred[1],
      Average = ifelse(nrow(sim_res_peak)==0 & nrow(sim_res_trough)==0,
                       sim_res_inf$ipred[1], # TRUE (==NA)
                       (sim_res_trough$ipred[1] + sim_res_peak$ipred[1])/2), # FALSE (peak and trough data generated)
      Peak = sim_res_peak$ipred[1]
    )
    
    # time table: list[[2]]
    sim_time_sum <- ifelse(nrow(sim_res_peak)==0 & nrow(sim_res_trough)==0,
                           as.numeric(sim_res_inf$time[1]), # TRUE (==NA)
                           as.numeric(( sim_res_trough$time[1] + sim_res_peak$time[1]) / 2)) # FALSE (peak and trough data generated)
    
    
    
    list(sim_conc_sum, sim_time_sum, sim_res_inf)
  })
  
  
  
  
  output$sim_conc_sum <- renderTable({ sim_summary()[[1]] })
  
  output$sim_time_sum <- renderPrint({
    
    cat(
      '<span style="color:grey">',
      "<i>",
      "- Steady-state time at :",
      "</i>",
      '</span>',
      sim_summary()[[2]] ,
      '<span style="color:grey">',
      "<i>",
      "hours",
      "<br>",
      '- From simulation point :',
      "</i>",
      '</span>',
      sim_summary()[[2]] - sim_start_time() ,
      '<span style="color:grey">',
      "<i>",
      "hours",
      "</i>",
      '</span>'
    )
    
  })
  
  
  output$data_arr <- renderTable({ hist_data() })
  output$data_arr2 <- renderTable({ est_table()[[1]] }) # noiiv simtab
  output$data_arr3 <- renderTable({ sim_summary()[[3]] })
  output$data_arr7 <- renderTable({ est_table()[[2]] }) # iiv simtab
  
  
  
} # Server end

# Run the application 
shinyApp(ui = ui, server = server)
