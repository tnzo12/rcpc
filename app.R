# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
# test - gitgit
#    http://shiny.rstudio.com/
# UI side libraries -------------------
library(shiny)
library(bs4Dash)
library(networkD3)
library(rhandsontable)
library(formattable)
library(shinyWidgets)
library(shinycssloaders)
library(htmltools)
library(plotly)

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
library(reactable)
library(formattable)
library(kableExtra)
library(knitr)
library(dplyr)
library(data.table)
library(furrr)

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
source("3.plot.R", local=TRUE)
source("4.dm.R", local=TRUE)
source("5.sim.R", local=TRUE)


# basic reacatable options
options(reactable.theme = reactable::reactableTheme(
  backgroundColor = "transparent",
  cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"),
  inputStyle = list(backgroundColor = "transparent"),
  pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)"),
  borderColor = 'rgba(102,102,102,0.3)',
  borderWidth = '1px'
))


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
  background-color: #59626A;
}
.handsontable.listbox td {
  background: #59626A;
}
.handsontable.listbox td.htDimmed {
  color: white;
}
.handsontable.listbox tr:hover td {
  background: #6699cc; 
}
.handsontable.listbox tr td.current {
  background: #ffb347; 
}
"
# badge theme (reactable)
css_rt <- " 
.tag {
  display: inline-block;
  padding: 0.125rem 0.75rem;
  border-radius: 15px;
  font-weight: 600;
  font-size: 0.75rem;
}

.drug-Vancomycin {
  background: hsl(350, 70%, 90%);
  color: hsl(350, 45%, 30%);
}

.drug-Phenobarbital {
  background: hsl(230, 70%, 90%);
  color: hsl(230, 45%, 30%);
}

.drug-Tacrolimus_Pediatric {
  background: hsl(116, 60%, 90%);
  color: hsl(116, 30%, 30%);
}

.drug-Tacrolimus_Liver {
  background: hsl(120, 33%, 85%);
  color: hsl(120, 33%, 30%);
}

.drug-Tacrolimus_Kidney {
  background: hsl(60, 50%, 80%);
  color: hsl(60, 50%, 30%);
}
"

abs_path_temp <- tools::file_path_as_absolute("./temp")

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
      label = "Simulation Settings",
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
      
      
    ),
    
    HTML("&nbsp;"), # spacing
    
    dropdown(
      label = "Estimation",
      icon = icon('signal'),
      selectInput(
        inputId = "est_method",
        label = HTML("<span style='color:grey'><i>
          Choose Estimation Method
               </i></span>"),
        choices = c("posthoc", "foce", "focei"),
        selected = "posthoc"
      ),
      numericInput(
        inputId = "eval_max",
        label = HTML("<span style='color:grey'><i>
          Maximum Number of Evaluations
               </i></span>"),
        value = 200,
        min = 0,
        max = 4000,
        step = 1
      ),
      numericInput(
        inputId = "inner_iter",
        label = HTML("<span style='color:grey'><i>
          Number of Iterations for n1qn1 Optimization
               </i></span>"),
        value = 10,
        min = 0,
        max = 1000,
        step = 1
      ),
      numericInput(
        inputId = "outer_iter",
        label = HTML("<span style='color:grey'><i>
          Number of Iterations for L-BFGS-B Optimization for Outer Problem
               </i></span>"),
        value = 50,
        min = 0,
        max = 5000,
        step = 1
      ),
      selectInput(
        inputId = "outer_opt",
        label = HTML("<span style='color:grey'><i>
          Optimization Method for Outer Problem
               </i></span>"),
        choices = c("nlminb", "bobyqa", "lbfgsb3c", "L-BFGS-B", "mma", "lbfgsbLG", "slsqp", "Rvmmin"),
        selected = "L-BFGS-B"
      ),
      selectInput(
        inputId = "inner_opt",
        label = HTML("<span style='color:grey'><i>
          Optimization Method for Inner Problem
               </i></span>"),
        choices = c("n1qn1", "BFGS"),
        selected = "n1qn1"
      ),
      HTML("<hr size='1px', style='color:#e0e0e0;border-style:dashed'>"), # horizental line
      HTML("<span style='color:grey'><i>
      [Manipulate estimation specs] <br>
      *Post-hoc: default method, only evaluate, no parameter change <br>
      *foce: estimate theta & eta <br>qt
               </i></span>")
      
      
    ),
    HTML("&nbsp;"), # spacing
    shiny::actionButton(inputId = "save_button", label = "Save"),
    HTML("&nbsp;"), # spacing
    shiny::actionButton(inputId = "load_button", label = "Load")
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
    
    elevation = 2,
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
          title = "INFORMATION", # ======================================================
          collapsed = TRUE,
          background = 'gray',
          gradient = FALSE,
          elevation = 2,
          "Information about the patient should put"
          
        )    
      )
    ),
    fluidRow(
      box(
        width=12,
        title="Data",
        elevation = 2,
        shiny::fileInput(inputId = "upload", label = "select file", multiple = TRUE,
                         accept = ".rds"),
        tags$head(tags$style(HTML(css_rt))),
        reactable::reactableOutput("rt"),
        br(),
        downloadButton("download", "Download the files")
        
      )
    ),
    fluidRow(
      box(
        width=6,
        title = "Patient Info",
        elevation = 2,
        
        HTML("<span style='color:grey'><i>
          *The information below is only used to identify patients
               </i></span>"),
        HTML("<hr size='1px', style='color:#e0e0e0;border-style:dashed'>"), # horizental line
        div(style="display: inline-block;vertical-align:top; width: 100%;",
            uiOutput("IDinput")),
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
        elevation = 2,
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
        elevation = 2,
        
        mods_ui("drugs"),
        
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
          title = "Model Search",
          elevation = 2,
          HTML("<span style='color:grey'><i>
          *Can find the best model with given information. Any time-varying covariates should be put in the most representitative value among the patient's observations
               </i></span>"),
          
          uiOutput(outputId = "scheme")
          
          
        ),
        box(
          width=12,
          title = "Notes",
          elevation = 2,
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
        elevation = 2,
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
        elevation = 2,
        
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
          gradient = FALSE,
          elevation = 2,
          "Estimate the model parameter based on the patient information input"
          
        )    
      )
    ),
    fluidRow(
      box(
        width=12,
        title = "Plot information",
        elevation = 2,
        maximizable = TRUE,
        
        htmlOutput("pkpd_des")
      ),
      box(
        width=12,
        title = "Pharmacokinetic Profiles",
        elevation = 2,
        maximizable = TRUE,
        HTML("<span style='color:grey'><i>
          *Time-Conc. plot of the patient given by the selected model
               </i></span>"),
        withSpinner(plotlyOutput("pk_est_plot"),
                    type = getOption("spinner.type", default = 8),
                    color = getOption("spinner.color", default = 'lightgrey'))
        
      ),
      box(
        width=12,
        title = "Pharmacodynamic Profiles",
        elevation = 2,
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
        elevation = 2,
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
        elevation = 2,
        HTML("<span style='color:grey'><i>
          *Estimation results presented in a table
               </i></span>"),
        br(),br(),
        
        formattableOutput("param_table"),
        
        htmlOutput("des_params")
      ),
      box(
        width=6,
        title = "uploaded file",
        elevation = 2,
        tableOutput("files")
      )
      
      
      
    ),
    fluidRow(
      
      box(
        collapsed = TRUE,
        width=12,
        title = "Additonal Diagnosis",
        elevation = 2,
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
          gradient = FALSE,
          elevation = 2,
          "Simulate dosings with estimation results"
          
        )    
      )
    ),
    fluidRow(
      box(
        width=8,
        title = "Simulation Options",
        elevation = 2,
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
        elevation = 2,
        
        HTML("<span style='color:grey'><i>
          *Simulation based steady state diagnosis
               </i></span>"),
        br(),br(),
        tableOutput('sim_conc_sum'),
        htmlOutput('sim_time_sum')
      ),
      box(
        width=12,
        title = "Scenario-based predictions",
        elevation = 2,
        fluidRow(
          shiny::column(width=6,
                        shiny::checkboxGroupInput("sce_doser", label="dosing ratio",
                                                  choices= c(0.125, .25, .5, .75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5),
                                                  selected=c(.25, .5, .75, 1, 1.25, 1.5),
                                                  inline=TRUE)
          ),
          shiny::column(width=6,
                        shiny::checkboxGroupInput("sce_tau", label="dosing interval",
                                                  choices = c(3, 4, 6, 8, 12, 24, 48, 72),
                                                  selected = c(3, 4, 6, 8, 12, 24),
                                                  inline=TRUE)
          )
        ),
        actionButton(
          label = "Calculate steady state PK params",
          icon = icon('sync-alt'),
          inputId = "sce_button",
          # icon: https://fontawesome.com/icons?from=io
        ),
        fluidRow(
          shiny::column(width=6,
                        sim_sce_ui1("sce_grid1")),
          shiny::column(width=6,
                        sim_sce_ui2("sce_grid2"))
        )
            
            
        
      ),
      box(
        width=12,
        title = "Probability Target Attainment",
        elevation = 2,
        fluidRow(
          shiny::column(width=6,
                        selectInput("conc_type", label = "", choices = c("c_peak","c_trou")),
                        shiny::uiOutput("conc_win"),
                        selectInput("auc_type", label = "", choices = c("auc_24","auc_tau")),
                        shiny::uiOutput("auc_win"),
                        actionButton(
                          label = "Calculate PTA",
                          icon = icon('sync-alt'),
                          inputId = "pta_button",
                          # icon: https://fontawesome.com/icons?from=io
                        ), br(), br(),
                        sim_pta_ui("pta")
          ),
          sim_pta_hist_ui("pta_hist")
        )
        
      ),
      box(
        width=12,
        collapsed = TRUE,
        title = "hist data()",
        elevation = 2,
        tableOutput("data_arr")
      ),
      box(
        width=12,
        collapsed = TRUE,
        title = "simulation without IIV",
        elevation = 2,
        tableOutput("data_arr2")
      ),
      box(
        width=12,
        collapsed = TRUE,
        title = "simulation with IIV (as percentile)",
        elevation = 2,
        tableOutput("data_arr7")
      ),
      box(
        width=12,
        collapsed = TRUE,
        title = 'obshis',
        elevation = 2,
        tableOutput("obshis")
      ),
      box(
        width=12,
        collapsed = TRUE,
        title = 'dosehis',
        elevation = 2,
        tableOutput("dosehis")
      ),
      box(
        width=12,
        collapsed = TRUE,
        title = 'fit.s',
        elevation = 2,
        tableOutput("data_arr5")
      ),
      box(
        width=12,
        collapsed = TRUE,
        title = 'ev',
        elevation = 2,
        tableOutput("data_arr6")
      ),
      box(
        width=12,
        collapsed = TRUE,
        title = 'sim_res_iiv',
        elevation = 2,
        tableOutput("data_arr8")
      )
      
      
      
    )
    
    
    # fluid ends
  )
  
)

  

# Server ========================================
server <- function(input, output, session) {
  
  values <- shiny::reactiveValues() # reactive values storage
 
  observeEvent(input$drug_selection, {
    values$drug_selection <-input$drug_selection # selected drug in picker
  })
  observeEvent(input$model, {
    values$model <- input$model # selected model in network  
  })
  observeEvent(input$load_button, {
    values$load_button <- input$load_button
  })
  observeEvent(input$run_button, {
    values$run_button <- input$run_button
    values$sce_doser <- input$sce_doser
    values$sce_tau <- input$sce_tau
  })
  observeEvent(input$sce_button, {
    values$sce_button <- input$sce_button
  })
  observeEvent(input$pta_button, {
    values$pta_button <- input$pta_button
    values$conc_win <- input$conc_win
    values$conc_type <- input$conc_type
    values$auc_win <- input$auc_win
    values$auc_type <- input$auc_type
    values$vpc_opt <- input$vpc_opt
  })
  
  # data management (save and load)
  # save
  observeEvent(input$save_button, {
    values$doseh_raw <- hot_to_r(input$doseh) # save dosing history to values
    values$sim_doseh_raw <- hot_to_r(input$sim_doseh) # save simulation dosing history to values
    values$obsh_raw <- hot_to_r(input$obsh) # save observation history to values
    saveRDS(serialize(
      object = list(
        drug_selection = values$drug_selection, # select values to save
        model = values$model,
        doseh_raw = values$doseh_raw,
        sim_doseh_raw = values$sim_doseh_raw,
        obsh_raw = values$obsh_raw,
        saved = Sys.time()),
      connection = NULL),
      file = ifelse(is.null(input$upload$datapath),
                            paste0("./temp/",input$ID,".rds"),
                            paste0(dirname(input$upload$datapath[1]),"/",input$ID,".rds"))) # saving directory
    showNotification("history saved", type = "warning")
  })
  # load
  observeEvent(input$load_button, {
    if(file.exists(paste0(dirname(ifelse(is.null(input$upload$datapath),".",input$upload$datapath[1])),"/",input$ID,".rds")) | # system temp
       file.exists(ifelse(is.null(values$rt),"./.rds",values$rt$rds_files[[getReactableState("rt","selected")]])) # shiny app
       ){
      loaded <- unserialize(
        readRDS( values$rt$rds_files[[reactable::getReactableState("rt","selected")]] )
      )
      
      values$drug_selection <- loaded$drug_selection
      values$model <- loaded$model
      
      values$doseh_ini <- loaded$doseh_raw # previous table to initial state
      values$sim_doseh_ini <- loaded$sim_doseh_raw
      values$obsh_ini <- loaded$obsh_raw
      showNotification("save file loaded", type = "message")
    } else {
      showNotification("could not find matching ID", type = "error")
    }
    
    
  })

  output$files <- renderTable(input$upload)
  
  # rename encoded names to original state
  observeEvent(input$upload, {
    file.rename(from = input$upload$datapath,
                to = paste0(dirname(input$upload$datapath),"/",input$upload$name))
  })
  
  # download as zip
  output$download <- downloadHandler(
    filename = 'export.zip',
    content = function(fname) {
      zip_temp <- zip::zip(fname, files = unlist(values$rt$rds_files),
                           mode = "cherry-pick")
      #zip_temp <- zip::zip(fname, files = list.files(path = abs_path_temp, pattern = "\\.rds$"),
      #                     root = abs_path_temp)
      #if(!is.null(input$upload$datapath)){
      #  zip::zip_append(zip_temp, files = list.files(path = dirname(input$upload$datapath[1]), pattern = "\\.rds$"),
      #                  root = dirname(input$upload$datapath[1]))  
      #}
    },
    contentType = "application/zip"
  )
  
  
  # observe event -> generate table
 observeEvent(
   eventExpr = {
   c(input$save_button,input$load_button,input$upload)
   },
   handlerExpr = {
     # remove identical files between two temp folders
     if(!is.null(input$upload$datapath[1])){
       temp_files <- list.files(abs_path_temp, pattern = "\\.rds$")
       datapath_files <- list.files(path = dirname(input$upload$datapath[1]), pattern = "\\.rds$")
       if(any(temp_files %in% datapath_files)){
         file.remove(paste0(abs_path_temp,"/",temp_files[temp_files %in% datapath_files]))  
       }
       
     }
     
     
     # sort out .rds files
     rds_files <- if(is.null(input$upload$datapath[1])){
       sapply(list.files(path = "./temp", # temp folder inside shiny application
                         full.names = TRUE,
                         pattern = "\\.rds$"),
              tools::file_path_as_absolute)
     }else{
       c(list.files(path = dirname(input$upload$datapath[1]), # generated temp folder by fileinput
                    full.names = TRUE,
                    pattern = "\\.rds$"),
         sapply(list.files(path = "./temp", # temp folder inside shiny application
                           full.names = TRUE,
                           pattern = "\\.rds$"),
                tools::file_path_as_absolute))
     }
     
   # rds table
     if(length(rds_files)!=0){
       values$rt <- lapply(rds_files, readRDS) %>%
         sapply(unserialize) %>% # unserialize the file
         rbind(rds_files) %>% # attach filename
         t() %>% data.frame() %>%
         rename(drug = drug_selection) %>% 
         rowwise() %>%
         mutate(id = gsub(".rds","",basename(rds_files)), # remove directory, extension
                no.dose = c(doseh_raw %>% filter(!is.na(Amt)) %>% summarize(do = sum(Rep, na.rm = T) + length(Amt))),
                no.sim_dose =  c(sim_doseh_raw %>% filter(!is.na(Amt)) %>% summarize(do = sum(Rep, na.rm = T) + length(Amt))),
                no.obs = c(obsh_raw %>% filter(!is.na(Val)) %>% summarize(length(Val)))) %>% 
         select(id, drug, model, no.dose, no.sim_dose, no.obs, saved, rds_files)
     }
   

   
   
 })
 output$rt <- reactable::renderReactable(
   if(is.null(values$rt)){
     reactable::reactable(
       data.frame(no_data_loaded=NA) 
     )
   
     }else{
   
     reactable::reactable(
       values$rt,
       resizable = TRUE,
       defaultExpanded = TRUE,
       compact = TRUE,
       defaultSelected = 1,
       selection = "single",
       onClick = "select",
       filterable = TRUE,
       columns = list(
         drug = reactable::colDef(cell = function(value) {
           class <- paste0("tag drug-", value)
           div(class = class, value)
         }),
         saved = reactable::colDef(
           format = reactable::colFormat(datetime = TRUE),
           width = 200
         ),
         rds_files = reactable::colDef(show = FALSE)
       ),
       highlight = TRUE
       
     )
       
   }
   
   
   
 )
 
 output$IDinput <- renderUI(
   numericInput(
     inputId = "ID",
     label = "Patient number",
     value = ifelse(is.null(reactable::getReactableState("rt","selected")),
                    1,
                    values$rt$id[reactable::getReactableState("rt","selected")]),
     min = NA,
     max = NA,
     step = 1
   )
 )  
 
  
  
  # data management module server
  #dm_server("rt", values)
  
  # model module server
  mods_server("drugs",values) # select model -> force network vis
  
  des_server("des_model", mod_env, values)
  des_server("des_notes", mod_env, values)
  des_server("des_abbr", mod_env, values)  # Load selected model environment
  
  sim_server("sce_grid1", mod_env, values)
  sim_server("sce_grid2", mod_env, values)
  sim_server("conc_win", mod_env, values)
  sim_server("auc_win", mod_env, values)
  sim_server("pta", mod_env, values)
  sim_server("pta_hist", mod_env, values)
  
  # model environment
  mod_env <- reactive({
    drug_selection <- values$drug_selection
    model <- values$model
    
    ifelse(
      # if
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
  observeEvent(
    eventExpr = {
      ifelse(is.null(input$model),TRUE,input$model) # regard TRUE when model is not selected
    },
    
    handlerExpr = {
    mod_env() # refresh model environment when model is selected
    values$doseh_ini <- data.frame(Date=Sys.Date(),
                               Hour=0,
                               Min=0,
                               Route=mod_route,
                               Amt=as.numeric(NA),
                               Dur=as.numeric(NA),
                               Rep=as.numeric(NA),
                               Inter=as.numeric(NA),
                               Steady=0,
                               stringsAsFactors = FALSE)
    
    values$obsh_ini <- data.frame(Date=Sys.Date(),
                              Hour=0,
                              Min=0,
                              Type=mod_obs,
                              Val=as.numeric(NA),
                              stringsAsFactors = FALSE) %>% 
      dplyr::bind_cols( # add columns for covariates
        matrix(ncol=length(mod_cov)) %>%
          data.frame() %>%
          mutate_all(as.character) %>% 
          setNames(mod_cov)
      )
    
    values$sim_doseh_ini <- data.frame(Date=Sys.Date()+1, # add one day from history
                                   Hour=0,
                                   Min=0,
                                   Route=mod_route,
                                   Amt=as.numeric(NA),
                                   Dur=as.numeric(NA),
                                   Rep=as.numeric(NA),
                                   Inter=as.numeric(NA),
                                   Steady=0,
                                   stringsAsFactors = FALSE)
  })
  
  
  # Uploading generated (updated by node selection = environment change) table
  observeEvent(
    eventExpr = {
      mod_env()  
    },
    handlerExpr = {
      mod_env()  
      output$doseh <- renderRHandsontable({ # needed administration routes will appear in this table
        dosht <- rhandsontable(values$doseh_ini, rowHeaders = NULL, stretchH = "all") %>% 
          hot_col(col = "Hour", default = 0) %>% 
          hot_validate_numeric(col = "Hour", min = 0, max = 24) %>% 
          hot_col(col = "Min", default = 0) %>% 
          hot_validate_numeric(col = "Min", min = 0, max = 59.999) %>% # between 0 ~ 60
          hot_cols(halign = "htCenter") %>% 
          hot_table(overflow = "visible", stretchH = "all")
          
        
        if(length(mod_route)>1){ # if: route of administration type is more than one
          dosht <- dosht %>% hot_col(col = "Route", type = "dropdown", source = mod_route)
        }
        dosht
      })
      
      output$obsh <- renderRHandsontable({ # needed observation types will appear in this table
        obsht <- rhandsontable(values$obsh_ini, rowHeaders = NULL,stretchH = "all", overflow="visible") %>% 
          hot_col(col = "Hour", default = 0) %>% 
          hot_col(col = "Min", default = 0) %>% 
          hot_cols(halign = "htCenter") %>% 
          hot_table(overflow = "visible", stretchH = "all")
        
        if(length(mod_obs)>1){
          # if: type of observation is more than one
          obsht <- obsht %>% hot_col(col = "Type", type = "dropdown", source = mod_obs)
        }
        if(!is.null(mod_lcov)){
          # if listed covariate is not null
          for (i in mod_lcov){
            obsht <- obsht %>% hot_col(col = i, type = "dropdown", source = names(mod_lcov_value[[i]]))
          }
        }
        obsht
      })
      
      output$sim_doseh <- renderRHandsontable({ # needed administration routes will appear in this table
        simt <- rhandsontable(values$sim_doseh_ini, rowHeaders = NULL, stretchH = "all") %>% 
          hot_col(col = "Hour", default = 0) %>% 
          hot_col(col = "Min", default = 0) %>%
          hot_cols(halign = "htCenter") %>% 
          hot_table(overflow = "visible", stretchH = "all")
        
        if(length(mod_route)>1){ # if: route of administration is more than one
          simt <- simt %>% hot_col(col = "Route", type = "dropdown", source = mod_route)
        }
        simt
      })
      
    }
  )
  

  # Get inputs form buttons: estimation and simulation  
  
  # Download table input from UI -> Processing ======================
  
  
  observeEvent(input$run_button, {
    mod_env() # load model's environment
    
    
    # Data processing method
    
    # Loading dosing history from ui input
    doseh <- dplyr::bind_rows( # combining estimation/simulation dataset
      hot_to_r(input$doseh) %>% mutate(condi='est'),
      hot_to_r(input$sim_doseh) %>% mutate(condi='sim')) %>%
      tidyr::fill(c('Date', 'Route'), .direction = "down") %>%
      mutate_at(vars('Hour', 'Min', 'Amt', 'Dur', 'Rep', 'Inter'), ~replace_na(., 0)) %>% 
      #mutate_at(vars('Rep'), ~replace_na(., 1)) %>%
      # data processing (NONMEM-like format)
      filter(Amt != 0) %>% # get rid of the unused dosing (dosing amount of 0)
      rename(AMT = Amt, ADDL = Rep, II = Inter, SS = Steady) %>% # rename columns
      mutate(MDV = 1,
             EVID = 1,
             CMT = mod_comp[Route],
             RATE = ifelse(Dur!=0, AMT/Dur,0)) # if duration exists, RATE generated
    
    output$dosehis <- renderTable({doseh})
    
    
    # Loading observation history from ui input
    obsh <- hot_to_r(input$obsh) %>% 
      tidyr::fill(c('Date', 'Type'), .direction = "down") %>%  # to fill NAs in the f_data
      mutate_at(vars('Hour', 'Min', 'Val'), ~replace_na(., 0)) %>% 
      arrange(Date, Hour, Min) %>% 
      tidyr::fill(mod_cov, .direction = "downup") %>% 
      mutate(MDV = 0,
             EVID = 0,
             CMT = mod_comp[Type],
             condi = 'est') %>% # labeling: estimation dataset
      mutate_at(vars(mod_cov[!mod_cov %in% mod_lcov]), as.numeric) %>% # only for covariates which is not included in listed covariate
      rename(DV = Val)
      
      if(!is.null(mod_lcov)){
        # if listed covariate is not null, numerize
        for (i in mod_lcov){
          obsh[[i]] <- mod_lcov_value[[i]][ obsh[[i]] ]
        }
      }

    output$obshis <- renderTable({obsh}) # debugging table, observation history
    
    # fitting data = dosing history + observation history
    f_data <- dplyr::bind_rows(obsh, doseh) %>% # dosing, observation data merging
      arrange(Date, Hour, Min) %>% # Time ordering
      mutate(TIME = difftime(
        paste0(Date," ",Hour,":",Min), # until
        paste0(first(Date)," ",first(Hour),":",first(Min)), # from
        units='hours' # unit: hours
      ),
      ID = input$ID) %>% # Patient info
      mutate(DOSE = AMT) %>%
      tidyr::fill(DOSE, .direction = "downup") # dosed amount
    
    
    # Model designated function -----------------------------------------------
    f_data$CRPZERO <- subset(f_data, Type=="CRP")[1,"DV"] # only applied to Jung et al.
    f_data <- f_data %>% subset(select= -c(Dur, Type))
    # put any logical equations about covariates in models
    # -------------------------------------------------------------------------
    fit.s <- nlmixr(
      object = f,
      data = subset(f_data, condi=='est'),
      est = input$est_method, # post-hoc method, https://github.com/nlmixrdevelopment/nlmixr/issues/32
      foceiControl(eval.max = input$eval_max,
                   maxInnerIterations = input$inner_iter,
                   maxOuterIterations = input$outer_iter,
                   outerOpt = input$outer_opt,
                   innerOpt = input$inner_opt)
    )
    fit.s <- fit.s %>%
      mutate(CMT = if(is.null(fit.s$CMT)) {pk_obs} else {mod_obs[as.numeric(CMT)]} ) %>% 
      rename(Time = TIME)
    
    
    
    # Table output (sorted history data)
    values$doseh <- doseh
    values$obsh <- obsh
    values$f_data <- f_data
    # simulation end time
    values$sim_endtime <- f_data %>% filter(condi=='sim') %>%
      replace(is.na(.), 0) %>% 
      last() %>% select(TIME) %>% as.vector() # last simulation record, select time as vector
    values$fit.s <- fit.s
    
    # estimation table for plot =======================================
    
    # ---------------------------------------------------------------------------------------------
    
    # data subsetting: amt + cov_data -> merged ev
    amt_data <- subset(f_data, !is.na(f_data$AMT))
    cov_data <- subset(f_data, MDV==0) %>%
      rename(time = TIME) %>%
      mutate(evid = MDV,
             amt = 0) %>% dplyr::select(time, evid, any_of(mod_cov), CRPZERO)
    
    values$cov_data <- cov_data
    
    est_hist <- subset(f_data, condi=='est') %>% replace(is.na(.), 0) %>% tail(1)
    
    est_endtime <- est_hist[1,"TIME"] + (est_hist[1,"ADDL"] * est_hist[1,"II"]) + 24 # set endtime
    
    ev <- et() %>% 
      et(seq(from=0, to=as.numeric(max( # follows the bigger record between simulation and estimation dosing history
        f_data$TIME[nrow(f_data)]+input$sim_obs_period*24,
        est_endtime
      )), by=input$step_size)) %>% # by 0.25 hour = 15 min, tracking for additional 48 hours
      merge(f_data %>%
              rename_all(tolower) %>%
              filter(!is.na(amt)) %>%
              select(any_of(c("time","amt","cmt","rate","addl","ii","evid","ss"))), all=TRUE) %>% 
      merge(cov_data, all=TRUE) %>% # merge (outer join)
      tidyr::fill(any_of(mod_cov), .direction = "downup") %>% # to fill NAs in the event table
      mutate(DOSE = amt) %>%
      tidyr::fill(any_of(c("DOSE","CRPZERO")), .direction = "downup")
    # ev table will be transferred into 'eta' version or 'no-eta' version
    
    output$data_arr6 <- renderTable({ ev }) # debugging table, event table
    
    # ETA definition / Simulation ---------------------------------------------
    
    
    
    
    # 'iiv' version for VPC(visual predictive check), 'no-iiv' version for simple simulation
    
    # 1) estimated eta
    eta_table <- fit.s$eta %>% slice(n()) %>% select(-ID) # fit result > last time eta estimates
    values$eta_table <- eta_table
    
    ev_noiiv <- dplyr::bind_cols(ev, eta_table) # 'fixed' eta from final output
    # 2) randomized eta   
    ev_iiv <- ev # final output. no corrections made from original event table
    
    # variable
    dosep <- ev %>% data.frame() %>% filter(!is.na(amt)) %>% # dosing points
      rowwise() %>% 
      mutate(addl = ifelse('addl' %in% names(.), addl, 0)) %>% # add addl label if it does not exist
      summarize(
        x = list(seq(from=time, to=(time + addl*ii), by = ii))
      ) %>% 
      unlist() %>% unname()
    
    hist_time <- f_data[1,"Hour"] + f_data[1,"Min"]/60
    
    
    
    sim_endtime <- values$sim_endtime
    
    # simulation without IIV: list[[1]] ------------------------
    sim_res_noiiv <- rxSolve(object = fit.s,
                             events = ev_noiiv,
                             nSub = 1) %>%
      data.table() %>% # date labeling, data.table
      .[,date := dplyr::if_else((hist_time + time) %% 24 != 0, # if
                                NA, # then
                                f_data[1,"Date"] + (hist_time + time) %/% 24)] %>% # else
      .[,date := format(date, "%m/%d")] %>% 
      .[time %in% dosep, dosed := 1] %>% # check where dose was given
      .[is.na(dosed), dosed := 0] %>%
      .[, ind_auc := lapply(.SD, auc, time), .SDcols = pk] %>% # individual AUC by time
      .[, dose_divide := cumsum(dosed == 1)] %>% # cumulative dose grouping
      .[, auc_divide := cumsum(ind_auc), by=dose_divide] %>% # cumulative individual AUC by time
      .[, auc_divide := cumsum(ind_auc), by=dose_divide] %>%
      setnafill(cols = "auc_divide", fill=0) %>% 
      .[, tad := (time - first(time)), by=dose_divide] # time after dose
    #.[, auc_divide := if_else(last(auc_divide)==auc_divide, auc_divide, NA), by=dose_divide] # individual AUC
    
    
    
    
    
    sim_res_noiiv$condi <- ifelse(sim_res_noiiv$time < as.numeric(sim_endtime),'est','sim')
    sim_res_noiiv$condi[is.na(sim_res_noiiv$condi)] <- 'est'
    
    # simulation with IIV: list[[2]] ---------------------------
    sim_res_iiv <- rxSolve(object = fit.s,
                           events=ev_iiv,
                           nSub = input$vpc_opt, # how many simulations will be generated
                           seed = input$vpc_seed) %>%  data.table() %>% rename(Time=time)
    
    sim_res_iiv$condi <- ifelse(sim_res_iiv$Time < as.numeric(sim_endtime),'est','sim')
    sim_res_iiv$condi[is.na(sim_res_iiv$condi)] <- 'est'
    
    
    values$sim_res_iiv <- sim_res_iiv
    
    sim_res_piiv <- data.table::melt(sim_res_iiv,
                                     id.vars = c("Time","condi"),
                                     measure.vars = c(if(is.na(pk)){NULL}else{pk},
                                                      if(is.na(pd)){NULL}else{pd})) %>%
      .[, .(P05 = quantile(value, 0.05),
            P25 = quantile(value, 0.25),
            P50 = quantile(value, 0.50),
            P75 = quantile(value, 0.75),
            P95 = quantile(value, 0.95)), by=.(Time,condi,variable)] # summarize by its quantiles
    
    
    
    sim_res_iiv_param <- sim_res_iiv
    # visual parameter diagnostics (prm_iivs), list[[3]]
    prm_iivs <- dplyr::bind_rows(sim_res_iiv_param, sim_res_noiiv) %>% 
      .[, sim.id, mget(est_eta)] %>%
      .[,lapply(.SD, mean),by=.(sim.id)] %>%
      .[,paste0(est_eta,".z"):=lapply(.SD,scale), .SDcols = est_eta] %>% 
      setnames(old = est_eta, new = paste0(est_eta,".v")) %>% 
      data.table::melt(id.vars = 'sim.id',
                       measure.vars = patterns("v$","z$"), # column names ends with v and z
                       variable.name= "Param",
                       value.name = c("Value","Z.score")) %>% 
      .[,`:=`(Value=round(Value,3), Z.score=round(Z.score,3))] %>% # rounding 
      .[,Param:=as.factor(est_eta)[Param]]
    
    
    # list generated data
    values$sim_res_noiiv <- sim_res_noiiv
    values$sim_res_iiv <- sim_res_iiv # sim_res_iiv: raw
    values$sim_res_piiv <- sim_res_piiv # sim_res_iiv: percentile
    values$prm_iivs <- prm_iivs
    
    
  })
  
  
  # Estimation tab ==================================================
  
  # model scheme rendering from website 
  output$scheme <- renderUI({
    mod_env() # load model's environment
    tags$img(src = scheme_image)
  })
  

  
  
  output$data_arr5 <- renderTable({ values$fit.s }) # debugging table, fit results
  
  output$param_vis <- renderPlotly({
    
    prm_iivs <- values$prm_iivs
    # visual parameter diagnostic plot
    vis_param(prm_iivs)
    
  })
  
  
  
  output$param_table <- renderFormattable({
    prm_iivs <- values$prm_iivs
    
    prm_iivs_tbl <- prm_iivs[,`:=`(Ind = last(Value), Median = median(Value), Diff = last(Value) - median(Value)), by=Param] %>% # last value = no iiv sim.id (of NA)
      unique(by="Param") %>%
      select(-c(sim.id, Value, Z.score)) %>%
      .[,`Change(%)` := round(Diff/Median*100,2)] # changes in percent
    
    # table output
    formattable(
      prm_iivs_tbl,
      align = c("c"),
      list(
        `Change(%)` = formatter(
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
      no_plot("No pharmacokinetic analysis supported","Explore another model to configure")
    }else{
      
      # pk observation period
      est_hist <- subset(values$f_data, condi=='est' & ID==input$ID)
      est_hist[is.na(est_hist)] <- 0
      est_hist <- tail(est_hist, 1)
      est_endtime <- est_hist[1,"TIME"] + est_hist[1,"ADDL"] * est_hist[1,"II"] + 48
      
      fit.s <- values$fit.s
      sim_res_noiiv <- values$sim_res_noiiv %>%
        rename(Time = time, Estimated = pk)
      sim_res_piiv <- values$sim_res_piiv %>%
        filter(variable == pk)
      
      # plot
      subplot(
        pkd_plot(sim_res_piiv, sim_res_noiiv, fit.s, pk_color, pk_obs, pk_x_label, pk_y_label),
        auc_plot(sim_res_noiiv, pk_x_label, pk_y_label),
        nrows = 2, heights = c(0.85, 0.15), shareX = TRUE
      )
      
      
    }
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
      no_plot("No pharmacodynamic analysis supported","Explore another model to configure")
    }else{
      
      # pd observation period
      est_hist <- subset(values$f_data, condi=='est' & ID==input$ID)
      est_hist[is.na(est_hist)] <- 0
      est_hist <- tail(est_hist, 1)
      est_endtime <- est_hist[1,"TIME"] + est_hist[1,"ADDL"] * est_hist[1,"II"] + 48
      
      fit.s <- values$fit.s
      sim_res_noiiv <- values$sim_res_noiiv %>%
        rename(Time = time, Estimated=pd)
      sim_res_piiv <- values$sim_res_piiv %>%
        filter(variable == pd)
      
      # plot
      pkd_plot(sim_res_piiv, sim_res_noiiv, fit.s, pd_color, pd_obs, pd_x_label, pd_y_label)
      
      
    }
    
  })
  
  
  
  
  output$gof <- renderPlotly({
    fit.s <- values$fit.s
    fit.s$CMT <- factor(fit.s$CMT, levels=mod_obs)
    
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
  
  
  # Simulation result table
  sim_summary <- reactive({
    
    sim_endtime <- values$sim_endtime
    
    
    sim_res_noiiv <- values$sim_res_noiiv
    
    
    # if it is intermittent dosing (comparison between peaks)
    sim_res_peak <- sim_res_noiiv %>%
      subset(diff(sign(diff(c(Inf, sim_res_noiiv$ipredSim, Inf)))) == -2, select = c(time,ipredSim)) %>% 
      mutate(dif=c(NA,diff(ipredSim))) %>%
      mutate(dif_ratio = abs(dif / ipredSim)) %>% 
      subset(dif_ratio<=0.01 & dif_ratio>=0) %>% 
      filter(time>sim_endtime)
    
    
    sim_res_trough <- sim_res_noiiv %>%
      subset(diff(sign(diff(c(Inf, sim_res_noiiv$ipredSim, Inf)))) == 2, select = c(time,ipredSim)) %>% 
      mutate(dif=c(NA,diff(ipredSim))) %>%
      mutate(dif_ratio = abs(dif / ipredSim)) %>% 
      slice(2:n()) %>% 
      slice(1:n()-1) %>% 
      subset(dif_ratio<=0.01 & dif_ratio>=0) %>% 
      filter(time>sim_endtime)
    
    
    # if it is continuous infusion (comparison between prior point)
    sim_res_inf <- sim_res_noiiv %>%
      mutate(dif = c(NA,diff(sim_res_noiiv$ipredSim))) %>% 
      mutate(dif_ratio = abs(dif / ipredSim)) %>% 
      subset(dif_ratio<=0.001 & dif_ratio>=0) %>% 
      filter(time>sim_endtime)
    
    
    # concentration table: list[[1]]
    sim_conc_sum <- data.frame(
      Trough = sim_res_trough$ipredSim[1],
      Average = ifelse(nrow(sim_res_peak)==0 & nrow(sim_res_trough)==0,
                       sim_res_inf$ipredSim[1], # TRUE (==NA)
                       (sim_res_trough$ipredSim[1] + sim_res_peak$ipredSim[1])/2), # FALSE (peak and trough data generated)
      Peak = sim_res_peak$ipredSim[1]
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
      sim_summary()[[2]] - values$sim_endtime ,
      '<span style="color:grey">',
      "<i>",
      "hours",
      "</i>",
      '</span>'
    )
    
  })
  
  
  observeEvent(values$sce_res, {
    output$conc_win <- renderUI({
      cmin <- min(values$sce_iter[[input$conc_type]], na.rm = TRUE) %>% floor()
      cmax <- max(values$sce_iter[[input$conc_type]], na.rm = TRUE) %>% ceiling()
      
      sliderInput("conc_win", label = "concentration window",
                  min = cmin, max = cmax,
                  value = c(cmin, cmax))        
    })
    output$auc_win <- renderUI({
      amin <- min(values$sce_iter[[input$auc_type]], na.rm = TRUE) %>% floor()
      amax <- max(values$sce_iter[[input$auc_type]], na.rm = TRUE) %>% ceiling()
      
      sliderInput("auc_win", label = "AUC window",
                  min = amin, max = amax,
                  value = c(amin, amax))
    })
    
  })
  
  

  

  output$data_arr <- renderTable({ values$f_data })
  output$data_arr2 <- renderTable({ values$sim_res_noiiv }) # noiiv simtab
  output$data_arr3 <- renderTable({ sim_summary()[[3]] })
  output$data_arr7 <- renderTable({ values$sim_res_piiv }) # iiv simtab
  output$data_arr8 <- renderTable({ values$sim_res_iiv })
  
  
} # Server end

# Run the application 
shinyApp(ui = ui, server = server)
