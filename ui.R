library(shiny)
library(bs4Dash)
library(networkD3)
library(rhandsontable)
library(formattable)
library(plotly)
library(shinyWidgets)
library(thematic)
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
          
          forceNetworkOutput(
            'drug_network',
            width = '100%',
            height = '300px'
          ),
          
          # Selected model
          
          verbatimTextOutput('drug'),
          htmlOutput('des_model')
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
            
            htmlOutput('mod_abbr'),
            htmlOutput("des_notes")
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
        plotlyOutput("pk_est_plot")
      ),
      box(
        width=6,
        title = "Pharmcodynamic Profiles",
        elevation = 3,
        maximizable = TRUE,
        HTML("<span style='color:grey'><i>
          *Time-Effect plot of the patient given by the selected model
               </i></span>"),
        plotlyOutput("pd_est_plot")  
        
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
        plotlyOutput("param_vis")
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
        plotlyOutput("gof"), 
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
        plotlyOutput("pk_sim_plot"),
        plotlyOutput("pd_sim_plot")
        
        
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
    
    
  
  
  
  
  
  
  # material_tab_content(
  #   tab_id = 'tab2',
  #   
  #   material_row(
  #     material_column(
  #       width=12,
  #       material_card(
  #         title = tags$span(style="color:grey", "Arranged Dataset"),
  #         divider = TRUE,
  #         
  #         tableOutput('data_arr')
  #       )
  #     ),
  #     material_column(
  #       width=12,
  #       material_card(
  #         title = tags$span(style="color:grey", "Arranged Dataset"),
  #         divider = TRUE,
  #         
  #         tableOutput('data_arr2')
  #       )
  #     ),
  #     material_column(
  #       width=12,
  #       material_card(
  #         title = tags$span(style="color:grey", "Arranged Dataset"),
  #         divider = TRUE,
  #         
  #         tableOutput('data_arr3')
  #       )
  #     ),
  #     material_column(
  #       width=12,
  #       material_card(
  #         title = tags$span(style="color:grey", "Arranged Dataset"),
  #         divider = TRUE,
  #         
  #         tableOutput('data_arr4')
  #       )
  #     )
  #   )
  # )
  
  
  
)


#menuItem("Title", tabName = "tab1", icon = "address-card"),
#menuItem("Estimation", tabName = "tab2", icon = "dashboard"),
#menuItem("Simulation", tabName = "tab3", icon = 'chart-area'),
#menuItem("Disease-progression", tabName = "tab5", icon = 'bed'),
#menuItem("Settings", tabName = "tab4", icon = 'cog')
