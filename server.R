# Server libraries
library(shiny)
library(devtools)
library(rsconnect)
library(RxODE)
library(nlmixr)
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


# networkD3 settings
my_color <- 'd3.scaleOrdinal()
  .domain(["group_A", "group_B","group_C", "group_D", "group_E", "group_F", "group_G", "group_H"])
  .range(["grey", "teal" , "orange", "red", "brown", "green", "purple", "pink"])'

# rhandsontable settings
renderer <- "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.TextRenderer.apply(this, arguments);
             td.style.background = '#59626A';
             td.style.color = 'white';
             td.style.fontSize = '14px';
             
           }" 

# Generate drug dir. data frame (doesn't need to be included in reactive expr.)
drug_dir <- strsplit(list.files("./drug/", pattern = ".R", recursive = TRUE), # Get the direction
                     split="[/.]") # seperate / and . in path characters

drug_list <- as.data.frame(t(as.data.frame(drug_dir)),row.names = FALSE, stringsAsFactors = FALSE)
names(drug_list) = c("Drug","Class","Author","Extension")
drug_list <- drug_list[,-ncol(drug_list)]



# Server ========================================
server <- function(input, output, session) {
  
  
  # Network Visualization::networkD3
  selected_drug <- reactive({
    selected_drug <- filter(drug_list, Drug==input$drug_selection)
    
    # Generate drug node
    drug_node <- unique(unlist(selected_drug))
    drug_node <- data.frame(node = drug_node, idx = 0:(length(drug_node)-1) )
    drug_node$n_size <- ifelse(drug_node$node %in% c(drug_list$Drug, drug_list$Class), 4, 10)
    drug_node$group <- ifelse(drug_node$node %in% c(drug_list$Drug, drug_list$Class), 1, 2:nrow(drug_node))

    
    
    # Generate drug links in hierarchy
    drug_link = data.frame()
    for (i in 1:(ncol(selected_drug)-1) ){
      drug_link <- rbind(
        drug_link, cbind(selected_drug[,i],selected_drug[,i+1]) 
      )
    }
    names(drug_link) = c("source","target")
    
    drug_link$s_idx <- match(drug_link$source, drug_node$node) - 1
    drug_link$t_idx <- match(drug_link$target, drug_node$node) - 1
    
    return(list(drug_node = drug_node, drug_link = drug_link))
  })
  

  # Loading model environment
  mod_env <- reactive({
    ifelse(
      sum(paste0(
        input$drug_selection,"/",
        list.files(paste0("./drug/",input$drug_selection), # Specified drug name should be put
                   pattern = paste0(input$model,".R"), recursive = TRUE) # Finding specified model name
      ) %in% list.files("./drug/", pattern = ".R", recursive = TRUE) ) == 1 # TRUE when only 1 model is returned
      ,
    
    # then
    source(
      paste0(
        "./drug/",input$drug_selection,"/",
        list.files(paste0("./drug/",input$drug_selection), # Specified drug name should be put
                   pattern = paste0(input$model,".R"), recursive = TRUE) # Finding specified name
      )
    ),
    
    # else
    source("./default.R") # Default state (:no model loaded)
    )
  }) # Loading model environment ends
  
 
  
  # Model description
  # based on loaded model environment
  output$des_model <- renderPrint({
    mod_env()
    cat(
      '<span style="color:grey">',
      "<i>Model of</i>",
      "</span>",
      
      "<i>","<strong>",input$model,"et al.","</strong>","</i>","<br>",
      "<hr size='1px', style='color:#e0e0e0;border-style:solid'>",
      '<span style="color:grey">',
      "<i>- description:</i>","<br>",
      des_intro,
      "<hr size='1px', style='color:#e0e0e0;border-style:dashed'>",
      
      "<i>- compartments:</i>","<br>",
      des_comp,
      
      "<hr size='1px', style='color:#e0e0e0;border-style:dashed'>",
      
      "<i>- covariates:</i>","<br>",
      des_cov,
      "</span>"
    )
  })
  
  output$des_notes <- renderPrint({
    mod_env()
    cat(
      '<span style="color:grey">',
      "<i>",
      des_notes,
      "</i>",
      "</span>"
    )
  })
  
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
  
  output$mod_abbr <- renderText({
    mod_env()
    # this is where the model covariates' abbreviation is put

    #obs_abbr <-unlist(strsplit(mod_obs_abbr, split=", "))
    #cov_abbr <-unlist(strsplit(mod_cov_abbr, split=", "))
    ifelse(length(mod_obs) > length(mod_cov),
           mod_cov <- c(mod_cov, rep(" ", length(mod_obs)-length(mod_cov))),
           mod_obs <- c(mod_obs, rep(" ", length(mod_cov)-length(mod_obs))))
    ifelse(length(mod_obs_abbr) > length(mod_cov_abbr),
           mod_cov_abbr <- c(mod_cov_abbr, rep(" ", length(mod_obs_abbr)-length(mod_cov_abbr))),
           mod_obs_abbr <- c(mod_obs_abbr, rep(" ", length(mod_cov_abbr)-length(mod_obs_abbr))))
    
    
    abbr_table <- data.frame(mod_obs, mod_obs_abbr, mod_cov, mod_cov_abbr)
    names(abbr_table) <- NULL
    
    abbr_table %>% 
      kable(escape = F) %>%
      kable_styling("hover", full_width = T, font_size = 13) %>%
      
      add_header_above(c("Observations" = 2, "Covariates" = 2), bold=F)
    
  })

  # Visualization of drug-network
  output$drug_network <- renderForceNetwork({
    
    forceNetwork(
      fontSize = 16,
      Nodes = selected_drug()$drug_node, # object: drug_node
      Links = selected_drug()$drug_link, # object: drug_link
      Nodesize = 'n_size',
      radiusCalculation = JS("d.nodesize"),
      linkWidth = 3,
      linkDistance = 60,
      linkColour = 'lightgrey',
      colourScale = my_color,
      charge = -100,
      Source = 's_idx',
      Target = 't_idx',
      NodeID = 'node',
      Group = 'group',
      opacityNoHover = TRUE,
      opacity = 0.7,
      bounded = TRUE,
      clickAction = 'Shiny.onInputChange("model",d.name)'
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
    doseh$CMT <- mod_comp[doseh$Route] # model dependent
    doseh$AMT <- doseh$Amt
    doseh$RATE <- ifelse(doseh$Dur!=0, doseh$Amt/doseh$Dur, 0)
    doseh$ADDL <- doseh$Rep
    doseh$II <- doseh$Inter
    
    # Loading observation history from ui input
    obsh <- hot_to_r(input$obsh)
    
    obsh$MDV <- 0
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
      nlmixr(object = f,
             data = subset(hist_data(), condi=='est'),
             est="focei",
             control=foceiControl(covMethod="r,s",
                                  interaction=TRUE,
                                  #maxInnerIterations = 0,
                                  maxOuterIterations = 0,
                                  iter.max=0)
      ), ID==input$ID)
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
    ev <- ev %>% tidyr::fill_(fill_cols=mod_cov, .direction = "downup") # to fill NAs in the event table
    
    ev # ev table will be transferred into 'eta' version or 'no-eta' version
    
    output$data_arr6 <- renderTable({ ev })
    
    # ETA definition / Simulation ---------------------------------------------
    
    # Visual parametric diagnosis table (df_iiv), list[[3]]
    eta_table <- tail(fit.s$eta,1)[,-1]
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
    sim_res_noiiv <- simulate(object = fit.s,
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
    sim_res_iiv <- simulate(object = fit.s,
                           events=ev_iiv,
                           nSub = input$vpc_opt,
                           seed = input$vpc_seed)
    
    sim_res_iiv$condi <- ifelse(sim_res_iiv$time<as.numeric(sim_start_time),
                                  'est',
                                  'sim')
    sim_res_iiv$condi[is.na(sim_res_iiv$condi)] <- 'est'
    
    list(sim_res_noiiv, sim_res_iiv, df_iiv)
    
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
        
        scale_fill_manual(values=c(pk_color, '#999999')) +
        scale_color_manual(values=c(pk_color, '#999999')) +
        
        geom_ribbon(data=sim_res_iiv, aes(x=Time, ymin=P5, ymax=P95, fill=condi), alpha=0.15) +
        geom_ribbon(data=sim_res_iiv, aes(x=Time, ymin=P25, ymax=P75, fill=condi), alpha=0.25, ) +
        geom_line(data=sim_res_iiv, aes(x=Time, y=P50, color=condi), size=0.6, alpha=0.4) +
        
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
        mutate(
          P5 = quantile(Estimated, probs = 0.05),
          P25 = quantile(Estimated, probs = 0.25),
          P50 = quantile(Estimated, probs = 0.5),
          P75 = quantile(Estimated, probs = 0.75),
          P95 = quantile(Estimated, probs = 0.95)
        )
      sim_res_iiv <- subset(sim_res_iiv, sim.id==1)
      
      
      #fit.s$CMT <- as.numeric(fit.s$CMT) # Compartment numbering in fitted table
      
      # plot
      ggp <- ggplot(sim_res_noiiv) +
        
        geom_ribbon(data=sim_res_iiv, aes(x=Time, ymin=P5, ymax=P95), alpha=0.15, fill=pd_color) +
        geom_ribbon(data=sim_res_iiv, aes(x=Time, ymin=P25, ymax=P75), alpha=0.25, fill=pd_color) +
        geom_line(data=sim_res_iiv, aes(x=Time, y=P50), size=0.6, alpha=0.4, color=pd_color) +
        
        
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
  output$data_arr2 <- renderTable({ est_table()[[1]] })
  output$data_arr3 <- renderTable({ sim_summary()[[3]] })


  
} # Server end
