# scenario based simulations
sim_sce_ui1 <- function(id){
  ns <- shiny::NS(id)
  plotlyOutput( ns("sce_grid1"), height = "230px" )
}
sim_sce_ui2 <- function(id){
  ns <- shiny::NS(id)
  plotlyOutput( ns("sce_grid2"), height = "230px" )
}
sim_sce_ui3 <- function(id){
  ns <- shiny::NS(id)
  plotlyOutput( ns("sce_grid3"), height = "230px" )
}
sim_sce_ui4 <- function(id){
  ns <- shiny::NS(id)
  plotlyOutput( ns("sce_grid4"), height = "230px" )
}

# filtering function for pta estimation
filvar <- function(df, filt, values){
  df[ df[[filt]] >= values[1] & df[[filt]] <= values[2], ]
}

sim_pta_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::fluidRow(
    valueBoxOutput(ns("pta_auc")),
    valueBoxOutput(ns("pta_conc")),
    valueBoxOutput(ns("pta_both"))  
  )
  
}
sim_pta_hist_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::column(width=6,
  plotlyOutput(ns("pta_hist_conc")),
  plotlyOutput(ns("pta_hist_auc"))
  )
}


sim_server <- function(id, mod_env, values){
  shiny::moduleServer(id, function(input, output, session) {
    ns <- shiny::NS(id)
    
    observeEvent(values$sce_button,{
      mod_env()
      sce_doser <- values$sce_doser %>% as.vector() %>% as.numeric()
      sce_tau <- values$sce_tau %>% as.vector() %>% as.numeric()
      f_data <- values$f_data
      cov_data <- values$cov_data
      fit.s <- values$fit.s
      eta_table <- values$eta_table
      
      
      if( any(is.null(values$f_data), values$is_sim == FALSE) ){
        showModal(
          modalDialog(
            title = "Scenario-based prediction aborted",
            size = "l",
            easyClose = TRUE,
            "At least one dosing history for simulation is required"
          )
        )
        return()
      }
      
      # last simulation record
      sim_lastr <- f_data %>% data.table() %>%
        setnames(., tolower(names(.))) %>%
        .[!is.na(amt),] %>% 
        .[condi=='sim',] %>%
        .[.N] %>%
        select(any_of(c("time","amt","cmt","rate","ii","evid","ss")))
      
      evt <- et(seq(from=0,to=24, by=0.5))
      
      sce_et_gen <- function(doser, tau, ind){
        
        evt %>% 
          merge(sim_lastr %>% mutate(time=0,
                                     ss=1,
                                     amt=amt*doser,
                                     rate=rate*doser,
                                     ii=tau)
                , # last row
                all=TRUE) %>% 
          merge(cov_data, all=TRUE) %>% # merge (outer join)
          tidyr::fill(any_of(mod_cov), .direction = "downup") %>% # to fill NAs in the event table
          tidyr::fill("CRPZERO", .direction = "downup") %>% 
          dplyr::bind_cols(eta_table)
      }
      
      combs <- expand.grid(doser=sce_doser, tau=sce_tau) %>% mutate(id = row_number()) # list the number of cases
      
      sce_grid <- combs %>% 
        data.table() %>%
        .[, sce_et_gen(doser, tau), by=id] %>%
        #furrr::future_pmap(sce_et_gen) %>% 
        #rbindlist() %>%
        #mutate(id = rep(1:nrow(combs), each=nrow(evt)+1)) %>%  # dosing record
        rxSolve(object=fit.s, events=., nSub=1) %>%
        data.table() %>% 
        .[,ind_auc := lapply(.SD, auc, time), .SDcols="ipredSim", by=id] %>% 
        .[,cum_auc := cumsum(ind_auc), by=id] %>% 
        .[,c_peak := max(ipredSim), by=id] %>% 
        .[,c_trou := .SD[time==sim_lastr$ii, ipredSim], by=id] %>% 
        .[,auc_tau := .SD[time==sim_lastr$ii, cum_auc], by=id] %>% 
        .[,auc_24 := auc_tau*24/sim_lastr$ii, by=id] %>% 
        .[,.SD[1], by=id]
      
      sce_res <-  left_join(combs, sce_grid, by="id") %>% mutate(dose=doser*sim_lastr$amt)
   
      
      
      sce_iter <- sce_et_gen(1,sim_lastr$ii) %>% 
        select(-names(eta_table)) %>%  # get rid of the etas
        rxSolve(object=fit.s, events=., nSub=50) %>% 
        data.table %>% 
        .[,ind_auc := lapply(.SD, auc, time), .SDcols="ipredSim", by=sim.id] %>% 
        .[,cum_auc := cumsum(ind_auc), by=sim.id] %>% 
        .[,c_peak := max(ipredSim), by=sim.id] %>% 
        .[,c_trou := .SD[time==sim_lastr$ii, ipredSim], by=sim.id] %>% 
        .[,auc_tau := .SD[time==sim_lastr$ii, cum_auc], by=sim.id] %>% 
        .[,auc_24 := auc_tau*24/sim_lastr$ii, by=sim.id] %>% 
        .[,.SD[1], by=sim.id]
      
      values$sce_res <- sce_res
      values$sce_iter <- sce_iter
      values$conc_win <- input$conc_win
      values$conc_type <- input$conc_type
      values$auc_win <- input$auc_win
      values$auc_type <- input$auc_type
    })
    
    
    
    
    output$sce_grid1 <- renderPlotly({
      if(any(is.null(values$f_data), values$is_sim == FALSE, is.null(values$sce_button), is.null(values$sce_res))){
        no_plot("AUC 24", "No simulation done or new run is done")
      }else{
        hm_plot(values$sce_res, "#17a2b8", "#DC3545", "dosing amount", "interval (AUC 24)", "auc_24")  
      }
    })
    output$sce_grid2 <- renderPlotly({
      if(any(is.null(values$f_data), values$is_sim == FALSE, is.null(values$sce_button), is.null(values$sce_res))){
        no_plot("AUC tau", "No simulation done or new run is done")
      }else{
        hm_plot(values$sce_res, "#17a2b8", "#DC3545", "dosing amount", "interval (AUC tau)", "auc_tau")  
      }
    })
    output$sce_grid3 <- renderPlotly({
      if(any(is.null(values$f_data), values$is_sim == FALSE, is.null(values$sce_button), is.null(values$sce_res))){
        no_plot("C peak", "No simulation done or new run is done")
      }else{
        hm_plot(values$sce_res, "#17a2b8", "#FFC107", "dosing amount", "interval (Conc. peak)", "c_peak")  
      }
    })
    output$sce_grid4 <- renderPlotly({
      if(any(is.null(values$f_data), values$is_sim == FALSE, is.null(values$sce_button), is.null(values$sce_res))){
        no_plot("C trough", "No simulation done or new run is done")
      }else{
        hm_plot(values$sce_res, "#17a2b8", "#FFC107", "dosing amount", "interval (Conc. trough)", "c_trou")  
      }
    })
    
    
    observeEvent(values$pta_button, {
      sce_iter_tbl <- values$sce_iter
      
      conc_type <- values$conc_type
      auc_type <- values$auc_type
      
      filt_auc <- sce_iter_tbl %>%
        select(c_peak, c_trou, auc_tau, auc_24) %>% 
        filvar(auc_type, values$auc_win ) %>% nrow()
      
      filt_conc <- sce_iter_tbl %>% 
        select(c_peak, c_trou, auc_tau, auc_24) %>% 
        filvar(conc_type, values$conc_win ) %>% nrow()
      
      filt_both <- sce_iter_tbl %>% 
        select(c_peak, c_trou, auc_tau, auc_24) %>% 
        filvar(auc_type, values$auc_win ) %>% 
        filvar(conc_type, values$conc_win ) %>% nrow()
      
      
      output$pta_auc <- renderbs4ValueBox(
        valueBox(paste0(filt_auc / values$vpc_opt*100,"%"), subtitle = "AUC", color="danger")
      )
      output$pta_conc <- renderbs4ValueBox(
        valueBox(paste0(filt_conc / values$vpc_opt*100,"%"), subtitle = "Conc.", color="warning")  
      )
      output$pta_both <- renderbs4ValueBox(
        valueBox(paste0(filt_both / values$vpc_opt*100,"%"), subtitle = "Both", color="info")  
      )
      
      
      output$pta_hist_conc <- renderPlotly({
        plot_ly(
          data = sce_iter_tbl,
          x = ~sim.id,
          y = sce_iter_tbl[[values$conc_type]]
        ) %>% add_trace(type="bar",
                        marker = list(color = ifelse(
                          {sce_iter_tbl[[values$conc_type]]>=values$conc_win[1] &
                            sce_iter_tbl[[values$conc_type]]<=values$conc_win[2]},
                                                     "#FFC107", "#999999"))) %>% 
                            layout(
                              xaxis = list(title = values$conc_type,
                                           color = "#999999"),
                              yaxis = list(title = "Conc.",
                                           color = "#999999"),
                              plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
                              autosize = TRUE)
        
      })
      output$pta_hist_auc <- renderPlotly({
        plot_ly(
          data = sce_iter_tbl,
          x = ~sim.id,
          y = sce_iter_tbl[[values$auc_type]]
        ) %>% add_trace(type="bar",
                        marker = list(color = ifelse(
                          {sce_iter_tbl[[values$auc_type]]>=values$auc_win[1] &
                              sce_iter_tbl[[values$auc_type]]<=values$auc_win[2]},
                          "#DC3545", "#999999"))) %>% 
                            layout(
                                xaxis = list(title = values$auc_type,
                                             color = "#999999"),
                                yaxis = list(title = "AUC",
                                             color = "#999999"),
                                plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
                                autosize = TRUE)
      })
    })
   
    
    
    
  })
}
  
    

# probability target attainment