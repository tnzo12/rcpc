# UI ----------------------------------
des_model_ui <- function(id){ # ui for description
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("des_model"))
}
des_notes_ui <- function(id){ # ui for description
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("des_notes"))
}
des_abbr_ui <- function(id){ # ui for description
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("des_abbr"))
}

# Server ------------------------------
des_server <- function(id, mod_env, values){
  shiny::moduleServer(id, function(input, output, session) {
    
    # Model description
    # based on loaded model environment
    output[["des_model"]] <- renderPrint({
      mod_env()
      model <- values$model
      cat(
        '<span style="color:grey">',
        "<i>Model of</i>",
        "</span>",
        
        "<i>","<strong>",model,"et al.","</strong>","</i>","<br>",
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
    
    
    # Model notes
    output[["des_notes"]] <- renderPrint({
      mod_env()
      cat(
        '<span style="color:grey">',
        "<i>",
        des_notes,
        "</i>",
        "</span>"
      )
    })
    
    # abbreviations
    output[["des_abbr"]] <- renderText({
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
    
    
  } # module Server ends
    
  )
}