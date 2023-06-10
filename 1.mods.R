# Author: Woojin Jung
# List up drug models

# Initial settings ------------------------------
# networkD3 settings
my_color <- 'd3.scaleOrdinal()
  .domain(["group_A", "group_B","group_C", "group_D", "group_E", "group_F", "group_G", "group_H", "group_I", "group_J", "group_K"])
  .range(["grey", "teal" , "orange", "red", "brown", "green", "purple", "pink", "slategray", "blue", "fuchsia"])'

# Generate drug dir. data frame (doesn't need to be included in reactive expr.)
#drug_dir <- strsplit(list.files("./drug/", pattern = ".R", recursive = TRUE), # Get the direction
#                     split="[/.]") # seperate / and . in path characters
#
#drug_list <- as.data.frame(t(as.data.frame(drug_dir)),row.names = FALSE, stringsAsFactors = FALSE)
#names(drug_list) = c("Drug","Class","Author","Extension")
#drug_list <- drug_list[,-ncol(drug_list)]


# version 2
drug_dir <- list.files("./drug", pattern = ".R", recursive = TRUE)


read_mod_cov <- function(drug_dir, i){
  
  source(paste0("./drug/",drug_dir[i]), local=TRUE)  
  if ( exists(quote(mod_cov), where = environment(), inherits = FALSE ) ){
    cov_temp[[i]] <<- list(dir = strsplit(drug_dir[i], split="[/.]"), cov = mod_cov, cov_val = list(mod_lcov_value))
    rm(mod_cov)
  } else {
    cov_temp[[i]] <<- list(dir = strsplit(drug_dir[i], split="[/.]"), cov = "None", cov_val = "None")
  }
  
  return(cov_temp)

}
cov_temp <- list() # template for drug / covariates / authors
for (i in 1:length(drug_dir)){ read_mod_cov(drug_dir, i) }

cov_temp <- data.table::rbindlist(cov_temp, fill=TRUE) %>%
  unnest_wider(dir, names_sep = ".") %>% 
  select(dir.1, cov, dir.2, cov_val) %>%  # exclude: extension name
  rename(Drug = dir.1, Cov = cov, Author = dir.2, Cov_value = cov_val) %>% 
  data.table()


#unicov <- cov_temp %>%
#  filter(Drug=="Tacrolimus_Kidney") %>%
#  pull(Cov_value) %>% unique() %>%
#  unlist() %>% names() # get unique values of covariates
#
#list_unicov <- unicov %>%
#  stringr::str_split(pattern="[.]") %>% # get unique selection list of covariates
#  unlist() %>%
#  matrix(nrow=length(unicov), byrow=TRUE) %>%
#  data.frame() %>% rename(item=X1, category=X2) %>%
#  group_by(item) %>% reframe(categories = list(category)) %>% 
#  rowwise() %>% 
#  mutate(categories = list(unique(categories))) %>% 
#  ungroup() %>% 
#  mutate(categories = stats::setNames(categories,item))
#
#cov_val <- cov_temp %>% filter(Drug=="Tacrolimus_Kidney") %>% pull(Cov) %>% unique()
#
#rt <- rhandsontable::rhandsontable(
#  rep(NA,length(cov_val)) %>% as.numeric() %>% t() %>% data.frame() %>% setnames(cov_val)
#) %>% 
#  hot_table(overflow = "visible", stretchH = "all")
#
#for (i in list_unicov$item){
#  rt <- rt %>% hot_col(col = i, type = "dropdown", source = list_unicov$categories[[i]] )
#}
#rt
#
#table <- rep(NA,length(cov_val)) %>% as.numeric() %>% t() %>% data.frame() %>% setnames(cov_val)
#table$DOT <- 1
#table$AGE <- 50
#table$CS <- 200
#table$APGAR <- 1
#table$WT <- 50
#cov_input <- table[,!is.na(table)] %>% names()
#
#
#d <- cov_temp %>%
#  group_by(Drug,Author) %>%
#  reframe(Cov = list(Cov)) %>% 
#  rowwise() %>% 
#  filter(all(Cov %in% cov_input))
#
#g <- data.frame()
#
#for(i in d$Author){
#  source(paste0("./drug/Phenobarbital/",i,".R"))
#  fit <- nlmixr(f,dataset, est="posthoc")
#  fitdf <- fit$objDf %>% mutate(model_of = i)
#  g <- rbind(g,fitdf)
#}

# UI --------------------------------------------
mods_drug <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("drugs"))
}
mods_netwk <- function(id) { # ui for theta
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("mod_netwk"),
                  width = '100%',
                  height = '300px')
    
} # ui function ends


# Server ----------------------------------------
mods_server <- function(id, values){
  shiny::moduleServer(id, function(input, output, session) {
    
   
    # Network Visualization::networkD3
    selected_drug <- shiny::reactive({
      
      drug_selection <- values$drug_selection
      model <- values$model
      
      #drug_selection <- 'Vancomycin'
      #model <- 'Jung'
      selected_drug <- filter(cov_temp, Drug==drug_selection) %>% select(-c(Drug, Cov_value)) %>% data.frame
      
      # Generate drug node
      drug_node <- unique(unlist(selected_drug))
      drug_node <- data.frame(node = drug_node, idx = 0:(length(drug_node)-1) )
      drug_node$n_size <- ifelse(drug_node$node %in% c(cov_temp$Drug), 4, ifelse(drug_node$node %in% c(cov_temp$Cov), 7, 10))
      drug_node$group <- ifelse(drug_node$node %in% c(cov_temp$Drug, cov_temp$Cov), 1, 2:nrow(drug_node))
      
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
    
    # Visualization of drug-network
    
    
    output$mod_netwk <- shiny::renderUI({
      ns <- session$ns
      networkD3::renderForceNetwork({
        
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
          opacity = 0.8,
          bounded = TRUE,
          clickAction = 'Shiny.onInputChange("model",d.name)'
        )
        
      })
    })
    
    output$drugs <- shiny::renderUI({
      ns <- session$ns
      shiny::selectInput(
        
        inputId = 'drug_selection',
        label = 'Drug',
        choices = c(list.files("./drug/")),
        selected = values$drug_selection
        
      )
    })
    
#    output[["param_theta_view"]] <- shiny::renderUI({
#      ns <- session$ns
#      reactable::renderReactable({ param_tbl()$theta })
#    })
#    
#    param_tbl
    
  }) # moduleServer ends
}
