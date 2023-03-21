# Author: Woojin Jung
# List up drug models

# Initial settings ------------------------------
# networkD3 settings
my_color <- 'd3.scaleOrdinal()
  .domain(["group_A", "group_B","group_C", "group_D", "group_E", "group_F", "group_G", "group_H"])
  .range(["grey", "teal" , "orange", "red", "brown", "green", "purple", "pink"])'

# Generate drug dir. data frame (doesn't need to be included in reactive expr.)
drug_dir <- strsplit(list.files("./drug/", pattern = ".R", recursive = TRUE), # Get the direction
                     split="[/.]") # seperate / and . in path characters

drug_list <- as.data.frame(t(as.data.frame(drug_dir)),row.names = FALSE, stringsAsFactors = FALSE)
names(drug_list) = c("Drug","Class","Author","Extension")
drug_list <- drug_list[,-ncol(drug_list)]



# UI --------------------------------------------
mods_ui <- function(id) { # ui for theta
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("mod_netwk"),
                  width = '100%',
                  height = '300px')
} # ui function ends


# Server ----------------------------------------
mods_server <- function(id, drug_selection, model){
  shiny::moduleServer(id, function(input, output, session) {
    
   
    # Network Visualization::networkD3
    selected_drug <- shiny::reactive({
      
      drug_selection <- drug_selection()
      model <- model()
      
      selected_drug <- filter(drug_list, Drug==drug_selection)
      
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
    
    # Visualization of drug-network
    
    
    output[["mod_netwk"]] <- shiny::renderUI({
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
          opacity = 0.7,
          bounded = TRUE,
          clickAction = 'Shiny.onInputChange("model",d.name)'
        )
        
      })
    })
    
#    output[["param_theta_view"]] <- shiny::renderUI({
#      ns <- session$ns
#      reactable::renderReactable({ param_tbl()$theta })
#    })
#    
#    param_tbl
    
  }) # moduleServer ends
}
