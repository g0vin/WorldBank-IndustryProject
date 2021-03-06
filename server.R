## DEFINE SERVER---------------------------------------------------------------- 
# Create the server which helps run the functions associated within the UI to create and manipulate the tables/graphs
server <- function(input, output, session) {
  
  # Show the introduction welcome page
  observeEvent("", {
    # showModal() to create the welcome page and Intro tour
    showModal(modalDialog(
      welcome_page,
      easyClose = TRUE,
      footer = tagList(
        
        # Click on button to have a introduction tour to the shiny application. 
        actionButton(inputId = "intro", label = "INTRODUCTION TOUR", 
                     icon = icon("info-circle"), class = "btn-info")
        
      ) #End tagList() 
    )) #End showModal() and modelDialog()
  })
  
  # Only show introduction welcome page once
  observeEvent(input$intro,{
    removeModal()
  })
  
  # Activate introduction tour when the 'INTRODUCTION TOUR' button is clicked on
  observeEvent(input$intro, {
    introjs(session, options = list("nextLabel" = "Continue",
                                    "prevLabel" = "Previous",
                                    "doneLabel" = "Alright. Let's go"))
  })
  
  # Hide all visuals from all panels when we reset the filters 
  # Except the original data table
  observeEvent(input[["param-filters-reset_all"]], {
    hide("plot")
    hide("transfTab")
    hide("plotTransf")
  })
  
  # Only show selected plots and tables for each panel  
  observeEvent("", {
    show("original_panel")
    hide("transformed_panel")
  }, once = TRUE)
  
  observeEvent(input$original, {
    show("original_panel")
    hide("transformed_panel")
  })
  
  observeEvent(input$transformed, {
    show("transformed_panel")
    hide("original_panel")
  })
  
  # Invoke the selectizeGroupServer module
  # Returns a reactive function containing the filtered data frame.
  # Calling the reactive function, res_mod(), will returns a filter data frame.
  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "param-filters",
    data = results,
    vars = colnames(results)
  )
  
  # The renderDataTable() is also a function in shiny, so a conflict will occur if we do not call the DT namespace (Source: https://stackoverflow.com/questions/38825394/r-shiny-data-table-sometimes-does-not-display).
  # More info about the extension option in DT can be found here: https://rstudio.github.io/DT/extensions.html 
  output$table <- DT::renderDataTable({
    res_mod() %>%
      pivot_wider(names_from = "Year", values_from = "Value") %>% # pivot_wider() widens the data and increases the number of columns and decreases the number of rows
      datatable(extensions = c("Buttons"), 
                options = list(scrollX = TRUE,
                               lengthMenu = list(c(10, 30, 50), c('10', '30', '50')),
                               dom = 'Bfrtip',
                               buttons = c('pageLength', 'copy', 'csv', 'excel'),
                               searching = F))
  })
  
  
  # References the graphs.R and formulas.R files, where the code for graphs and transformations options are located
  # These are necessary for the code below to execute without error
  source("graphs.R", local = T)
  source("formulas.R", local = T)
  
  # Create the selected visuals when the 'Create' button is clicked for both panels
  observeEvent(input$all_graphs, {
    
    all_filters_name <- c("param-filters-Simulation", "param-filters-Variable", 
                          "param-filters-Sector", "param-filters-Qualifier", 
                          "param-filters-Year")
    
    # If the user didn't select filters from up to 3 drop-down box, a modal dialog will pop up.
    # No graphs will be rendered in the ORIGINAL & TRANSFORMED PANEL
    length_filters <- sapply(all_filters_name, function(x){ length(input[[x]]) })
    if(sum(length_filters == 0) > 3){
      showModal(modalDialog(
        "No filters are selected. Please go back and select some. 
         If you did but this still shows up, then make sure you select 
         filters for up to 3 dropdown boxes",
        easyClose = T,
        size = "m",
        footer = NULL,
        title = "Stop!"
      ))
      return()
    }
    
    # Render the graphs the user wants in the ORIGINAL PANEL
    # Show notification when this is done successfully
    p <- graphs(res_mod(), input$plots, input$x_val, input$y_val, input$facet)
    
    #Assign the plot values p to the output screen as a new name called plot and show it
    output$plot <- renderPlotly({ p })
    show("plot")
    #Creates notification when plot is successfully created
    showNotification(id = "graph_created", 
                     paste("Graph Created Successfully For Original Data"),
                     closeButton = T, type = "message")
    
    
    # Perform the transformation the user had selected on the Values
    pivotTab <- res_mod()
    pivotTab$Change <- NA
    for(r in 1:nrow(pivotTab)){
      pivotTab[r, "Change"] <- transf(pivotTab[r, "Simulation"], 
                                      input$sim2, 
                                      pivotTab[r, "Variable"],
                                      pivotTab[r, "Sector"], 
                                      pivotTab[r, "Qualifier"], 
                                      pivotTab[r, "Year"], 
                                      input$typeConv)
    }  
    
    
    # Render the graphs and tables the user wants in the TRANSFORMED PANEL
    # Show notification when this is done successfully.
    
    # Note: The graphs here change along with new transformed table.
    # So creating a new transformed table with a new formula will result in a new graph
    # if it rendered before in the TRANSFORMED PANEL. But won't happen for the other way around.
    output$transfTab <- DT::renderDataTable({
      pivotTab %>%
        select(!Value) %>%
        pivot_wider(names_from = "Year", values_from = "Change") %>% #pivotwider() widens the data and increases the number of columns and decreases the number of rows
        datatable(extensions = c("Buttons"), 
                  options = list(scrollX = TRUE,
                                 lengthMenu = list(c(10, 30, 50), c('10', '30', '50')),
                                 dom = 'Bfrtip',
                                 buttons = c('pageLength', 'copy', 'csv', 'excel'),
                                 searching = F))
      
    }) # End for renderDataTable()
    
    # Output the transformed table and create a notification when it is successful  
    show("transfTab")
    showNotification(id = "transf_table_created", 
                     paste("Table Created Successfully For Transformed Data"), 
                     closeButton = T, type = "message")
    
    # Assign a new variable called p2 which plots the transformed plots  
   if(input$typeConv == "Propotion of Sector Per Year") {
     p2 <- graphs(pivotTab, input$plotType2, input$x_val_transf, 
                 input$y_val_transf, input$facet_transf)
    output$plotTransf <- renderPlotly({ p2 })
  }
  else {
    p2 <- graphs(pivotTab, input$plotType, input$x_val_transf, 
                 input$y_val_transf, input$facet_transf)
    output$plotTransf <- renderPlotly({ p2 })
  }
    
    
    #Output the transformed table and create a notification when it is successful  
    show("plotTransf")
    showNotification(id = "transf_graph_created", 
                     paste("Graph Created Successfully For Transformed Data"), 
                     closeButton = T, type = "message")
    
  }) # End for observeEvent()
  
  # Perform the saving when the user click on 'Save'
  # Show notification on the side when add, cancel, or update when it runs successfully
  observeEvent(input$save, {
    name <- input$name_sav_filters
    
    # Create list of inputs we won't save 
    dont_save <- c("sidebarCollapsed", "delete", "transformed", "original", 
                   "createTransf", "graph", "intro", "sidebarItemExpanded", 
                   "`param-filters-reset_all`", "save", "load", 
                   "save_filters_selection", "reuse_saved_filters")
    
    # Check if the name is already in use
    if(name %in% sav_name){
      # Print some warning message (shiny alert) if the name is already in use
      shinyalert(text = "Name already existed. Choose a different name or 
                         proceed if you want to update instead.", 
                 showCancelButton = T, confirmButtonText = "Update")
      
      # Update the inputs selection if user clicked "Update"; else do nothing
      observeEvent(input$shinyalert, {
        # Update the inputs selection
        if(input$shinyalert == T){
          
          for(i in names(reactiveValuesToList(input))){
            if(!(i %in% dont_save)){
              inputs[[name]][[i]] <<- unclass(input[[i]])
            }
          }
          showNotification(id = "updated", paste("Updated ", name, " Successfully"), 
                           closeButton = T, type = "message")
          # Do nothing otherwise; don't update the selection
        } else {
          showNotification(id = "cancel", paste("Cancel updating ", name), 
                           closeButton = T, type = "message")
        }
        
      }) # End of observeEvent()
      
      # If name is not in use, then create a new one and save its selected inputs
    } else {
      sav_name <<- c(sav_name, name)
      for(i in names(reactiveValuesToList(input))){
        if(!(i %in% dont_save)){
          inputs[[name]][[i]] <<- unclass(input[[i]])
        }
      }
      showNotification(id = "added", paste("Added ", name, " Successfully"), 
                       closeButton = T, type = "message")
    }
    
    # Update the options users can choose to load/delete
    updateSelectizeInput(session, inputId = "sav_filters", choices = na.omit(sav_name))
  })
  
  # Perform the loading when the user click on 'Load' 
  observeEvent(input$load, {
    choosen_i <- input$sav_filters
    
    if(!is.null(inputs[[choosen_i]])){
      a <- inputs[[choosen_i]]
      lapply(names(a), function(x) session$sendInputMessage(x, list(value = a[[x]])))
    }
    
    showNotification(id = "loaded", paste("Loaded ", choosen_i, " Successfully"), 
                     closeButton = T, type = "message")
  })
  
  # Delete the saved inputs when user click on 'Delete'
  observeEvent(input$delete, {
    choosen_i <- input$sav_filters
    
    # Remove the name and the saved inputs from the system
    sav_name <<- sav_name[sav_name != choosen_i]
    inputs <<- inputs[names(inputs) != choosen_i]
    
    # Update the choices user can see under the 'Load' drop down button
    updateSelectizeInput(session, inputId = "sav_filters", choices = na.omit(sav_name))
    
    showNotification(id = "deleted", paste("Deleted ", choosen_i, " Successfully"), 
                     closeButton = T, type = "message")
  })
  
} # End of the server
