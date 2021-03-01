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
  
  # List of graph types used when we invoke a graph - reusable function
  # Create a function called "graphs" which is used to create the graphs 
  graphs <- function(dat, plot_type, x, y, facet){
    # If the selection is a barplot
    if(plot_type == "bar"){
      g <- ggplot(dat, aes(fill = as.factor(UQ(as.name(facet))),
                           text = paste0(facet, ": ", as.factor(UQ(as.name(facet)))),
                           y = UQ(as.name(y)),
                           x = UQ(as.name(x)))) +
        geom_bar(position ="dodge", stat = "identity") + # dodge is to ensure that the bar plots are not stacked and identity is so we assign the y-values
        scale_fill_brewer(palette = "Blues") + # Blue color palette
        guides(fill = guide_legend(nrow = 1, byrow = TRUE, title = facet)) + # Creates the guides for each scale
        theme_minimal() + # Assign a minimal look for the theme
        theme(legend.position = "bottom", legend.justification = "left") # Create a legend and place it to the bottom left of the plot
      p <- ggplotly(g, tooltip = c("x", "y", "text")) # Make the plot into plotly type 
    }
    # If the selection is a stacked barplot
    if(plot_type == "stack"){
      g <- ggplot(dat, aes(fill = as.factor(UQ(as.name(facet))),
                           text = paste0(facet, ": ", as.factor(UQ(as.name(facet)))),
                           y = UQ(as.name(y)),
                           x = UQ(as.name(x)))) +
        geom_bar(position ="stack", stat = "identity") + # Stack is to ensure that the bar plots are stacked
        scale_fill_brewer(palette = "Blues") + # Blue color palette
        guides(fill = guide_legend(nrow = 1, byrow = TRUE, title = facet)) + # Creates the guides for each scale
        theme_minimal() + # Assign a minimal look for the theme
        theme(legend.position = "bottom", legend.justification = "left") # Create a legend and place it to the bottom left of the plot
      p <- ggplotly(g, tooltip = c("x", "y", "text")) # Make the plot into plotly type 
    }
    # If the selection is a histogram
    if(plot_type == "histogram"){
      g <- ggplot(dat, aes(fill = as.factor(UQ(as.name(facet))),
                           text = paste0(facet, ": ", as.factor(UQ(as.name(facet)))),
                           x = UQ(as.name(x)))) + # Change the types of the following aesthetics 
        geom_histogram(position="identity",stat = 'count') + # Function to initialize the histogram
        scale_fill_brewer(palette = "Blues") + # Blue color palette
        guides(fill = guide_legend(nrow = 1, byrow = TRUE, title = facet)) + # Creates the guides for each scale
        theme_minimal() + # Assign a minimal look for the theme
        theme(legend.position = "bottom", legend.justification = "left") # Create a legend and place it to the bottom left of the plot
      p <- ggplotly(g, tooltip = c("x", "y", "text")) # Make the plot into plotly type 
    }
    # If the selection is a line plot
    if(plot_type == "line"){
      g <- ggplot(dat, aes(group = as.factor(UQ(as.name(facet))),
                           color = as.factor(UQ(as.name(facet))),
                           text = paste0(facet, ": ", as.factor(UQ(as.name(facet)))),
                           y = UQ(as.name(y)),
                           x = UQ(as.name(x)))) + # Change the types of the following aesthetics 
        geom_line() + # Function to initialize the line
        geom_point()+ # Function to initialize the points
        scale_color_brewer(facet, palette = "Blues") + # Blue color palette
        theme_minimal() + # Assign a minimal look for the theme
        theme(legend.position = "bottom", legend.justification  = "left") # Create a legend and place it to the bottom left of the plot
      p <- ggplotly(g, tooltip = c("x", "y", "text")) # Make the plot into plotly type 
    }
    # If the selection is an area plot
    if(plot_type == "area"){
      g <- ggplot(dat, aes(fill = as.factor(UQ(as.name(facet))),
                           group = as.factor(UQ(as.name(facet))),
                           color = as.factor(UQ(as.name(facet))),
                           text = paste0(facet, ": ", as.factor(UQ(as.name(facet)))),
                           y = UQ(as.name(y)),
                           x = UQ(as.name(x)))) + # Change the types of the following aesthetics 
        geom_area() + # Function to initialize the area
        scale_fill_brewer(facet, palette = "Blues") + # Blue color palette
        scale_color_brewer(facet, palette = "Blues") + # Blue color palette
        theme_minimal() + # Assign a minimal look for the theme
        theme(legend.position = "bottom", legend.justification = "left") # Create a legend and place it to the bottom left of the plot
      p <- ggplotly(g, tooltip = c("x", "y", "text")) # Make the plot into plotly type 
    }
    # If the selection is a pie chart
    if(plot_type == "pie"){
      g <- ggplot(dat, aes(fill = UQ(as.name(facet)),
                           x = "",
                           y = UQ(as.name(y)))) + # Change the types of the following aesthetics 
        geom_bar(position ="dodge", stat = "identity") + 
        coord_polar("y", start = 0) + # Function to initialize the pie
        scale_fill_brewer(palette = "Blues") + # Blue color palette
        guides(fill = guide_legend(nrow = 1, byrow = TRUE)) + # Creates the guides for each scale
        theme_minimal() + # Assign a minimal look for the theme
        theme(legend.position = "bottom", legend.justification = "left") # Create a legend and place it to the bottom left of the plot
      p <- ggplotly(g) # Make the plot into plotly type 
    }
    # If the selection is a bubble plot
    if(plot_type == "bubble"){
      g <- ggplot(dat, aes(value = UQ(as.name(facet)),
                           y = UQ(as.name(y)),
                           x = UQ(as.name(x)))) + # Change the types of the following aesthetics 
        geom_point() + # Function to initialize the points and bubble
        scale_fill_brewer(palette = "Blues") + # Blue color palette
        guides(fill = guide_legend(nrow = 1, byrow = TRUE)) + # Creates the guides for each scale
        theme_minimal() + # Assign a minimal look for the theme
        theme(legend.position = "bottom", legend.justification = "left") # Create a legend and place it to the bottom left of the plot
      p <- ggplotly(g) # Make the plot into plotly type 
    }
    # If the selection is a scatter plot
    if(plot_type == "scatter"){
      g <- ggplot(dat, aes(color = as.factor(UQ(as.name(facet))),
                           text = paste0(facet, ": ", as.factor(UQ(as.name(facet)))),
                           y = UQ(as.name(y)),
                           x = UQ(as.name(x)))) + # Change the types of the following aesthetics 
        geom_point() + # Function to initialize the points
        scale_color_brewer(facet, palette = "Blues") + # Blue color palette
        theme_minimal() + # Assign a minimal look for the theme
        theme(legend.position = "bottom", legend.justification = "left") # Create a legend and place it to the bottom left of the plot
      p <- ggplotly(g, tooltip = c("x", "y", "text")) # Make the plot into plotly type 
    }
    # If the selection is a box plot
    if(plot_type == "boxplot"){
      g <- ggplot(dat, aes(fill = as.factor(UQ(as.name(facet))), 
                           text = paste0(facet, ": ", as.factor(UQ(as.name(facet)))),
                           y = UQ(as.name(y)),
                           x = UQ(as.name(x)))) + # Change the types of the following aesthetics 
        geom_boxplot() + # Function to initialize the box plot
        scale_fill_brewer(palette = "Blues") + # Blue color palette
        guides(fill = guide_legend(nrow = 1, byrow = TRUE, title = facet)) + # Creates the guides for each scale
        theme_minimal() + # Assign a minimal look for the theme
        theme(legend.position = "bottom", legend.justification = "left") # Create a legend and place it to the bottom left of the plot
      p <- ggplotly(g, tooltip = c("x", "y", "text")) # Make the plot into plotly type 
    }
    
    # Changes the size of the ticks on the axes
    p <- p %>% layout(xaxis = list(tickfont = list(size = 10)), 
                      yaxis = list(tickfont = list(size = 10)))
    
    # Change the y-lab and/or x-lab for graphs whose y-values are transformed
    if(y == "Change"){
      p <- p %>% layout(yaxis = list(title = input$typeConv, titlefont = list(size = 14)))
    }
    if(x == "Change"){
      p <- p %>% layout(xaxis = list(title = input$typeConv, titlefont = list(size = 14)))
    }
    
    # return the plotly object
    return(p)
  } # End of the graphs function we created.
  
  
  
  # Reference: f2
  # Create a function called "transf" to change the values to percentage
  transf <- function(sim1, sim2, var, sec, qual, yr, formula){
    val_sim1 <- res_mod() %>%
      filter(Simulation == sim1, Variable == var, Sector == sec, Qualifier == qual, Year == yr) %>% # Filter the data based on our selections
      select(Value) # Only have the value column display
    
    # Reference for diff only: f7, f10, f16
    # Difference between simulation
    if(formula == "Difference Between Simulations"){
      val_sim2 <- results %>%
        filter(Simulation == sim2, Variable == var, Sector == sec, Qualifier == qual, Year == yr) %>% # Filter the data based on our selections
        select(Value) # Only have the value column display
      return(round(val_sim1-val_sim2, 2)) # Return the values based on this formula
    }
    
    # Reference for % diff: Sheet t4, f4, f6, f8, f9, t3, t6, f12, f13, f14, f15, t5, f17, f18, f20, f21, f22, f23, f25, t7, f27, f28, f29, Sectors per, and parts of these sheets (f26 & t5 & t3 & t0 & NIA per & Summary per)
    # Percent-change formula between simulation 
    if(formula == "Percent Difference Between Simulations"){
      val_sim2 <- results %>%
        filter(Simulation == sim2, Variable == var, Sector == sec, Qualifier == qual, Year == yr) %>% # Filter the data based on our selections
        select(Value) # Only have the value column display
      return(round(val_sim1/val_sim2*100-100, 2)) # Return the values based on this formula
    }
    
    # Reference: f1
    # find the proportion for each sector per year
    if(formula == "Proportion of Sector Per Year"){
      obs_cat2 <- res_mod() %>%
        filter(Simulation == sim1, Variable == var, Qualifier == qual, Year == yr) %>% # Filter the data based on our selections
        select(Value) %>%  # Only have the value column display
        summarise(sumval=sum(Value)) # Sum up the values
      return(round(val_sim1/sum(obs_cat2$sumval), 2)) # Return the values based on this formula
    }
    
    # Reference: f5, f11, f19, f24 
    # Note: exr in the Excel represent the conversion of USD per 1 LCU
    # formula to convert value from LCU to USD per 1 billion ton of selected sector (ie co2) = Value*exr/1000000000
    if(formula == "LCU to USD Per Billion Ton"){ 
      exr <- input$gdp_usd/input$gdp_lcu
      bil <- 1000000000
      
      return(round(val_sim1*exr/bil, 2))
    }
    if(formula == "Percentage"){
      return(round(val_sim1*100, 3))
    }
  }
  
  # Create the selected visuals when the 'Create' button is clicked for both panels
  observeEvent(input$all_graphs, {
    
    all_filters_name <- c("param-filters-Simulation", "param-filters-Varaible", 
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
    p2 <- graphs(pivotTab, input$plotType, input$x_val_transf, 
                 input$y_val_transf, input$facet_transf)
    output$plotTransf <- renderPlotly({ p2 })
    
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