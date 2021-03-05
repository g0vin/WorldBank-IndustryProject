# List of graph types used when we invoke a graph - reusable function
# Create a function called "graphs" which is used to create the graphs 
graphs <- function(dat, plot_type, x, y, facet){
  
  # Wrap the values for each specified column.
  # But don't wrap the "Value" column
  # This is so labels and keys on the axes and legends will not cluttered the plot when the words got too long
  if(x != "Value"){
    dat[, x] <- str_wrap(dat[, x], 20)
  }
  if(y != "Value"){
    dat[, y] <- str_wrap(dat[, y], 20)
  }
  if(facet != "Value"){
    dat[, facet] <- str_wrap(dat[, facet], 20)
  }
  
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
  # If the selection is a bubble plot
  if(plot_type == "bubble"){
    g <- ggplot(dat, aes(size = UQ(as.name(facet)),
                         y = UQ(as.name(y)),
                         x = UQ(as.name(x)))) + # Change the types of the following aesthetics 
      geom_point(alpha=0.4) + # Function to initialize the points and bubble
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
