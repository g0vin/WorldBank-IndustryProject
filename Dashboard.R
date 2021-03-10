## Dependencies-----------------------------------------------------------------
# Install the packages needed to create the shiny application
# install.packages("shiny")
# install.packages("shinyWidgets")
# install.packages("shinydashboard)
# install.packages("shinyalert")
# install.packages("shinyBS")
# install.packages("shinyjs)
# install.packages("rintrojs")
# install.packages("readr")
# install.packages("readxl")
# install.packages("plyr")
# install.packages("tidyverse")
# install.packages("DT")
# install.packages("rlang")
# install.packages("plotly")
# install.packages("ggplot2")

#Call all the packages needed to create the shiny application
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyalert)       # for shinyalert()
library(shinyBS)          # for bsButton()
library(shinyjs)          # for hide() and show()
library(rintrojs)         # for introBox()
library(readr)            # for read_csv()
library(readxl)           # for read_excel()
library(plyr)             # for mapvalues()
library(tidyverse)
library(DT)
library(rlang)            # for UQ()
library(plotly)
library(ggplot2)

## LOAD RData-------------------------------------------------------------------
# Un-comment the code if you want to load the environment to re-use saved filters.
# Don't forget to specify the path the RData file is saved in setwd()
# Before that we need to clear the environment (so whatever the client save on their own will be erased when the app refreshed but not the one we pre-loaded in for them)

# rm(list=ls())
# setwd("C:/Users/Owner/Downloads")
# load("saved_filters.RData")


## SAVE RData-------------------------------------------------------------------
# Un-comment if you want to save the environment. As long as the environment is saved before you close RStudio, you can load it back in and use the saved filters again.
# Don't forget to specify the path you want the RData file to save in setwd()
# Only do this step after you finish using the app. Only run this line of code individually.

# setwd("C:/Users/Owner/Downloads")
# if(length(sav_name) > 1 & length(inputs) > 0){
#   save(sav_name, inputs, file = "saved_filters.RData")
# }


## GLOBAL VARIABLES-------------------------------------------------------------
# Used to stored sets of selected inputs we want to save with their names
# You will not lose the sets of selected inputs even if you close and re-load the app. But this doesn't apply when you close RStudio.
if(!exists("sav_name")){ sav_name <<- NA}
if(!exists("inputs")) { inputs <<- list()}


## LOAD RESULTS FILE -----------------------------------------------------------
# Read in the results file and do some data preparation. 
# Don't forget to change the path to where the results file is located
# Any character data will be converted into factor variable. Same for the Year variable.
results <- read_csv("C:/Users/Owner/Downloads/results.csv")
results <- results %>% 
              mutate_if(is.character, as.factor) %>% 
              mutate(Year = factor(Year)) %>%
              mutate(Value = round(Value, 3))


## CHANGE ABBREV TO READABLE TEXT-----------------------------------------------
# Load the dictionary file that contains the real names of the abbreviations
# Don't forget to change the path to where the dictionary file is located
dict <- read_excel("C:/Users/Owner/Downloads/results real names.xlsm",
                   sheet = "Lookup Table")

# Take out all empty columns
emptycols <- colSums(is.na(dict)) == nrow(dict)
dict <- dict[!emptycols]

# Rename the columns
names(dict) <- c("Sim", "sim_name", "Var", "var_name", "Sec", "sec_name", "Qual", "qual_name")

# Convert the abbreviations to readable text
results$Simulation <- mapvalues(x = results$Simulation,
                                from = dict$Sim, to = dict$sim_name,
                                warn_missing = F)
results$Variable <- mapvalues(x = results$Variable,
                              from = dict$Var, to = dict$var_name,
                              warn_missing = F)
results$Sector <- mapvalues(x = results$Sector,
                            from = dict$Sec, to = dict$sec_name, warn_missing = F)
results$Qualifier <- mapvalues(x = results$Qualifier,
                               from = dict$Qual, to = dict$qual_name,
                               warn_missing = F)


## CONTENT FOR INTRODUCTION & HELP MENU----------------------------------------- 
# The text box content for each step in the introduction tutorial
intro <- data.frame(
  step = c(1:8),
  text = c("Choose from 5 different selection criteria to refine you selection.",
          "Here are some graph options you can choose to generate in the <b>ORIGINAL DATA</b> panel. Don't forget to click on <b>CREATE</b> if you want the visuals to show up.",
          "Here are the options to transform the values to percentage or other formats. A table  or graphs will appear in the <b>TRANSFORMED DATA</b> panel once a selection is made",
          "You start automatically in the <b>ORIGINAL DATA</b> panel. The first object you will see in this panel is the full dataset. Check the other panel for transformed data value",
          "Here's the original data set. Once, you select some filters, the table will automatically update. On the top, there are options to download and copy the data that is on this visible page.",
          "You can save the filters for later use. Don't forget to name the set of filters",
          "You can reuse the filters you saved previously and load it back in. There's also an option to delete whatever you saved",
          "Here are some helpful instructions to navigate the app if you get lost."
          )
)

# The content for the welcome page
welcome_page <- HTML(
                    "<h2><span style='color: #009FDA;'><b>WELCOME TO WORLD BANK!</b></span> <br> <hr></h2>
                    Here's a website where you can go on, explore the data, and do some exploratory analysis. We have pre-loaded some useful filters. You can choose one set of filters we have given you, load it back into the system, and play around with their visuals OR you can create your own combinations of filters. <br> <hr>
                    Click on the <span style='color: #0000A0;'><b>INTRODUCTION TOUR</b></span> button and it will give you a tour around the website to get you familarize with the buttons and the options we offer to you. If you need more help after the introduction tour, please click on the <span style='color: #0000A0;'><b>?</b></span> icon. <br> <br>
                    <span style='color: #0000A0;'><b>We hope you have a wonderful adventure!</b></span>"
                    )

# The content for each step in the dropdown help menu
help <- data.frame(
  step <- c(1:8),
  text <- c("Select from at least 3 dropdown filters to create visuals.",
            "<b>Plot Options</b> are for changing and creating the plot you want to display in the <b>ORIGINAL DATA</b> panel",
            "In terms of what we have here, <b>TRANSFORM</b> means that the values in the data are converted into another format or plugged into a formula and created a new value. So the <b>Transform Tables and Graph Options</b> are for changing and creating the table and plot you want to display in the <b>TRANSFORMED DATA</b> panel.",
            "Click on the <b>Create</b> button to generate all visuals and transformed table in the <b>ORIGINAL</b> and <b>TRANSFORMED</b> data panel.",
            "<b>You can only download the data that is on the visible page.</b> For example, if the table only shows 10 rows, the csv or excel files will only saved those 10 rows. If you want to save and display more than 10 rows in the data table, click on the <b>Show 10 rows</b> button and click the other length options.",
            "Hover your mouse on the top-right corner of the graphs and click on the <b>Camera</b> icon. This allows you to download the static form of the plot in a png file. Feel free to explore the other icons next to the <b>Camera</b> icon.",
            "If you want to save the inputs and filters you have selected, click on the <b>Save</b> icon in the top-right corner and type in a name and click <b>Save</b>. If the name you use is already in the system, you have the option to click <b>Cancel</b> and choose another name or <b>Update</b> the one you already have.",
            "If you want to load back in the filters you have previously saved, click on the <b>Recycle</b> icon in the top-right corner and choose the one you want and click <b>Load</b>. Once the filters load back in successfully, click on <b>CREATE</b> again to see all visuals. But if you want to delete what you have previously saved, click on the <b>Delete</b> button instead."
            )
  
)

## HELPER FUNCTION---------------------------------------------------------------
# A function to wrap the text in the 'HELP' dropdown menu
wrap_text <- function(text){
  tags$div(HTML(text), style = "white-space: normal;
                                display: inline-block; 
                                vertical-align: top; 
                                width: 200px;")
}

## Create and Define the UI --------------------------------------------------------------------
ui <- function(request){
  
  #Create the dashboard page using the dashboardPage function which has three parts: a header, a sidebar, and a body.
  dashboardPage(
    
    # THE TITLE AND THEME OF DASHBOARD------------------------------------------
    skin = "black",
    title = "World Bank",
    
    ## HEADER-------------------------------------------------------------------
    dashboardHeader(
      title = "World Bank",
      titleWidth = 300,
      
      # The HELP dropdown menu
      # Creates the help dropdown menu in case the user were to get stuck
      dropdownMenu(
        type = "notifications", 
        headerText = strong("HELP"),
        icon = introBox(data.step = 8,
                        data.intro = intro$text[8],
                        icon("question")), 
        badgeStatus = NULL,
        notificationItem(
          text = wrap_text(help$text[1]),
          icon = icon("filter"),
        ),
        notificationItem(
          text = wrap_text(help$text[2]),
          icon = icon("chart-bar")
        ),
        notificationItem(
          text = wrap_text(help$text[3]),
          icon = icon("exchange-alt")
        ),
        notificationItem(
          text = wrap_text(help$text[4]),
          icon = icon("plus")
        ),
        notificationItem(
          text = wrap_text(help$text[5]),
          icon = icon("download")
        ),
        notificationItem(
          text = wrap_text(help$text[6]),
          icon = icon("camera")
        ),
        notificationItem(
          text = wrap_text(help$text[7]),
          icon = icon("save")
        ),
        notificationItem(
          text = wrap_text(help$text[8]),
          icon = icon("recycle")
        )
      ), # End of dropdownMenu() - help dropdown
      
      # Directs the user to the World Bank home page when user clicks on "ABOUT US"
      tags$li(
        a(
          strong("ABOUT US"),
          height = 40,
          href = "https://www.worldbank.org/en/home",
          title = "",
          target = "_blank"
        ),
        class = "dropdown"
      )
    ), # End of dashboardHeader()
    
    ## SIDEBAR------------------------------------------------------------------
    # Creates the side bar where the tabs are to create and customize the outputs
    dashboardSidebar(
      width = 325,
      sidebarMenu(    
        # Outputs the World Bank image seen in the sidebar
        HTML(paste0(
        "<br>",
        "<a href='https://www.worldbank.org/en/home' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='world.svg' width = '186'></a>",
        "<br>",
        "<p style = 'text-align: center;'><small><a href='https://www.worldbank.org/en/where-we-work' target='_blank'> World Bank Project</a></small></p>",
        "<br>"
      )),
        
        # Filter options--------------------------------------------------------
      # Creates a new tab for filtering the dashboard to whatever is desired for the user output
        menuItem(
          startExpanded = T,
          introBox("Filters",
                   data.step = 1,
                   data.intro = intro$text[1]),
          tabName = "filters",
          badgeColor = "orange",
          
          # Filtering the results file with dependent drop down lists
          selectizeGroupUI(id = "param-filters",
                           inline = F,
                           params = list(
                             Simulation = list(inputId = "Simulation", 
                                               title = "Simulation:", 
                                               placeholder = 'select'),
                             Variable = list(inputId = "Variable", 
                                             title = "Variable:", 
                                             placeholder = 'select'),
                             Sector = list(inputId = "Sector", 
                                           title = "Sector:", 
                                           placeholder = 'select'),
                             Qualifier = list(inputId = "Qualifier", 
                                              title = "Qualifier:", 
                                              placeholder = 'select'),
                             Year = list(inputId = "Year", 
                                         title = "Year:", 
                                         placeholder = 'select')
                             
                           ),
                           btn_label = "Reset All")
        ), # End of menuItem() - Filters
        
      # Line breaks
        br(),
        br(),
        
        # Plot Options----------------------------------------------------------
      # Create a new tab for the plotting options
        menuItem(
          introBox("Plot Options",
                   data.step = 2,
                   data.intro = intro$text[2]),
          tabName = "plot_options",
          
          # Create the choices for the type of visuals you want to create
          selectizeInput(inputId = "plots", label = "Plot Options", choices = c("bar", "stack", "histogram", "line", "area", "bubble", "scatter", "boxplot")),
          selectizeInput(inputId = "x_val", label = "Please choose a variable as X", choices = colnames(results), selected = "Year"),
          selectizeInput(inputId = "y_val", label = "Please choose a variable as Y", choices = colnames(results), selected = "Value"),
          selectizeInput(inputId = "facet", label = "Please choose a variable to facet by", choices = colnames(results))
          
        ), # End of menuItem() - Plot Options
        
      # Line breaks
        br(),
        br(),
        
        # Transform Tables and Graphs Options-----------------------------------
      # Create a new tab for transforming the tables and graph options
        menuItem(
          introBox("Transform Tables and Graphs Options",
                   data.step = 3,
                   data.intro = intro$text[3]),
          tabName = "transform_table_and_graph_options",
          
          # Create the choices for the type of conversions to perform on the Values
          selectizeInput(inputId = "typeConv", label = "Select The Type of Conversion You Want",
                         choices = c("Difference Between Simulations", 
                                     "Percent Difference Between Simulations", 
                                     "LCU to USD Per Billion Ton", 
                                     "Proportion of Sector Per Year", "Percentage")),
          
          # Only show the comparison dropdown input if the formula requires 2 simulations
          conditionalPanel(
            condition = "input.typeConv == 'Difference Between Simulations' 
                        || input.typeConv == 'Percent Difference Between Simulations'",
            selectizeInput(inputId = "sim2", label = "Select The Simulation For Comparison",
                           choices = levels(results$Simulation))
          ), 
          
          # Only ask for USD gdp and LCU gdp if we are converting LCU to USD per billion ton
          conditionalPanel(
            condition = "input.typeConv == 'LCU to USD Per Billion Ton'",
            numericInput(inputId = "gdp_usd", label = "Enter the Current GDP USD", value = 1e+9),
            numericInput(inputId = "gdp_lcu", label = "Enter the Current GDP LCU", value = 1)
          ),
          
          # Select whether you want to change the graph or table in the TRANSFORMED DATA panel
          selectizeInput(inputId = "displayTransf", label = "Table or Graph?", 
                         choices = c("Table", "Graph")),
          
          # Only show the other Graph options when we selected "Graph" in the input above
          conditionalPanel(
            condition = "input.displayTransf == 'Graph'",
            conditionalPanel(
              condition = "input.typeConv != 'Proportion of Sector Per Year'",
              selectizeInput(inputId = "plotType", label = "Plot Options",
                             choices = c("bar", "stack", "histogram", "line", 
                                         "area", "bubble", "scatter", "boxplot"))
            ),
            conditionalPanel(
              condition = "input.typeConv == 'Proportion of Sector Per Year'",
              selectizeInput(inputId = "plotType2", label = "Plot Options", 
                             choices = c("bar", "stack"))
            ),
            selectizeInput(inputId = "x_val_transf", label = "Please choose a variable as X", 
                           choices = c(colnames(results %>% select(!Value)), "Change"), 
                           selected = "Sector"),
            selectizeInput(inputId = "y_val_transf", label = "Please choose a variable as Y", 
                           choices = c(colnames(results %>% select(!Value)), "Change"), 
                           selected = "Change"),
            selectizeInput(inputId = "facet_transf", 
                           label = "Please choose a variable to facet by", 
                           choices = c(colnames(results %>% select(!Value)), "Change"), 
                           selected = "Year")
            
          )
          
        ), # End of menuItem() - Transform Tables and Plot Options
        
      # Line breaks
        br(),
        br(),
      
        # Create a button that ouputs all the visuals ---------------------------------------------
        actionButton("all_graphs", "Create", icon = icon("plus"), width = "90%") 
        
      ) # End of the sidebarMenu() function
    ), # End of the DashboardSidebar() function
    
    ## BODY----------------------------------------------------------------------
    # Create the body of the dashboard
    dashboardBody(
      
      # Functions we need to call in order for the introduction tutorial, the alert pop-up dialog, and the hide and show panel features to work
      introjsUI(),
      useShinyalert(),
      useShinyjs(),        
      
      # BUTTONS AT THE TOP OF THE BODY------------------------------------------
      # Create a row in the main panel (center of the dashboard) where there will be buttons to view the original data and the transformed data
      fluidRow(
        
        # The UI for buttons the user clicks to switch between ORIGINAL and TRANSFORMED data panel
        column(
          width = 5,
          introBox(
            bsButton("original", 
                     label = "ORIGINAL DATA", 
                     icon = icon("database"), 
                     style = "primary"),
            bsButton("transformed", 
                     label = "TRANSFORMED DATA", 
                     icon = icon("spinner", class = "spinner-box"), 
                     style = "primary"),
            data.step = 4, data.intro = intro$text[4]
          ) # End of the introBox() function
        ), # End of the column() function
        
        # The UI for the drop down save and load filters button on the right
        column(
          width = 2,
          offset = 5,
          align = 'right',
          
          # Create the drop down button for saving a set of selected filters
          dropdownButton(
            inputId = "save_filters_selection",
            inline = T,
            size = "sm",
            right = T,
            icon = introBox(icon("save"),
                            data.step = 6,
                            data.intro = intro$text[6]),
            
            # Create the label for saving the filters
            textInput(inputId = "name_sav_filters", 
                      label = "Name The Set Of Filters You Want To Save", value = ""),
            
            # Click to confirm the save
            actionButton("save", "Save")
            
          ), # End of dropdownButton() - Saving the filters
          
          # Create the drop down button for loading/deleting the saved filters
          dropdownButton(
            inputId = "reuse_saved_filters",
            inline = T,
            size = "sm",
            right = T,
            icon = introBox(icon("recycle"),
                            data.step = 7,
                            data.intro = intro$text[7]),
            
            # Choose which set of filters we want to load/delete
            selectizeInput(inputId = "sav_filters", 
                           label = "Select A Set Of Filters You Want To Reuse", 
                           choices = na.omit(sav_name)),
            
            # Click to confirm the loading and saving. Both buttons need to place in the column(); else the buttons won't render in a line but into 2 rows
            column(
              width = 5.5,
              actionButton("load", "Load", icon = icon("spinner"), width = 145),
              actionButton("delete", "Delete", icon = icon("trash"), width = 145)
            )
            
          ) # End of dropdownButton() - Reusing the saved filters
        ) # End of column()
      ), # End of fluidRow()
     
      #Line break
      br(),
      
      # OUTPUTS TO SHOW IN EACH PANEL--------------------------------------------
      # In the ORIGINAL panel, only show the data table and graph that were created based on the original data set
      fluidRow(
        div(
          id = "original_panel",
          column(width = 12,
                 conditionalPanel(
                   condition = "input.all_graphs > 0",
                   plotlyOutput("plot")
                 ),
                 br(),
                 introBox(data.step = 5,
                          data.intro = intro$text[5],
                          dataTableOutput("table"))
          ) # End of column()
        ) # End of div()
      ), # End of fluidRow() - Original panel
      
      # In the TRANSFORMED panel, only show the table and graph that were created based on the transformed data set
      fluidRow(
        div(
          id = "transformed_panel",
          column(width = 12,
                 plotlyOutput("plotTransf"),
                 br(),
                 dataTableOutput("transfTab")
          )
        )
      ) # End of fluidRow() - transformed panel
      
    ) # End of dashboardBody()
    
  ) # End of dashboardPage()

} # End of the ui


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
    
    # Wrap the values for each specified column.
    # But don't wrap the "Value" and "Change" column
    # This is so labels and keys on the axes and legends will not cluttered the plot when the words got too long
    if(!x %in% c("Value", "Change")){
      dat[, x] <- str_wrap(dat[, x], 20)
    }
    if(!y %in% c("Value", "Change")){
      dat[, y] <- str_wrap(dat[, y], 20)
    }
    if(!facet %in% c("Value", "Change")){
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

## CREATE SHINY APP-------------------------------------------------------------  
# Connect the ui (front-end) with the server (back-end)
shinyApp(ui = ui, server = server)
