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
            selectizeInput(inputId = "plotType", label = "Plot Options", 
                           choices = c("bar", "stack", "histogram", "line", 
                                       "area", "pie", "bubble", "scatter", "boxplot")),
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
                 introBox(data.step = 5,
                          data.intro = intro$text[5],
                          dataTableOutput("table")),
                 br(),
                 plotlyOutput("plot")
          ) # End of column()
        ) # End of div()
      ), # End of fluidRow() - Original panel
      
      # In the TRANSFORMED panel, only show the table and graph that were created based on the transformed data set
      fluidRow(
        div(
          id = "transformed_panel",
          column(width = 12,
                 dataTableOutput("transfTab"),
                 plotlyOutput("plotTransf")
          )
        )
      ) # End of fluidRow() - transformed panel
      
    ) # End of dashboardBody()
    
  ) # End of dashboardPage()
  
} # End of the ui
