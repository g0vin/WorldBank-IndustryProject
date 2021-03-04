## CONTENT FOR INTRODUCTION & HELP MENU----------------------------------------- 
# The text box content for each step in the introduction tutorial
intro <- data.frame(
  step = c(1:8),
  text = c("Choose from 5 different selection criteria to refine you selection.",
           "Here are some graph options you can choose to generate in the <b>ORIGINAL DATA</b> panel. Don't forget to click on <b>CREATE</b> if you want the visuals to show up!",
           "Here are the options to transform the values to percentage or other formats. The table/graph will appear in the <b>TRANSFORMED DATA</b> panel once a selection is made",
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
                    Here's our interactive dashboard where you can explore World Bank data based on models and do some exploratory analysis. We have pre-loaded some useful filters. You can choose one set of filters we have given you, load it back into the system, and play around with their visuals OR you can create your own combinations of filters. <br> <hr>
                    Click on the <span style='color: #0000A0;'><b>INTRODUCTION TOUR</b></span> button and it will give you a tour around the dashboard to get you familarized with the buttons and the options we offer to you. If you need more help after the introduction tour, please click on the <span style='color: #0000A0;'><b>?</b></span> icon. <br> <br>
                    <span style='color: #0000A0;'><b>We hope you enjoy the board!</b></span>"
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
