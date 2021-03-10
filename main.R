## Dependencies-----------------------------------------------------------------
# install.packages("shiny")
# install.packages("shinyWidgets")
# install.packages("shinydashboard")
# install.packages("shinyalert")
# install.packages("shinyBS")
# install.packages("shinyjs")
# install.packages("rintrojs")
# install.packages("readr")
# install.packages("readxl")
# install.packages("plyr")
# install.packages("tidyverse")
# install.packages("DT")
# install.packages("rlang")
# install.packages("plotly")
# install.packages("ggplot2")

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
dict <- read_excel("C:/Users/Owner/Downloads/results_real_names.xlsm",
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

## READ R CODE FROM OTHER FILES-------------------------------------------------
# Make sure we are in the right directory to run the other code below. 
# The path should be where ui.R, server.R, and other related R files should be located.
setwd("C:/Users/Owner/Documents/WorldBank-IndustryProject")

# References the Intro_Help_Pages.R file where the code for creating the intro & help pages are located
source("Intro_Help_Pages.R", local = T)

# References the ui.R file where the ui(front-end) code is located
source("ui.R", local = T)

# References the server.R file where the server(back-end) code is located
source("server.R", local = T)


## CREATE SHINY APP-------------------------------------------------------------  
# Connect the ui (front-end) with the server (back-end)
shinyApp(ui = ui, server = server)
