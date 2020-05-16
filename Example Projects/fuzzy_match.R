#####################################################################
#####################################################################
##                                                                 ##
##  +-----------------------------------------------------------+  ##
##  | Walmart US - AR/AP Contact Details Fuzzy Match            |  ##
##  +-----------------------------------------------------------+  ##
##                                                                 ##
##  Copyright (C) 2020 by Genpact                                  ##
##                                                                 ##
##  Contact Simon Cropper (simon.cropper@genpact.com) for more     ##
##  information or a demonstration.                                ##
##                                                                 ##
##  Algorithm to determine the closest matching contact between    ##
##  the AR and AP systems using "bag of words" and vectorization   ##
##  and acosine similarity techniques.                             ##
##                                                                 ##
##  UNDERLYING FUNCTIONS                                           ##
##  --------------------                                           ##
##  Run this code before launching UI (below).                     ##
##                                                                 ##
#####################################################################
#####################################################################
# options(shiny.trace = TRUE)

#################################
# Step 0: Initialized Libraries #
#################################
step_0_initialize <- function() {
  library(shiny)
  library(shinydashboard)
  library(dplyr)
  library(reticulate)
  library(DT)
}

########################################
# Step 1: Import Address Matching File #
########################################
Step_1_Import_Item_File <- function() {
  #--------#
  # Import #
  #--------#
 
  # Find current dir and set working dir
  wd <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(wd)
  # Load the various files we need - put data in a folder called 'data' under the dir where the R script is
  # combined AR and AP input data
  combined_input_filename = "./data/combined.csv"
  combined_input_data <<- read.csv(combined_input_filename, header=TRUE)
  
  #----------------------#
  # Split into AR and AP #
  #----------------------#
  AR_contact_data <<- combined_input_data %>%
    filter(SOURCE == "AR") %>%
    select(-SOURCE, -ADDRESS_NOSPACES)
  
  AP_contact_data <<- combined_input_data %>%
    filter(SOURCE == "AP") %>%
    select(-SOURCE, -ADDRESS_NOSPACES)
  
  # match results
  match_results_filename = "./data/match_results.csv"
  match_results_data <<- read.csv(match_results_filename, header=TRUE)
  
}

# End of functions #

###########################################################################################################################
###########################################################################################################################
###########################################################################################################################


#==================================#
# Initialize before running the UI #
#==================================#
step_0_initialize()
Step_1_Import_Item_File()

#############################################################
#                                                           #
#  Walmart AR/AP Contact Details Fuzzy Match Demonstration  #
#                                                           #
#  USER INTERFACE                                           #
#  --------------                                           #
#  Run the Companion Functions before launching UI          #
#                                                           #
#  Copyright by Genpact (C) 2020.                           #
#  For more information contact Simon Cropper, Data Science #
#  Team                                                     #
#                                                           #
#############################################################


##################
# User Interface #
##################

Address_Match_UI <- dashboardPage(
  
  dashboardHeader(title = "AR/AP Contact Match Identification"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("View AR Data",                  tabName = "View_AR_Data",            icon = icon("database",      "fa-1x")),
      menuItem("View AP Data",                  tabName = "View_match_results_data",         icon = icon("database",      "fa-1x")),
      menuItem("View Address Matches",          tabName = "View_Address_Matches",    icon = icon("equals",        "fa-1x"))
    )
  ),
  dashboardBody(
      fluidRow(width = 12, align = "center",
        tags$table(style = "width: 100%",
            tags$tr(
              tags$head(tags$script(src = "message-handler.js")),
                tags$td(style = "width: 20%",
                    align = "middle",
                    sliderInput("threshold_distance", "Distance Threshold:", 0,1,0.7)
                ),
                tags$td(style = "width: 20%",
                    align = "middle",
                    sliderInput("top_n_matches", "Top n matches:", 1,10,5)
                ),
                tags$td(style = "width: 20%",
                    align = "middle",
                    sliderInput("ngrams_char_split", "Ngram Character Split:", 2,5,2)
                ),
              tags$td(style = "width: 15%",
                      align = "middle",
                      actionButton("run_model_button", "Run Model", icon = icon("play-circle"))
                ),
              tags$td(style = "width: 25%",
                      align = "middle",
                      verbatimTextOutput("python_script_response")
              )
            )
          )
      ),
      
      #---------------------#
      # Selections and Tabs #
      #---------------------#
      tabItems(
        ##################
        # View AR Data   #
        ##################
        tabItem(tabName = "View_AR_Data",
                h2("AR Data"),
                fluidRow(
                  infoBoxOutput("number_of_AR_contacts", width = 2)            
                ),
                fluidRow(
                  box(solidHeader = TRUE, status = "primary",   width = 12,
                      DT::dataTableOutput("AR_contact_data")))
                )
        )
  )
)
#------------------------------------#
# Specifiy the python script to run  #
#------------------------------------#
source_python("Address Match v2.py")

Address_Match_Server <- function(input, output, session) {
  
  observe({
    showModal(modalDialog("Please click on 'Run Model' button", footer=NULL, easyClose = TRUE))
  })
  
  #------------------------------------#
  # Run the python script              #
  #------------------------------------#
  model_update_message <- eventReactive(input$run_model_button, {
    py_script_response = main(input$threshold_distance, input$top_n_matches, input$ngrams_char_split)
  })
  
  #------------------------------------------#
  # Output result message from python script #
  #------------------------------------------#
  output$python_script_response <- renderText({
    model_update_message()
  })
  
  output$number_of_AR_contacts <- renderValueBox({
    StatusBox(x     = Comma_Format(nrow(AR_contact_data)),
              Title = "AR Contacts",
              Icon  = "list-ol",
              Color = "blue") })
  
  #------------------#
  # Output AR Data   #
  #------------------#
  output$AR_contact_data <- renderDataTable({
    
    datatable(AR_contact_data,
              class = 'cell-border stripe',
              rownames = TRUE,
              escape = FALSE,
              options = list(scrollX = TRUE, sDom = '<"top">lrt<"bottom">ip') )
  })  
  
}

Start_App <- function() {
  shinyApp(ui = Address_Match_UI,
           server = Address_Match_Server)  
}

###############
# Run the App #
###############
Start_App()