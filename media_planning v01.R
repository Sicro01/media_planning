#####################################################################
#####################################################################
##                                                                 ##
##  +-----------------------------------------------------------+  ##
##  |Company XYZ                                                |  ##
##  +-----------------------------------------------------------+  ##
##                                                                 ##
##  Copyright (C) 2020 by Simon and Thomas                         ##
##                                                                 ##
##  Contact Simon Cropper (simon.p.cropper@gmail.com) for more     ##
##  information or a demonstration.                                ##
##                                                                 ##
##  Algorithm to build out a digital media plan                    ##
##  accepting a series of paramters whic inform the plan structure ##
##  and content                                                    ##
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
  library(DT)
  library(rhandsontable)
}

########################################
# Step 1: Import Address Matching File #
########################################
step_1_import_data <- function() {
  #--------#
  # Import #
  #--------#
  
  # Find current dir and set working dir
  wd <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(wd)
  
  # Load the various files we need - put data in a folder called 'data' under the dir where the R script is
  ad_name_filename <- "./data/ad_name.csv"
  df_ad_name <<- as.data.frame(read.csv(ad_name_filename, header=TRUE))
  
  country_filename <- "./data/country.csv"
  df_country <<- read.csv(country_filename, header=TRUE)
  
  plan_filename <- "./data/plan.csv"
  df_plan <<- read.csv(plan_filename, header=TRUE)
  
  strategy_filename <- "./data/strategy.csv"
  df_strategy <<- read.csv(strategy_filename, header=TRUE)
  
  phase_filename <- "./data/phase.csv"
  df_phase <<- read.csv(phase_filename, header=TRUE)
  
  strategy_phase_filename <- "./data/strategy_phase.csv"
  df_strategy_phase <<- read.csv(strategy_phase_filename, header=TRUE)
  
}
step_2_init_df <- function() {
  #---------------------------------------------#
  # Initialise df's to hold media planning data #
  #---------------------------------------------#
  # df_strategy <<- data.frame(strategy=integer(),
  #                            strategy_name=character())
  # df_phase <<- data.frame(phase=integer(),
  #                         phase_name=character())
  # df_strategy_phase <<- data.frame(phase=integer(),
  #                                  phase_name=character(),
  #                                  phase_weighting=integer(),
  #                                  strategy=integer(),
  #                                  strategy_name=character()
  # 
  # )
}

###############################
# Functions for recurring use #
###############################


###############################
# End of Functions            #
###############################



##########################################################
#                                                        #
#  Media Planning Demonstration                          #
#                                                        #
#  USER INTERFACE                                        #
#  --------------                                        #
#  Run the Companion Functions before launching UI       #
#                                                        #
##########################################################


Media_Plan_UI <- dashboardPage(
  
  dashboardHeader(title = "Digital Media Planning Tool"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Media Plan",          tabName = "Media_Plan",         icon = icon("database",      "fa-1x"))
    )
  ),
  dashboardBody(
    # fluidrow(),
    #---------------------#
    # Selections and Tabs #
    #---------------------#
    tabItems(
            tabItem(tabName = "Media_Plan",
                    fluidRow(
                      column(width = 12,
                            helpText("Non Editable table"),
                            tableOutput("notmaltable"),
                            )
                  ),
                  fluidRow(
                    column(width = 12,
                           helpText("Editable table"),
                           rHandsontableOutput("hottable"),
                           br(),
                           actionButton("save_button", "Save")
                    )
                  )
           )
    )
  )
)

#==================================#
# Initialize before running the UI #
#==================================#
step_0_initialize()
step_1_import_data()
# step_2_init_df()

###############################
# Server and Report Generator #
###############################
Media_Plan_Server <- function(input, output, session) {
  
  previous <- df_plan
  View(previous)
  Trigger_orders <- reactive({
    # View(previous)
    if(is.null(input$hottable1)) {return(previous())}
    else if(!identical(previous(), input$hottable)){
      # hot.to.df function will convert your updated table into the dataframe
      as.data.frame(hot_to_r(input$hottable))
    }
  })

  # output$table1 <- renderTable({
  #   df_plan
  # })
  output$hottable <- renderRHandsontable({
    View(Trigger_orders)
    rhandsontable(Trigger_orders)
  })
  # output$hottable <- renderRHandsontable({
  #   big = LETTERS[1:10]
  #   # View(Trigger_orders)
  #   rhandsontable(Trigger_orders,
  #                 rowHeaders = NULL,
  #                 fillHandle = list(direction='vertical', autoInsertRow=TRUE)) %>%
  #       hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) %>%
  #       hot_col(col = "Plan.Number", type = "dropdown", source = big) %>%
  #       hot_rows(rowHeights = 20, fixedRowsTop = 0) %>%
  #       hot_cols(columnSorting = TRUE)
  #   
  #   # output$table1 = DT::renderDataTable(Trigger_orders())
  #   
  #   # hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
  #     # hot_row(c(2,3:5), readOnly = TRUE)
  # })
  
  observeEvent(input$save_button,
               write.csv(hot_to_r(input$table), file = "MyData.csv", row.names=FALSE))
  # output$table1 <- renderTable({
  #   df_plan
  # })
  
}

Start_App <- function() {
  shinyApp(ui = Media_Plan_UI,
           server = Media_Plan_Server)  
}


###############
# Run the App #
###############
Start_App()