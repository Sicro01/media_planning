library(shiny)
library(shinydashboard)
library(timevis)

step_1_import_data <- function() {
  #--------#
  # Import #
  #--------#
  
  # Find current dir and set working dir
  wd <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(wd)
  
  # Load the various files we need - put data in a folder called 'data' under the dir where the R script is
  data_filename <- "./Data/data.csv"
  df_data <<- as.data.frame(read.csv(data_filename, header=TRUE))
  View(df_data)
}

digital_media_planning_UI <- fluidPage(
  timevisOutput("timelineCustom")
  
)
step_1_import_data()


digital_media_planning_server <- function(input, output, session){
  
  output$timelineCustom <- renderTimevis({
    timevis(
      df_data,
      options = list(editable = TRUE, multiselect = TRUE, align = "center")
      )
  })
  
}

Start_App <- function() {
  shinyApp(ui = digital_media_planning_UI,
           server = digital_media_planning_server)  
}


###############
# Run the App #
###############
Start_App()