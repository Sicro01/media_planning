library(shiny)
library(rhandsontable)

########################################
# Step 1: Import Address Matching File #
########################################
step_1_import_data <- function() {
  #-------------#
  # Import Data #
  #-------------#
  
  # Find current dir and set working dir
  wd <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(wd)
  
  # Load the various files we need - put data in a folder called 'data' under the dir where the R script is
 plan_filename <- "./data/plan.csv"
  df_plan <<- as.data.frame(read.csv(plan_filename, header=TRUE))
}

step_1_import_data()

ui <- fluidPage(
  
  DT::dataTableOutput("tbl"),
  rHandsontableOutput("hot")
 
)

server <- function(input, output, session) {
  
  values <- reactiveValues()
  
  #-----------#
  # Plan Data #
  #-----------#
  # Detect changes to Plan data table and set reactive values according to user actions
  observe({
    
    if(!is.null(input$hot)) {
      print("1")
      values[["previous"]] <- isolate(values[["df_plan"]])
      df_plan = hot_to_r(input$hot)
    } else {
      if(is.null(values[["df_plan"]])){
        print("2")
        df_plan <- df_plan
      } else {
        print("3")
        df_plan <- values[["df_plan"]]
      }
    }
    values[["df_plan"]] <- df_plan
    
    output$tbl <- DT::renderDataTable({
      df_plan
    })
  })
  
  # Display data
  output$hot <- renderRHandsontable({
    df_plan <- values[["df_plan"]]
    View(df_plan)
    if (!is.null(df_plan)) {
      rhandsontable(df_plan, rowHeaders=NULL, stretchH = "all") %>%
        hot_col(df_plan, col = "Plan.Name", type = "autocomplete", source = df_plan$Plan.Name) %>%
        hot_col(df_plan, col = "Plan.Date", type = "date", source = df_plan$Plan.Date)
        # hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE) %>%
        # hot_table(highlightCol = TRUE, highlightRow = TRUE)
    }
  })
}

shinyApp(ui = ui, server = server)