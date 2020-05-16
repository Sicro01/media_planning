step_0_initialise <- function() {
  library(shiny)
  library(shinydashboard)
  library(timevis)
  # library(ids)
  library(DT)
  # library(dplyr)
}

step_1_load_data <- function() {
  #--------#
  # Import #
  #--------#
  
  # Find current dir and set working dir
  wd <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(wd)
  
  # Load the various files we need - put data in a folder called 'data' under the dir where the R script is
  data_filename <- "./Data/test_data.csv"
  df_data <<- read.csv(data_filename, header=TRUE)
  group_filename <- "./Data/group_data.csv"
  df_group <<- read.csv(group_filename, header=TRUE)
}

#==================================#
# Initialize before running the UI #
#==================================#
step_0_initialise()
step_1_load_data()

##################
# User Interface #
##################
media_plan_ui <- dashboardPage(
  
  dashboardHeader(title = "Media Plan Management"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Media Plan") 
    )
  ),
    dashboardBody(
      fluidRow(
        box(title = "Media Plan Timeline", solidHeader = TRUE, status = "primary", width = 12,
          timevisOutput("media_plan_timeline")
        )
      ),
      fluidRow(width = 12,
          box(title = "Add an Item", solidHeader = TRUE, status = "primary", width = 6,
            textInput("item_text", NULL, "New item content"),
            dateInput("item_start_date", "Start Date:"),
            dateInput("item_end_date", "End Date:"),
            # uiOutput("check_item_end_date"),
            selectInput("select_group", "Choose a group:", df_group$id, selected = NULL),
            actionButton("add_item_btn", "Add Item"),
        ),
          box(title = "Remove an Item", solidHeader = TRUE, status = "primary", width = 6,
            uiOutput("remove_items"),
            actionButton("remove_item_btn", "Remove Item"),
          )
      ),
      fluidRow(width = 12,
        box(title = "Media Plan Data", solidHeader = TRUE, status = "primary", width = 12,
        dataTableOutput("my_dt")
        )
      )
  )
)

##################
# Server        #
##################
media_plan_server <- function(input, output, session) {
  
  # Initialise reatives
  RV <- reactiveValues()
  RV$df_data = df_data
  
# Display timevis
  output$media_plan_timeline <- renderTimevis({
    config <- list(
      editable = TRUE
      ,align = 'center'
      ,orientation = 'top'
      ,snap = NULL
      ,margin = list(item =50, axis = 50)
    )
    timevis(data = df_data, groups = df_group, fit = TRUE, zoomFactor = 1, options = config)
  })
  
  # Display datatable
  output$my_dt <- renderDataTable({
    if (!is.null(nrow(input$media_plan_timeline_data))) {
      # data_tb <- as.data.frame(input$media_plan_timeline_data)
      datatable(RV$df_data,
                rownames = FALSE,
                class = "cell-border stripe",
                options = list(
                sDom = '<"top">lrt<"bottom">ip'
                )
              )
    }
  })
  
  output$check_item_end_date <- renderUI({
    dateInput("item_end_date", "End Date:")
  })
  
  output$remove_items <- renderUI({
    selectInput("remove_selected_items", "Choose an id to delete",input$media_plan_timeline_ids, multiple = TRUE)
  })
  
  #------------------------------#
  # Observe                      #
  #------------------------------#
  observeEvent(input$media_plan_timeline_data, {
    RV$df_data <- input$media_plan_timeline_data
    write.csv(RV$df_data, file = "./Data/test_data.csv", row.names = FALSE)
  })
  
  observeEvent(input$add_item_btn,{
   # Create next Id
    all_ids <- as.integer(input$media_plan_timeline_ids)
    if (length(all_ids) > 0) {
      new_id <- max(all_ids) + 1
    } else {
      new_id = 1
    }
    
    # Make sure dates are entered
    req(input$item_start_date)
    req(input$item_end_date)
    
    # If start date equals end date then leave end date out - it's not a ranged item
    if (input$item_start_date == input$item_end_date) {
      addItem("media_plan_timeline",
        data = list(
          id = new_id,
          content = input$item_text,
          start = input$item_start_date,
          group = input$select_group
        )
      )
    } else {
      addItem("media_plan_timeline",
        data = list(
          id = new_id,
          content = input$item_text,
          start = input$item_start_date,
          end = input$item_end_date,
          group = input$select_group
              )
      )
    }
  })
  
  observeEvent(input$remove_item_btn, {
    removeItem("media_plan_timeline", input$remove_selected_items)
  })
}

Start_App <- function() {
  shinyApp(ui = media_plan_ui,
           server = media_plan_server)  
}

###############
# Run the App #
###############
Start_App()