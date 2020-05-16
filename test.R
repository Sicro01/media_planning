step_0_initialise <- function() {
  library(shiny)
  library(shinydashboard)
  library(timevis)
  library(ids)
  library(DT)
  library(dplyr)
  library(plyr)
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
  print(df_group$id)
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
        h2("Media Plan Timeline"),
        box(solidHeader = TRUE, status = "primary", width = 12,
          timevisOutput("media_plan_timeline")
        )
      ),
      fluidRow(width = 12,
        column(width = 6,
          box(title = "Add an Item", solidHeader = TRUE, status = "primary", width = 12,
            textInput("item_text", NULL, "New item content"),
            dateInput("item_start_date", "Start Date:"),
            dateInput("item_end_date", "End Date:"),
            selectInput("select_group", "Choose a group:", df_group$id, selected = NULL),
            actionButton("add_item_btn", "Add Item"),
          ),
        ),
        column(width = 6,
          box(title = "Remove an Item", solidHeader = TRUE, status = "primary", width = 8,
            uiOutput("remove_items"),
            # selectInput("remove_selected_items", "Choose an id:", df_data$Id, selected = NULL),
            actionButton("remove_item_btn", "Remove Item"),
          )
        ),
      ),
      fluidRow(width = 12,
        dataTableOutput("my_dt")  
      )
  )
)


media_plan_server <- function(input, output, session) {
  
  write.csv(data, file = "./Data/test_data.csv", row.names = FALSE)
  
  # RV <- reactiveValues(df5 = df_data)
  # print("Top RV$df5")
  # print(isolate(RV$df5))
  # print("")
  
  output$media_plan_timeline <- renderTimevis({
    # print("Timevis RV$df5")
    # print(RV$df5)
    # print("")
    config <- list(
      editable = TRUE
      ,align = 'center'
      ,orientation = 'top'
      ,snap = NULL
      ,margin = list(item =50, axis = 50)
    )
    timevis(data = df_data, groups = df_group, fit = TRUE, zoomFactor = 1, options = config)
  })
  
  output$my_dt <- renderDataTable({
    if (!is.null(nrow(input$media_plan_timeline_data))) {
      # print("Datatable RV$df5")
      # print(RV$df5)
      # print("")
      df <- as.data.frame(input$media_plan_timeline_data)
      datatable(df,
                rownames = FALSE,
                class = "cell-border stripe"
                )
    }
    # RV$df5 <- as.data.frame(input$media_plan_timeline_data)
  })
  
  # output$media_plan_dt <- renderDataTable({
  #   if (!is.null(nrow(input$media_plan_timeline_data))) {
  # 
  #     df <- as.data.frame(input$media_plan_timeline_data)
  # 
  #     datatable(df,
  #       class = 'cell-border stripe',
  #       rownames = FALSE,
  #       escape = FALSE,
  #       filter = "top",
  #       options = list(
  #         autowidth = TRUE,
  #         scrollX = TRUE,
  #         # dom = "t"
  #         sDom = '<"top">lrt<"bottom">ip'
  #       )
  #     )
  #     df_data <- as.data.frame(input$media_plan_timeline_data)
  #   }
  # })
  
  output$remove_items <- renderUI({
    selectInput("remove_selected_items", "Choose an id to delete",input$media_plan_timeline_ids, multiple = TRUE)
  })
  
  # output$selected <- renderText(
  #   paste(input$media_plan_timeline_selected, collapse = " ")
  # )
  # 
  # output$window <- renderText(
  #   paste(input$media_plan_timeline_window[1], "to", input$media_plan_timeline_window[2])
  # )
  
  # output$table <- renderTable({
  #   
  #   input$media_plan_timeline_data
  # })
  
  # df_data <- eventReactive(input$media_plan_timeline_data, {
  #   if (!is.null(nrow(input$media_plan_timeline_data))) {
  # 
  #     df <- as.data.frame(input$media_plan_timeline_data)
  #   }
  # })
  
  observeEvent(input$media_plan_timeline_data, {
    print("Here")
    write.csv(input$media_plan_timeline_data, file = "./Data/test_data.csv", row.names = FALSE)
  })
  
  observeEvent(input$add_item_btn,{
    # random_id(1)
    new_id <- as.integer(max(input$media_plan_timeline_ids)) + 1
    addItem("media_plan_timeline",
            data = list(id = new_id,
                        content = input$item_text,
                        start = input$item_start_date,
                        end = input$item_end_date,
                        group = input$select_group
            )
    )
    # RV$df5 <- as.data.frame(input$media_plan_timeline_data)
    # print("Add RV$df5")
    # print(RV$df5)
    # print("")
  })
  
  observeEvent(input$remove_item_btn, {
    removeItem("media_plan_timeline", input$remove_selected_items)
    # RV$df5 <- as.data.frame(input$media_plan_timeline_data)
    # print("Remove RV$df5")
    # print(RV$df5)
    # print("")
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