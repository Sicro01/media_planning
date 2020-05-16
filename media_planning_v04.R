step_0_initialise <- function() {
  library(shiny)
  library(shinydashboard)
  library(timevis)
  library(DT)
  library(dplyr)
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Section - Functions - Load Data  #############################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
step_1_load_data <- function() {
  #--------#
  # Import #
  #--------#
  
  # Find current dir and set working dir
  wd <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(wd)
  
  # Load the various files we need - put data in a folder called 'data' under the dir where the R script is
  data_filename <- "./Data/timevis_data.csv"
  
  # If all rows were deleted or starting from scratch then cerate empty dataframe else read the file
  df_timevis_data <<- tryCatch({
    
    if (file.size(data_filename) < 10) {
      df <- data.frame(
        id=numeric(0),
        content=character(0),
        start=date,
        group=character(0),
        stringsAsFactors = FALSE
      )
      df
    } else {
    read.csv(data_filename, header=TRUE)
    }
  })
  
  # Read the group data  
  group_filename <- "./Data/group_data.csv"
  df_group <<- read.csv(group_filename, header=TRUE)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Section - Function Create Phase/Strategies #########################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
step_2_create_phase_strategy_links <- function(df) {
  
  # Create empty df to hold all phasr strategy links
  df_phase_strategy_links <- data.frame(
    id=numeric(0),
    phase=character(0),
    strategy=character(0),
    stringsAsFactors=FALSE
  )
  
  # Split off phases and stratregies into seperate df's
  all_phases <- df %>%
    filter(group == "Phase")
  
  all_strategies <- df %>%
    filter(group == "Strategy")
  
  # Process each phase - find matching strategies
  for (phase in 1:nrow(all_phases)) {
    
    # Assign this phase to a variable
    this_phase <- all_phases[phase,]
    
    # Find all strategies with dates within the phase dates
    strategy_start_date_within <- filter(all_strategies, between(as.Date(start), as.Date(this_phase$start), as.Date(this_phase$end)))
    strategy_end_date_within <- filter(all_strategies, between(as.Date(end), as.Date(this_phase$start), as.Date(this_phase$end)))
    phase_dates_within <- filter(all_strategies, as.Date(start) <= as.Date(this_phase$start) & as.Date(end) >= as.Date(this_phase$end) ) 
    
    # Combine the matches
    all_matches <- rbind(strategy_start_date_within, strategy_end_date_within, phase_dates_within)
    all_matches <- distinct(all_matches, content, .keep_all = TRUE)
    
    # Create the Phase/Strategy match output for this phase
    for (match in 1:nrow(all_matches)) {
  
      # Get next Id
      all_ids <- as.integer(df_phase_strategy_links$id)
      if (length(all_ids) > 0) {
        new_id <- max(all_ids) + 1
      } else {
        new_id = 1
      }
     
      this_match <- all_matches[match,]
      df_phase_strategy_links[new_id,] <- c(new_id, this_phase$content, this_match$content)
    }
  }
  # Return the phase/strategy links
  return(df_phase_strategy_links)
}

#==================================#
# Initialize before running the UI #
#==================================#
step_0_initialise()
step_1_load_data()

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Section - User Interface ###########################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
media_plan_ui <- dashboardPage(
  
  dashboardHeader(title = "Media Plan Management"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Phases and Strategy",      tabName = "strategy_phase",      icon = icon("database",      "fa-1x")),
      menuItem("Countries",      tabName = "countries",           icon = icon("database",      "fa-1x"))
    )
  ),
    dashboardBody(
      fluidRow(
        tabItems(
          tabItem(tabName = "strategy_phase",
            tags$head(
              tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
            ),
            fluidRow(
              box(title = "Media Plan Timeline", solidHeader = TRUE, status = "primary", width = 12,
                timevisOutput("media_plan_timeline")
              )
            ),
            fluidRow(width = 12,
              tags$div(style = "width: 100%; padding-left:15px; padding-right: 15px;",
                h4("Update Media Plan", style = "font-weight: bold;"),
                tags$table(style = "border-radius: 3px; border:solid 3px #3c8dbc;",
                  tags$tr(style = "border-bottom:solid 1px #3c8dbc;",
                    tags$td(style = "width: 20%",
                      align = "middle",
                      textInput("item_text", "Enter content:", "New item content")
                    ),
                    tags$td(style = "width: 10%",
                      align = "middle",
                      dateInput("item_start_date", "Enter start date:")
                    ),
                    tags$td(style = "width: 10%",
                      align = "middle",
                      dateInput("item_end_date", "Enter end date (if applicable):")
                    ),              
                    tags$td(style = "width: 10%",
                      align = "middle",
                      selectInput("select_group", "Select a group:", df_group$id, selected = NULL)
                    ),
                    tags$td(style = "width: 10%;",
                      align = "middle",
                      actionButton("add_item_btn", "Add Item", class = "btn-primary", style = "color: white; font-weight: bold;")
                    )
                  ), # tbrow end
                  tags$tr(
                    tags$td(style = "width: 20%; padding-top: 10px;"),
                    tags$td(style = "width: 10%; padding-top: 10px;"),
                    tags$td(style = "width: 10%; padding-top: 10px;"),
                    tags$td(style = "width: 10%; padding-top: 10px;", align = "middle",
                            uiOutput("remove_items"),
                    ),
                    tags$td(style = "width: 10%;",
                            align = "middle",
                            actionButton("remove_item_btn", "Remove Item", class = "btn-primary", style = "color: white; font-weight: bold;"),
                    )
                  ) # tbrow end
                ) # table end
              ) # div end
            ), # row end
            br(),br(),
            fluidRow(width = 12,
              box(title = "Media Plan Data", solidHeader = TRUE, status = "primary", width = 12,
              dataTableOutput("phase_strategy_dt")
              )
            ) # row end
          ), #tab end
          tabItem(tabName = "countries",
          ) # tab end
        )
      )
    )
)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Section - Server ###################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
media_plan_server <- function(input, output, session) {
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Section - Reactives ################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Initialise reatives
<<<<<<< HEAD
  RV <- reactiveValues(df_timevis_data = df_timevis_data)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Section -  Observations ############################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Change in data
=======
  RV <- reactiveValues()
  RV$df_timevis_data = df_timevis_data
  
  # output$phase_strategy_dt <- renderDataTable({
  #   datatable(step_2_phase_strategy_alignment())
  # })

# Outputs ---------------------------------------
# Display timevis
  output$media_plan_timeline <- renderTimevis({
    config <- list(
      editable = TRUE
      ,align = 'center'
      ,orientation = 'top'
      ,snap = NULL
      ,margin = list(item =50, axis = 50)
    )
    timevis(data = df_timevis_data, groups = df_group, fit = TRUE, zoomFactor = 1, options = config)
  })
  
  # Display datatable
  output$phase_strategy_dt <- renderDataTable({
    if (!is.null(nrow(input$media_plan_timeline_data))) {
      if ("end" %in% colnames(input$media_plan_timeline_data)) {
        
        str(RV$df_timevis_data)
        print(as.POSIXct(input$media_plan_timeline_data$start))
        print(as.Date(input$media_plan_timeline_data$start, format = "%Y-%m"))
        
        datatable(RV$df_timevis_data,
                  rownames = FALSE,
                  class = "cell-border stripe",
                  options = list(
                  sDom = '<"top">lrt<"bottom">ip'
                  )
                )
      } else {
        datatable(RV$df_timevis_data,
                  rownames = FALSE,
                  class = "cell-border stripe",
                  options = list(
                    sDom = '<"top">lrt<"bottom">ip'
                  )
        ) %>%
          formatDate("start", "toDateString")
      }
    }
  })
  
  output$check_item_end_date <- renderUI({
    dateInput("item_end_date", "End Date:")
  })
  
  output$remove_items <- renderUI({
    selectInput("remove_selected_items", "Choose an item (pick ID number) to delete:",input$media_plan_timeline_ids, multiple = TRUE)
  })
  
# Observations ----------------------------------
>>>>>>> 6e22921ba63a4759abfd84a07fe7c4c985770c48
  observeEvent(input$media_plan_timeline_data, {
    print("1")
    RV$df_timevis_data <- input$media_plan_timeline_data
    write.csv(RV$df_timevis_data, file = "./Data/timevis_data.csv", row.names = FALSE)
  })
  
  # Add a new strategy or phase
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
  
  # Remove a strategy or phase
  observeEvent(input$remove_item_btn, {
    removeItem("media_plan_timeline", input$remove_selected_items)
  })
  
  observeEvent(input$media_plan_timeline_data, {
    # df <- sp_2_create_phase_strategy_links(as.data.frame(RV$df_timevis_data))
  })
  
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Section - Tab - Strategy and Phase Outputs #########################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Display timevis
  output$media_plan_timeline <- renderTimevis({
    config <- list(
      editable = TRUE
      ,align = 'center'
      ,orientation = 'top'
      ,snap = NULL
      ,margin = list(item =50, axis = 50)
    )
    timevis(data = RV$df_timevis_data, groups = df_group, fit = TRUE, zoomFactor = 1, options = config)
  })
  
  # Display datatable
  output$phase_strategy_dt <- renderDataTable({
    
    # If table not empty display rows
    if (!is.null(nrow(input$media_plan_timeline_data))) {
        
        datatable(RV$df_timevis_data,
                  rownames = FALSE,
                  class = "cell-border stripe",
                  options = list(
                  sDom = '<"top">lrt<"bottom">ip'
                  )
                ) %>%
                formatDate("start", "toDateString")
    }
    
  })
  
  # output$check_item_end_date <- renderUI({
  #   dateInput("item_end_date", "End Date:")
  # })
  
  output$remove_items <- renderUI({
    selectInput("remove_selected_items", "Choose an item (pick ID number) to delete:",input$media_plan_timeline_ids, multiple = TRUE)
  })
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Section - Start App - ##############################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Start_App <- function() {
  shinyApp(ui = media_plan_ui,
           server = media_plan_server)  
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Section - Run the App ##############################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Start_App()