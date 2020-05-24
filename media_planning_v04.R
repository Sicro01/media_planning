#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Section - Initialise libraries ##################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
step_0_initialise <- function() {
  library(shiny)
  library(shinydashboard)
  library(timevis)
  library(DT)
  library(dplyr)
  library(rhandsontable)
  library(reactable)
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Section - Functions - (1) Load Data  ############################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
step_1_load_data <- function() {
  #--------#
  # Import #
  #--------#
  # Find current dir and set working dir
  wd <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(wd)
  print(wd)
  # Load the various files we need - put data in a folder called 'data' under the dir where the R script is
  data_filename <- "./Data/timevis_data.csv"
  
  df_timevis_data <<- tryCatch({
  # If file exists but is empty create an empty dataframe
    if (file.size(data_filename) < 10 || is.na(file.size(data_filename))) {
    
      df <- data.frame(
        id=numeric(0),
        content=character(0),
        start=as.Date(character()),
        ebd=as.Date(character()),
        group=character(0),
        stringsAsFactors = FALSE
      )
    } else {
    read.csv(data_filename, header=TRUE)
    }
  })
  
  # Read the group data  
  group_filename <- "./Data/group_data.csv"
  df_group <<- read.csv(group_filename, header=TRUE)
 
  # Read Channel data
  channel_filename <- "./Data/channel_data.csv"
  df_channel <<- read.csv(channel_filename, header=TRUE)
  
  # Read Country data
  country_filename <- "./Data/country_data.csv"
  df_country <<- read.csv(country_filename, header=TRUE)
  # df_country <<- data.frame(df_country, stringsAsFactors = FALSE)
  
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Section - Function - (2) Create Phase & Strategy df's ##############################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
step_2_create_phase_strategy_df <- function(df) {
  
  # Create Phase df
  df_phase <- df %>%
    filter(group == "Phase") %>%
    select("phase" = content)
  
  if (nrow(df_phase) > 0) {
    df_phase$id <- 1:nrow(df_phase)
    df_phase <- subset(df_phase, select=c(id, phase))
  }
  # Create Strategy df
  df_strategy <- df %>%
    filter(group == "Strategy") %>%
    select("strategy" = content)
  
  if (nrow(df_strategy) > 0) {
    df_strategy$id <- 1:nrow(df_strategy)
    df_strategy <- subset(df_strategy, select=c(id, strategy))
  }
  
  return_values <- list("df_strategy" = df_strategy, "df_phase" = df_phase)
  return(return_values)
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Section - Function - (3) Create Phase/Strategies ################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
step_3_create_phase_strategy_links <- function(df) {
 
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
  
  # If there are both at least one Strategy and one Phase then process them
  
  if (!(nrow(all_phases) == 0 || nrow(all_strategies) == 0)) {
  
    # Process each phase - find matching strategies, if any
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
     
      # Check it there's anything to match to
      if (nrow(all_matches) > 0) {
       
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
    }
  }
 
  # Return the phase/strategy links
  return(df_phase_strategy_links)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Section - Function - (4) Add Channel COls #######################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
step_4_add_channel_cols <- function(df) {
 
  # Initialise return vars
  df_phase_strategy_all_channels <- data.frame(phase=character(0), strategy=character(0), stringsAsFactors = FALSE)
  
  if (nrow(df) > 0) {

    # Get all the Strategy / Phase links
    df_phase_strategy_all_channels <-  as.data.frame(df) %>%
      select("Phase" = phase, "Strategy" = strategy)

    # Add channel cols to new list
    display_channel_cols <- as.character(df_channel$channel)

    # Wrap col headers
    for (col in display_channel_cols) {
      if (nchar(col) > 12) {
        col <- strwrap(col, width = 12)
      }
    }

    # Add all channels as columns
    df_phase_strategy_all_channels[, display_channel_cols] <- FALSE
  }
  return(df_phase_strategy_all_channels)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Section - Function - (5) React Channels #########################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
step_5_react_channels <- function(df) {
  
  t <- reactable(df, columns = list(
    phase = colDef(name = "Phase", defaultSortOrder = "asc")
    ,strategy = colDef(name = "Strategy", defaultSortOrder = "asc")
    ,channel = colDef(name = "Channel", defaultSortOrder = "asc")
    )
    ,filterable = TRUE
    ,searchable = TRUE
    ,highlight = TRUE
    ,bordered = TRUE
    ,compact = TRUE
    ,width = "auto"
    ,height = "auto"
    ,groupBy = c("phase","strategy")
  )
  return(t)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Section - Function - (6) Add Country Cols #######################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
step_6_add_country_cols <- function(df) {
  
  # Initialise return vars
  df_phase_strategy_all_countries <- data.frame(phase=character(0), strategy=character(0), channel=character(0), stringsAsFactors = FALSE)
  
  if (nrow(df) > 0) {
    
    # Get all the Strategy / Phase links
    df_phase_strategy_all_countries <-  as.data.frame(df) %>%
      select("Phase" = phase, "Strategy" = strategy, "Channel" = channel)
    
    # Add channel cols to new list
    display_country_cols <- as.character(df_country$country)
    
    # Wrap col headers
    for (col in display_country_cols) {
      if (nchar(col) > 12) {
        col <- strwrap(col, width = 12)
      }
    }
    
    # Add all channels as columns
    df_phase_strategy_all_countries[, display_country_cols] <- FALSE
  }
  return(df_phase_strategy_all_countries)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Section - Function - (7) React Countries ########################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
step_7_react_countries <- function(df) {
  
  t <- reactable(df, columns = list(
    phase = colDef(name = "Phase", defaultSortOrder = "asc")
    ,strategy = colDef(name = "Strategy", defaultSortOrder = "asc")
    ,channel = colDef(name = "Channel", defaultSortOrder = "asc")
    ,country = colDef(name = "Country", defaultSortOrder = "asc")
  )
  ,filterable = TRUE
  ,searchable = TRUE
  ,highlight = TRUE
  ,bordered = TRUE
  ,compact = TRUE
  ,width = "auto"
  ,height = "auto"
  ,groupBy = c("phase","strategy", "channel")
  )
  return(t)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Section - Initialize before running the UI ######################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
step_0_initialise()
step_1_load_data()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Section - User Interface ########################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
media_plan_ui <- dashboardPage(
### UI - Dashboard Header ###########################################
  dashboardHeader(title = "Media Plan Management"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Phases and Strategy",         tabName = "strategy_phase",           icon = icon("database",      "fa-1x")),
      menuItem("Channels and Countries",      tabName = "channels_countries",       icon = icon("globe",         "fa-1x"))
    )
  ),
    dashboardBody(
      tags$head(includeCSS("www/style.css")),
      fluidRow(
        tabItems(
### UI - Strategy / Phase Creation ##################################
          tabItem(tabName = "strategy_phase",
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
                      textInput("item_text", "Enter Phase / Strategy name:", "New item content")
                    ),
                    tags$td(style = "width: 10%",
                      align = "middle",
                      dateInput("item_start_date", "Enter start date:")
                    ),
                    tags$td(style = "width: 10%",
                      align = "middle",
                      dateInput("item_end_date", value = Sys.Date()+1, "Enter end date (if applicable):")
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
                    tags$td(style = "width: 10%; padding-top: 10px;", align = "middle",
                            verbatimTextOutput("error_message"),
                    ),
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
### UI - Add Countries and Channels #######################
          tabItem(tabName = "channels_countries",
                  tabsetPanel(
                    tabPanel("Add Channels",
                             box(solidHeader = TRUE, status = "primary", width = NULL, heheight = NULL,
                                title = "Select channels for each phase / strategy:",
                                rHandsontableOutput("hot_channels")),
                             box(solidHeader = TRUE, status = "primary", width = NULL, heheight = NULL,
                                title = "Selected channels for each phase / strategy:",
                                reactableOutput("react_channels"))
                    ),
                    tabPanel("Add Countries",
                             box(solidHeader = TRUE, status = "primary", width = NULL, heheight = NULL,
                                title = "Select countries for each phase / strategy /  channel:",
                                rHandsontableOutput("hot_countries")),
                             box(solidHeader = TRUE, status = "primary", width = NULL, heheight = NULL,
                                 title = "Selected countries for each phase / strategy / channel:",
                                 reactableOutput("react_countries"))
                    )
                  ) # end tabsetpanel
          ) # tab end
        )
      )
    )
)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Section - Server ################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
media_plan_server <- function(input, output, session) {
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Section - Reactives ################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Initialise reatives
  rv1 <- reactiveValues(df_timevis_data = df_timevis_data)
  rv2 <- reactiveValues(df_phase_strategy_links = NULL)
  rv3 <- reactiveValues(error_message = NULL)
  rv4 <- reactiveValues(df_phase = NULL)
  rv5 <- reactiveValues(df_strategy = NULL)
  rv6 <- reactiveValues(df_phase_strategy_all_channels = data.frame(phase=character(0), strategy=character(0),
                                                                    channel=character(0), stringsAsFactors = FALSE))
  rv7 <- reactiveValues(df_phase_strategy_sel_channels = data.frame(phase=character(0), strategy=character(0),
                                                                    channel=character(0), stringsAsFactors = FALSE))
  rv8 <- reactiveValues(df_phase_strategy_all_countries = data.frame(phase=character(0), strategy=character(0),
                                                                     channel=character(0), country=character(0), stringsAsFactors = FALSE))
  rv9 <- reactiveValues(df_phase_strategy_sel_countries = data.frame(phase=character(0), strategy=character(0),
                                                                     channel=character(0), country=character(0), stringsAsFactors = FALSE))
  
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Section -  Observations ############################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ### Look for change in data on timevis ####################################
  observeEvent(input$media_plan_timeline_data, {
    
    # Save current tinevis data as a reactive dataframe
    rv1$df_timevis_data <- as.data.frame(input$media_plan_timeline_data)
    
    # Check there are entries in the timevis
    if (!is.null(nrow(input$media_plan_timeline_data))) {
      # Check changes made to timevis timeline end dates are not less than or equal to start dates - if not print an error message  
      for (row in 1:nrow(rv1$df_timevis_data)) {
        this_row = rv1$df_timevis_data[row,]
        if (this_row$end <= this_row$start) {
          msg = paste("Error: End date must be greater than Start date - item: ", this_row$content)
          rv3$error_message = paste(" ", msg, sep = "\n")
        } else {
          rv3$error_message = ""
        }
      }
    
      #Save data to file
      write.csv( rv1$df_timevis_data, file = "./Data/timevis_data.csv", row.names = FALSE)
      
      # Create Phase and Strategy df's
      selected_vars <- step_2_create_phase_strategy_df(rv1$df_timevis_data)
      rv4$df_phase <- selected_vars$df_phase
      rv5$df_strategy <- selected_vars$df_strategy
      
      # Calculate phase / strategy relationships
      rv2$df_phase_strategy_links <- step_3_create_phase_strategy_links(rv1$df_timevis_data)
      rv6$df_phase_strategy_all_channels <- step_4_add_channel_cols(rv2$df_phase_strategy_links)
  }
  })
  
  ### Add a new strategy or phase ###################################
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
    if (input$item_end_date <= input$item_start_date) {
      msg = paste("Error: Error: End date must be greater than Start date - item: ", input$item_text)
      rv3$error_message = paste("", msg, sep = "\n")

    } else {
      rv3$error_message = ""
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
  
  ### Remove a strategy or phase ####################################
  observeEvent(input$remove_item_btn, {
    removeItem("media_plan_timeline", input$remove_selected_items)
  })
  
  ### Detect change - Phase / Strategy / Channel df #################
  observeEvent(input$hot_channels$changes$changes, {
    
    # Extract the changed data from the hot table
    row_number = input$hot_channels$changes$changes[[1]][[1]] + 1
    col_number = input$hot_channels$changes$changes[[1]][[2]] - 1
    col_name = as.character(df_channel$channel[col_number])
    new_value = input$hot_channels$changes$changes[[1]][[4]]
    phase_value = rv6$df_phase_strategy_all_channels[row_number, 1]
    strategy_value = rv6$df_phase_strategy_all_channels[row_number, 2]
    
    # Add / Delete the changed data to the phase / strategy / channel dataframe
    if (new_value) {
      # Add channel to phase / strategy link
      new_row <- list(phase = phase_value, strategy = strategy_value, channel = col_name)
      rv7$df_phase_strategy_sel_channels <- rbind(rv7$df_phase_strategy_sel_channels, new_row, stringsAsFactors = FALSE)
    } else {
      # Delete channel from phase / strategy link
      rv7$df_phase_strategy_sel_channels <- rv7$df_phase_strategy_sel_channels %>%
        filter(!(phase == phase_value & strategy == strategy_value & channel == col_name))
    }
    rv8$df_phase_strategy_all_countries <- step_6_add_country_cols(rv7$df_phase_strategy_sel_channels)
  })
  
  ### Detect change - Phase / Strategy / Channel / Country df #################
  observeEvent(input$hot_countries$changes$changes, {
    
    # Extract the changed data from the hot table
    row_number = input$hot_countries$changes$changes[[1]][[1]] + 1
    col_number = input$hot_countries$changes$changes[[1]][[2]] - 2
    col_name = as.character(df_country$country[col_number])
    new_value = input$hot_countries$changes$changes[[1]][[4]]
    phase_value = rv8$df_phase_strategy_all_countries[row_number, 1]
    strategy_value = rv8$df_phase_strategy_all_countries[row_number, 2]
    channel_value = rv8$df_phase_strategy_all_countries[row_number, 3]
    
    # Add / Delete the changed data to the phase / strategy / channel / country dataframe
    if (new_value) {
      # Add Country to phase / strategy / channel link
      new_row <- list(phase = phase_value, strategy = strategy_value, channel = channel_value, country = col_name)
      rv9$df_phase_strategy_sel_countries <- rbind(rv9$df_phase_strategy_sel_countries, new_row, stringsAsFactors = FALSE)
    } else {
      # Delete channel from phase / strategy link
      rv9$df_phase_strategy_sel_countries <- rv9$df_phase_strategy_sel_countries %>%
        filter(!(phase == phase_value & strategy == strategy_value & channel == channel_value & country == col_name))
    }
  })
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Section - Tab - Strategy and Phase Outputs ########################
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Display timevis
  output$media_plan_timeline <- renderTimevis({
    # Set up config list for timevis display
    config <- list(
      editable = TRUE
      ,align = 'center'
      ,orientation = 'top'
      ,snap = NULL
      ,margin = list(item =50, axis = 50)
    )
    timevis(data =  rv1$df_timevis_data, groups = df_group, fit = TRUE, zoomFactor = 1, options = config)
  })
  
  # Display datatable
  output$phase_strategy_dt <- renderDataTable({
    if (!is.null(nrow(input$media_plan_timeline_data))) {
        datatable( rv1$df_timevis_data,
                  rownames = FALSE,
                  class = "cell-border stripe",
                  options = list(
                  sDom = '<"top">lrt<"bottom">ip'
                  )
                ) %>%
                formatDate("start", "toDateString")
    }
    
  })
  
  output$remove_items <- renderUI({
    selectInput("remove_selected_items", "Choose an item (pick ID number) to delete:",input$media_plan_timeline_ids, multiple = TRUE)
  })
  
  output$error_message <- renderText({
    rv3$error_message
  })

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Section - Tab - Channels and Country Outputs ####################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ### Channel Outputs ###############################################
  output$hot_channels <- renderRHandsontable({
    
    # Check we have rows to display
    if (nrow(rv6$df_phase_strategy_all_channels) > 0) {
    # Display table
      rhandsontable(rv6$df_phase_strategy_all_channels
                    ,stretchH = 'all'
                    ) %>%
        hot_cols(colWidths = 50) %>%
        hot_col("Phase", readOnly = TRUE) %>%
        hot_col("Strategy",readOnly = TRUE )
    }
  })
  
  output$react_channels <- renderReactable({
    t <- step_5_react_channels(rv7$df_phase_strategy_sel_channels)
    print(t)
  })
  
  ### Country Outputs ###############################################
  output$hot_countries <- renderRHandsontable({
    
    # Check we have rows to display
    if (nrow(rv8$df_phase_strategy_all_countries)) {
      # Display table
      rhandsontable(rv8$df_phase_strategy_all_countries
                    ,stretchH = 'all'
      ) %>%
        hot_cols(colWidths = 50) %>%
        hot_col("Phase", readOnly = TRUE) %>%
        hot_col("Strategy",readOnly = TRUE ) %>%
        hot_col("Channel",readOnly = TRUE )
    }
   
  })
  
  output$react_countries <- renderReactable({
    t <- step_7_react_countries(rv9$df_phase_strategy_sel_countries)
    print(t)
  })

}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Section - Start App - ###########################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Start_App <- function() {
  shinyApp(ui = media_plan_ui,
           server = media_plan_server)  
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Section - Run the App ###########################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Start_App()