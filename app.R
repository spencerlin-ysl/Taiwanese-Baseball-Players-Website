# TODO: fix the color of the font on the side bar to #bcbcbc and when hover #fffff
# TODO: make the background color of the plot changeable by the mode of the site



#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/

library(shiny)
library(DT)
library(bs4Dash)
library(tidyverse)
library(rvest)
library(plotly)
library(ggiraph)
source("mlb_pbp_new.R")
source("get_player_transactions.R")
source("get_player_team_affiliate.R")
source("get_game_log.R")
source("get_pbp_data.R")

ui <- dashboardPage(
  
  # Header
  help = NULL,
  dashboardHeader(
    title = dashboardBrand(
      title = "Taiwanese Baseball Players"
    ), 
    status = "white"
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 300,
    minified = FALSE,
    sidebarMenu(
      menuItem("All Players", tabName = "all_players"),
      menuItem("MLB", tabName = "mlb"),
      menuItem("MiLB", tabName = "milb"), 
      menuItem("AAA", tabName = "aaa"), 
      menuItem("AA", tabName = "aa"),
      menuItem("A+", tabName = "aplus"),
      menuItem("A", tabName = "a"),
      menuItem("Rk", tabName = "r"), 
      menuItem("Others", tabName = "others")
    ),
    
    # Search box
    br(),
    div(
      style = "padding: 15px;",
      textInput("search", "Search Players", placeholder = "Enter name...")
    ),
    
    # Filter by position
    div(
      style = "padding: 15px;",
      selectInput("position_filter", "Filter by Position",
                  choices = c("All", "Pitcher", "Catcher", "Infielder", "Outfield"))
    )
  ),
  
  # Body
  dashboardBody(
    tags$head(
      tags$style(HTML("
        body, .content-wrapper, .main-sidebar, .sidebar-menu, .main-header,
        .navbar, .navbar-static-top, .logo, .modal-title {
          font-family: 'Book Antiqua', Palatino, serif !important;
        }
        
        .modal-content, .modal-content * {
          font-family: 'Book Antiqua', Palatino, serif !important;
        }
        
        .modal-dialog .fa-spinner {
          font-family: 'FontAwesome' !important;
        }
        
        .modal-dialog .fa-exclamation {
          font-family: 'FontAwesome' !important;
        }

        .player-button {
          width: 100%;
          margin: 8px 0;
          padding: 15px;
          font-size: 16px;
          font-weight: 500;
          text-align: left;
          font-family: 'Book Antiqua', Palatino, serif;
          background-color: #ffffff;
          border: 1px solid #ddd;
          border-radius: 5px;
          transition: all 0.3s ease;
          cursor: pointer;
        }
        
        .player-button:hover {
          background-color: #f5f5f5;
          border-color: #6e77db;
          transform: translateX(5px);
          box-shadow: 0 2px 5px rgba(0,0,0,0.1);
        }
        
        .player-grid {
          display: grid;
          grid-template-columns: repeat(auto-fill, minmax(250px, 1fr));
          gap: 10px;
          padding: 15px;
        }
        
        .section-header {
          font-size: 24px;
          font-weight: 600;
          font-family: 'Book Antiqua', Palatino, serif;
          margin-bottom: 20px;
          padding-bottom: 10px;
          border-bottom: 2px solid #6e77db;
        }
        
        .level-section {
          margin-bottom: 30px;
        }
        
        .level-title {
          font-size: 20px;
          font-weight: 600;
          # color: #555;
          margin: 20px 0 10px 15px;
          padding: 8px 12px;
          # background-color: #ffffff;
          border-left: 4px solid #6e77db;
        }
        
        .main-sidebar {
          width: 250px;
          background: #0e1012;
          font-color: #bcbcbc; 
        }
        
        .sidebar-menu > li {
          border-bottom: 1px solid #bcbcbc;
          margin-top: 3px;
        }
        
        .league-badge {
          display: inline-block;
          padding: 3px 8px;
          margin-left: 10px;
          font-size: 12px;
          border-radius: 3px;
          background-color: #6e77db;
          color: white;
        }
        
        .position-badge {
          display: inline-block;
          padding: 3px 8px;
          margin-left: 5px;
          font-size: 11px;
          border-radius: 3px;
          background-color: #e8e8e8;
          color: #666;
        }
        
      .modal-header .modal-title {
        font-size: 28px;
        font-weight: 600;
      }
        
        /* Transaction table styling */
      body.dark-mode .dataTables_scrollBody tr {
        color: #e0e0e0 ;
      }  
            
      body.dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button {
        background-color: #2b2b2b !important;
        color: #e0e0e0 !important;
        border: 1px solid #555 !important;
      }
      
      body.dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
        background-color: #3a3a3a !important;
        color: #fff !important;
      }
      
      body.dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button.current {
        background-color: #8a8a8a !important;
        color: #fff !important;
        border: 1px solid #f5f5f5 !important;
      }
        
      .transactions-container {
        margin: 20px 0;
      }
      
      .transactions-table {
        width: 100%;
        border-collapse: collapse;
        # background-color: white;
      }
      
      .transactions-table thead {
        background-color: #6e77db;
        text-align: center;
        color: white;
      }
      
      .transactions-table h2 {
        text-align: center;
      }
      
      .transactions-table th {
        padding: 12px;
        text-align: center;
        font-weight: 600;
        font-size: 18px;
      }
      
      .transactions-table td {
        padding: 12px;
        border-bottom: 1px solid #e0e0e0;
        vertical-align: middle;
        font-size: 16px;
      }
      
      .transactions-table tbody tr:hover {
        background-color: #f5f5f5;
      }
      
      body.dark-mode .transactions-table tbody tr:hover {
        background-color: #1a1a1a;
      }
      
      .transactions-table .team-logo {
        display: inline-block;
        width: 30px;
        height: 30px;
        text-align: center;
      }
      
      .transactions-table .team-logo img {
        max-width: 30px;
        max-height: 30px;
        width: auto;
        height: auto;
        vertical-align: middle;
      }
      
      /* Make the first column (team logo) narrower */
      .transactions-table td:first-child {
        width: 60px;
        text-align: center;
      }
      
      /* Date column */
      .transactions-table td:nth-child(2) {
        width: 150px;
        white-space: nowrap;
      }
      
      /* Transaction description takes remaining space */
      .transactions-table td:nth-child(3) {
        width: auto;
      }
      
      .modal-dialog {
        max-width: 900px;
      }
      "))
    ),
    
    tabItems(
      # All Players Tab
      tabItem(
        tabName = "all_players",
        h2("All Players", class = "section-header"),
        uiOutput("all_players_ui")
      ),
      
      # MLB Tab
      tabItem(
        tabName = "mlb",
        h2("MLB Players", class = "section-header"),
        uiOutput("mlb_players_ui")
      ),
      
      # Minor League Tab (sorted by level)
      tabItem(
        tabName = "milb",
        h2("MiLB Players", class = "section-header"),
        uiOutput("milb_players_ui")
      ),
      
      # AAA Tab
      tabItem(
        tabName = "aaa",
        h2("AAA Players", class = "section-header"),
        uiOutput("aaa_players_ui")
      ),
      
      # AA Tab
      tabItem(
        tabName = "aa",
        h2("AA Players", class = "section-header"),
        uiOutput("aa_players_ui")
      ),
      
      # A+ Tab
      tabItem(
        tabName = "aplus",
        h2("A+ Players", class = "section-header"),
        uiOutput("aplus_players_ui")
      ),
      
      # A Tab
      tabItem(
        tabName = "a",
        h2("A Players", class = "section-header"),
        uiOutput("a_players_ui")
      ),
      
      # Rk Tab
      tabItem(
        tabName = "r",
        h2("Rookie Players", class = "section-header"),
        uiOutput("r_players_ui")
      ),
      
      # Others Tab
      tabItem(
        tabName = "others",
        h2("Other Players", class = "section-header"),
        uiOutput("others_players_ui")
      )
    )
  )
)

server <- function(input, output, session) {
  
  players_data <- player_info %>% 
    select(id, full_name, level_abb, primary_position_name, team_name, photo_link, parent_org, org_rank)
  
  #Create a cache for transactions
  transactions_cache <- reactiveValues(data = list())
  # game_log_cache <- reactiveValues(data = list())
  
  # Filter players based on search and position
  filtered_players <- reactive({
    data <- players_data
    
    # Search filter
    if (!is.null(input$search) && input$search != "") {
      data <- data[grepl(input$search, data$full_name, ignore.case = TRUE), ]
    }
    
    # Position filter
    if (!is.null(input$position_filter) && input$position_filter != "All") {
      data <- data[data$primary_position_name == input$position_filter, ]
    }
    
    return(data)
  })
  
  # Create player buttons
  create_player_buttons <- function(data) {
    if (nrow(data) == 0) {
      return(div(style = "padding: 20px; text-align: center; color: #999;",
                 "No players found"))
    }
    
    buttons <- lapply(1:nrow(data), function(i) {
      league_text <- ifelse(
        data$level_abb[i] == "MLB",
        data$team_name[i], 
        paste0(data$parent_org[i], " ", data$level_abb[i])
      )
      
      if (!is.na(data$org_rank[i])) {
        league_text <- paste0(league_text, " #", data$org_rank[i])
      }
      
      actionButton(
        inputId = paste0("player_", i, "_", gsub(" ", "_", data$full_name[i])),
        label = tagList(
          tags$img(src = data$photo_link[i], height = "150px", style = "display:block; margin: 0 auto 5px auto;"),
          div(
          style = "text-align:center;",
          strong(data$full_name[i], style = "font-size: 20px;"),
          br(),
          span(class = "league-badge", league_text),
          br(), 
          span(class = "position-badge", data$primary_position_name[i]))
        ),
        class = "player-button",
        onclick = sprintf(
          "Shiny.setInputValue('selected_player_data', {name: '%s', id: '%s'}, {priority: 'event'})", 
          data$full_name[i], data$id[i]
        )
      )
    })
    
    div(class = "player-grid", buttons)
  }
  
  # Create sorted MiLB section by level
  create_sorted_milb_ui <- function(data) {
    if (nrow(data) == 0) {
      return(div(style = "padding: 20px; text-align: center; color: #999;",
                 "No players found"))
    }
    
    level_order <- c("MLB", "AAA", "AA", "A+", "A", "Rk", "AFL")
    
    # Sort data by level
    data$level_abb <- factor(data$level_abb, levels = level_order)
    data <- data %>% arrange(level_abb)
    
    # Get unique levels present in data
    levels_present <- unique(data$level_abb)
    levels_present <- levels_present[!is.na(levels_present)]
    
    # Create sections for each level
    sections <- lapply(levels_present, function(level) {
      level_data <- data[data$level_abb == level, ]
      
      tagList(
        div(class = "level-section",
            div(class = "level-title", paste0(level, " (", nrow(level_data), " players)")),
            create_player_buttons(level_data)
        )
      )
    })
    
    div(sections)
  }
  
  # All players
  output$all_players_ui <- renderUI({
    create_sorted_milb_ui(filtered_players())
  })
  
  # MLB players
  output$mlb_players_ui <- renderUI({
    mlb_data <- filtered_players()[filtered_players()$level_abb == "MLB", ]
    create_player_buttons(mlb_data)
  })
  
  # Minor League players (sorted by level)
  output$milb_players_ui <- renderUI({
    milb_data <- filtered_players()[filtered_players()$level_abb != "MLB", ]
    create_sorted_milb_ui(milb_data)
  })
  
  # AAA players
  output$aaa_players_ui <- renderUI({
    aaa_data <- filtered_players()[filtered_players()$level_abb == "AAA", ]
    create_player_buttons(aaa_data)
  })
  
  # AA players
  output$aa_players_ui <- renderUI({
    aa_data <- filtered_players()[filtered_players()$level_abb == "AA", ]
    create_player_buttons(aa_data)
  })
  
  # A+ players
  output$aplus_players_ui <- renderUI({
    aplus_data <- filtered_players()[filtered_players()$level_abb == "A+", ]
    create_player_buttons(aplus_data)
  })
  
  # A players
  output$a_players_ui <- renderUI({
    a_data <- filtered_players()[filtered_players()$level_abb == "A", ]
    create_player_buttons(a_data)
  })
  
  # Rk players
  output$r_players_ui <- renderUI({
    r_data <- filtered_players()[filtered_players()$level_abb == "Rk", ]
    create_player_buttons(r_data)
  })
  
  # Other players
  output$others_players_ui <- renderUI({
    others_data <- filtered_players()[!(filtered_players()$level_abb  %in% c("MLB", "AAA", "AA", "A+", "A", "Rk")), ]
    create_player_buttons(others_data)
  })
  
  # Handle player selection
  observeEvent(input$selected_player_data, {
    
    req(input$selected_player_data)
    mlbam_id <- input$selected_player_data$id
    player_name <- input$selected_player_data$name
    
    if (!dir.exists("cache")) dir.create("cache")
    
    game_logs <- NULL 
    
    tryCatch({
      
      cache_path <- paste0("cache/game_log_", mlbam_id, ".rds")
    
      showModal(modalDialog(
        title = player_name,
        div(style = "text-align: center; padding: 20px;",
            icon("spinner", class = "fa-spin fa-3x"),
            h4("Loading...")
        ),
        footer = NULL
      ))
      
      if (is.null(transactions_cache$data[[mlbam_id]])) {
        transactions_cache$data[[mlbam_id]] <- get_player_transactions(mlbam_id)
      }
      
      game_logs <- get_game_log(player_name, player_info, 2025, 2025)
      pbp_data <- get_pbp_data(player_name, player_info) %>% 
        mutate(across(
          any_of("hitData.coordinates.coordY"),
          ~ -.
        ))
      pbp_data_ordered <- pbp_data %>% 
        mutate(
          home_level_name_ordered = if ("home_level_name" %in% names(.)) {
            factor(home_level_name, 
                   levels = c("Major League Baseball", "Triple-A", "Double-A", 
                              "High-A", "Single-A", "Rookie"))
          },
          result.event_ordered = if ("result.event" %in% names(.)) {
            factor(result.event, 
                   levels = c("Single", "Double", "Triple", "Home Run"))
          }
        )
      
      if (!("Message" %in% names(game_logs))) {
        saveRDS(game_logs, file = cache_path)
      }
      
      transactions_html <- transactions_cache$data[[mlbam_id]]
      
      spray_chart_id <- paste0("game_spray_chart_", mlbam_id)
      hover_id <- paste0("plot_hover_", mlbam_id)
      hover_info_id <- paste0("hover_info_", mlbam_id)
      
      showModal(modalDialog(
        title = player_name,
        tabsetPanel(
          id = paste0("player_tabs_", mlbam_id),
          type = "tabs",
          tabPanel(
            "Game Logs",
            br(),
            uiOutput(paste0("game_log_table_", mlbam_id)) 
          ), 
          tabPanel(
            "Spray Chart", 
            br(),
            div(
              style = "
                min-height: 65vh;",
              uiOutput(paste0("charts_ui_", mlbam_id))
            )
          ),
          tabPanel(
            "Strike Zone", 
            br(), 
            div(
              style = "
                min-height: 65vh;",
              girafeOutput(paste0("strike_zone_", mlbam_id), 
                           height = "550px")
            )
          ),
          tabPanel(
            "Transactions",
            br(),
            div(
              style = "
                max-height: 65vh;
                overflow-y: auto;
                overflow-x: hidden;
                padding-right: 10px;
              ",
              htmlOutput("transactions_html")
            )
          )
        ),
        size = "l",
        class = "transactions-table",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      
      output$transactions_html <- renderUI({
        HTML(transactions_html)
      })

      output[[paste0("game_log_table_", mlbam_id)]] <- renderUI({
        
        if ("Message" %in% names(game_logs)) {
          return(
            div(style = "text-align: center; color: #999; 
                font-size: 1.2em; padding: 50px;",
                icon("exclamation", style = "font-size: 3em; margin-bottom: 20px;"),
                p(game_logs$Message[1])
            )
          )
        }
          
        game_logs_formatted <- game_logs %>% 
          select(-any_of("gamedate")) %>% 
          arrange(desc(Date))
        
        cols_round_2 <- c("ERA", "FIP", "WHIP", "K/9", "BB/9", "HR/9", "K/BB", "BB/K", "Spd", "wRC", "wRAA", "wBsR")
        cols_round_3 <- c("AVG", "OBP", "SLG", "OPS", "ISO", "BABIP", "wOBA") 
        cols_percent <- c("K%", "BB%", "HR%", "LOB%", "K-BB%")
        cols_round <- c("ERA-", "FIP-", "xFIP-", "wRC+")
        
        for (col in cols_percent) {
          if (col %in% colnames(game_logs_formatted)) {
            game_logs_formatted[[col]] <- sprintf("%.1f%%",round(game_logs_formatted[[col]] * 100, 1))
          }
        }
        for (col in cols_round_2) {
          if (col %in% colnames(game_logs_formatted)) {
            game_logs_formatted[[col]] <- sprintf("%.2f",round(game_logs_formatted[[col]], 2))
          }
        }
        for (col in cols_round_3) {
          if (col %in% colnames(game_logs_formatted)) {
            game_logs_formatted[[col]] <- sprintf("%.3f",round(game_logs_formatted[[col]], 3))
          }
        }
        for (col in cols_round) {
          if (col %in% colnames(game_logs_formatted)) {
            game_logs_formatted[[col]] <- round(game_logs_formatted[[col]], 0)
          }
        }
        
        output[[paste0("game_log_table_dt_", mlbam_id)]] <- DT::renderDataTable({
          DT::datatable(
            game_logs_formatted,
            options = list(
              scrollX = TRUE, 
              scrollY = "400px",
              pageLength = 100,
              lengthMenu = c(10, 25, 50, 100), 
              dom = 'tp',        # Layout: Buttons, filter, table, info, pagination
              fixedHeader = TRUE,
              autoWidth = FALSE,
              columnDefs = list(
                list(className = 'dt-center', targets = '_all', filter = 'none'), 
                list(width = '150px', targets = 0),
                list(width = '80px', targets = 1:(ncol(game_logs_formatted)-1))
              )
            ),
            rownames = FALSE,        # Hide row numbers
            class = 'cell-border stripe hover',  # Styling classes
            filter = 'none'           # Add column filters at the top
          )
        })
        return(DT::dataTableOutput(paste0("game_log_table_dt_", mlbam_id)))  
      })
      
      output[[paste0("game_spray_chart_", mlbam_id)]] <- renderPlot({
        bb_palette <- c('Single' = "#006BA4",
                        'Double' = "#A2CEEC", 
                        'Triple'= "#FFBC79", 
                        'Home Run'= "#C85200", 
                        'Out/Other' = "#595959")
        
        ggspraychart(pbp_data_ordered, 
                     x_value = 'hitData.coordinates.coordX', 
                     y_value = 'hitData.coordinates.coordY', 
                     fill_value = 'result.event_ordered', 
                     fill_palette = bb_palette, 
                     point_size = 3) +
          facet_wrap(~home_level_name) +
          labs(title = paste0(player_name, ' Batted Balls 2025')) + 
          facet_wrap(~home_level_name_ordered)
      })
      
      output[[hover_info_id]] <- renderText({
        hover <- input[[hover_id]]
        if(is.null(hover)) return("")

        point <- nearPoints(pbp_data_ordered, hover, threshold = 20, maxpoints = 1)
        if(nrow(point) == 0) return("")
        paste0(
          "Event: ", point$result.event_ordered, "\n",
          "LA: ", round(point$hitData.launchAngle, 1), "°\n",
          "EV: ", round(point$hitData.launchSpeed, 1), " mph\n", 
          "Hardness: ", point$hitData.hardness
        )
      })
      
      output[[paste0("charts_ui_", mlbam_id)]] <- renderUI({
        if ("Message" %in% names(game_logs)) {
          div(style = "text-align: center; color: #999; 
              font-size: 1.2em; padding: 50px;",
              icon("exclamation", style = "font-size: 3em; margin-bottom: 20px;"),
              p(game_logs$Message[1])
          )
        } else {
          tagList(
            plotOutput(spray_chart_id, hover = hoverOpts(
              id = hover_id)),
            verbatimTextOutput(hover_info_id)
          )
        }
      })
      
      output[[paste0("strike_zone_", mlbam_id)]] <- renderGirafe({
        
        if ("Message" %in% names(game_logs)) {
          div(style = "text-align: center; color: #999; 
              font-size: 1.2em; padding: 50px;",
              icon("exclamation", style = "font-size: 3em; margin-bottom: 20px;"),
              p(game_logs$Message[1])
          )
        } else {
          home_plate_coords <- data.frame(
            x = c(-0.7083, -0.95, 0.0000, 0.95, 0.7083),
            z = c(0.125, -0.38, -0.6, -0.38, 0.125)
          )
          
          most_common_zone <- pbp_data %>%
            group_by(pitchData.strikeZoneTop, pitchData.strikeZoneBottom) %>%
            summarise(n = n(), .groups = 'drop') %>%
            arrange(desc(n)) %>%
            slice(1)
          
          pitch_type_colors <- c(`Four-Seam Fastball` = "#d22d49", Fastball = "#d22d49", 
                                 Sinker = "#ca6a04", Cutter = "#933f2c", 
                                 Changeup = "#1dbe3a", Splitter = "#3bacac", Forkball = "#55ccab", Screwball = "#60DB33",
                                 Curveball = "#00d1ed", `Knuckle Curve` = "#6236cd", `Slow Curve` = "#0068ff", 
                                 Slider = "#c3bd0e", Sweeper = "#ddb33a", Slurve = "#93afd4",
                                 `Knuckle Ball` = "#3c44cd", 
                                 Eephus = "#888", `NA` = "#888")
          
          pitch_types_in_data <- names(pitch_type_colors)[names(pitch_type_colors) %in% unique(pbp_data$details.type.description)]
          
          pbp_data_by_pitch_type <- pbp_data %>%
            mutate(.ptype = as.character(details.type.description)) %>%
            mutate(.ptype_key = match(.ptype, pitch_types_in_data)) %>%
            arrange(.ptype_key, row_number()) %>%
            select(-.ptype_key)
          
          k_zone_plot <- ggplot(pbp_data_by_pitch_type, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ)) +
            geom_polygon(data = home_plate_coords, aes(x = x, y = z),
                         fill = "lightgray", color = "darkgray", size = 0.25) +
            geom_point_interactive(aes(`data-id` = details.type.description, 
                                       color = details.type.description, 
                                       data_id = details.type.description), 
                                   extra_interactive_params = "data-id", 
                                   size = 0.5) + 
            scale_color_manual_interactive(extra_interactive_params = "data-id", 
                                           `data-id` = unique(pbp_data_by_pitch_type$details.type.description), 
                                           values = pitch_type_colors, 
                                           breaks = unique(pbp_data_by_pitch_type$details.type.description), 
                                           data_id = function(breaks) as.character(breaks)) + 
            coord_equal() + 
            scale_x_continuous("Horizontal location(ft.)", 
                               limits = c(-2.5, 2.5)) + 
            scale_y_continuous("Vertical location(ft.)") + 
            theme_classic() + 
            guides(color = guide_legend_interactive(title = "Pitch Type", override.aes = list(size = 3)))
          
          if(pbp_data[1,]$matchup.pitcher.fullName == player_name) {
            k_zone_plot <- k_zone_plot + 
              geom_rect(xmin = -0.7083, xmax = 0.7083,
                        ymin = 1.5, 
                        ymax = 3.6, 
                        fill = "lightgray", alpha = 0.01, color = "black", size = 0.25)
          } else {
            k_zone_plot <- k_zone_plot + 
              geom_rect(xmin = -0.7083, xmax = 0.7083,
                        ymin = most_common_zone$pitchData.strikeZoneBottom, 
                        ymax = most_common_zone$pitchData.strikeZoneTop, 
                        fill = "lightgray", alpha = 0.01, color = "black", size = 0.25)
          }
          
          
          k_zone_plot = girafe(ggobj = k_zone_plot)
          k_zone_plot = girafe_options(k_zone_plot,
                                       opts_hover(girafe_css("opacity:1")),
                                       opts_hover_inv(css = "opacity:0.1;"))
          k_zone_plot
        }
      })
    })
  })
}

shinyApp(ui, server)