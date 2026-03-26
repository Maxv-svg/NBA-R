# =============================================================================
# NBA Shot Analytics Dashboard — 2014-2015 Season
# Shiny Dashboard with Data Profiling, EDA, Interactive Viz & Predictive Model
# =============================================================================

# --- Load Libraries -----------------------------------------------------------
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(caret)
library(randomForest)
library(pROC)
library(scales)
library(viridis)
library(gridExtra)
library(corrplot)
library(e1071)

# --- Source Helpers ------------------------------------------------------------
source("helpers.R")

# --- Load & Pre-process Data --------------------------------------------------
# Place "shot_logs.csv" in the same folder as this app
raw_data <- read.csv("shot_logs.csv", stringsAsFactors = FALSE)
shots <- preprocess_shots(raw_data)

# --- Pre-compute summaries for performance ------------------------------------
player_summary <- shots %>%
  group_by(player_name) %>%
  summarise(
    total_shots  = n(),
    fg_pct       = mean(made, na.rm = TRUE),
    avg_dist     = mean(SHOT_DIST, na.rm = TRUE),
    avg_def_dist = mean(CLOSE_DEF_DIST, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(total_shots >= 50) %>%
  arrange(desc(total_shots))

team_summary <- shots %>%
  group_by(team) %>%
  summarise(
    total_shots = n(),
    fg_pct      = mean(made, na.rm = TRUE),
    avg_dist    = mean(SHOT_DIST, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(fg_pct))

# ==============================================================================
# UI
# ==============================================================================
ui <- dashboardPage(
  skin = "black",

  # --- Header -----------------------------------------------------------------
  dashboardHeader(
    title = span(
      icon("basketball-ball"), " NBA Shot Analytics",
      style = "font-weight:700; font-size:16px;"
    ),
    titleWidth = 300
  ),


  # --- Sidebar ----------------------------------------------------------------
  dashboardSidebar(
    width = 260,
    sidebarMenu(
      id = "tabs",
      menuItem("Overview",         tabName = "overview",   icon = icon("home")),
      menuItem("Data Profile",     tabName = "profile",    icon = icon("database")),
      menuItem("Shot Explorer",    tabName = "explorer",   icon = icon("search")),
      menuItem("Court Heatmap",    tabName = "court",      icon = icon("map")),
      menuItem("Player Analysis",  tabName = "player",     icon = icon("user")),
      menuItem("Defender Impact",  tabName = "defender",   icon = icon("shield-alt")),
      menuItem("Clutch Analysis",  tabName = "clutch",     icon = icon("clock")),
      menuItem("Predictive Model", tabName = "model",      icon = icon("brain")),
      menuItem("Make Probability", tabName = "calculator",  icon = icon("calculator"))
    ),
    br(),
    div(
      style = "padding: 10px 15px; color: #999; font-size: 11px;",
      "Data: NBA Shot Logs 2014-15",
      br(),
      "Source: stats.nba.com / SportVU"
    )
  ),

  # --- Body -------------------------------------------------------------------
  dashboardBody(
    # Custom CSS
    tags$head(tags$style(HTML("
      /* Dark theme overrides */
      .content-wrapper, .right-side { background-color: #1a1a2e; }
      .box { background-color: #16213e; border-top: 3px solid #e94560; color: #eee; }
      .box-header { color: #eee; }
      .box-header .box-title { font-weight: 600; }
      .small-box { border-radius: 8px; }
      .small-box .inner h3 { font-size: 28px; font-weight: 700; }
      .small-box .inner p  { font-size: 13px; }
      .skin-black .main-header .logo { background-color: #0f3460; font-weight: 700; }
      .skin-black .main-header .navbar { background-color: #0f3460; }
      .skin-black .main-sidebar { background-color: #0a0a23; }
      .sidebar-menu > li.active > a { border-left-color: #e94560; }
      .sidebar-menu > li > a:hover { background-color: #16213e !important; }
      .nav-tabs-custom > .nav-tabs > li.active > a { color: #e94560; border-top-color: #e94560; }
      .dataTables_wrapper { color: #ccc; }
      table.dataTable { color: #ddd; }
      table.dataTable thead th { color: #e94560; border-bottom: 1px solid #333; }
      table.dataTable tbody tr { background-color: #16213e !important; }
      table.dataTable tbody tr:hover { background-color: #1a1a4e !important; }
      .info-box { background-color: #16213e; color: #eee; border-radius: 8px; }
      .info-box .info-box-icon { background-color: #0f3460; border-radius: 8px 0 0 8px; }
      .selectize-input, .selectize-dropdown { background-color: #16213e !important; color: #eee !important; border-color: #333 !important; }
      .selectize-dropdown-content .option { color: #eee; }
      .selectize-dropdown-content .option.active { background-color: #e94560; }
      .form-control { background-color: #16213e; color: #eee; border-color: #333; }
      .irs--shiny .irs-bar { background: #e94560; border-top-color: #e94560; border-bottom-color: #e94560; }
      .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background-color: #e94560; }
      h3, h4, h5, p, label { color: #ddd; }
      .kpi-card { background: linear-gradient(135deg, #0f3460 0%, #16213e 100%); border-radius: 12px; padding: 20px; text-align: center; margin-bottom: 15px; }
      .kpi-card .kpi-value { font-size: 36px; font-weight: 800; color: #e94560; }
      .kpi-card .kpi-label { font-size: 12px; color: #aaa; text-transform: uppercase; letter-spacing: 1px; }
    "))),

    tabItems(

      # ---- Tab 1: Overview ---------------------------------------------------
      tabItem(
        tabName = "overview",
        fluidRow(
          column(12, h3("2014-15 NBA Shot Analytics Dashboard",
                        style = "color:#e94560; font-weight:800; margin-bottom:20px;"))
        ),
        fluidRow(
          valueBoxOutput("vb_total_shots",  width = 3),
          valueBoxOutput("vb_fg_pct",       width = 3),
          valueBoxOutput("vb_avg_dist",     width = 3),
          valueBoxOutput("vb_players",      width = 3)
        ),
        fluidRow(
          box(
            title = "FG% by Shot Zone", width = 4, solidHeader = TRUE,
            plotlyOutput("overview_zone_bar", height = "320px")
          ),
          box(
            title = "Shot Distribution by Quarter", width = 4, solidHeader = TRUE,
            plotlyOutput("overview_quarter_bar", height = "320px")
          ),
          box(
            title = "FG% by Defender Proximity", width = 4, solidHeader = TRUE,
            plotlyOutput("overview_defender_bar", height = "320px")
          )
        ),
        fluidRow(
          box(
            title = "Shot Distance vs Make Rate", width = 6, solidHeader = TRUE,
            plotlyOutput("overview_dist_line", height = "300px")
          ),
          box(
            title = "Top 15 Players by Volume", width = 6, solidHeader = TRUE,
            plotlyOutput("overview_top_players", height = "300px")
          )
        )
      ),

      # ---- Tab 2: Data Profile -----------------------------------------------
      tabItem(
        tabName = "profile",
        fluidRow(
          column(12, h3("Data Profile & Quality Report",
                        style = "color:#e94560; font-weight:800;"))
        ),
        fluidRow(
          box(
            title = "Dataset Summary", width = 12, solidHeader = TRUE,
            fluidRow(
              column(3, div(class = "kpi-card",
                div(class = "kpi-value", textOutput("prof_rows")),
                div(class = "kpi-label", "Total Rows")
              )),
              column(3, div(class = "kpi-card",
                div(class = "kpi-value", textOutput("prof_cols")),
                div(class = "kpi-label", "Columns")
              )),
              column(3, div(class = "kpi-card",
                div(class = "kpi-value", textOutput("prof_na_pct")),
                div(class = "kpi-label", "Missing Values %")
              )),
              column(3, div(class = "kpi-card",
                div(class = "kpi-value", textOutput("prof_balance")),
                div(class = "kpi-label", "Made / Missed Ratio")
              ))
            )
          )
        ),
        fluidRow(
          box(
            title = "Column Types & Missing Values", width = 6, solidHeader = TRUE,
            DTOutput("prof_column_table")
          ),
          box(
            title = "Class Balance (Target Variable)", width = 6, solidHeader = TRUE,
            plotlyOutput("prof_balance_plot", height = "250px"),
            br(),
            h5("Numerical Feature Distributions", style = "color:#e94560;"),
            plotlyOutput("prof_numeric_hist", height = "350px")
          )
        ),
        fluidRow(
          box(
            title = "Correlation Matrix (Numerical Features)", width = 12,
            solidHeader = TRUE,
            plotOutput("prof_corr_plot", height = "500px")
          )
        )
      ),

      # ---- Tab 3: Shot Explorer -----------------------------------------------
      tabItem(
        tabName = "explorer",
        fluidRow(
          column(12, h3("Interactive Shot Explorer",
                        style = "color:#e94560; font-weight:800;"))
        ),
        fluidRow(
          box(
            title = "Filters", width = 3, solidHeader = TRUE,
            selectizeInput("exp_player", "Player:",
                           choices = c("All", sort(unique(shots$player_name))),
                           selected = "All"),
            selectizeInput("exp_team", "Team:",
                           choices = c("All", sort(unique(shots$team))),
                           selected = "All"),
            selectInput("exp_zone", "Shot Zone:",
                        choices = c("All", "Paint", "Mid-Range", "Three-Point"),
                        selected = "All"),
            selectInput("exp_pressure", "Defender Pressure:",
                        choices = c("All", "Wide Open", "Open", "Contested", "Tightly Contested"),
                        selected = "All"),
            sliderInput("exp_dist", "Shot Distance (ft):",
                        min = 0, max = 40, value = c(0, 40), step = 1),
            sliderInput("exp_clock", "Shot Clock (sec):",
                        min = 0, max = 24, value = c(0, 24), step = 1),
            checkboxInput("exp_clutch", "Clutch Time Only", FALSE),
            actionButton("exp_apply", "Apply Filters",
                         style = "background-color:#e94560; color:white; width:100%;
                                  border:none; font-weight:600;")
          ),
          column(9,
            fluidRow(
              valueBoxOutput("exp_vb_shots",   width = 4),
              valueBoxOutput("exp_vb_fg",      width = 4),
              valueBoxOutput("exp_vb_avg_def", width = 4)
            ),
            box(
              title = "Filtered Shot Scatter", width = 12, solidHeader = TRUE,
              plotlyOutput("exp_scatter", height = "400px")
            ),
            box(
              title = "Filtered Data Table", width = 12, solidHeader = TRUE,
              DTOutput("exp_table")
            )
          )
        )
      ),

      # ---- Tab 4: Court Heatmap -----------------------------------------------
      tabItem(
        tabName = "court",
        fluidRow(
          column(12, h3("Court Shot Heatmap",
                        style = "color:#e94560; font-weight:800;"))
        ),
        fluidRow(
          box(
            title = "Controls", width = 3, solidHeader = TRUE,
            selectizeInput("court_player", "Player:",
                           choices = c("All", sort(unique(shots$player_name))),
                           selected = "All"),
            selectInput("court_metric", "Color By:",
                        choices = c("Shot Count" = "count", "FG%" = "fg_pct"),
                        selected = "fg_pct"),
            selectInput("court_result", "Result:",
                        choices = c("All", "Made", "Missed"), selected = "All"),
            sliderInput("court_min_shots", "Minimum Shots (for FG%):",
                        min = 1, max = 50, value = 5)
          ),
          box(
            title = "Shot Chart", width = 9, solidHeader = TRUE,
            plotOutput("court_heatmap", height = "600px")
          )
        )
      ),

      # ---- Tab 5: Player Analysis ----------------------------------------------
      tabItem(
        tabName = "player",
        fluidRow(
          column(12, h3("Player Shooting Profile",
                        style = "color:#e94560; font-weight:800;"))
        ),
        fluidRow(
          box(
            width = 12, solidHeader = TRUE,
            fluidRow(
              column(4, selectizeInput("player_sel", "Select Player:",
                                       choices = player_summary$player_name,
                                       selected = player_summary$player_name[1])),
              column(4, selectizeInput("player_comp", "Compare With (optional):",
                                       choices = c("None", player_summary$player_name),
                                       selected = "None")),
              column(4, br(), actionButton("player_go", "Analyze",
                       style = "background-color:#e94560; color:white; border:none;
                                font-weight:600; padding: 8px 30px;"))
            )
          )
        ),
        fluidRow(
          box(
            title = "Shot Zone Breakdown", width = 6, solidHeader = TRUE,
            plotlyOutput("player_zone_chart", height = "350px")
          ),
          box(
            title = "FG% by Distance Bin", width = 6, solidHeader = TRUE,
            plotlyOutput("player_dist_chart", height = "350px")
          )
        ),
        fluidRow(
          box(
            title = "Performance by Defender Proximity", width = 6, solidHeader = TRUE,
            plotlyOutput("player_def_chart", height = "320px")
          ),
          box(
            title = "Quarter-by-Quarter Efficiency", width = 6, solidHeader = TRUE,
            plotlyOutput("player_quarter_chart", height = "320px")
          )
        )
      ),

      # ---- Tab 6: Defender Impact -----------------------------------------------
      tabItem(
        tabName = "defender",
        fluidRow(
          column(12, h3("Defender Proximity Impact",
                        style = "color:#e94560; font-weight:800;"))
        ),
        fluidRow(
          box(
            title = "FG% vs Closest Defender Distance", width = 6, solidHeader = TRUE,
            plotlyOutput("def_scatter", height = "400px")
          ),
          box(
            title = "FG% by Pressure Category & Zone", width = 6, solidHeader = TRUE,
            plotlyOutput("def_heatmap", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = "Best Defenders (Most FG% Reduction)", width = 6, solidHeader = TRUE,
            DTOutput("def_best_table")
          ),
          box(
            title = "Defender Distance Distribution by Result", width = 6, solidHeader = TRUE,
            plotlyOutput("def_density", height = "350px")
          )
        )
      ),

      # ---- Tab 7: Clutch Analysis -----------------------------------------------
      tabItem(
        tabName = "clutch",
        fluidRow(
          column(12, h3("Clutch Time Analysis (Last 5 Minutes)",
                        style = "color:#e94560; font-weight:800;"))
        ),
        fluidRow(
          valueBoxOutput("clutch_vb_shots",  width = 3),
          valueBoxOutput("clutch_vb_fg",     width = 3),
          valueBoxOutput("clutch_vb_nonclutch_fg", width = 3),
          valueBoxOutput("clutch_vb_diff",   width = 3)
        ),
        fluidRow(
          box(
            title = "Clutch vs Non-Clutch FG% by Zone", width = 6, solidHeader = TRUE,
            plotlyOutput("clutch_zone_chart", height = "350px")
          ),
          box(
            title = "Top Clutch Shooters (min 20 clutch shots)", width = 6,
            solidHeader = TRUE,
            plotlyOutput("clutch_top_players", height = "350px")
          )
        ),
        fluidRow(
          box(
            title = "Clutch FG% by Shot Clock Remaining", width = 12, solidHeader = TRUE,
            plotlyOutput("clutch_clock_chart", height = "300px")
          )
        )
      ),

      # ---- Tab 8: Predictive Model --------------------------------------------
      tabItem(
        tabName = "model",
        fluidRow(
          column(12, h3("Predictive Model: Will the Shot Go In?",
                        style = "color:#e94560; font-weight:800;"))
        ),
        fluidRow(
          box(
            title = "Model Configuration", width = 4, solidHeader = TRUE,
            selectInput("model_type", "Algorithm:",
                        choices = c("Logistic Regression" = "glm",
                                    "Random Forest" = "rf"),
                        selected = "glm"),
            sliderInput("model_split", "Training %:", min = 60, max = 90,
                        value = 80, step = 5),
            checkboxGroupInput("model_features", "Features:",
                               choices = c("Shot Distance"       = "SHOT_DIST",
                                           "Defender Distance"   = "CLOSE_DEF_DIST",
                                           "Shot Clock"          = "SHOT_CLOCK",
                                           "Dribbles"            = "DRIBBLES",
                                           "Touch Time"          = "TOUCH_TIME",
                                           "Shot Zone"           = "shot_zone",
                                           "Defender Pressure"   = "pressure_cat",
                                           "Quarter"             = "QUARTER",
                                           "Home/Away"           = "location",
                                           "Shot Number"         = "SHOT_NUMBER"),
                               selected = c("SHOT_DIST", "CLOSE_DEF_DIST",
                                            "SHOT_CLOCK", "shot_zone",
                                            "pressure_cat")),
            actionButton("model_train", "Train Model",
                         style = "background-color:#e94560; color:white; width:100%;
                                  border:none; font-weight:700; padding: 10px;")
          ),
          column(8,
            fluidRow(
              valueBoxOutput("model_vb_acc",  width = 4),
              valueBoxOutput("model_vb_auc",  width = 4),
              valueBoxOutput("model_vb_n",    width = 4)
            ),
            box(
              title = "ROC Curve", width = 6, solidHeader = TRUE,
              plotOutput("model_roc", height = "350px")
            ),
            box(
              title = "Confusion Matrix", width = 6, solidHeader = TRUE,
              plotOutput("model_cm", height = "350px")
            ),
            box(
              title = "Feature Importance", width = 12, solidHeader = TRUE,
              plotlyOutput("model_importance", height = "300px")
            )
          )
        )
      ),

      # ---- Tab 9: Calculator --------------------------------------------------
      tabItem(
        tabName = "calculator",
        fluidRow(
          column(12, h3("Shot Make Probability Calculator",
                        style = "color:#e94560; font-weight:800;"))
        ),
        fluidRow(
          box(
            title = "Enter Shot Parameters", width = 5, solidHeader = TRUE,
            sliderInput("calc_dist", "Shot Distance (ft):", 0, 35, 15, 0.5),
            sliderInput("calc_def", "Closest Defender (ft):", 0, 25, 4, 0.5),
            sliderInput("calc_clock", "Shot Clock (sec):", 0, 24, 12, 1),
            sliderInput("calc_dribbles", "Dribbles:", 0, 15, 2, 1),
            sliderInput("calc_touch", "Touch Time (sec):", 0, 24, 3, 0.5),
            selectInput("calc_zone", "Shot Zone:",
                        choices = c("Paint", "Mid-Range", "Three-Point")),
            selectInput("calc_pressure", "Defender Pressure:",
                        choices = c("Wide Open", "Open", "Contested",
                                    "Tightly Contested")),
            selectInput("calc_quarter", "Quarter:", choices = 1:4, selected = 1),
            actionButton("calc_predict", "Calculate Probability",
                         style = "background-color:#e94560; color:white; width:100%;
                                  border:none; font-weight:700; padding:12px;
                                  font-size:16px;")
          ),
          column(7,
            div(
              style = "text-align:center; padding-top:40px;",
              h4("Predicted Make Probability", style = "color:#aaa;"),
              div(
                style = "font-size: 80px; font-weight: 900; color: #e94560;
                         margin: 30px 0;",
                textOutput("calc_result")
              ),
              div(
                style = "font-size: 14px; color: #888; max-width: 400px;
                         margin: 0 auto;",
                textOutput("calc_context")
              )
            ),
            box(
              title = "Historical Comparison", width = 12, solidHeader = TRUE,
              plotlyOutput("calc_comparison", height = "300px")
            )
          )
        )
      )

    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage


# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {

  # ---- Overview Tab ----------------------------------------------------------
  output$vb_total_shots <- renderValueBox({
    valueBox(format(nrow(shots), big.mark = ","), "Total Shots",
             icon = icon("basketball-ball"), color = "red")
  })
  output$vb_fg_pct <- renderValueBox({
    valueBox(percent(mean(shots$made, na.rm = TRUE), accuracy = 0.1),
             "Overall FG%", icon = icon("bullseye"), color = "blue")
  })
  output$vb_avg_dist <- renderValueBox({
    valueBox(paste0(round(mean(shots$SHOT_DIST, na.rm = TRUE), 1), " ft"),
             "Avg Shot Distance", icon = icon("ruler"), color = "purple")
  })
  output$vb_players <- renderValueBox({
    valueBox(length(unique(shots$player_name)), "Players",
             icon = icon("users"), color = "olive")
  })

  output$overview_zone_bar <- renderPlotly({
    d <- shots %>%
      group_by(shot_zone) %>%
      summarise(fg_pct = mean(made, na.rm = TRUE), n = n(), .groups = "drop")
    plot_ly(d, x = ~shot_zone, y = ~fg_pct, type = "bar",
            marker = list(color = c("#e94560", "#0f3460", "#533483")),
            text = ~paste0(round(fg_pct*100,1), "% (n=", format(n, big.mark=","), ")"),
            textposition = "auto") %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#ccc"),
             xaxis = list(title = ""), yaxis = list(title = "FG%", tickformat = ".0%"))
  })

  output$overview_quarter_bar <- renderPlotly({
    d <- shots %>%
      group_by(QUARTER) %>%
      summarise(n = n(), fg_pct = mean(made, na.rm = TRUE), .groups = "drop")
    plot_ly(d, x = ~factor(QUARTER), y = ~n, type = "bar",
            marker = list(color = "#0f3460"),
            text = ~paste0("FG%: ", round(fg_pct*100,1), "%"),
            textposition = "auto") %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#ccc"),
             xaxis = list(title = "Quarter"), yaxis = list(title = "Shot Count"))
  })

  output$overview_defender_bar <- renderPlotly({
    d <- shots %>%
      filter(!is.na(pressure_cat)) %>%
      group_by(pressure_cat) %>%
      summarise(fg_pct = mean(made, na.rm = TRUE), n = n(), .groups = "drop") %>%
      mutate(pressure_cat = factor(pressure_cat,
             levels = c("Wide Open", "Open", "Contested", "Tightly Contested")))
    plot_ly(d, x = ~pressure_cat, y = ~fg_pct, type = "bar",
            marker = list(color = c("#27ae60","#f39c12","#e67e22","#e94560")),
            text = ~paste0(round(fg_pct*100,1), "%"), textposition = "auto") %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#ccc"),
             xaxis = list(title = ""), yaxis = list(title = "FG%", tickformat = ".0%"))
  })

  output$overview_dist_line <- renderPlotly({
    d <- shots %>%
      mutate(dist_bin = floor(SHOT_DIST)) %>%
      group_by(dist_bin) %>%
      summarise(fg_pct = mean(made, na.rm = TRUE), n = n(), .groups = "drop") %>%
      filter(n >= 50)
    plot_ly(d, x = ~dist_bin, y = ~fg_pct, type = "scatter", mode = "lines+markers",
            line = list(color = "#e94560", width = 2),
            marker = list(color = "#e94560", size = 5),
            text = ~paste0(dist_bin, "ft: ", round(fg_pct*100,1), "%")) %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#ccc"),
             xaxis = list(title = "Distance (ft)"),
             yaxis = list(title = "FG%", tickformat = ".0%"))
  })

  output$overview_top_players <- renderPlotly({
    d <- player_summary %>% slice_max(total_shots, n = 15)
    plot_ly(d, x = ~reorder(player_name, total_shots), y = ~total_shots,
            type = "bar", orientation = "h",
            marker = list(color = ~fg_pct, colorscale = list(c(0,"#0f3460"),c(1,"#e94560"))),
            text = ~paste0("FG%: ", round(fg_pct*100,1), "%")) %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#ccc"),
             xaxis = list(title = "Total Shots"), yaxis = list(title = ""))
  })

  # ---- Data Profile Tab -------------------------------------------------------
  output$prof_rows <- renderText(format(nrow(shots), big.mark = ","))
  output$prof_cols <- renderText(ncol(raw_data))
  output$prof_na_pct <- renderText({
    paste0(round(sum(is.na(raw_data)) / (nrow(raw_data)*ncol(raw_data)) * 100, 2), "%")
  })
  output$prof_balance <- renderText({
    paste0(round(mean(shots$made)*100, 1), " / ", round((1-mean(shots$made))*100, 1))
  })

  output$prof_column_table <- renderDT({
    col_info <- data.frame(
      Column   = names(raw_data),
      Type     = sapply(raw_data, class),
      Missing  = sapply(raw_data, function(x) sum(is.na(x))),
      Pct_Miss = sapply(raw_data, function(x) round(sum(is.na(x))/length(x)*100, 2)),
      Unique   = sapply(raw_data, function(x) length(unique(x))),
      stringsAsFactors = FALSE
    )
    datatable(col_info, options = list(pageLength = 25, dom = "tp"),
              rownames = FALSE,
              style = "bootstrap4") %>%
      formatStyle(columns = names(col_info),
                  backgroundColor = "#16213e", color = "#ddd")
  })

  output$prof_balance_plot <- renderPlotly({
    d <- shots %>% count(shot_result = ifelse(made == 1, "Made", "Missed"))
    plot_ly(d, x = ~shot_result, y = ~n, type = "bar",
            marker = list(color = c("#27ae60", "#e94560")),
            text = ~paste0(format(n, big.mark=","), " (", round(n/sum(n)*100,1), "%)"),
            textposition = "auto") %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#ccc"),
             xaxis = list(title = ""), yaxis = list(title = "Count"))
  })

  output$prof_numeric_hist <- renderPlotly({
    num_cols <- c("SHOT_DIST", "CLOSE_DEF_DIST", "SHOT_CLOCK", "TOUCH_TIME", "DRIBBLES")
    existing <- num_cols[num_cols %in% names(shots)]
    d <- shots %>% select(all_of(existing)) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
      filter(!is.na(value))
    ggp <- ggplot(d, aes(x = value, fill = variable)) +
      geom_histogram(bins = 40, alpha = 0.8) +
      facet_wrap(~variable, scales = "free", ncol = 3) +
      scale_fill_manual(values = c("#e94560","#0f3460","#533483","#27ae60","#f39c12")) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "none",
            plot.background  = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            text = element_text(color = "#ccc"),
            axis.text = element_text(color = "#999"),
            strip.text = element_text(color = "#e94560", face = "bold"))
    ggplotly(ggp) %>% layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent")
  })

  output$prof_corr_plot <- renderPlot({
    num_cols <- c("SHOT_DIST","CLOSE_DEF_DIST","SHOT_CLOCK","TOUCH_TIME",
                  "DRIBBLES","SHOT_NUMBER","QUARTER")
    existing <- num_cols[num_cols %in% names(shots)]
    cor_mat <- cor(shots[, existing], use = "pairwise.complete.obs")
    par(bg = "#16213e")
    corrplot(cor_mat, method = "color", type = "lower", tl.col = "#ddd",
             addCoef.col = "#fff", number.cex = 0.9,
             col = colorRampPalette(c("#0f3460","#16213e","#e94560"))(200),
             cl.cex = 0.8, tl.cex = 0.9)
  }, bg = "transparent")

  # ---- Shot Explorer Tab -------------------------------------------------------
  filtered_shots <- eventReactive(input$exp_apply, {
    d <- shots
    if (input$exp_player != "All") d <- d %>% filter(player_name == input$exp_player)
    if (input$exp_team   != "All") d <- d %>% filter(team == input$exp_team)
    if (input$exp_zone   != "All") d <- d %>% filter(shot_zone == input$exp_zone)
    if (input$exp_pressure != "All") d <- d %>% filter(pressure_cat == input$exp_pressure)
    d <- d %>% filter(SHOT_DIST >= input$exp_dist[1], SHOT_DIST <= input$exp_dist[2])
    if (!all(is.na(d$SHOT_CLOCK))) {
      d <- d %>% filter(is.na(SHOT_CLOCK) |
                         (SHOT_CLOCK >= input$exp_clock[1] &
                          SHOT_CLOCK <= input$exp_clock[2]))
    }
    if (input$exp_clutch) d <- d %>% filter(clutch == 1)
    d
  }, ignoreNULL = FALSE)

  output$exp_vb_shots <- renderValueBox({
    valueBox(format(nrow(filtered_shots()), big.mark = ","), "Shots",
             icon = icon("basketball-ball"), color = "red")
  })
  output$exp_vb_fg <- renderValueBox({
    fg <- if (nrow(filtered_shots()) > 0) mean(filtered_shots()$made, na.rm = TRUE) else 0
    valueBox(percent(fg, accuracy = 0.1), "FG%", icon = icon("bullseye"), color = "blue")
  })
  output$exp_vb_avg_def <- renderValueBox({
    ad <- if (nrow(filtered_shots()) > 0) mean(filtered_shots()$CLOSE_DEF_DIST, na.rm = TRUE) else 0
    valueBox(paste0(round(ad, 1), " ft"), "Avg Defender Dist",
             icon = icon("shield-alt"), color = "purple")
  })

  output$exp_scatter <- renderPlotly({
    d <- filtered_shots()
    if (nrow(d) == 0) return(plotly_empty())
    # Sample if too many points
    if (nrow(d) > 5000) d <- d %>% sample_n(5000)
    plot_ly(d, x = ~SHOT_DIST, y = ~CLOSE_DEF_DIST,
            color = ~factor(made, labels = c("Missed","Made")),
            colors = c("#e94560","#27ae60"),
            type = "scatter", mode = "markers",
            marker = list(size = 4, opacity = 0.5),
            text = ~paste0(player_name, "\n", shot_zone, " | ",
                           pressure_cat, "\nDist: ", SHOT_DIST, "ft")) %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#ccc"),
             xaxis = list(title = "Shot Distance (ft)"),
             yaxis = list(title = "Closest Defender (ft)"),
             legend = list(font = list(color = "#ccc")))
  })

  output$exp_table <- renderDT({
    d <- filtered_shots() %>%
      select(player_name, team, shot_zone, pressure_cat, SHOT_DIST,
             CLOSE_DEF_DIST, SHOT_CLOCK, made) %>%
      mutate(Result = ifelse(made == 1, "Made", "Missed")) %>%
      select(-made) %>%
      head(500)
    datatable(d, options = list(pageLength = 10, scrollX = TRUE, dom = "ftp"),
              rownames = FALSE, style = "bootstrap4") %>%
      formatStyle(columns = names(d), backgroundColor = "#16213e", color = "#ddd")
  })

  # ---- Court Heatmap Tab -------------------------------------------------------
  output$court_heatmap <- renderPlot({
    d <- shots
    if (input$court_player != "All") d <- d %>% filter(player_name == input$court_player)
    if (input$court_result == "Made")   d <- d %>% filter(made == 1)
    if (input$court_result == "Missed") d <- d %>% filter(made == 0)

    # Bin shots by distance and approximate angle using shot_dist only
    # (We don't have x/y coords, so we simulate a 1D heatmap by zone)
    d_zone <- d %>%
      mutate(dist_bin = cut(SHOT_DIST, breaks = c(0, 3, 8, 14, 22, 24, 30, 50),
                            labels = c("Rim (0-3)","Paint (3-8)","Short Mid (8-14)",
                                       "Long Mid (14-22)","3PT Corner (22-24)",
                                       "3PT Wing (24-30)","Deep 3 (30+)"))) %>%
      group_by(dist_bin) %>%
      summarise(count = n(), fg_pct = mean(made, na.rm = TRUE), .groups = "drop") %>%
      filter(!is.na(dist_bin))

    metric_col <- if (input$court_metric == "fg_pct") "fg_pct" else "count"

    if (input$court_metric == "fg_pct") {
      d_zone <- d_zone %>% filter(count >= input$court_min_shots)
    }

    # Draw a stylized half-court with zone bars
    p <- draw_court_zones(d_zone, metric_col)
    p
  }, bg = "transparent")

  # ---- Player Analysis Tab ---------------------------------------------------
  player_data <- eventReactive(input$player_go, {
    p1 <- shots %>% filter(player_name == input$player_sel)
    p2 <- if (input$player_comp != "None") {
      shots %>% filter(player_name == input$player_comp)
    } else NULL
    list(p1 = p1, p2 = p2, name1 = input$player_sel, name2 = input$player_comp)
  }, ignoreNULL = FALSE)

  output$player_zone_chart <- renderPlotly({
    pd <- player_data()
    d1 <- pd$p1 %>% group_by(shot_zone) %>%
      summarise(fg_pct = mean(made, na.rm = TRUE), n = n(), .groups = "drop") %>%
      mutate(player = pd$name1)
    if (!is.null(pd$p2)) {
      d2 <- pd$p2 %>% group_by(shot_zone) %>%
        summarise(fg_pct = mean(made, na.rm = TRUE), n = n(), .groups = "drop") %>%
        mutate(player = pd$name2)
      d1 <- bind_rows(d1, d2)
    }
    plot_ly(d1, x = ~shot_zone, y = ~fg_pct, color = ~player, type = "bar",
            text = ~paste0(round(fg_pct*100,1), "% (n=", n, ")"),
            textposition = "auto",
            colors = c("#e94560", "#0f3460")) %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#ccc"), barmode = "group",
             xaxis = list(title = ""), yaxis = list(title = "FG%", tickformat = ".0%"),
             legend = list(font = list(color = "#ccc")))
  })

  output$player_dist_chart <- renderPlotly({
    pd <- player_data()
    make_dist_data <- function(df, nm) {
      df %>%
        mutate(dist_bin = cut(SHOT_DIST, breaks = seq(0, 40, 5))) %>%
        group_by(dist_bin) %>%
        summarise(fg_pct = mean(made, na.rm = TRUE), n = n(), .groups = "drop") %>%
        filter(!is.na(dist_bin)) %>%
        mutate(player = nm)
    }
    d <- make_dist_data(pd$p1, pd$name1)
    if (!is.null(pd$p2)) d <- bind_rows(d, make_dist_data(pd$p2, pd$name2))
    plot_ly(d, x = ~dist_bin, y = ~fg_pct, color = ~player, type = "bar",
            colors = c("#e94560","#0f3460"),
            text = ~paste0(round(fg_pct*100,1),"%"), textposition = "auto") %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#ccc"), barmode = "group",
             xaxis = list(title = "Distance Range"), yaxis = list(title = "FG%", tickformat = ".0%"),
             legend = list(font = list(color = "#ccc")))
  })

  output$player_def_chart <- renderPlotly({
    pd <- player_data()
    make_def_data <- function(df, nm) {
      df %>% filter(!is.na(pressure_cat)) %>%
        group_by(pressure_cat) %>%
        summarise(fg_pct = mean(made, na.rm = TRUE), .groups = "drop") %>%
        mutate(player = nm,
               pressure_cat = factor(pressure_cat,
                 levels = c("Wide Open","Open","Contested","Tightly Contested")))
    }
    d <- make_def_data(pd$p1, pd$name1)
    if (!is.null(pd$p2)) d <- bind_rows(d, make_def_data(pd$p2, pd$name2))
    plot_ly(d, x = ~pressure_cat, y = ~fg_pct, color = ~player, type = "bar",
            colors = c("#e94560","#0f3460")) %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#ccc"), barmode = "group",
             yaxis = list(title = "FG%", tickformat = ".0%"), xaxis = list(title = ""),
             legend = list(font = list(color = "#ccc")))
  })

  output$player_quarter_chart <- renderPlotly({
    pd <- player_data()
    make_q_data <- function(df, nm) {
      df %>% group_by(QUARTER) %>%
        summarise(fg_pct = mean(made, na.rm = TRUE), .groups = "drop") %>%
        mutate(player = nm)
    }
    d <- make_q_data(pd$p1, pd$name1)
    if (!is.null(pd$p2)) d <- bind_rows(d, make_q_data(pd$p2, pd$name2))
    plot_ly(d, x = ~factor(QUARTER), y = ~fg_pct, color = ~player,
            type = "scatter", mode = "lines+markers",
            colors = c("#e94560","#0f3460")) %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#ccc"),
             xaxis = list(title = "Quarter"), yaxis = list(title = "FG%", tickformat = ".0%"),
             legend = list(font = list(color = "#ccc")))
  })

  # ---- Defender Impact Tab ---------------------------------------------------
  output$def_scatter <- renderPlotly({
    d <- shots %>%
      mutate(def_bin = round(CLOSE_DEF_DIST)) %>%
      group_by(def_bin) %>%
      summarise(fg_pct = mean(made, na.rm = TRUE), n = n(), .groups = "drop") %>%
      filter(n >= 30)
    plot_ly(d, x = ~def_bin, y = ~fg_pct, size = ~n, type = "scatter",
            mode = "markers",
            marker = list(color = "#e94560", opacity = 0.7, sizemode = "diameter",
                          sizeref = max(d$n)/30),
            text = ~paste0(def_bin, "ft: ", round(fg_pct*100,1), "% (n=", n, ")")) %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#ccc"),
             xaxis = list(title = "Closest Defender Distance (ft)"),
             yaxis = list(title = "FG%", tickformat = ".0%"))
  })

  output$def_heatmap <- renderPlotly({
    d <- shots %>%
      filter(!is.na(pressure_cat)) %>%
      group_by(pressure_cat, shot_zone) %>%
      summarise(fg_pct = mean(made, na.rm = TRUE), n = n(), .groups = "drop") %>%
      mutate(pressure_cat = factor(pressure_cat,
             levels = c("Wide Open","Open","Contested","Tightly Contested")))
    plot_ly(d, x = ~shot_zone, y = ~pressure_cat, z = ~fg_pct, type = "heatmap",
            colorscale = list(c(0,"#0a0a23"), c(0.5,"#0f3460"), c(1,"#e94560")),
            text = ~paste0(round(fg_pct*100,1), "% (n=", n, ")"),
            hoverinfo = "text") %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#ccc"),
             xaxis = list(title = ""), yaxis = list(title = ""))
  })

  output$def_best_table <- renderDT({
    d <- shots %>%
      filter(!is.na(CLOSEST_DEFENDER)) %>%
      group_by(CLOSEST_DEFENDER) %>%
      summarise(
        shots_defended = n(),
        opp_fg_pct     = round(mean(made, na.rm = TRUE) * 100, 1),
        avg_dist       = round(mean(CLOSE_DEF_DIST, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      filter(shots_defended >= 100) %>%
      arrange(opp_fg_pct) %>%
      head(20)
    datatable(d, options = list(pageLength = 10, dom = "tp"),
              rownames = FALSE, style = "bootstrap4",
              colnames = c("Defender","Shots Defended","Opp FG%","Avg Distance")) %>%
      formatStyle(columns = names(d), backgroundColor = "#16213e", color = "#ddd")
  })

  output$def_density <- renderPlotly({
    d <- shots %>% filter(!is.na(CLOSE_DEF_DIST)) %>%
      mutate(result = ifelse(made == 1, "Made", "Missed"))
    plot_ly(alpha = 0.5) %>%
      add_histogram(data = filter(d, result == "Made"), x = ~CLOSE_DEF_DIST,
                    name = "Made", marker = list(color = "#27ae60"), nbinsx = 50) %>%
      add_histogram(data = filter(d, result == "Missed"), x = ~CLOSE_DEF_DIST,
                    name = "Missed", marker = list(color = "#e94560"), nbinsx = 50) %>%
      layout(barmode = "overlay",
             paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#ccc"),
             xaxis = list(title = "Closest Defender Distance (ft)"),
             yaxis = list(title = "Count"),
             legend = list(font = list(color = "#ccc")))
  })

  # ---- Clutch Analysis Tab ---------------------------------------------------
  clutch_shots     <- reactive(shots %>% filter(clutch == 1))
  non_clutch_shots <- reactive(shots %>% filter(clutch == 0))

  output$clutch_vb_shots <- renderValueBox({
    valueBox(format(nrow(clutch_shots()), big.mark = ","), "Clutch Shots",
             icon = icon("clock"), color = "red")
  })
  output$clutch_vb_fg <- renderValueBox({
    fg <- if (nrow(clutch_shots()) > 0) mean(clutch_shots()$made, na.rm = TRUE) else 0
    valueBox(percent(fg, accuracy = 0.1), "Clutch FG%",
             icon = icon("fire"), color = "yellow")
  })
  output$clutch_vb_nonclutch_fg <- renderValueBox({
    fg <- mean(non_clutch_shots()$made, na.rm = TRUE)
    valueBox(percent(fg, accuracy = 0.1), "Non-Clutch FG%",
             icon = icon("chart-line"), color = "blue")
  })
  output$clutch_vb_diff <- renderValueBox({
    diff_val <- mean(clutch_shots()$made, na.rm = TRUE) -
                mean(non_clutch_shots()$made, na.rm = TRUE)
    valueBox(paste0(ifelse(diff_val >= 0, "+", ""), round(diff_val*100, 1), " pp"),
             "Clutch Differential", icon = icon("exchange-alt"), color = "purple")
  })

  output$clutch_zone_chart <- renderPlotly({
    d_c  <- clutch_shots() %>% group_by(shot_zone) %>%
      summarise(fg_pct = mean(made, na.rm = TRUE), .groups = "drop") %>%
      mutate(type = "Clutch")
    d_nc <- non_clutch_shots() %>% group_by(shot_zone) %>%
      summarise(fg_pct = mean(made, na.rm = TRUE), .groups = "drop") %>%
      mutate(type = "Non-Clutch")
    d <- bind_rows(d_c, d_nc)
    plot_ly(d, x = ~shot_zone, y = ~fg_pct, color = ~type, type = "bar",
            colors = c("#e94560","#0f3460")) %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#ccc"), barmode = "group",
             yaxis = list(title = "FG%", tickformat = ".0%"), xaxis = list(title = ""),
             legend = list(font = list(color = "#ccc")))
  })

  output$clutch_top_players <- renderPlotly({
    d <- clutch_shots() %>%
      group_by(player_name) %>%
      summarise(fg_pct = mean(made, na.rm = TRUE), n = n(), .groups = "drop") %>%
      filter(n >= 20) %>%
      slice_max(fg_pct, n = 15)
    plot_ly(d, x = ~reorder(player_name, fg_pct), y = ~fg_pct, type = "bar",
            orientation = "h", marker = list(color = "#e94560"),
            text = ~paste0(round(fg_pct*100,1), "% (", n, " shots)"),
            textposition = "auto") %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#ccc"),
             xaxis = list(title = "FG%", tickformat = ".0%"), yaxis = list(title = ""))
  })

  output$clutch_clock_chart <- renderPlotly({
    d <- clutch_shots() %>%
      filter(!is.na(SHOT_CLOCK)) %>%
      mutate(clock_bin = cut(SHOT_CLOCK, breaks = seq(0, 24, 3))) %>%
      group_by(clock_bin) %>%
      summarise(fg_pct = mean(made, na.rm = TRUE), n = n(), .groups = "drop") %>%
      filter(!is.na(clock_bin))
    plot_ly(d, x = ~clock_bin, y = ~fg_pct, type = "bar",
            marker = list(color = "#533483"),
            text = ~paste0(round(fg_pct*100,1), "% (n=", n, ")"),
            textposition = "auto") %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#ccc"),
             xaxis = list(title = "Shot Clock (sec)"),
             yaxis = list(title = "FG%", tickformat = ".0%"))
  })

  # ---- Predictive Model Tab --------------------------------------------------
  model_results <- reactiveValues(
    trained = FALSE, accuracy = NA, auc = NA, n_test = NA,
    roc_obj = NULL, cm = NULL, importance = NULL, model_obj = NULL
  )

  observeEvent(input$model_train, {
    withProgress(message = "Training model...", value = 0, {
      features <- input$model_features
      if (length(features) < 2) {
        showNotification("Select at least 2 features.", type = "error")
        return()
      }

      # Prepare data
      model_data <- shots %>%
        select(all_of(c(features, "made"))) %>%
        drop_na()

      # Convert categoricals to factors
      cat_cols <- intersect(features, c("shot_zone","pressure_cat","location"))
      for (col in cat_cols) model_data[[col]] <- as.factor(model_data[[col]])
      if ("QUARTER" %in% features) model_data$QUARTER <- as.factor(model_data$QUARTER)
      model_data$made <- as.factor(model_data$made)

      incProgress(0.2, detail = "Splitting data...")

      # Train/test split
      set.seed(42)
      train_idx <- createDataPartition(model_data$made, p = input$model_split/100,
                                       list = FALSE)
      train_set <- model_data[train_idx, ]
      test_set  <- model_data[-train_idx, ]

      incProgress(0.3, detail = "Fitting model...")

      # Train
      if (input$model_type == "glm") {
        fit <- glm(made ~ ., data = train_set, family = binomial())
        probs <- predict(fit, test_set, type = "response")
        imp_df <- data.frame(
          feature    = names(coef(fit))[-1],
          importance = abs(coef(fit)[-1])
        ) %>% arrange(desc(importance)) %>% head(15)
      } else {
        # Random forest - subsample for speed
        rf_train <- if (nrow(train_set) > 20000) train_set[sample(nrow(train_set), 20000), ] else train_set
        fit <- randomForest(made ~ ., data = rf_train, ntree = 200, importance = TRUE)
        probs <- predict(fit, test_set, type = "prob")[, "1"]
        imp_raw <- importance(fit)
        imp_df <- data.frame(
          feature    = rownames(imp_raw),
          importance = imp_raw[, "MeanDecreaseGini"]
        ) %>% arrange(desc(importance)) %>% head(15)
      }

      incProgress(0.3, detail = "Evaluating...")

      preds <- ifelse(probs > 0.5, 1, 0)
      acc <- mean(preds == as.numeric(as.character(test_set$made)), na.rm = TRUE)
      roc_obj <- roc(as.numeric(as.character(test_set$made)), probs, quiet = TRUE)
      cm <- confusionMatrix(factor(preds), factor(as.numeric(as.character(test_set$made))))

      model_results$trained    <- TRUE
      model_results$accuracy   <- acc
      model_results$auc        <- auc(roc_obj)
      model_results$n_test     <- nrow(test_set)
      model_results$roc_obj    <- roc_obj
      model_results$cm         <- cm
      model_results$importance <- imp_df
      model_results$model_obj  <- fit

      incProgress(0.2, detail = "Done!")
    })
  })

  output$model_vb_acc <- renderValueBox({
    acc <- if (model_results$trained) percent(model_results$accuracy, accuracy = 0.1) else "—"
    valueBox(acc, "Accuracy", icon = icon("check"), color = "green")
  })
  output$model_vb_auc <- renderValueBox({
    au <- if (model_results$trained) round(model_results$auc, 3) else "—"
    valueBox(au, "AUC-ROC", icon = icon("chart-area"), color = "blue")
  })
  output$model_vb_n <- renderValueBox({
    n <- if (model_results$trained) format(model_results$n_test, big.mark = ",") else "—"
    valueBox(n, "Test Samples", icon = icon("vial"), color = "purple")
  })

  output$model_roc <- renderPlot({
    req(model_results$trained)
    roc_obj <- model_results$roc_obj
    par(bg = "#16213e", col.axis = "#999", col.lab = "#ccc", col.main = "#e94560",
        fg = "#999")
    plot(roc_obj, col = "#e94560", lwd = 3, main = paste0("AUC = ", round(auc(roc_obj), 3)))
    abline(a = 0, b = 1, lty = 2, col = "#555")
  }, bg = "transparent")

  output$model_cm <- renderPlot({
    req(model_results$trained)
    cm <- model_results$cm
    cm_table <- as.data.frame(cm$table)
    ggplot(cm_table, aes(x = Reference, y = Prediction, fill = Freq)) +
      geom_tile(color = "#0a0a23") +
      geom_text(aes(label = format(Freq, big.mark = ",")), color = "white", size = 6, fontface = "bold") +
      scale_fill_gradient(low = "#0f3460", high = "#e94560") +
      labs(x = "Actual", y = "Predicted") +
      scale_x_discrete(labels = c("Missed","Made")) +
      scale_y_discrete(labels = c("Missed","Made")) +
      theme_minimal(base_size = 14) +
      theme(
        plot.background  = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        text = element_text(color = "#ccc"),
        axis.text = element_text(color = "#ddd"),
        legend.position = "none"
      )
  }, bg = "transparent")

  output$model_importance <- renderPlotly({
    req(model_results$trained)
    d <- model_results$importance
    plot_ly(d, x = ~reorder(feature, importance), y = ~importance, type = "bar",
            orientation = "h", marker = list(color = "#e94560")) %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#ccc"),
             xaxis = list(title = "Importance"), yaxis = list(title = ""))
  })

  # ---- Calculator Tab --------------------------------------------------------
  calc_model <- reactive({
    # Train a quick logistic regression on all data for the calculator
    d <- shots %>%
      select(SHOT_DIST, CLOSE_DEF_DIST, SHOT_CLOCK, DRIBBLES, TOUCH_TIME,
             shot_zone, pressure_cat, QUARTER, made) %>%
      drop_na()
    d$shot_zone     <- as.factor(d$shot_zone)
    d$pressure_cat  <- as.factor(d$pressure_cat)
    d$QUARTER       <- as.factor(d$QUARTER)
    d$made          <- as.numeric(d$made)
    glm(made ~ ., data = d, family = binomial())
  })

  calc_prob <- eventReactive(input$calc_predict, {
    fit <- calc_model()
    new_data <- data.frame(
      SHOT_DIST      = input$calc_dist,
      CLOSE_DEF_DIST = input$calc_def,
      SHOT_CLOCK     = input$calc_clock,
      DRIBBLES       = input$calc_dribbles,
      TOUCH_TIME     = input$calc_touch,
      shot_zone      = factor(input$calc_zone, levels = levels(factor(shots$shot_zone))),
      pressure_cat   = factor(input$calc_pressure,
                              levels = levels(factor(shots$pressure_cat))),
      QUARTER        = factor(input$calc_quarter, levels = as.character(1:4))
    )
    predict(fit, new_data, type = "response")
  })

  output$calc_result <- renderText({
    req(input$calc_predict)
    paste0(round(calc_prob() * 100, 1), "%")
  })

  output$calc_context <- renderText({
    req(input$calc_predict)
    overall <- mean(shots$made, na.rm = TRUE)
    diff <- calc_prob() - overall
    direction <- if (diff >= 0) "above" else "below"
    paste0("This is ", round(abs(diff)*100, 1), " percentage points ", direction,
           " the league average FG% of ", round(overall*100, 1), "%.")
  })

  output$calc_comparison <- renderPlotly({
    req(input$calc_predict)
    prob <- calc_prob()
    overall  <- mean(shots$made, na.rm = TRUE)
    zone_avg <- shots %>% filter(shot_zone == input$calc_zone) %>%
      summarise(fg = mean(made, na.rm = TRUE)) %>% pull(fg)
    pres_avg <- shots %>% filter(pressure_cat == input$calc_pressure) %>%
      summarise(fg = mean(made, na.rm = TRUE)) %>% pull(fg)

    d <- data.frame(
      Category = c("Your Shot", "League Average",
                    paste(input$calc_zone, "Avg"),
                    paste(input$calc_pressure, "Avg")),
      Probability = c(prob, overall, zone_avg, pres_avg)
    )
    d$Category <- factor(d$Category, levels = d$Category)
    colors <- c("#e94560", "#0f3460", "#533483", "#f39c12")
    plot_ly(d, x = ~Category, y = ~Probability, type = "bar",
            marker = list(color = colors),
            text = ~paste0(round(Probability*100,1), "%"), textposition = "auto") %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#ccc"),
             xaxis = list(title = ""), yaxis = list(title = "Make Probability", tickformat = ".0%"))
  })

} # end server

# ==============================================================================
# Run
# ==============================================================================
shinyApp(ui = ui, server = server)
