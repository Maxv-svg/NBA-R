# =============================================================================
# NBA Shot Analytics Dashboard — 2014-2015 Season
# McKinsey-style Consulting Design
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
  skin = "blue",

  dashboardHeader(
    title = span(
      "NBA Shot Analytics",
      style = "font-family: Georgia, serif; font-weight:700; font-size:15px;
               letter-spacing: 0.5px; color:#003A70;"
    ),
    titleWidth = 280
  ),

  dashboardSidebar(
    width = 260,
    sidebarMenu(
      id = "tabs",
      menuItem("Executive Summary",  tabName = "overview",   icon = icon("chart-line")),
      menuItem("Data Profile",       tabName = "profile",    icon = icon("table")),
      menuItem("Shot Explorer",      tabName = "explorer",   icon = icon("filter")),
      menuItem("Court Heatmap",      tabName = "court",      icon = icon("th")),
      menuItem("Player Analysis",    tabName = "player",     icon = icon("user")),
      menuItem("Defender Impact",    tabName = "defender",    icon = icon("shield-alt")),
      menuItem("Clutch Analysis",    tabName = "clutch",     icon = icon("clock")),
      menuItem("Predictive Model",   tabName = "model",      icon = icon("cogs")),
      menuItem("Probability Calc.",  tabName = "calculator",  icon = icon("calculator"))
    ),
    br(),
    div(
      style = "padding: 10px 18px; color: #9CA3AF; font-size: 10px;
               font-family: Georgia, serif; line-height: 1.6;
               border-top: 1px solid #374151; margin-top: 10px; padding-top: 15px;",
      "NBA Shot Logs 2014-15", br(),
      "Source: stats.nba.com", br(),
      "SportVU Player Tracking"
    )
  ),

  dashboardBody(
    tags$head(
      tags$style(HTML("

      /* ============================================ */
      /* McKINSEY CONSULTING THEME                    */
      /* ============================================ */

      body, .content-wrapper, .right-side {
        background-color: #F8F9FA !important;
        font-family: 'Helvetica Neue', Arial, sans-serif;
        color: #1F2937;
      }

      /* --- Header --- */
      .skin-blue .main-header .logo {
        background-color: #FFFFFF !important;
        color: #003A70 !important;
        border-bottom: 2px solid #003A70;
        font-family: Georgia, serif;
      }
      .skin-blue .main-header .logo:hover { background-color: #F8F9FA !important; }
      .skin-blue .main-header .navbar {
        background-color: #FFFFFF !important;
        border-bottom: 1px solid #E5E7EB;
      }
      .skin-blue .main-header .navbar .sidebar-toggle { color: #003A70; }

      /* --- Sidebar --- */
      .skin-blue .main-sidebar, .skin-blue .left-side {
        background-color: #001E3C !important;
      }
      .skin-blue .sidebar-menu > li > a {
        color: #CBD5E1 !important;
        font-size: 13px;
        font-weight: 400;
        border-left: 3px solid transparent;
        padding: 11px 18px;
      }
      .skin-blue .sidebar-menu > li > a:hover {
        background-color: #003A70 !important;
        color: #FFFFFF !important;
      }
      .skin-blue .sidebar-menu > li.active > a {
        background-color: rgba(0, 119, 200, 0.15) !important;
        color: #FFFFFF !important;
        border-left: 3px solid #0077C8 !important;
        font-weight: 600;
      }
      .skin-blue .sidebar a { color: #94A3B8; }
      .sidebar-menu > li > a > .fa { color: #64748B; width: 22px; }
      .skin-blue .sidebar-menu > li.active > a > .fa { color: #0077C8; }

      /* --- Boxes --- */
      .box {
        background-color: #FFFFFF !important;
        border: 1px solid #E5E7EB !important;
        border-top: 3px solid #003A70 !important;
        border-radius: 2px !important;
        box-shadow: 0 1px 3px rgba(0,0,0,0.06) !important;
        color: #1F2937 !important;
      }
      .box-header { color: #1F2937 !important; border-bottom: 1px solid #F3F4F6; }
      .box-header .box-title {
        font-size: 12px !important;
        font-weight: 700 !important;
        text-transform: uppercase;
        letter-spacing: 0.8px;
        color: #003A70 !important;
      }
      .box-body { padding: 15px 20px; }

      /* --- Value Boxes --- */
      .small-box { border-radius: 2px !important; box-shadow: 0 1px 3px rgba(0,0,0,0.06); }
      .small-box .inner h3 { font-size: 26px; font-weight: 700; font-family: Georgia, serif; }
      .small-box .inner p  { font-size: 11px; text-transform: uppercase; letter-spacing: 0.8px; font-weight: 600; }
      .small-box .icon-large { font-size: 50px; opacity: 0.15; }
      .bg-navy   { background-color: #003A70 !important; }
      .bg-blue   { background-color: #0077C8 !important; }
      .bg-teal   { background-color: #00857C !important; }
      .bg-olive  { background-color: #4B5563 !important; }
      .bg-green  { background-color: #0F766E !important; }
      .bg-yellow { background-color: #B45309 !important; }
      .bg-red    { background-color: #B91C1C !important; }
      .bg-purple { background-color: #003A70 !important; }

      /* --- KPI Cards --- */
      .kpi-card {
        background: #FFFFFF;
        border: 1px solid #E5E7EB;
        border-top: 3px solid #0077C8;
        border-radius: 2px;
        padding: 24px 16px;
        text-align: center;
        margin-bottom: 15px;
      }
      .kpi-card .kpi-value {
        font-size: 32px; font-weight: 700; color: #003A70; font-family: Georgia, serif;
      }
      .kpi-card .kpi-label {
        font-size: 10px; color: #6B7280; text-transform: uppercase;
        letter-spacing: 1.5px; margin-top: 6px; font-weight: 600;
      }

      /* --- Tables --- */
      .dataTables_wrapper { color: #374151; }
      table.dataTable { color: #1F2937 !important; font-size: 12px; }
      table.dataTable thead th {
        color: #003A70 !important; font-weight: 700 !important;
        text-transform: uppercase; font-size: 10px; letter-spacing: 0.8px;
        border-bottom: 2px solid #003A70 !important; background-color: #F8F9FA !important;
      }
      table.dataTable tbody tr { background-color: #FFFFFF !important; }
      table.dataTable tbody tr:nth-child(even) { background-color: #F9FAFB !important; }
      table.dataTable tbody tr:hover { background-color: #EFF6FF !important; }
      table.dataTable tbody td { border-bottom: 1px solid #F3F4F6; }

      /* --- Form Inputs --- */
      .selectize-input, .selectize-dropdown {
        background-color: #FFFFFF !important; color: #1F2937 !important;
        border-color: #D1D5DB !important; border-radius: 2px !important; font-size: 13px;
      }
      .selectize-dropdown-content .option.active { background-color: #0077C8; color: #fff; }
      .form-control { background-color: #FFFFFF; color: #1F2937; border: 1px solid #D1D5DB; border-radius: 2px; font-size: 13px; }
      label { color: #374151 !important; font-size: 11px; font-weight: 600; text-transform: uppercase; letter-spacing: 0.5px; }

      /* --- Sliders --- */
      .irs--shiny .irs-bar { background: #0077C8; border-top-color: #0077C8; border-bottom-color: #0077C8; }
      .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background-color: #003A70; font-size: 10px; }
      .irs--shiny .irs-line { background: #E5E7EB; }

      /* --- Section Headers --- */
      h3.section-title {
        color: #003A70; font-family: Georgia, serif; font-weight: 700;
        font-size: 22px; border-bottom: 2px solid #003A70;
        padding-bottom: 8px; margin-bottom: 24px;
      }
      h4, h5 { color: #374151 !important; }
      .checkbox label { color: #374151 !important; font-weight: 400 !important; text-transform: none !important; }

      "))
    ),

    tabItems(

      # ---- TAB 1: EXECUTIVE SUMMARY ------------------------------------------
      tabItem(tabName = "overview",
        fluidRow(column(12, h3("Executive Summary \u2014 2014-15 NBA Shot Analysis", class = "section-title"))),
        fluidRow(
          valueBoxOutput("vb_total_shots",  width = 3),
          valueBoxOutput("vb_fg_pct",       width = 3),
          valueBoxOutput("vb_avg_dist",     width = 3),
          valueBoxOutput("vb_players",      width = 3)
        ),
        fluidRow(
          box(title = "FG% by Shot Zone", width = 4, solidHeader = TRUE, plotlyOutput("overview_zone_bar", height = "320px")),
          box(title = "Shot Volume by Quarter", width = 4, solidHeader = TRUE, plotlyOutput("overview_quarter_bar", height = "320px")),
          box(title = "FG% by Defender Proximity", width = 4, solidHeader = TRUE, plotlyOutput("overview_defender_bar", height = "320px"))
        ),
        fluidRow(
          box(title = "Make Rate by Distance", width = 6, solidHeader = TRUE, plotlyOutput("overview_dist_line", height = "300px")),
          box(title = "Top 15 Players by Shot Volume", width = 6, solidHeader = TRUE, plotlyOutput("overview_top_players", height = "300px"))
        )
      ),

      # ---- TAB 2: DATA PROFILE ------------------------------------------------
      tabItem(tabName = "profile",
        fluidRow(column(12, h3("Data Profile & Quality Assessment", class = "section-title"))),
        fluidRow(
          box(title = "Dataset Overview", width = 12, solidHeader = TRUE,
            fluidRow(
              column(3, div(class = "kpi-card", div(class = "kpi-value", textOutput("prof_rows")), div(class = "kpi-label", "Total Observations"))),
              column(3, div(class = "kpi-card", div(class = "kpi-value", textOutput("prof_cols")), div(class = "kpi-label", "Features"))),
              column(3, div(class = "kpi-card", div(class = "kpi-value", textOutput("prof_na_pct")), div(class = "kpi-label", "Missing Values"))),
              column(3, div(class = "kpi-card", div(class = "kpi-value", textOutput("prof_balance")), div(class = "kpi-label", "Made / Missed")))
            )
          )
        ),
        fluidRow(
          box(title = "Column Inventory", width = 6, solidHeader = TRUE, DTOutput("prof_column_table")),
          box(title = "Target Distribution", width = 6, solidHeader = TRUE,
              plotlyOutput("prof_balance_plot", height = "250px"), br(),
              h5("Numerical Feature Distributions", style = "color:#003A70; font-weight:700; font-size:11px; text-transform:uppercase; letter-spacing:0.8px;"),
              plotlyOutput("prof_numeric_hist", height = "350px"))
        ),
        fluidRow(box(title = "Feature Correlation Matrix", width = 12, solidHeader = TRUE, plotOutput("prof_corr_plot", height = "500px")))
      ),

      # ---- TAB 3: SHOT EXPLORER -----------------------------------------------
      tabItem(tabName = "explorer",
        fluidRow(column(12, h3("Interactive Shot Explorer", class = "section-title"))),
        fluidRow(
          box(title = "Filter Parameters", width = 3, solidHeader = TRUE,
            selectizeInput("exp_player", "Player:", choices = c("All", sort(unique(shots$player_name))), selected = "All"),
            selectizeInput("exp_team", "Team:", choices = c("All", sort(unique(shots$team))), selected = "All"),
            selectInput("exp_zone", "Shot Zone:", choices = c("All", "Paint", "Mid-Range", "Three-Point"), selected = "All"),
            selectInput("exp_pressure", "Defender Pressure:", choices = c("All", "Wide Open", "Open", "Contested", "Tightly Contested"), selected = "All"),
            sliderInput("exp_dist", "Shot Distance (ft):", min = 0, max = 40, value = c(0, 40), step = 1),
            sliderInput("exp_clock", "Shot Clock (sec):", min = 0, max = 24, value = c(0, 24), step = 1),
            checkboxInput("exp_clutch", "Clutch Time Only", FALSE),
            actionButton("exp_apply", "Apply Filters", style = "background-color:#003A70; color:white; width:100%; border:none; font-weight:600; border-radius:2px; padding:10px; font-size:11px; text-transform:uppercase; letter-spacing:1px;")
          ),
          column(9,
            fluidRow(valueBoxOutput("exp_vb_shots", width = 4), valueBoxOutput("exp_vb_fg", width = 4), valueBoxOutput("exp_vb_avg_def", width = 4)),
            box(title = "Shot Distribution", width = 12, solidHeader = TRUE, plotlyOutput("exp_scatter", height = "400px")),
            box(title = "Filtered Data", width = 12, solidHeader = TRUE, DTOutput("exp_table"))
          )
        )
      ),

      # ---- TAB 4: COURT HEATMAP -----------------------------------------------
      tabItem(tabName = "court",
        fluidRow(column(12, h3("Court Shot Distribution", class = "section-title"))),
        fluidRow(
          box(title = "Parameters", width = 3, solidHeader = TRUE,
            selectizeInput("court_player", "Player:", choices = c("All", sort(unique(shots$player_name))), selected = "All"),
            selectInput("court_metric", "Metric:", choices = c("Shot Count" = "count", "FG%" = "fg_pct"), selected = "fg_pct"),
            selectInput("court_result", "Result:", choices = c("All", "Made", "Missed"), selected = "All"),
            sliderInput("court_min_shots", "Min. Sample Size:", min = 1, max = 50, value = 5)
          ),
          box(title = "Shot Chart by Distance Zone", width = 9, solidHeader = TRUE, plotOutput("court_heatmap", height = "600px"))
        )
      ),

      # ---- TAB 5: PLAYER ANALYSIS -----------------------------------------------
      tabItem(tabName = "player",
        fluidRow(column(12, h3("Player Shooting Profile", class = "section-title"))),
        fluidRow(
          box(width = 12, solidHeader = TRUE,
            fluidRow(
              column(4, selectizeInput("player_sel", "Primary Player:", choices = player_summary$player_name, selected = player_summary$player_name[1])),
              column(4, selectizeInput("player_comp", "Comparison Player:", choices = c("None", player_summary$player_name), selected = "None")),
              column(4, br(), actionButton("player_go", "Run Analysis", style = "background-color:#003A70; color:white; border:none; font-weight:600; padding:8px 30px; border-radius:2px; font-size:11px; text-transform:uppercase; letter-spacing:1px;"))
            )
          )
        ),
        fluidRow(
          box(title = "Efficiency by Shot Zone", width = 6, solidHeader = TRUE, plotlyOutput("player_zone_chart", height = "350px")),
          box(title = "FG% by Distance Range", width = 6, solidHeader = TRUE, plotlyOutput("player_dist_chart", height = "350px"))
        ),
        fluidRow(
          box(title = "Impact of Defender Proximity", width = 6, solidHeader = TRUE, plotlyOutput("player_def_chart", height = "320px")),
          box(title = "Quarterly Performance Trend", width = 6, solidHeader = TRUE, plotlyOutput("player_quarter_chart", height = "320px"))
        )
      ),

      # ---- TAB 6: DEFENDER IMPACT -----------------------------------------------
      tabItem(tabName = "defender",
        fluidRow(column(12, h3("Defensive Pressure Analysis", class = "section-title"))),
        fluidRow(
          box(title = "FG% vs Defender Distance", width = 6, solidHeader = TRUE, plotlyOutput("def_scatter", height = "400px")),
          box(title = "Pressure x Zone Matrix", width = 6, solidHeader = TRUE, plotlyOutput("def_heatmap", height = "400px"))
        ),
        fluidRow(
          box(title = "Top Defenders by Opponent FG% Suppression", width = 6, solidHeader = TRUE, DTOutput("def_best_table")),
          box(title = "Defender Distance Distribution", width = 6, solidHeader = TRUE, plotlyOutput("def_density", height = "350px"))
        )
      ),

      # ---- TAB 7: CLUTCH ANALYSIS -----------------------------------------------
      tabItem(tabName = "clutch",
        fluidRow(column(12, h3("Clutch Performance \u2014 Final 5 Minutes", class = "section-title"))),
        fluidRow(
          valueBoxOutput("clutch_vb_shots", width = 3), valueBoxOutput("clutch_vb_fg", width = 3),
          valueBoxOutput("clutch_vb_nonclutch_fg", width = 3), valueBoxOutput("clutch_vb_diff", width = 3)
        ),
        fluidRow(
          box(title = "Clutch vs Non-Clutch by Zone", width = 6, solidHeader = TRUE, plotlyOutput("clutch_zone_chart", height = "350px")),
          box(title = "Top Clutch Performers (min 20 attempts)", width = 6, solidHeader = TRUE, plotlyOutput("clutch_top_players", height = "350px"))
        ),
        fluidRow(box(title = "Clutch FG% by Shot Clock Window", width = 12, solidHeader = TRUE, plotlyOutput("clutch_clock_chart", height = "300px")))
      ),

      # ---- TAB 8: PREDICTIVE MODEL -----------------------------------------------
      tabItem(tabName = "model",
        fluidRow(column(12, h3("Predictive Model \u2014 Shot Outcome Classification", class = "section-title"))),
        fluidRow(
          box(title = "Model Configuration", width = 4, solidHeader = TRUE,
            selectInput("model_type", "Algorithm:", choices = c("Logistic Regression" = "glm", "Random Forest" = "rf"), selected = "glm"),
            sliderInput("model_split", "Training Split:", min = 60, max = 90, value = 80, step = 5, post = "%"),
            checkboxGroupInput("model_features", "Feature Selection:",
              choices = c("Shot Distance" = "SHOT_DIST", "Defender Distance" = "CLOSE_DEF_DIST", "Shot Clock" = "SHOT_CLOCK",
                          "Dribbles" = "DRIBBLES", "Touch Time" = "TOUCH_TIME", "Shot Zone" = "shot_zone",
                          "Defender Pressure" = "pressure_cat", "Quarter" = "QUARTER", "Home/Away" = "location", "Shot Number" = "SHOT_NUMBER"),
              selected = c("SHOT_DIST", "CLOSE_DEF_DIST", "SHOT_CLOCK", "shot_zone", "pressure_cat")),
            actionButton("model_train", "Train Model", style = "background-color:#003A70; color:white; width:100%; border:none; font-weight:700; padding:10px; border-radius:2px; font-size:11px; text-transform:uppercase; letter-spacing:1px;")
          ),
          column(8,
            fluidRow(valueBoxOutput("model_vb_acc", width = 4), valueBoxOutput("model_vb_auc", width = 4), valueBoxOutput("model_vb_n", width = 4)),
            box(title = "ROC Curve", width = 6, solidHeader = TRUE, plotOutput("model_roc", height = "350px")),
            box(title = "Confusion Matrix", width = 6, solidHeader = TRUE, plotOutput("model_cm", height = "350px")),
            box(title = "Feature Importance Ranking", width = 12, solidHeader = TRUE, plotlyOutput("model_importance", height = "300px"))
          )
        )
      ),

      # ---- TAB 9: CALCULATOR -----------------------------------------------
      tabItem(tabName = "calculator",
        fluidRow(column(12, h3("Shot Make Probability Calculator", class = "section-title"))),
        fluidRow(
          box(title = "Input Parameters", width = 5, solidHeader = TRUE,
            sliderInput("calc_dist", "Shot Distance (ft):", 0, 35, 15, 0.5),
            sliderInput("calc_def", "Closest Defender (ft):", 0, 25, 4, 0.5),
            sliderInput("calc_clock", "Shot Clock (sec):", 0, 24, 12, 1),
            sliderInput("calc_dribbles", "Dribbles:", 0, 15, 2, 1),
            sliderInput("calc_touch", "Touch Time (sec):", 0, 24, 3, 0.5),
            selectInput("calc_zone", "Shot Zone:", choices = c("Paint", "Mid-Range", "Three-Point")),
            selectInput("calc_pressure", "Defender Pressure:", choices = c("Wide Open", "Open", "Contested", "Tightly Contested")),
            selectInput("calc_quarter", "Quarter:", choices = 1:4, selected = 1),
            actionButton("calc_predict", "Calculate", style = "background-color:#003A70; color:white; width:100%; border:none; font-weight:700; padding:12px; font-size:13px; border-radius:2px; text-transform:uppercase; letter-spacing:1px;")
          ),
          column(7,
            div(style = "text-align:center; padding-top:40px;",
              h4("Predicted Make Probability", style = "color:#6B7280; font-size:11px; text-transform:uppercase; letter-spacing:2px; font-weight:600;"),
              div(style = "font-size:72px; font-weight:700; color:#003A70; margin:20px 0; font-family:Georgia, serif;", textOutput("calc_result")),
              div(style = "font-size:13px; color:#6B7280; max-width:400px; margin:0 auto; line-height:1.6;", textOutput("calc_context"))
            ),
            box(title = "Benchmark Comparison", width = 12, solidHeader = TRUE, plotlyOutput("calc_comparison", height = "300px"))
          )
        )
      )

    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage


# ==============================================================================
# SERVER
# ==============================================================================

# --- Plotly consulting layout helper ------------------------------------------
mckinsey_layout <- function(p, ...) {
  p %>% layout(
    paper_bgcolor = "transparent", plot_bgcolor = "#FFFFFF",
    font = list(family = "Helvetica Neue, Arial, sans-serif", color = "#374151", size = 11),
    xaxis = list(gridcolor = "#F3F4F6", zerolinecolor = "#E5E7EB"),
    yaxis = list(gridcolor = "#F3F4F6", zerolinecolor = "#E5E7EB"),
    margin = list(t = 30, b = 40, l = 50, r = 20),
    ...
  )
}

# --- ggplot consulting theme --------------------------------------------------
theme_mckinsey <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.background  = element_rect(fill = "#FFFFFF", colour = NA),
      panel.background = element_rect(fill = "#FFFFFF", colour = NA),
      panel.grid.major = element_line(color = "#F3F4F6"),
      panel.grid.minor = element_blank(),
      text = element_text(color = "#374151"),
      axis.text = element_text(color = "#6B7280", size = 9),
      strip.text = element_text(color = "#003A70", face = "bold", size = 10),
      plot.title = element_text(color = "#003A70", face = "bold", size = 13)
    )
}

# --- Color constants ----------------------------------------------------------
mc_primary  <- "#003A70"
mc_blue     <- "#0077C8"
mc_teal     <- "#00857C"
mc_gray     <- "#94A3B8"
mc_warm     <- "#B45309"
mc_3colors  <- c("#003A70", "#0077C8", "#00857C")
mc_4colors  <- c("#003A70", "#0077C8", "#00857C", "#B45309")
mc_2colors  <- c("#003A70", "#0077C8")
mc_result   <- c("#B91C1C", "#0F766E")


server <- function(input, output, session) {

  # ---- OVERVIEW TAB ----------------------------------------------------------
  output$vb_total_shots <- renderValueBox(valueBox(format(nrow(shots), big.mark = ","), "Total Shot Attempts", icon = icon("chart-bar"), color = "navy"))
  output$vb_fg_pct <- renderValueBox(valueBox(percent(mean(shots$made, na.rm = TRUE), accuracy = 0.1), "League FG%", icon = icon("bullseye"), color = "blue"))
  output$vb_avg_dist <- renderValueBox(valueBox(paste0(round(mean(shots$SHOT_DIST, na.rm = TRUE), 1), " ft"), "Avg Shot Distance", icon = icon("ruler"), color = "teal"))
  output$vb_players <- renderValueBox(valueBox(length(unique(shots$player_name)), "Unique Players", icon = icon("users"), color = "olive"))

  output$overview_zone_bar <- renderPlotly({
    d <- shots %>% group_by(shot_zone) %>% summarise(fg_pct = mean(made, na.rm = TRUE), n = n(), .groups = "drop")
    plot_ly(d, x = ~shot_zone, y = ~fg_pct, type = "bar", marker = list(color = mc_3colors),
            text = ~paste0(round(fg_pct*100,1), "%  (n=", format(n, big.mark=","), ")"),
            textposition = "auto", textfont = list(color = "#fff", size = 11)) %>%
      mckinsey_layout(xaxis = list(title = ""), yaxis = list(title = "FG%", tickformat = ".0%"))
  })

  output$overview_quarter_bar <- renderPlotly({
    d <- shots %>% group_by(QUARTER) %>% summarise(n = n(), fg_pct = mean(made, na.rm = TRUE), .groups = "drop")
    plot_ly(d, x = ~factor(QUARTER), y = ~n, type = "bar", marker = list(color = mc_primary),
            text = ~paste0("FG%: ", round(fg_pct*100,1), "%"), textposition = "auto", textfont = list(color = "#fff", size = 11)) %>%
      mckinsey_layout(xaxis = list(title = "Quarter"), yaxis = list(title = "Shot Count"))
  })

  output$overview_defender_bar <- renderPlotly({
    d <- shots %>% filter(!is.na(pressure_cat)) %>% group_by(pressure_cat) %>%
      summarise(fg_pct = mean(made, na.rm = TRUE), n = n(), .groups = "drop") %>%
      mutate(pressure_cat = factor(pressure_cat, levels = c("Wide Open", "Open", "Contested", "Tightly Contested")))
    plot_ly(d, x = ~pressure_cat, y = ~fg_pct, type = "bar", marker = list(color = mc_4colors),
            text = ~paste0(round(fg_pct*100,1), "%"), textposition = "auto", textfont = list(color = "#fff", size = 11)) %>%
      mckinsey_layout(xaxis = list(title = ""), yaxis = list(title = "FG%", tickformat = ".0%"))
  })

  output$overview_dist_line <- renderPlotly({
    d <- shots %>% mutate(dist_bin = floor(SHOT_DIST)) %>% group_by(dist_bin) %>%
      summarise(fg_pct = mean(made, na.rm = TRUE), n = n(), .groups = "drop") %>% filter(n >= 50)
    plot_ly(d, x = ~dist_bin, y = ~fg_pct, type = "scatter", mode = "lines+markers",
            line = list(color = mc_primary, width = 2.5), marker = list(color = mc_primary, size = 5)) %>%
      mckinsey_layout(xaxis = list(title = "Distance (ft)"), yaxis = list(title = "FG%", tickformat = ".0%"))
  })

  output$overview_top_players <- renderPlotly({
    d <- player_summary %>% slice_max(total_shots, n = 15)
    plot_ly(d, x = ~reorder(player_name, total_shots), y = ~total_shots, type = "bar", orientation = "h",
            marker = list(color = mc_primary), text = ~paste0("FG%: ", round(fg_pct*100,1), "%"),
            textposition = "auto", textfont = list(color = "#fff", size = 10)) %>%
      mckinsey_layout(xaxis = list(title = "Total Shots"), yaxis = list(title = ""))
  })

  # ---- DATA PROFILE TAB -------------------------------------------------------
  output$prof_rows <- renderText(format(nrow(shots), big.mark = ","))
  output$prof_cols <- renderText(ncol(raw_data))
  output$prof_na_pct <- renderText(paste0(round(sum(is.na(raw_data)) / (nrow(raw_data)*ncol(raw_data)) * 100, 2), "%"))
  output$prof_balance <- renderText(paste0(round(mean(shots$made)*100, 1), " / ", round((1-mean(shots$made))*100, 1)))

  output$prof_column_table <- renderDT({
    col_info <- data.frame(
      Column = names(raw_data), Type = sapply(raw_data, class),
      Missing = sapply(raw_data, function(x) sum(is.na(x))),
      Pct_Miss = sapply(raw_data, function(x) round(sum(is.na(x))/length(x)*100, 2)),
      Unique = sapply(raw_data, function(x) length(unique(x))), stringsAsFactors = FALSE)
    datatable(col_info, options = list(pageLength = 25, dom = "tp"), rownames = FALSE, style = "bootstrap4")
  })

  output$prof_balance_plot <- renderPlotly({
    d <- shots %>% count(shot_result = ifelse(made == 1, "Made", "Missed"))
    plot_ly(d, x = ~shot_result, y = ~n, type = "bar", marker = list(color = c(mc_teal, mc_primary)),
            text = ~paste0(format(n, big.mark=","), " (", round(n/sum(n)*100,1), "%)"),
            textposition = "auto", textfont = list(color = "#fff", size = 12)) %>%
      mckinsey_layout(xaxis = list(title = ""), yaxis = list(title = "Count"))
  })

  output$prof_numeric_hist <- renderPlotly({
    num_cols <- c("SHOT_DIST", "CLOSE_DEF_DIST", "SHOT_CLOCK", "TOUCH_TIME", "DRIBBLES")
    existing <- num_cols[num_cols %in% names(shots)]
    d <- shots %>% select(all_of(existing)) %>% pivot_longer(everything(), names_to = "variable", values_to = "value") %>% filter(!is.na(value))
    ggp <- ggplot(d, aes(x = value, fill = variable)) +
      geom_histogram(bins = 40, alpha = 0.85) +
      facet_wrap(~variable, scales = "free", ncol = 3) +
      scale_fill_manual(values = c(mc_primary, mc_blue, mc_teal, mc_warm, mc_gray)) +
      theme_mckinsey() + theme(legend.position = "none")
    ggplotly(ggp) %>% mckinsey_layout()
  })

  output$prof_corr_plot <- renderPlot({
    num_cols <- c("SHOT_DIST","CLOSE_DEF_DIST","SHOT_CLOCK","TOUCH_TIME","DRIBBLES","SHOT_NUMBER","QUARTER")
    existing <- num_cols[num_cols %in% names(shots)]
    cor_mat <- cor(shots[, existing], use = "pairwise.complete.obs")
    par(bg = "#FFFFFF")
    corrplot(cor_mat, method = "color", type = "lower", tl.col = "#003A70",
             addCoef.col = "#374151", number.cex = 0.9,
             col = colorRampPalette(c("#0077C8","#FFFFFF","#003A70"))(200), cl.cex = 0.8, tl.cex = 0.9)
  }, bg = "#FFFFFF")

  # ---- SHOT EXPLORER TAB -------------------------------------------------------
  filtered_shots <- eventReactive(input$exp_apply, {
    d <- shots
    if (input$exp_player != "All") d <- d %>% filter(player_name == input$exp_player)
    if (input$exp_team   != "All") d <- d %>% filter(team == input$exp_team)
    if (input$exp_zone   != "All") d <- d %>% filter(shot_zone == input$exp_zone)
    if (input$exp_pressure != "All") d <- d %>% filter(pressure_cat == input$exp_pressure)
    d <- d %>% filter(SHOT_DIST >= input$exp_dist[1], SHOT_DIST <= input$exp_dist[2])
    if (!all(is.na(d$SHOT_CLOCK))) d <- d %>% filter(is.na(SHOT_CLOCK) | (SHOT_CLOCK >= input$exp_clock[1] & SHOT_CLOCK <= input$exp_clock[2]))
    if (input$exp_clutch) d <- d %>% filter(clutch == 1)
    d
  }, ignoreNULL = FALSE)

  output$exp_vb_shots <- renderValueBox(valueBox(format(nrow(filtered_shots()), big.mark = ","), "Shots", icon = icon("chart-bar"), color = "navy"))
  output$exp_vb_fg <- renderValueBox({
    fg <- if (nrow(filtered_shots()) > 0) mean(filtered_shots()$made, na.rm = TRUE) else 0
    valueBox(percent(fg, accuracy = 0.1), "FG%", icon = icon("bullseye"), color = "blue")
  })
  output$exp_vb_avg_def <- renderValueBox({
    ad <- if (nrow(filtered_shots()) > 0) mean(filtered_shots()$CLOSE_DEF_DIST, na.rm = TRUE) else 0
    valueBox(paste0(round(ad, 1), " ft"), "Avg Defender Dist", icon = icon("shield-alt"), color = "teal")
  })

  output$exp_scatter <- renderPlotly({
    d <- filtered_shots()
    if (nrow(d) == 0) return(plotly_empty())
    if (nrow(d) > 5000) d <- d %>% sample_n(5000)
    plot_ly(d, x = ~SHOT_DIST, y = ~CLOSE_DEF_DIST, color = ~factor(made, labels = c("Missed","Made")),
            colors = mc_result, type = "scatter", mode = "markers", marker = list(size = 4, opacity = 0.45),
            text = ~paste0(player_name, "\n", shot_zone, " | ", pressure_cat, "\nDist: ", SHOT_DIST, "ft")) %>%
      mckinsey_layout(xaxis = list(title = "Shot Distance (ft)"), yaxis = list(title = "Closest Defender (ft)"))
  })

  output$exp_table <- renderDT({
    d <- filtered_shots() %>% select(player_name, team, shot_zone, pressure_cat, SHOT_DIST, CLOSE_DEF_DIST, SHOT_CLOCK, made) %>%
      mutate(Result = ifelse(made == 1, "Made", "Missed")) %>% select(-made) %>% head(500)
    datatable(d, options = list(pageLength = 10, scrollX = TRUE, dom = "ftp"), rownames = FALSE, style = "bootstrap4")
  })

  # ---- COURT HEATMAP TAB -------------------------------------------------------
  output$court_heatmap <- renderPlot({
    d <- shots
    if (input$court_player != "All") d <- d %>% filter(player_name == input$court_player)
    if (input$court_result == "Made")   d <- d %>% filter(made == 1)
    if (input$court_result == "Missed") d <- d %>% filter(made == 0)
    d_zone <- d %>%
      mutate(dist_bin = cut(SHOT_DIST, breaks = c(0,3,8,14,22,24,30,50),
             labels = c("Rim (0-3)","Paint (3-8)","Short Mid (8-14)","Long Mid (14-22)","3PT Corner (22-24)","3PT Wing (24-30)","Deep 3 (30+)"))) %>%
      group_by(dist_bin) %>% summarise(count = n(), fg_pct = mean(made, na.rm = TRUE), .groups = "drop") %>% filter(!is.na(dist_bin))
    metric_col <- if (input$court_metric == "fg_pct") "fg_pct" else "count"
    if (input$court_metric == "fg_pct") d_zone <- d_zone %>% filter(count >= input$court_min_shots)
    draw_court_zones(d_zone, metric_col)
  }, bg = "#FFFFFF")

  # ---- PLAYER ANALYSIS TAB ---------------------------------------------------
  player_data <- eventReactive(input$player_go, {
    p1 <- shots %>% filter(player_name == input$player_sel)
    p2 <- if (input$player_comp != "None") shots %>% filter(player_name == input$player_comp) else NULL
    list(p1 = p1, p2 = p2, name1 = input$player_sel, name2 = input$player_comp)
  }, ignoreNULL = FALSE)

  output$player_zone_chart <- renderPlotly({
    pd <- player_data()
    d1 <- pd$p1 %>% group_by(shot_zone) %>% summarise(fg_pct = mean(made, na.rm = TRUE), n = n(), .groups = "drop") %>% mutate(player = pd$name1)
    if (!is.null(pd$p2)) { d2 <- pd$p2 %>% group_by(shot_zone) %>% summarise(fg_pct = mean(made, na.rm = TRUE), n = n(), .groups = "drop") %>% mutate(player = pd$name2); d1 <- bind_rows(d1, d2) }
    plot_ly(d1, x = ~shot_zone, y = ~fg_pct, color = ~player, type = "bar", text = ~paste0(round(fg_pct*100,1), "%"),
            textposition = "auto", textfont = list(color = "#fff", size = 10), colors = mc_2colors) %>%
      mckinsey_layout(barmode = "group", xaxis = list(title = ""), yaxis = list(title = "FG%", tickformat = ".0%"))
  })

  output$player_dist_chart <- renderPlotly({
    pd <- player_data()
    mdd <- function(df, nm) df %>% mutate(dist_bin = cut(SHOT_DIST, breaks = seq(0,40,5))) %>% group_by(dist_bin) %>% summarise(fg_pct = mean(made, na.rm = TRUE), n = n(), .groups = "drop") %>% filter(!is.na(dist_bin)) %>% mutate(player = nm)
    d <- mdd(pd$p1, pd$name1)
    if (!is.null(pd$p2)) d <- bind_rows(d, mdd(pd$p2, pd$name2))
    plot_ly(d, x = ~dist_bin, y = ~fg_pct, color = ~player, type = "bar", colors = mc_2colors, text = ~paste0(round(fg_pct*100,1),"%"), textposition = "auto", textfont = list(color="#fff",size=10)) %>%
      mckinsey_layout(barmode = "group", xaxis = list(title = "Distance Range"), yaxis = list(title = "FG%", tickformat = ".0%"))
  })

  output$player_def_chart <- renderPlotly({
    pd <- player_data()
    mdf <- function(df, nm) df %>% filter(!is.na(pressure_cat)) %>% group_by(pressure_cat) %>% summarise(fg_pct = mean(made, na.rm = TRUE), .groups = "drop") %>% mutate(player = nm, pressure_cat = factor(pressure_cat, levels = c("Wide Open","Open","Contested","Tightly Contested")))
    d <- mdf(pd$p1, pd$name1)
    if (!is.null(pd$p2)) d <- bind_rows(d, mdf(pd$p2, pd$name2))
    plot_ly(d, x = ~pressure_cat, y = ~fg_pct, color = ~player, type = "bar", colors = mc_2colors) %>%
      mckinsey_layout(barmode = "group", yaxis = list(title = "FG%", tickformat = ".0%"), xaxis = list(title = ""))
  })

  output$player_quarter_chart <- renderPlotly({
    pd <- player_data()
    mqd <- function(df, nm) df %>% group_by(QUARTER) %>% summarise(fg_pct = mean(made, na.rm = TRUE), .groups = "drop") %>% mutate(player = nm)
    d <- mqd(pd$p1, pd$name1)
    if (!is.null(pd$p2)) d <- bind_rows(d, mqd(pd$p2, pd$name2))
    plot_ly(d, x = ~factor(QUARTER), y = ~fg_pct, color = ~player, type = "scatter", mode = "lines+markers",
            colors = mc_2colors, line = list(width = 2.5), marker = list(size = 8)) %>%
      mckinsey_layout(xaxis = list(title = "Quarter"), yaxis = list(title = "FG%", tickformat = ".0%"))
  })

  # ---- DEFENDER IMPACT TAB ---------------------------------------------------
  output$def_scatter <- renderPlotly({
    d <- shots %>% mutate(def_bin = round(CLOSE_DEF_DIST)) %>% group_by(def_bin) %>%
      summarise(fg_pct = mean(made, na.rm = TRUE), n = n(), .groups = "drop") %>% filter(n >= 30)
    plot_ly(d, x = ~def_bin, y = ~fg_pct, size = ~n, type = "scatter", mode = "markers",
            marker = list(color = mc_blue, opacity = 0.6, sizemode = "diameter", sizeref = max(d$n)/30, line = list(color = mc_primary, width = 1))) %>%
      mckinsey_layout(xaxis = list(title = "Closest Defender Distance (ft)"), yaxis = list(title = "FG%", tickformat = ".0%"))
  })

  output$def_heatmap <- renderPlotly({
    d <- shots %>% filter(!is.na(pressure_cat)) %>% group_by(pressure_cat, shot_zone) %>%
      summarise(fg_pct = mean(made, na.rm = TRUE), n = n(), .groups = "drop") %>%
      mutate(pressure_cat = factor(pressure_cat, levels = c("Wide Open","Open","Contested","Tightly Contested")))
    plot_ly(d, x = ~shot_zone, y = ~pressure_cat, z = ~fg_pct, type = "heatmap",
            colorscale = list(c(0,"#E0F2FE"), c(0.5,"#0077C8"), c(1,"#001E3C")),
            text = ~paste0(round(fg_pct*100,1), "% (n=", n, ")"), hoverinfo = "text") %>%
      mckinsey_layout(xaxis = list(title = ""), yaxis = list(title = ""))
  })

  output$def_best_table <- renderDT({
    d <- shots %>% filter(!is.na(CLOSEST_DEFENDER)) %>% group_by(CLOSEST_DEFENDER) %>%
      summarise(shots_defended = n(), opp_fg_pct = round(mean(made, na.rm = TRUE)*100,1), avg_dist = round(mean(CLOSE_DEF_DIST, na.rm = TRUE),1), .groups = "drop") %>%
      filter(shots_defended >= 100) %>% arrange(opp_fg_pct) %>% head(20)
    datatable(d, options = list(pageLength = 10, dom = "tp"), rownames = FALSE, style = "bootstrap4", colnames = c("Defender","Shots Defended","Opp FG%","Avg Distance"))
  })

  output$def_density <- renderPlotly({
    d <- shots %>% filter(!is.na(CLOSE_DEF_DIST)) %>% mutate(result = ifelse(made == 1, "Made", "Missed"))
    plot_ly(alpha = 0.5) %>%
      add_histogram(data = filter(d, result == "Made"), x = ~CLOSE_DEF_DIST, name = "Made", marker = list(color = mc_teal), nbinsx = 50) %>%
      add_histogram(data = filter(d, result == "Missed"), x = ~CLOSE_DEF_DIST, name = "Missed", marker = list(color = mc_primary), nbinsx = 50) %>%
      mckinsey_layout(barmode = "overlay", xaxis = list(title = "Closest Defender Distance (ft)"), yaxis = list(title = "Count"))
  })

  # ---- CLUTCH ANALYSIS TAB ---------------------------------------------------
  clutch_shots     <- reactive(shots %>% filter(clutch == 1))
  non_clutch_shots <- reactive(shots %>% filter(clutch == 0))

  output$clutch_vb_shots <- renderValueBox(valueBox(format(nrow(clutch_shots()), big.mark = ","), "Clutch Attempts", icon = icon("clock"), color = "navy"))
  output$clutch_vb_fg <- renderValueBox({ fg <- if(nrow(clutch_shots())>0) mean(clutch_shots()$made, na.rm=TRUE) else 0; valueBox(percent(fg, accuracy=0.1), "Clutch FG%", icon=icon("bullseye"), color="yellow") })
  output$clutch_vb_nonclutch_fg <- renderValueBox(valueBox(percent(mean(non_clutch_shots()$made, na.rm=TRUE), accuracy=0.1), "Non-Clutch FG%", icon=icon("chart-line"), color="blue"))
  output$clutch_vb_diff <- renderValueBox({
    diff_val <- mean(clutch_shots()$made, na.rm=TRUE) - mean(non_clutch_shots()$made, na.rm=TRUE)
    valueBox(paste0(ifelse(diff_val>=0,"+",""), round(diff_val*100,1), " pp"), "Differential", icon=icon("exchange-alt"), color="teal")
  })

  output$clutch_zone_chart <- renderPlotly({
    d_c <- clutch_shots() %>% group_by(shot_zone) %>% summarise(fg_pct = mean(made, na.rm=TRUE), .groups="drop") %>% mutate(type="Clutch")
    d_nc <- non_clutch_shots() %>% group_by(shot_zone) %>% summarise(fg_pct = mean(made, na.rm=TRUE), .groups="drop") %>% mutate(type="Non-Clutch")
    plot_ly(bind_rows(d_c, d_nc), x=~shot_zone, y=~fg_pct, color=~type, type="bar", colors=mc_2colors) %>%
      mckinsey_layout(barmode="group", yaxis=list(title="FG%", tickformat=".0%"), xaxis=list(title=""))
  })

  output$clutch_top_players <- renderPlotly({
    d <- clutch_shots() %>% group_by(player_name) %>% summarise(fg_pct=mean(made, na.rm=TRUE), n=n(), .groups="drop") %>% filter(n>=20) %>% slice_max(fg_pct, n=15)
    plot_ly(d, x=~reorder(player_name, fg_pct), y=~fg_pct, type="bar", orientation="h", marker=list(color=mc_primary),
            text=~paste0(round(fg_pct*100,1),"% (",n,")"), textposition="auto", textfont=list(color="#fff",size=10)) %>%
      mckinsey_layout(xaxis=list(title="FG%", tickformat=".0%"), yaxis=list(title=""))
  })

  output$clutch_clock_chart <- renderPlotly({
    d <- clutch_shots() %>% filter(!is.na(SHOT_CLOCK)) %>% mutate(clock_bin=cut(SHOT_CLOCK, breaks=seq(0,24,3))) %>%
      group_by(clock_bin) %>% summarise(fg_pct=mean(made, na.rm=TRUE), n=n(), .groups="drop") %>% filter(!is.na(clock_bin))
    plot_ly(d, x=~clock_bin, y=~fg_pct, type="bar", marker=list(color=mc_blue),
            text=~paste0(round(fg_pct*100,1),"% (n=",n,")"), textposition="auto", textfont=list(color="#fff",size=11)) %>%
      mckinsey_layout(xaxis=list(title="Shot Clock Window (sec)"), yaxis=list(title="FG%", tickformat=".0%"))
  })

  # ---- PREDICTIVE MODEL TAB --------------------------------------------------
  model_results <- reactiveValues(trained=FALSE, accuracy=NA, auc=NA, n_test=NA, roc_obj=NULL, cm=NULL, importance=NULL, model_obj=NULL)

  observeEvent(input$model_train, {
    withProgress(message = "Training model...", value = 0, {
      features <- input$model_features
      if (length(features) < 2) { showNotification("Select at least 2 features.", type = "error"); return() }
      model_data <- shots %>% select(all_of(c(features, "made"))) %>% drop_na()
      cat_cols <- intersect(features, c("shot_zone","pressure_cat","location"))
      for (col in cat_cols) model_data[[col]] <- as.factor(model_data[[col]])
      if ("QUARTER" %in% features) model_data$QUARTER <- as.factor(model_data$QUARTER)
      model_data$made <- as.factor(model_data$made)
      incProgress(0.2, detail = "Splitting data...")
      set.seed(42); train_idx <- createDataPartition(model_data$made, p = input$model_split/100, list = FALSE)
      train_set <- model_data[train_idx, ]; test_set <- model_data[-train_idx, ]
      incProgress(0.3, detail = "Fitting model...")
      if (input$model_type == "glm") {
        fit <- glm(made ~ ., data = train_set, family = binomial()); probs <- predict(fit, test_set, type = "response")
        imp_df <- data.frame(feature = names(coef(fit))[-1], importance = abs(coef(fit)[-1])) %>% arrange(desc(importance)) %>% head(15)
      } else {
        rf_train <- if (nrow(train_set) > 20000) train_set[sample(nrow(train_set), 20000), ] else train_set
        fit <- randomForest(made ~ ., data = rf_train, ntree = 200, importance = TRUE); probs <- predict(fit, test_set, type = "prob")[, "1"]
        imp_raw <- importance(fit); imp_df <- data.frame(feature = rownames(imp_raw), importance = imp_raw[, "MeanDecreaseGini"]) %>% arrange(desc(importance)) %>% head(15)
      }
      incProgress(0.3, detail = "Evaluating...")
      preds <- ifelse(probs > 0.5, 1, 0); acc <- mean(preds == as.numeric(as.character(test_set$made)), na.rm = TRUE)
      roc_obj <- roc(as.numeric(as.character(test_set$made)), probs, quiet = TRUE)
      cm <- confusionMatrix(factor(preds), factor(as.numeric(as.character(test_set$made))))
      model_results$trained <- TRUE; model_results$accuracy <- acc; model_results$auc <- auc(roc_obj)
      model_results$n_test <- nrow(test_set); model_results$roc_obj <- roc_obj; model_results$cm <- cm
      model_results$importance <- imp_df; model_results$model_obj <- fit
      incProgress(0.2, detail = "Done!")
    })
  })

  output$model_vb_acc <- renderValueBox({ acc <- if(model_results$trained) percent(model_results$accuracy, accuracy=0.1) else "\u2014"; valueBox(acc, "Accuracy", icon=icon("check"), color="green") })
  output$model_vb_auc <- renderValueBox({ au <- if(model_results$trained) round(model_results$auc, 3) else "\u2014"; valueBox(au, "AUC-ROC", icon=icon("chart-area"), color="blue") })
  output$model_vb_n <- renderValueBox({ n <- if(model_results$trained) format(model_results$n_test, big.mark=",") else "\u2014"; valueBox(n, "Test Samples", icon=icon("vial"), color="navy") })

  output$model_roc <- renderPlot({
    req(model_results$trained); roc_obj <- model_results$roc_obj
    par(bg="#FFFFFF", col.axis="#6B7280", col.lab="#374151", col.main="#003A70", fg="#9CA3AF")
    plot(roc_obj, col=mc_primary, lwd=3, main=paste0("AUC = ", round(auc(roc_obj),3)), cex.main=1.1, font.main=2)
    abline(a=0, b=1, lty=2, col="#D1D5DB")
  }, bg = "#FFFFFF")

  output$model_cm <- renderPlot({
    req(model_results$trained); cm_table <- as.data.frame(model_results$cm$table)
    ggplot(cm_table, aes(x=Reference, y=Prediction, fill=Freq)) +
      geom_tile(color="#FFFFFF", linewidth=2) +
      geom_text(aes(label=format(Freq, big.mark=",")), color="white", size=6, fontface="bold") +
      scale_fill_gradient(low="#0077C8", high="#001E3C") +
      labs(x="Actual", y="Predicted") + scale_x_discrete(labels=c("Missed","Made")) + scale_y_discrete(labels=c("Missed","Made")) +
      theme_mckinsey() + theme(legend.position="none", axis.text=element_text(size=11, face="bold"))
  }, bg = "#FFFFFF")

  output$model_importance <- renderPlotly({
    req(model_results$trained)
    plot_ly(model_results$importance, x=~reorder(feature, importance), y=~importance, type="bar", orientation="h", marker=list(color=mc_primary)) %>%
      mckinsey_layout(xaxis=list(title="Importance Score"), yaxis=list(title=""))
  })

  # ---- CALCULATOR TAB --------------------------------------------------------
  calc_model <- reactive({
    d <- shots %>% select(SHOT_DIST, CLOSE_DEF_DIST, SHOT_CLOCK, DRIBBLES, TOUCH_TIME, shot_zone, pressure_cat, QUARTER, made) %>% drop_na()
    d$shot_zone <- as.factor(d$shot_zone); d$pressure_cat <- as.factor(d$pressure_cat); d$QUARTER <- as.factor(d$QUARTER); d$made <- as.numeric(d$made)
    glm(made ~ ., data = d, family = binomial())
  })

  calc_prob <- eventReactive(input$calc_predict, {
    fit <- calc_model()
    new_data <- data.frame(SHOT_DIST=input$calc_dist, CLOSE_DEF_DIST=input$calc_def, SHOT_CLOCK=input$calc_clock,
      DRIBBLES=input$calc_dribbles, TOUCH_TIME=input$calc_touch,
      shot_zone=factor(input$calc_zone, levels=levels(factor(shots$shot_zone))),
      pressure_cat=factor(input$calc_pressure, levels=levels(factor(shots$pressure_cat))),
      QUARTER=factor(input$calc_quarter, levels=as.character(1:4)))
    predict(fit, new_data, type = "response")
  })

  output$calc_result <- renderText({ req(input$calc_predict); paste0(round(calc_prob()*100,1), "%") })
  output$calc_context <- renderText({
    req(input$calc_predict); overall <- mean(shots$made, na.rm=TRUE); diff <- calc_prob() - overall
    paste0("This is ", round(abs(diff)*100,1), " percentage points ", if(diff>=0) "above" else "below", " the league average FG% of ", round(overall*100,1), "%.")
  })

  output$calc_comparison <- renderPlotly({
    req(input$calc_predict); prob <- calc_prob(); overall <- mean(shots$made, na.rm=TRUE)
    zone_avg <- shots %>% filter(shot_zone==input$calc_zone) %>% summarise(fg=mean(made, na.rm=TRUE)) %>% pull(fg)
    pres_avg <- shots %>% filter(pressure_cat==input$calc_pressure) %>% summarise(fg=mean(made, na.rm=TRUE)) %>% pull(fg)
    d <- data.frame(Category=c("Predicted","League Avg",paste(input$calc_zone,"Avg"),paste(input$calc_pressure,"Avg")), Probability=c(prob, overall, zone_avg, pres_avg))
    d$Category <- factor(d$Category, levels=d$Category)
    plot_ly(d, x=~Category, y=~Probability, type="bar", marker=list(color=mc_4colors),
            text=~paste0(round(Probability*100,1),"%"), textposition="auto", textfont=list(color="#fff",size=12)) %>%
      mckinsey_layout(xaxis=list(title=""), yaxis=list(title="Make Probability", tickformat=".0%"))
  })

}

shinyApp(ui = ui, server = server)
