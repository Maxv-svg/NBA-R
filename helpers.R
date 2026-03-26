# =============================================================================
# helpers.R — Data Preprocessing & Utility Functions
# =============================================================================

library(dplyr)
library(ggplot2)

# --- Preprocess raw shot log data ---------------------------------------------
preprocess_shots <- function(df) {

  # Standardize column names (handle variations in the Kaggle dataset)
  names(df) <- toupper(names(df))

  # Rename common variations
  rename_map <- c(
    "SHOT_RESULT"       = "FGM",
    "CLOSEST_DEFENDER"  = "CLOSEST_DEFENDER",
    "CLOSE_DEF_DIST"    = "CLOSE_DEF_DIST",
    "SHOT_DIST"         = "SHOT_DIST",
    "SHOT_CLOCK"        = "SHOT_CLOCK",
    "TOUCH_TIME"        = "TOUCH_TIME",
    "DRIBBLES"          = "DRIBBLES",
    "PTS_TYPE"          = "PTS_TYPE",
    "SHOT_NUMBER"       = "SHOT_NUMBER"
  )

  # Detect the target column
  if ("SHOT_RESULT" %in% names(df)) {
    df$made <- ifelse(toupper(df$SHOT_RESULT) == "MADE", 1, 0)
  } else if ("FGM" %in% names(df)) {
    df$made <- as.numeric(df$FGM)
  } else {
    # Try to find it
    possible <- grep("RESULT|MADE|FGM", names(df), value = TRUE)
    if (length(possible) > 0) {
      vals <- unique(df[[possible[1]]])
      if (all(vals %in% c(0, 1, "0", "1"))) {
        df$made <- as.numeric(df[[possible[1]]])
      } else {
        df$made <- ifelse(toupper(df[[possible[1]]]) == "MADE", 1, 0)
      }
    }
  }

  # Standardize player name
  if ("PLAYER_NAME" %in% names(df)) {
    df$player_name <- df$PLAYER_NAME
  } else {
    name_col <- grep("PLAYER", names(df), value = TRUE)
    if (length(name_col) > 0) df$player_name <- df[[name_col[1]]]
  }

  # Standardize team
  if ("TEAM" %in% names(df)) {
    df$team <- df$TEAM
  } else {
    # Try common alternatives
    team_cols <- grep("TEAM|TM", names(df), value = TRUE)
    if (length(team_cols) > 0) df$team <- df[[team_cols[1]]]
    else df$team <- "Unknown"
  }

  # Ensure numeric columns
  num_cols <- c("SHOT_DIST", "CLOSE_DEF_DIST", "SHOT_CLOCK", "TOUCH_TIME",
                "DRIBBLES", "SHOT_NUMBER", "QUARTER")
  for (col in num_cols) {
    if (col %in% names(df)) {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    }
  }

  # Ensure QUARTER exists
  if (!"QUARTER" %in% names(df)) {
    if ("PERIOD" %in% names(df)) df$QUARTER <- as.numeric(df$PERIOD)
    else df$QUARTER <- NA_real_
  }

  # --- Engineered Features ---

  # 1. Shot Zone (from distance)
  df$shot_zone <- case_when(
    is.na(df$SHOT_DIST)    ~ NA_character_,
    df$SHOT_DIST <= 8       ~ "Paint",
    df$SHOT_DIST <= 22      ~ "Mid-Range",
    TRUE                    ~ "Three-Point"
  )

  # 2. Defender Pressure Category
  df$pressure_cat <- case_when(
    is.na(df$CLOSE_DEF_DIST)    ~ NA_character_,
    df$CLOSE_DEF_DIST >= 6      ~ "Wide Open",
    df$CLOSE_DEF_DIST >= 4      ~ "Open",
    df$CLOSE_DEF_DIST >= 2      ~ "Contested",
    TRUE                        ~ "Tightly Contested"
  )

  # 3. Location (Home / Away)
  if ("LOCATION" %in% names(df)) {
    df$location <- ifelse(toupper(df$LOCATION) %in% c("H", "HOME"), "Home", "Away")
  } else {
    df$location <- "Unknown"
  }

  # 4. Clutch indicator (Q4 or OT, last 5 minutes)
  # Game clock might be in various formats; we'll use QUARTER + GAME_CLOCK
  if ("GAME_CLOCK" %in% names(df)) {
    # Parse game clock (MM:SS format)
    parse_clock <- function(x) {
      parts <- strsplit(as.character(x), ":")
      sapply(parts, function(p) {
        if (length(p) == 2) {
          as.numeric(p[1]) * 60 + as.numeric(p[2])
        } else if (length(p) == 1) {
          suppressWarnings(as.numeric(p[1]))
        } else {
          NA_real_
        }
      })
    }
    df$game_clock_sec <- parse_clock(df$GAME_CLOCK)
    # Clutch = Q4+ and <=5 min remaining (300 seconds)
    df$clutch <- ifelse(!is.na(df$QUARTER) & df$QUARTER >= 4 &
                        !is.na(df$game_clock_sec) & df$game_clock_sec <= 300, 1, 0)
  } else {
    # Fallback: just use quarter
    df$clutch <- ifelse(!is.na(df$QUARTER) & df$QUARTER >= 4, 1, 0)
  }

  # 5. Closest defender name standardization
  if ("CLOSEST_DEFENDER" %in% names(df)) {
    df$CLOSEST_DEFENDER <- trimws(df$CLOSEST_DEFENDER)
  }

  # Remove rows where critical columns are all NA
  df <- df %>% filter(!is.na(made))

  return(df)
}


# --- Draw Court Zone Chart (stylized bar chart in court shape) -----------------
draw_court_zones <- function(zone_data, metric_col = "fg_pct") {

  # Color palette
  if (metric_col == "fg_pct") {
    fill_vals <- zone_data$fg_pct
    fill_label <- "FG%"
  } else {
    fill_vals <- zone_data$count
    fill_label <- "Shot Count"
  }

  # Create court background
  court <- ggplot() +
    # Court outline
    annotate("rect", xmin = -25, xmax = 25, ymin = 0, ymax = 47,
             fill = NA, color = "#333", linewidth = 1) +
    # Paint
    annotate("rect", xmin = -8, xmax = 8, ymin = 0, ymax = 19,
             fill = NA, color = "#444", linewidth = 0.5) +
    # Free throw circle
    annotate("path",
             x = 6 * cos(seq(0, pi, length.out = 100)),
             y = 19 + 6 * sin(seq(0, pi, length.out = 100)),
             color = "#444", linewidth = 0.5) +
    # Three point arc
    annotate("path",
             x = 22 * cos(seq(0.22, pi - 0.22, length.out = 200)),
             y = 5.25 + 22 * sin(seq(0.22, pi - 0.22, length.out = 200)),
             color = "#555", linewidth = 0.8) +
    # Corner three lines
    annotate("segment", x = -22, xend = -22, y = 0, yend = 9, color = "#555", linewidth = 0.8) +
    annotate("segment", x = 22, xend = 22, y = 0, yend = 9, color = "#555", linewidth = 0.8) +
    # Rim
    annotate("point", x = 0, y = 5.25, size = 3, color = "#e94560") +
    # Backboard
    annotate("segment", x = -3, xend = 3, y = 4, yend = 4, color = "#888", linewidth = 1.5)

  # Add zone data as positioned tiles
  zone_positions <- data.frame(
    dist_bin = zone_data$dist_bin,
    x = c(0, 0, -12, 12, -20, 15, 0),
    y = c(3, 10, 16, 16, 6, 25, 32),
    w = c(8, 14, 10, 10, 6, 12, 16),
    h = c(4, 5, 6, 6, 5, 8, 8),
    stringsAsFactors = FALSE
  )

  # Match up with data
  zone_positions <- zone_positions[1:nrow(zone_data), ]
  zone_positions$fill_val <- fill_vals
  zone_positions$label <- if (metric_col == "fg_pct") {
    paste0(zone_data$dist_bin, "\n", round(zone_data$fg_pct * 100, 1), "%\n(n=",
           format(zone_data$count, big.mark = ","), ")")
  } else {
    paste0(zone_data$dist_bin, "\n", format(zone_data$count, big.mark = ","))
  }

  court <- court +
    geom_tile(data = zone_positions,
              aes(x = x, y = y, width = w, height = h, fill = fill_val),
              alpha = 0.8, color = "#e94560", linewidth = 0.3) +
    geom_text(data = zone_positions,
              aes(x = x, y = y, label = label),
              color = "white", size = 3.2, fontface = "bold", lineheight = 0.9) +
    scale_fill_gradient(low = "#0f3460", high = "#e94560", name = fill_label,
                        labels = if (metric_col == "fg_pct") scales::percent else scales::comma) +
    coord_fixed(xlim = c(-26, 26), ylim = c(-1, 48)) +
    theme_void() +
    theme(
      plot.background  = element_rect(fill = "transparent", colour = NA),
      panel.background = element_rect(fill = "transparent", colour = NA),
      legend.text      = element_text(color = "#ccc", size = 9),
      legend.title     = element_text(color = "#e94560", size = 10, face = "bold"),
      legend.position  = "right"
    )

  return(court)
}
