# NBA Shot Analytics Dashboard — 2014-15 Season

A **Shiny dashboard** built in R for exploring, visualising, and modelling NBA shot data from the 2014-15 season (~128 000 shots tracked by SportVU).

---

## Quick Start

```bash
# 1. Install R packages (one time)
Rscript install_packages.R

# 2. Download the dataset from Kaggle and place it in this folder:
#    https://www.kaggle.com/datasets/dansbecker/nba-shot-logs
#    File needed: shot_logs.csv

# 3. Launch the dashboard
Rscript -e "shiny::runApp('app.R', port = 3838, launch.browser = TRUE)"
```

---

## Project Structure

```
nba-shot-dashboard/
├── app.R                  # Main Shiny app (UI + Server)
├── helpers.R              # Data preprocessing & court drawing utilities
├── install_packages.R     # One-time package installer
├── shot_logs.csv          # ← Place the Kaggle CSV here
└── README.md
```

---

## Dashboard Tabs

| Tab | Description |
|-----|-------------|
| **Overview** | KPIs, shot zone breakdown, quarter distribution, defender proximity impact, distance vs make rate, top players by volume |
| **Data Profile** | Row/column counts, missing values, class balance, column type table, numeric distributions, correlation matrix |
| **Shot Explorer** | Multi-filter interface (player, team, zone, pressure, distance, clock, clutch) with scatter plot and data table |
| **Court Heatmap** | Stylised half-court visualisation with distance-zone tiles coloured by FG% or shot count |
| **Player Analysis** | Head-to-head player comparison across zones, distance bins, defender pressure, and quarters |
| **Defender Impact** | FG% vs defender distance bubble chart, pressure × zone heatmap, best defenders table, result density |
| **Clutch Analysis** | Clutch vs non-clutch KPIs, zone comparison, top clutch shooters, shot clock breakdown |
| **Predictive Model** | Train logistic regression or random forest with selectable features; ROC curve, confusion matrix, feature importance |
| **Make Probability** | Interactive calculator — enter shot parameters and get a predicted make probability with historical comparison |

---

## Data Pre-processing (helpers.R)

Engineered features created automatically:

- **Shot Zone**: Paint (0-8 ft), Mid-Range (8-22 ft), Three-Point (22+ ft)
- **Defender Pressure**: Wide Open (6+ ft), Open (4-6 ft), Contested (2-4 ft), Tightly Contested (<2 ft)
- **Clutch Indicator**: Q4 or OT with ≤5 minutes remaining
- **Location**: Home / Away

---

## Requirements

- **R** ≥ 4.0
- Packages listed in `install_packages.R`
- Dataset: `shot_logs.csv` from [Kaggle](https://www.kaggle.com/datasets/dansbecker/nba-shot-logs)

---

## Dataset Details

| Property | Value |
|----------|-------|
| Rows | ~128 000 |
| Columns | 21 |
| Source | NBA Stats API / SportVU |
| Season | 2014-15 |
| Class balance | ~45% made / ~55% missed |
| Key features | Shot distance, defender distance, touch time, dribbles, shot clock, player, team |
