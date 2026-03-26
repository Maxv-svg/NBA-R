#!/usr/bin/env Rscript
# =============================================================================
# install_packages.R — Run this ONCE before launching the dashboard
# =============================================================================

cat("Installing required packages for NBA Shot Analytics Dashboard...\n\n")

packages <- c(
  "shiny",
  "shinydashboard",
  "shinydashboardPlus",
  "shinyWidgets",
  "DT",
  "ggplot2",
  "plotly",
  "dplyr",
  "tidyr",
  "caret",
  "randomForest",
  "pROC",
  "scales",
  "viridis",
  "gridExtra",
  "corrplot",
  "e1071"
)

# Install missing packages
installed <- rownames(installed.packages())
to_install <- packages[!packages %in% installed]

if (length(to_install) > 0) {
  cat("Installing:", paste(to_install, collapse = ", "), "\n\n")
  install.packages(to_install, repos = "https://cran.r-project.org", dependencies = TRUE)
} else {
  cat("All packages already installed!\n")
}

# Verify
cat("\n--- Verification ---\n")
for (pkg in packages) {
  status <- if (requireNamespace(pkg, quietly = TRUE)) "OK" else "MISSING"
  cat(sprintf("  %-25s %s\n", pkg, status))
}

cat("\nDone! You can now run the dashboard with: shiny::runApp('app.R')\n")
