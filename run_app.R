#!/usr/bin/env Rscript

# Launch script for the Complete Calcium Imaging Analysis App

# Ensure we're in the correct directory
if (!file.exists("app_complete.R")) {
  stop("Please run this script from the app directory containing app_complete.R")
}

# Check that all required files exist
required_files <- c(
  "R/global.R",
  "R/utils/validation.R",
  "R/utils/calculations.R",
  "R/modules/mod_data_loading.R",
  "R/modules/mod_preprocessing.R",
  "R/modules/mod_metrics.R",
  "R/modules/mod_visualization.R",
  "R/modules/mod_group_comparison.R",
  "R/modules/mod_metric_guide.R",
  "R/modules/mod_tables.R",
  "R/modules/mod_export.R",
  "R/modules/mod_statistical_analysis.R"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  cat("❌ Missing required files:\n")
  for (f in missing_files) {
    cat("  -", f, "\n")
  }
  stop("Please ensure all required files are present")
}

cat("✅ All required files found\n")
cat("📂 Working directory:", getwd(), "\n")
cat("🚀 Launching Calcium Imaging Analysis App v2.5...\n\n")

# Launch the app
shiny::runApp("app_complete.R", launch.browser = TRUE)