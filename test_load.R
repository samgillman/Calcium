#!/usr/bin/env Rscript

# Test script to verify all modules load correctly

cat("Current working directory:", getwd(), "\n")
cat("Files in current directory:\n")
print(list.files())
cat("\nFiles in R/ directory:\n")
print(list.files("R", recursive = TRUE))

# Try loading each file
tryCatch({
  cat("\n=== Loading modules ===\n")
  
  cat("Loading R/global.R... ")
  source("R/global.R")
  cat("OK\n")
  
  cat("Loading R/utils/validation.R... ")
  source("R/utils/validation.R")
  cat("OK\n")
  
  cat("Loading R/utils/calculations.R... ")
  source("R/utils/calculations.R")
  cat("OK\n")
  
  cat("Loading R/modules/mod_data_loading.R... ")
  source("R/modules/mod_data_loading.R")
  cat("OK\n")
  
  cat("Loading R/modules/mod_preprocessing.R... ")
  source("R/modules/mod_preprocessing.R")
  cat("OK\n")
  
  cat("Loading R/modules/mod_metrics.R... ")
  source("R/modules/mod_metrics.R")
  cat("OK\n")
  
  cat("Loading R/modules/mod_visualization.R... ")
  source("R/modules/mod_visualization.R")
  cat("OK\n")
  
  cat("Loading R/modules/mod_group_comparison.R... ")
  source("R/modules/mod_group_comparison.R")
  cat("OK\n")
  
  cat("Loading R/modules/mod_metric_guide.R... ")
  source("R/modules/mod_metric_guide.R")
  cat("OK\n")
  
  cat("Loading R/modules/mod_tables.R... ")
  source("R/modules/mod_tables.R")
  cat("OK\n")
  
  cat("Loading R/modules/mod_export.R... ")
  source("R/modules/mod_export.R")
  cat("OK\n")
  
  cat("Loading R/modules/mod_statistical_analysis.R... ")
  source("R/modules/mod_statistical_analysis.R")
  cat("OK\n")
  
  cat("\n✅ All modules loaded successfully!\n")
  
}, error = function(e) {
  cat("\n❌ Error loading modules:\n")
  print(e)
})