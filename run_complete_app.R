#!/usr/bin/env Rscript

# Launcher for the COMPLETE Calcium Imaging Analysis App

cat("═══════════════════════════════════════════════════════════════\n")
cat("   Calcium Imaging Analysis App - Complete Version\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Check which version to run
if (file.exists("calcium app pipeline.R")) {
  cat("✅ Found original complete app (calcium app pipeline.R)\n")
  cat("   This contains ALL features including:\n")
  cat("   • Group Comparison (all features)\n")
  cat("   • Individual Analysis (all features)\n") 
  cat("   • Metric Guide with interactive visualization\n")
  cat("   • Advanced preprocessing (3 baseline methods)\n")
  cat("   • Statistical analysis with post-hoc tests\n")
  cat("   • All visualization options\n")
  cat("   • Tables and export functionality\n")
  cat("   • All 12+ metrics including Time to 25/50/75% Peak\n\n")
  
  response <- readline(prompt = "Run the original complete app? (y/n): ")
  
  if (tolower(response) == "y") {
    cat("\n🚀 Launching original complete app...\n\n")
    source("calcium app pipeline.R")
  }
} else {
  stop("Cannot find calcium app pipeline.R")
}

# Note: The modular version created enhances the original with:
# - Better code organization (modules)
# - Memory management improvements
# - Performance optimizations
# - Bug fixes in metrics calculations
# But the ORIGINAL already has 100% of the features!