#!/usr/bin/env Rscript

# Run the Complete Calcium Imaging Analysis App v2.5
# This script launches the fully modularized version with all features

# Set working directory to app location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Source the complete app
source("app_complete.R")