# Input validation utilities for Calcium Imaging Analysis App

# Validate input file structure and content
validate_input_file <- function(df) {
  # Check basic structure
  if (ncol(df) < 2) {
    return("File must have at least 2 columns (Time + at least one cell)")
  }
  
  if (nrow(df) < 10) {
    return("File must have at least 10 time points for meaningful analysis")
  }
  
  # Check first column (Time)
  if (!is.numeric(df[[1]])) {
    return("First column must be numeric (Time values)")
  }
  
  # Check for valid numeric data in cells
  valid_cols <- 0
  for (i in 2:ncol(df)) {
    if (is.numeric(df[[i]]) && any(is.finite(df[[i]]))) {
      valid_cols <- valid_cols + 1
    }
  }
  
  if (valid_cols == 0) {
    return("No valid numeric data found in cell columns. Please check:
            1. Data columns contain numeric values
            2. Data is not all NA or infinite values
            3. File format is correct (CSV or Excel)")
  }
  
  # Check time column integrity
  time_vals <- df[[1]]
  if (any(is.na(time_vals))) {
    return("Time column contains missing values")
  }
  
  if (length(unique(time_vals)) < length(time_vals)) {
    return("Warning: Time column contains duplicate values")
  }
  
  # Check if time is monotonically increasing
  if (!all(diff(time_vals) > 0)) {
    return("Time values must be monotonically increasing")
  }
  
  return(NULL)  # Valid file
}

# Validate preprocessing parameters
validate_preprocessing_params <- function(params) {
  errors <- character()
  
  # Check baseline frames
  if (!is.null(params$baseline_frames)) {
    if (params$baseline_frames < 1 || params$baseline_frames > 100) {
      errors <- c(errors, "Baseline frames must be between 1 and 100")
    }
  }
  
  # Check smoothing window
  if (!is.null(params$smoothing_window)) {
    if (params$smoothing_window < 0 || params$smoothing_window > 50) {
      errors <- c(errors, "Smoothing window must be between 0 and 50")
    }
  }
  
  # Check detrending polynomial order
  if (!is.null(params$detrend_order)) {
    if (params$detrend_order < 1 || params$detrend_order > 10) {
      errors <- c(errors, "Detrending polynomial order must be between 1 and 10")
    }
  }
  
  # Check artifact threshold
  if (!is.null(params$artifact_threshold)) {
    if (params$artifact_threshold < 0) {
      errors <- c(errors, "Artifact threshold must be non-negative")
    }
  }
  
  if (length(errors) > 0) {
    return(paste(errors, collapse = "\n"))
  }
  
  return(NULL)  # Valid parameters
}

# Validate cell selection
validate_cell_selection <- function(selected_cells, available_cells) {
  if (length(selected_cells) == 0) {
    return("Please select at least one cell for analysis")
  }
  
  invalid_cells <- setdiff(selected_cells, available_cells)
  if (length(invalid_cells) > 0) {
    return(paste("Invalid cell selection:", paste(invalid_cells, collapse = ", ")))
  }
  
  return(NULL)  # Valid selection
}

# Validate metric calculation results
validate_metrics <- function(metrics_df) {
  if (is.null(metrics_df) || nrow(metrics_df) == 0) {
    return("No metrics could be calculated. Please check your data and preprocessing settings.")
  }
  
  # Check if all metrics are NA
  metric_cols <- setdiff(names(metrics_df), c("Cell_ID", "Group", "Original_Column"))
  all_na <- all(sapply(metrics_df[metric_cols], function(x) all(is.na(x))))
  
  if (all_na) {
    return("All calculated metrics are NA. Possible issues:
            1. Signal is too noisy
            2. Baseline period is too short
            3. No clear calcium transients detected
            4. Data may be already normalized or processed")
  }
  
  return(NULL)  # Valid metrics
}

# Validate export parameters
validate_export_params <- function(format, data) {
  if (is.null(data) || nrow(data) == 0) {
    return("No data available to export")
  }
  
  if (!format %in% c("csv", "xlsx", "txt")) {
    return("Invalid export format. Please choose CSV, Excel, or TXT")
  }
  
  return(NULL)  # Valid export
}

# Create validation messages for UI
create_validation_message <- function(error_msg, type = "error") {
  icon_map <- list(
    error = "times-circle",
    warning = "exclamation-triangle",
    info = "info-circle",
    success = "check-circle"
  )
  
  color_map <- list(
    error = "#d32f2f",
    warning = "#f57c00",
    info = "#1976d2",
    success = "#388e3c"
  )
  
  list(
    type = type,
    message = error_msg,
    icon = icon_map[[type]],
    color = color_map[[type]]
  )
}

# Validate numeric input with range
validate_numeric_input <- function(value, min_val = NULL, max_val = NULL, name = "Value") {
  if (is.null(value) || is.na(value)) {
    return(paste(name, "must be provided"))
  }
  
  if (!is.numeric(value)) {
    return(paste(name, "must be a number"))
  }
  
  if (!is.null(min_val) && value < min_val) {
    return(paste(name, "must be at least", min_val))
  }
  
  if (!is.null(max_val) && value > max_val) {
    return(paste(name, "must be at most", max_val))
  }
  
  return(NULL)  # Valid
}