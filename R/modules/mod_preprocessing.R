# Preprocessing Module for Calcium Imaging Analysis App

# Module UI
mod_preprocessing_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
        h3("Data Preprocessing", icon("filter")),
        hr()
      )
    ),
    
    fluidRow(
      # Left column - Controls
      column(4,
        wellPanel(
          h4("Preprocessing Options"),
          
          # Normalization
          checkboxInput(
            ns("normalize"),
            "Apply ΔF/F₀ Normalization",
            value = TRUE
          ),
          
          conditionalPanel(
            condition = "input.normalize == true",
            ns = ns,
            
            numericInput(
              ns("baseline_frames"),
              "Baseline Frames",
              value = 20,
              min = 1,
              max = 100,
              step = 1
            ),
            
            radioButtons(
              ns("baseline_method"),
              "Baseline Method",
              choices = list(
                "Mean" = "mean",
                "Median" = "median",
                "Percentile" = "percentile"
              ),
              selected = "mean"
            ),
            
            conditionalPanel(
              condition = "input.baseline_method == 'percentile'",
              ns = ns,
              numericInput(
                ns("baseline_percentile"),
                "Percentile",
                value = 10,
                min = 1,
                max = 50,
                step = 1
              )
            )
          ),
          
          hr(),
          
          # Smoothing
          checkboxInput(
            ns("smooth"),
            "Apply Smoothing",
            value = FALSE
          ),
          
          conditionalPanel(
            condition = "input.smooth == true",
            ns = ns,
            
            radioButtons(
              ns("smooth_method"),
              "Smoothing Method",
              choices = list(
                "Moving Average" = "ma",
                "Gaussian" = "gaussian",
                "Savitzky-Golay" = "sg"
              ),
              selected = "ma"
            ),
            
            numericInput(
              ns("smooth_window"),
              "Window Size",
              value = 5,
              min = 3,
              max = 21,
              step = 2
            )
          ),
          
          hr(),
          
          # Detrending
          checkboxInput(
            ns("detrend"),
            "Remove Trend",
            value = FALSE
          ),
          
          conditionalPanel(
            condition = "input.detrend == true",
            ns = ns,
            
            radioButtons(
              ns("detrend_method"),
              "Detrending Method",
              choices = list(
                "Linear" = "linear",
                "Polynomial" = "poly",
                "Exponential" = "exp"
              ),
              selected = "linear"
            ),
            
            conditionalPanel(
              condition = "input.detrend_method == 'poly'",
              ns = ns,
              numericInput(
                ns("poly_order"),
                "Polynomial Order",
                value = 2,
                min = 2,
                max = 5,
                step = 1
              )
            )
          ),
          
          hr(),
          
          # Artifact removal
          checkboxInput(
            ns("remove_artifacts"),
            "Remove Artifacts",
            value = FALSE
          ),
          
          conditionalPanel(
            condition = "input.remove_artifacts == true",
            ns = ns,
            
            numericInput(
              ns("artifact_threshold"),
              "Artifact Threshold (SD)",
              value = 5,
              min = 2,
              max = 10,
              step = 0.5
            ),
            
            radioButtons(
              ns("artifact_method"),
              "Replacement Method",
              choices = list(
                "Interpolate" = "interpolate",
                "Previous Value" = "previous",
                "Set to NA" = "na"
              ),
              selected = "interpolate"
            )
          ),
          
          br(),
          
          # Apply button
          actionButton(
            ns("apply_preprocessing"),
            "Apply Preprocessing",
            icon = icon("play"),
            class = "btn-primary",
            width = "100%"
          ),
          
          br(), br(),
          
          # Reset button
          actionButton(
            ns("reset_preprocessing"),
            "Reset to Original",
            icon = icon("undo"),
            class = "btn-warning",
            width = "100%"
          )
        )
      ),
      
      # Middle column - Preview
      column(4,
        h4("Data Preview"),
        
        # Cell selector for preview
        selectInput(
          ns("preview_cell"),
          "Select Cell for Preview",
          choices = NULL,
          width = "100%"
        ),
        
        # Preview plot
        plotOutput(ns("preview_plot"), height = "300px") %>%
          withSpinner(type = 4),
        
        # Comparison plot
        h4("Before/After Comparison"),
        plotOutput(ns("comparison_plot"), height = "300px") %>%
          withSpinner(type = 4)
      ),
      
      # Right column - Statistics
      column(4,
        h4("Preprocessing Summary"),
        
        # Statistics table
        tableOutput(ns("stats_table")),
        
        br(),
        
        # Quality metrics
        h4("Quality Metrics"),
        verbatimTextOutput(ns("quality_metrics")),
        
        br(),
        
        # Processing log
        h4("Processing Log"),
        verbatimTextOutput(ns("processing_log"))
      )
    )
  )
}

# Module Server
mod_preprocessing_server <- function(id, data_module) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      original_data = NULL,
      processed_data = NULL,
      processing_log = character(),
      quality_metrics = NULL
    )
    
    # Update data when new data is loaded
    observe({
      data <- data_module$data()
      if (!is.null(data)) {
        values$original_data <- data
        values$processed_data <- data
        
        # Update cell selector
        if (length(data) > 0) {
          first_df <- data[[1]]
          cell_cols <- setdiff(names(first_df), "Time")
          updateSelectInput(
            session,
            "preview_cell",
            choices = cell_cols,
            selected = cell_cols[1]
          )
        }
      }
    })
    
    # Apply preprocessing
    observeEvent(input$apply_preprocessing, {
      req(values$original_data)
      
      withProgress(message = "Applying preprocessing...", value = 0, {
        
        # Clear log
        values$processing_log <- character()
        
        # Process each dataset
        processed <- list()
        n_datasets <- length(values$original_data)
        
        for (i in seq_along(values$original_data)) {
          name <- names(values$original_data)[i]
          df <- values$original_data[[name]]
          
          incProgress(1/(n_datasets * 4), detail = paste("Processing", name))
          
          # Apply preprocessing steps
          result <- df
          log_entries <- character()
          
          # 1. Normalization
          if (input$normalize) {
            result <- apply_normalization(
              result,
              baseline_frames = input$baseline_frames,
              method = input$baseline_method,
              percentile = input$baseline_percentile
            )
            log_entries <- c(log_entries, 
              paste("Applied ΔF/F₀ normalization (", input$baseline_method, ")")
            )
            incProgress(1/(n_datasets * 4))
          }
          
          # 2. Smoothing
          if (input$smooth) {
            result <- apply_smoothing(
              result,
              method = input$smooth_method,
              window = input$smooth_window
            )
            log_entries <- c(log_entries,
              paste("Applied", input$smooth_method, "smoothing (window:", input$smooth_window, ")")
            )
            incProgress(1/(n_datasets * 4))
          }
          
          # 3. Detrending
          if (input$detrend) {
            result <- apply_detrending(
              result,
              method = input$detrend_method,
              poly_order = input$poly_order
            )
            log_entries <- c(log_entries,
              paste("Removed trend (", input$detrend_method, ")")
            )
            incProgress(1/(n_datasets * 4))
          }
          
          # 4. Artifact removal
          if (input$remove_artifacts) {
            result <- remove_artifacts(
              result,
              threshold = input$artifact_threshold,
              method = input$artifact_method
            )
            log_entries <- c(log_entries,
              paste("Removed artifacts (threshold:", input$artifact_threshold, "SD)")
            )
            incProgress(1/(n_datasets * 4))
          }
          
          processed[[name]] <- result
          
          # Update log
          values$processing_log <- c(
            values$processing_log,
            paste0("\n", name, ":"),
            log_entries
          )
        }
        
        values$processed_data <- processed
        
        # Calculate quality metrics
        values$quality_metrics <- calculate_quality_metrics(
          values$original_data,
          values$processed_data
        )
        
        showNotification(
          "Preprocessing applied successfully!",
          type = "success"
        )
      })
    })
    
    # Reset preprocessing
    observeEvent(input$reset_preprocessing, {
      values$processed_data <- values$original_data
      values$processing_log <- "Data reset to original values"
      values$quality_metrics <- NULL
      
      showNotification(
        "Data reset to original values",
        type = "info"
      )
    })
    
    # Preview plot
    output$preview_plot <- renderPlot({
      req(values$processed_data, input$preview_cell)
      
      # Get first dataset for preview
      df <- values$processed_data[[1]]
      
      if (input$preview_cell %in% names(df)) {
        ggplot(data.frame(
          Time = df$Time,
          Signal = df[[input$preview_cell]]
        ), aes(x = Time, y = Signal)) +
          geom_line(color = "blue", size = 1) +
          labs(
            title = paste("Processed Signal:", input$preview_cell),
            x = "Time (s)",
            y = "Signal"
          ) +
          APP_THEME
      }
    })
    
    # Comparison plot
    output$comparison_plot <- renderPlot({
      req(values$original_data, values$processed_data, input$preview_cell)
      
      # Get first dataset
      orig_df <- values$original_data[[1]]
      proc_df <- values$processed_data[[1]]
      
      if (input$preview_cell %in% names(orig_df)) {
        comparison_df <- data.frame(
          Time = rep(orig_df$Time, 2),
          Signal = c(orig_df[[input$preview_cell]], proc_df[[input$preview_cell]]),
          Type = rep(c("Original", "Processed"), each = nrow(orig_df))
        )
        
        ggplot(comparison_df, aes(x = Time, y = Signal, color = Type)) +
          geom_line(size = 1, alpha = 0.7) +
          scale_color_manual(values = c("Original" = "gray50", "Processed" = "blue")) +
          labs(
            title = "Before/After Comparison",
            x = "Time (s)",
            y = "Signal"
          ) +
          APP_THEME +
          theme(legend.position = "bottom")
      }
    })
    
    # Statistics table
    output$stats_table <- renderTable({
      req(values$processed_data)
      
      # Calculate summary statistics
      df <- values$processed_data[[1]]
      cell_cols <- setdiff(names(df), "Time")
      
      if (length(cell_cols) > 0) {
        stats_df <- data.frame(
          Metric = c("Mean", "SD", "Min", "Max", "Range"),
          Value = c(
            round(mean(unlist(df[, ..cell_cols]), na.rm = TRUE), 4),
            round(sd(unlist(df[, ..cell_cols]), na.rm = TRUE), 4),
            round(min(unlist(df[, ..cell_cols]), na.rm = TRUE), 4),
            round(max(unlist(df[, ..cell_cols]), na.rm = TRUE), 4),
            round(diff(range(unlist(df[, ..cell_cols]), na.rm = TRUE)), 4)
          )
        )
        stats_df
      }
    })
    
    # Quality metrics
    output$quality_metrics <- renderPrint({
      if (!is.null(values$quality_metrics)) {
        cat("Signal Quality Metrics:\n")
        cat("====================\n")
        for (metric in names(values$quality_metrics)) {
          cat(metric, ":", round(values$quality_metrics[[metric]], 3), "\n")
        }
      } else {
        cat("No preprocessing applied yet")
      }
    })
    
    # Processing log
    output$processing_log <- renderPrint({
      if (length(values$processing_log) > 0) {
        cat(values$processing_log, sep = "\n")
      } else {
        cat("No preprocessing steps applied")
      }
    })
    
    # Return processed data
    return(
      list(
        data = reactive(values$processed_data),
        preprocessing_params = reactive(list(
          normalized = input$normalize,
          baseline_frames = input$baseline_frames,
          smoothed = input$smooth,
          detrended = input$detrend
        ))
      )
    )
  })
}

# Preprocessing functions (robust versions)

apply_normalization <- function(df, baseline_frames = 20, method = "mean", percentile = 10) {
  cell_cols <- setdiff(names(df), "Time")
  for (col in cell_cols) {
    signal <- df[[col]]
    if (!is.numeric(signal) || all(is.na(signal))) next
    baseline_idx <- min(baseline_frames, floor(nrow(df) * 0.1))
    baseline_signal <- signal[1:baseline_idx]
    baseline <- switch(method,
      "mean" = mean(baseline_signal, na.rm = TRUE),
      "median" = median(baseline_signal, na.rm = TRUE),
      "percentile" = quantile(baseline_signal, percentile / 100, na.rm = TRUE, names = FALSE)
    )
    if (!is.na(baseline) && baseline != 0) {
      df[[col]] <- (signal - baseline) / baseline
    }
  }
  df
}

apply_smoothing <- function(df, method = "ma", window = 5) {
  cell_cols <- setdiff(names(df), "Time")
  for (col in cell_cols) {
    signal <- df[[col]]
    if (!is.numeric(signal) || all(is.na(signal))) next
    if (window %% 2 == 0) window <- window + 1
    if (method == "ma") {
      df[[col]] <- as.numeric(stats::filter(signal, rep(1 / window, window), sides = 2))
    } else if (method == "gaussian") {
      sigma <- window / 4
      kernel <- dnorm(seq(-window, window, 1), 0, sigma)
      kernel <- kernel / sum(kernel)
      df[[col]] <- as.numeric(stats::filter(signal, kernel, sides = 2))
    }
  }
  df
}

apply_detrending <- function(df, method = "linear", poly_order = 2) {
  time_col <- df$Time
  cell_cols <- setdiff(names(df), "Time")
  for (col in cell_cols) {
    signal <- df[[col]]
    if (!is.numeric(signal) || all(is.na(signal))) next
    valid_idx <- !is.na(signal)
    if (sum(valid_idx) > 10) {
      if (method == "linear") {
        fit <- lm(signal[valid_idx] ~ time_col[valid_idx])
        trend <- predict(fit, newdata = data.frame(time_col = time_col))
        df[[col]] <- signal - trend
      } else if (method == "poly") {
        fit <- lm(signal[valid_idx] ~ poly(time_col[valid_idx], poly_order))
        trend <- predict(fit, newdata = data.frame(time_col = time_col))
        df[[col]] <- signal - trend
      }
    }
  }
  df
}

remove_artifacts <- function(df, threshold = 5, method = "interpolate") {
  cell_cols <- setdiff(names(df), "Time")
  for (col in cell_cols) {
    signal <- df[[col]]
    if (!is.numeric(signal) || all(is.na(signal))) next
    med <- median(signal, na.rm = TRUE)
    mad_val <- mad(signal, na.rm = TRUE)
    if (is.na(mad_val) || mad_val == 0) next
    artifacts <- abs(signal - med) > threshold * mad_val
    if (any(artifacts, na.rm = TRUE)) {
      if (method == "interpolate") {
        signal[artifacts] <- NA
        df[[col]] <- zoo::na.approx(signal, na.rm = FALSE)
      } else if (method == "previous") {
        for (i in which(artifacts)) if (i > 1) df[[col]][i] <- df[[col]][i - 1]
      } else if (method == "na") {
        df[[col]][artifacts] <- NA
      }
    }
  }
  df
}

calculate_quality_metrics <- function(original_data, processed_data) {
  metrics <- list()
  orig <- original_data[[1]]
  proc <- processed_data[[1]]
  cell_cols <- setdiff(names(orig), "Time")
  calc_snr <- function(df) {
    mean(sapply(cell_cols, function(col) {
      signal <- df[[col]]
      if (!is.numeric(signal) || all(is.na(signal))) return(NA)
      baseline_idx <- 1:min(20, length(signal))
      baseline <- mean(signal[baseline_idx], na.rm = TRUE)
      noise <- sd(signal[baseline_idx], na.rm = TRUE)
      peak <- max(signal, na.rm = TRUE)
      if (!is.na(noise) && noise > 0) (peak - baseline) / noise else NA
    }), na.rm = TRUE)
  }
  orig_snr <- calc_snr(orig)
  proc_snr <- calc_snr(proc)
  metrics$SNR_Original <- orig_snr
  metrics$SNR_Processed <- proc_snr
  metrics$SNR_Improvement <- if (!is.na(orig_snr) && orig_snr != 0) (proc_snr - orig_snr) / orig_snr * 100 else 0
  metrics
}