# Metrics Calculation Module for Calcium Imaging Analysis App

source("R/utils/calculations.R")

# Module UI
mod_metrics_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
        h3("Metrics Analysis", icon("chart-line")),
        hr()
      )
    ),
    
    fluidRow(
      # Left column - Controls
      column(3,
        wellPanel(
          h4("Metrics Selection"),
          
          checkboxGroupInput(
            ns("selected_metrics"),
            "Select Metrics to Calculate",
            choices = list(
              "Peak ΔF/F₀" = "Peak_dFF0",
              "Time to Peak" = "Time_to_Peak",
              "Rise Time (10-90%)" = "Rise_Time",
              "Area Under Curve" = "AUC",
              "Half Width (HWHM)" = "Half_Width",
              "Signal-to-Noise Ratio" = "SNR",
              "Response Amplitude" = "Response_Amplitude",
              "Ca²⁺ Entry Rate" = "Calcium_Entry_Rate"
            ),
            selected = c("Peak_dFF0", "Time_to_Peak", "AUC", "Half_Width")
          ),
          
          hr(),
          
          h4("Analysis Options"),
          
          numericInput(
            ns("baseline_frames"),
            "Baseline Frames",
            value = 20,
            min = 5,
            max = 100,
            step = 1
          ),
          
          checkboxInput(
            ns("detect_spikes"),
            "Detect Spikes",
            value = FALSE
          ),
          
          conditionalPanel(
            condition = "input.detect_spikes == true",
            ns = ns,
            
            numericInput(
              ns("spike_threshold"),
              "Spike Threshold (SD)",
              value = 3,
              min = 2,
              max = 5,
              step = 0.5
            ),
            
            numericInput(
              ns("min_spike_distance"),
              "Min Spike Distance (frames)",
              value = 10,
              min = 5,
              max = 50,
              step = 5
            )
          ),
          
          hr(),
          
          actionButton(
            ns("calculate_metrics"),
            "Calculate Metrics",
            icon = icon("calculator"),
            class = "btn-primary",
            width = "100%"
          ),
          
          br(), br(),
          
          downloadButton(
            ns("download_metrics"),
            "Download Metrics",
            class = "btn-success",
            style = "width: 100%;"
          )
        )
      ),
      
      # Middle column - Results
      column(6,
        tabsetPanel(
          id = ns("metrics_tabs"),
          
          # Summary tab
          tabPanel(
            "Summary",
            icon = icon("table"),
            br(),
            DT::dataTableOutput(ns("metrics_summary")) %>%
              withSpinner(type = 4)
          ),
          
          # Individual metrics tab
          tabPanel(
            "Individual Metrics",
            icon = icon("chart-bar"),
            br(),
            selectInput(
              ns("display_metric"),
              "Select Metric to Display",
              choices = NULL,
              width = "100%"
            ),
            plotOutput(ns("metrics_barplot"), height = "400px") %>%
              withSpinner(type = 4)
          ),
          
          # Heatmap tab
          tabPanel(
            "Heatmap",
            icon = icon("th"),
            br(),
            plotOutput(ns("metrics_heatmap"), height = "500px") %>%
              withSpinner(type = 4)
          ),
          
          # Correlation tab
          tabPanel(
            "Correlations",
            icon = icon("project-diagram"),
            br(),
            plotOutput(ns("correlation_plot"), height = "500px") %>%
              withSpinner(type = 4)
          ),
          
          # Spike analysis tab (conditional)
          tabPanel(
            "Spike Analysis",
            icon = icon("bolt"),
            br(),
            conditionalPanel(
              condition = "input.detect_spikes == true",
              ns = ns,
              plotOutput(ns("spike_raster"), height = "400px") %>%
                withSpinner(type = 4),
              br(),
              tableOutput(ns("spike_statistics"))
            )
          )
        )
      ),
      
      # Right column - Statistics
      column(3,
        h4("Group Statistics"),
        tableOutput(ns("group_stats")),
        
        br(),
        
        h4("Quality Control"),
        plotOutput(ns("qc_plot"), height = "250px"),
        
        br(),
        
        h4("Export Options"),
        radioButtons(
          ns("export_format"),
          "Export Format",
          choices = list(
            "CSV" = "csv",
            "Excel" = "xlsx",
            "R Data" = "rds"
          ),
          selected = "csv"
        ),
        
        checkboxInput(
          ns("include_stats"),
          "Include Summary Statistics",
          value = TRUE
        )
      )
    )
  )
}

# Module Server
mod_metrics_server <- function(id, preprocessed_module) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      metrics_data = NULL,
      spike_data = NULL,
      group_stats = NULL
    )
    
    # Calculate metrics
    observeEvent(input$calculate_metrics, {
      req(preprocessed_module$data())
      
      withProgress(message = "Calculating metrics...", value = 0, {
        
        data_list <- preprocessed_module$data()
        all_metrics <- list()
        
        n_datasets <- length(data_list)
        
        for (i in seq_along(data_list)) {
          name <- names(data_list)[i]
          df <- data_list[[name]]
          
          incProgress(1/n_datasets, detail = paste("Processing", name))
          
          # Calculate metrics using improved function
          metrics <- calculate_cell_metrics_improved(
            df,
            baseline_frames = input$baseline_frames,
            selected_metrics = input$selected_metrics
          )
          
          # Add group label
          metrics$Group <- name
          
          # Detect spikes if requested
          if (input$detect_spikes) {
            spike_results <- detect_spikes_in_dataset(
              df,
              threshold = input$spike_threshold,
              min_distance = input$min_spike_distance
            )
            
            # Add spike count to metrics
            metrics <- merge(
              metrics,
              spike_results$spike_counts,
              by.x = "Cell_ID",
              by.y = "Cell",
              all.x = TRUE
            )
            
            # Store spike data
            if (is.null(values$spike_data)) {
              values$spike_data <- spike_results
            } else {
              values$spike_data <- rbind(values$spike_data, spike_results)
            }
          }
          
          all_metrics[[name]] <- metrics
        }
        
        # Combine all metrics
        values$metrics_data <- dplyr::bind_rows(all_metrics)
        
        # Calculate group statistics
        values$group_stats <- calculate_group_statistics(values$metrics_data)
        
        # Update metric selector
        metric_choices <- intersect(
          input$selected_metrics,
          names(values$metrics_data)
        )
        
        updateSelectInput(
          session,
          "display_metric",
          choices = metric_choices,
          selected = metric_choices[1]
        )
        
        showNotification(
          "Metrics calculated successfully!",
          type = "success"
        )
      })
    })
    
    # Metrics summary table
    output$metrics_summary <- DT::renderDataTable({
      req(values$metrics_data)
      
      # Format numeric columns
      numeric_cols <- sapply(values$metrics_data, is.numeric)
      
      DT::datatable(
        values$metrics_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        filter = 'top'
      ) %>%
        DT::formatRound(columns = which(numeric_cols), digits = 3)
    })
    
    # Metrics barplot
    output$metrics_barplot <- renderPlot({
      req(values$metrics_data, input$display_metric)
      
      metric_label <- get_metric_label(input$display_metric)
      
      # Sort by metric value
      plot_data <- values$metrics_data %>%
        arrange(desc(.data[[input$display_metric]]))
      
      ggplot(plot_data, aes(x = reorder(Cell_ID, .data[[input$display_metric]]), 
                            y = .data[[input$display_metric]],
                            fill = Group)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(
          title = paste("Individual Cell", metric_label),
          x = "Cell ID",
          y = metric_label
        ) +
        scale_fill_manual(values = DEFAULT_COLORS) +
        APP_THEME +
        theme(axis.text.y = element_text(size = 8))
    })
    
    # Metrics heatmap
    output$metrics_heatmap <- renderPlot({
      req(values$metrics_data)
      
      # Prepare data for heatmap
      numeric_metrics <- input$selected_metrics
      
      heatmap_data <- values$metrics_data %>%
        select(Cell_ID, all_of(numeric_metrics)) %>%
        column_to_rownames("Cell_ID")
      
      # Normalize each metric to 0-1 scale
      heatmap_scaled <- scale(heatmap_data)
      
      # Create heatmap
      pheatmap::pheatmap(
        heatmap_scaled,
        cluster_rows = TRUE,
        cluster_cols = FALSE,
        show_rownames = TRUE,
        show_colnames = TRUE,
        main = "Metrics Heatmap (Z-scaled)",
        color = colorRampPalette(c("blue", "white", "red"))(100),
        fontsize_row = 8,
        fontsize_col = 10
      )
    })
    
    # Correlation plot
    output$correlation_plot <- renderPlot({
      req(values$metrics_data)
      
      # Select numeric columns
      numeric_data <- values$metrics_data %>%
        select(all_of(input$selected_metrics))
      
      # Calculate correlations
      cor_matrix <- cor(numeric_data, use = "complete.obs")
      
      # Create correlation plot
      corrplot::corrplot(
        cor_matrix,
        method = "circle",
        type = "upper",
        order = "hclust",
        tl.cex = 0.8,
        tl.col = "black",
        col = colorRampPalette(c("blue", "white", "red"))(100),
        addCoef.col = "black",
        number.cex = 0.7
      )
    })
    
    # Spike raster plot
    output$spike_raster <- renderPlot({
      req(values$spike_data)
      
      # Create raster plot
      spike_df <- values$spike_data$spike_times
      
      ggplot(spike_df, aes(x = Time, y = Cell, color = Group)) +
        geom_point(size = 0.5) +
        labs(
          title = "Spike Raster Plot",
          x = "Time (s)",
          y = "Cell"
        ) +
        scale_color_manual(values = DEFAULT_COLORS) +
        APP_THEME +
        theme(axis.text.y = element_text(size = 6))
    })
    
    # Spike statistics
    output$spike_statistics <- renderTable({
      req(values$spike_data)
      
      spike_stats <- values$spike_data$spike_counts %>%
        group_by(Group) %>%
        summarise(
          `Mean Spikes` = round(mean(Spike_Count), 2),
          `SD Spikes` = round(sd(Spike_Count), 2),
          `Max Spikes` = max(Spike_Count),
          `Cells with Spikes` = sum(Spike_Count > 0),
          .groups = "drop"
        )
      
      spike_stats
    })
    
    # Group statistics
    output$group_stats <- renderTable({
      req(values$group_stats)
      
      # Format for display
      display_stats <- values$group_stats %>%
        select(Group, Metric, Mean, SEM, N) %>%
        mutate(
          Mean = round(Mean, 3),
          SEM = round(SEM, 3)
        )
      
      display_stats
    })
    
    # QC plot
    output$qc_plot <- renderPlot({
      req(values$metrics_data)
      
      # Create QC plot showing distribution of SNR
      if ("SNR" %in% names(values$metrics_data)) {
        ggplot(values$metrics_data, aes(x = Group, y = SNR, fill = Group)) +
          geom_boxplot(alpha = 0.7) +
          geom_jitter(width = 0.2, alpha = 0.3) +
          labs(
            title = "Signal Quality (SNR)",
            x = "",
            y = "Signal-to-Noise Ratio"
          ) +
          scale_fill_manual(values = DEFAULT_COLORS) +
          APP_THEME +
          theme(legend.position = "none")
      }
    })
    
    # Download handler
    output$download_metrics <- downloadHandler(
      filename = function() {
        paste0("calcium_metrics_", Sys.Date(), ".", input$export_format)
      },
      content = function(file) {
        data_to_export <- values$metrics_data
        
        # Add statistics if requested
        if (input$include_stats) {
          stats_to_export <- values$group_stats
        }
        
        if (input$export_format == "csv") {
          write.csv(data_to_export, file, row.names = FALSE)
        } else if (input$export_format == "xlsx") {
          wb <- openxlsx::createWorkbook()
          openxlsx::addWorksheet(wb, "Metrics")
          openxlsx::writeData(wb, "Metrics", data_to_export)
          
          if (input$include_stats) {
            openxlsx::addWorksheet(wb, "Statistics")
            openxlsx::writeData(wb, "Statistics", stats_to_export)
          }
          
          openxlsx::saveWorkbook(wb, file)
        } else if (input$export_format == "rds") {
          export_list <- list(
            metrics = data_to_export,
            statistics = if (input$include_stats) stats_to_export else NULL
          )
          saveRDS(export_list, file)
        }
      }
    )
    
    # Return metrics data
    return(
      list(
        metrics = reactive(values$metrics_data),
        group_stats = reactive(values$group_stats)
      )
    )
  })
}

# Helper functions

calculate_cell_metrics_improved <- function(df, baseline_frames = 20, selected_metrics = NULL) {
  
  time_vec <- df$Time
  cell_cols <- setdiff(names(df), c("Time", "Label", "Group"))
  
  # Filter to valid columns
  valid_cols <- character()
  for (col in cell_cols) {
    if (is.numeric(df[[col]]) && any(is.finite(df[[col]]))) {
      valid_cols <- c(valid_cols, col)
    }
  }
  
  if (length(valid_cols) == 0) {
    return(data.frame())
  }
  
  # Use vectorized calculation if many cells
  if (length(valid_cols) > 50) {
    metrics <- compute_metrics_vectorized(df, time_vec, baseline_frames)
  } else {
    # Calculate metrics for each cell
    metrics_list <- lapply(valid_cols, function(col_name) {
      signal <- df[[col_name]]
      
      # Skip if all NA
      if (all(is.na(signal))) {
        return(NULL)
      }
      
      # Calculate all metrics
      cell_metrics <- calculate_single_cell_metrics(
        signal,
        time_vec,
        baseline_frames
      )
      
      cell_metrics$Cell_ID <- col_name
      cell_metrics
    })
    
    metrics <- dplyr::bind_rows(metrics_list)
  }
  
  # Filter to selected metrics if specified
  if (!is.null(selected_metrics)) {
    keep_cols <- c("Cell_ID", selected_metrics)
    metrics <- metrics[, intersect(names(metrics), keep_cols)]
  }
  
  metrics
}

calculate_single_cell_metrics <- function(signal, time_vec, baseline_frames = 20) {
  
  # Calculate baseline
  baseline_idx <- min(baseline_frames, floor(length(signal) * 0.1))
  baseline <- mean(signal[1:baseline_idx], na.rm = TRUE)
  baseline_sd <- sd(signal[1:baseline_idx], na.rm = TRUE)
  
  # Find peak
  peak_idx <- which.max(signal)
  peak_value <- signal[peak_idx]
  peak_time <- time_vec[peak_idx]
  
  # Response amplitude
  response_amp <- peak_value - baseline
  
  # SNR
  snr <- if (baseline_sd > 0) response_amp / baseline_sd else NA
  
  # AUC (trapezoidal integration)
  above_baseline <- pmax(signal - baseline, 0)
  valid_idx <- !is.na(above_baseline)
  if (sum(valid_idx) > 1) {
    x <- time_vec[valid_idx]
    y <- above_baseline[valid_idx]
    auc <- sum(diff(x) * (y[-1] + y[-length(y)]) / 2)
  } else {
    auc <- NA
  }
  
  # Rise times
  rise_times <- calculate_rise_times(time_vec, signal, baseline, peak_idx)
  
  # FWHM
  fwhm_result <- calculate_fwhm_hwhm(time_vec, signal, baseline)
  
  # Ca entry rate
  if (length(signal) > 5) {
    smoothed <- stats::filter(signal, rep(1/5, 5), sides = 2)
    rates <- diff(smoothed) / diff(time_vec)
    ca_entry_rate <- max(rates, na.rm = TRUE)
  } else {
    ca_entry_rate <- NA
  }
  
  # Return all metrics
  data.frame(
    Peak_dFF0 = peak_value,
    Time_to_Peak = peak_time,
    Rise_Time = rise_times$rise_10_90,
    AUC = auc,
    Half_Width = fwhm_result$hwhm,
    SNR = snr,
    Response_Amplitude = response_amp,
    Calcium_Entry_Rate = ca_entry_rate,
    stringsAsFactors = FALSE
  )
}

detect_spikes_in_dataset <- function(df, threshold = 3, min_distance = 10) {
  
  time_vec <- df$Time
  cell_cols <- setdiff(names(df), c("Time", "Label", "Group"))
  
  spike_times <- list()
  spike_counts <- data.frame(
    Cell = character(),
    Spike_Count = integer(),
    stringsAsFactors = FALSE
  )
  
  for (col in cell_cols) {
    signal <- df[[col]]
    
    # Detect spikes
    spikes <- detect_spikes(signal, threshold, min_distance)
    
    # Store results
    if (length(spikes) > 0) {
      spike_times[[col]] <- data.frame(
        Cell = col,
        Time = time_vec[spikes],
        stringsAsFactors = FALSE
      )
    }
    
    spike_counts <- rbind(
      spike_counts,
      data.frame(
        Cell = col,
        Spike_Count = length(spikes),
        stringsAsFactors = FALSE
      )
    )
  }
  
  list(
    spike_times = dplyr::bind_rows(spike_times),
    spike_counts = spike_counts
  )
}

calculate_group_statistics <- function(metrics_data) {
  
  # Calculate mean and SEM for each metric by group
  numeric_cols <- sapply(metrics_data, is.numeric)
  metric_cols <- names(metrics_data)[numeric_cols]
  
  stats_list <- list()
  
  for (metric in metric_cols) {
    group_stats <- metrics_data %>%
      group_by(Group) %>%
      summarise(
        Mean = mean(.data[[metric]], na.rm = TRUE),
        SD = sd(.data[[metric]], na.rm = TRUE),
        N = sum(!is.na(.data[[metric]])),
        SEM = SD / sqrt(N),
        .groups = "drop"
      ) %>%
      mutate(Metric = metric)
    
    stats_list[[metric]] <- group_stats
  }
  
  dplyr::bind_rows(stats_list)
}