#!/usr/bin/env Rscript

# Server function for Complete Calcium Imaging Analysis App v2.5
# This file contains the server logic for app_complete.R

server <- function(input, output, session) {
  
  # ========== REACTIVE VALUES ==========
  values <- reactiveValues(
    # Individual analysis data
    uploaded_data = list(),
    processed_data = list(),
    metrics_data = list(),
    
    # Group comparison data
    gc_groups = list(),
    gc_metrics = NULL,
    gc_stats = NULL,
    
    # System
    memory_info = NULL,
    session_data = NULL,
    
    # Batch processing
    batch_results = list(),
    batch_progress = 0,
    
    # Settings
    app_settings = list(
      max_cells = 1000,
      chunk_size = 100,
      use_parallel = FALSE,
      n_cores = 2,
      theme = "default",
      show_tooltips = TRUE,
      auto_save = FALSE,
      auto_save_interval = 10,
      default_baseline = 20,
      default_norm_method = "mean",
      auto_detect_peaks = TRUE,
      auto_classify_cells = FALSE,
      default_export_format = "csv",
      plot_dpi = 300
    )
  )
  
  # ========== MODULE SERVERS ==========
  
  # Individual Analysis Modules
  # Wire modules together so downstream modules receive the module return objects
  data_module <- mod_data_loading_server("ind_data_module")
  preproc_module <- mod_preprocessing_server("ind_preprocess_module", data_module)
  metrics_module <- mod_metrics_server("ind_metrics_module", preproc_module)
  viz_module <- mod_visualization_server("ind_viz_module", preproc_module, metrics_module)
  tables_module <- mod_tables_server("ind_tables_module", data_module, metrics_module)
  guide_module <- mod_metric_guide_server("guide_module", data_module)
  export_module <- mod_export_server("ind_export_module", reactive(values))
  
  # Advanced Analysis Modules  
  stats_module <- mod_statistical_analysis_server("stats_module", reactive(values))
  
  # ========== GROUP COMPARISON LOGIC ==========
  
  # Load group data
  observeEvent(input$gc_append_btn, {
    req(input$gc_files, input$gc_group_name)
    
    withProgress(message = "Loading group data...", {
      group_data <- list()
      
      for(i in seq_along(input$gc_files$datapath)) {
        tryCatch({
          dt <- safe_read(input$gc_files$datapath[i])
          dt <- ensure_time_first(dt)
          dt <- coerce_numeric_dt(dt)
          
          group_data[[input$gc_files$name[i]]] <- dt
          incProgress(1/length(input$gc_files$datapath))
        }, error = function(e) {
          showNotification(paste("Error loading", input$gc_files$name[i], ":", e$message), 
                         type = "error")
        })
      }
      
      if(length(group_data) > 0) {
        values$gc_groups[[input$gc_group_name]] <- group_data
        showNotification(paste("Added group:", input$gc_group_name), type = "success")
        
        # Reset input
        updateTextInput(session, "gc_group_name", value = "")
      }
    })
  })
  
  # Load demo data for group comparison
  observeEvent(input$gc_demo_btn, {
    withProgress(message = "Generating demo data...", {
      # Generate 3 groups with different characteristics
      group1 <- simulate_group("Control", n_cells = 50, amp = 0.3, noise = 0.05)
      group2 <- simulate_group("Treatment_A", n_cells = 50, amp = 0.6, noise = 0.08)
      group3 <- simulate_group("Treatment_B", n_cells = 50, amp = 0.9, noise = 0.10)
      
      values$gc_groups <- list(
        "Control" = list("demo_control.csv" = group1$dt),
        "Treatment_A" = list("demo_treatA.csv" = group2$dt),
        "Treatment_B" = list("demo_treatB.csv" = group3$dt)
      )
      
      showNotification("Demo data loaded", type = "success")
    })
  })
  
  # Clear all groups
  observeEvent(input$gc_clear_all, {
    values$gc_groups <- list()
    values$gc_metrics <- NULL
    values$gc_stats <- NULL
    showNotification("All groups cleared", type = "info")
  })
  
  # Group comparison metrics calculation
  gc_metrics_reactive <- reactive({
    req(length(values$gc_groups) > 0)
    
    withProgress(message = "Calculating metrics...", {
      all_metrics <- list()
      
      for(group_name in names(values$gc_groups)) {
        group_data <- values$gc_groups[[group_name]]
        
        for(file_name in names(group_data)) {
          dt <- group_data[[file_name]]
          
          # Calculate metrics for each cell
          cell_cols <- names(dt)[-1]
          for(cell in cell_cols) {
            metrics <- calculate_cell_metrics(
              dt[[cell]], 
              dt$Time,
              baseline_frames = isolate(values$app_settings$default_baseline)
            )
            metrics$Group <- group_name
            metrics$File <- file_name
            metrics$Cell <- cell
            all_metrics[[paste(group_name, file_name, cell, sep = "_")]] <- metrics
          }
        }
        incProgress(1/length(values$gc_groups))
      }
      
      do.call(rbind, all_metrics)
    })
  })
  
  # ========== GROUP COMPARISON OUTPUTS ==========
  
  # Groups table
  output$gc_groups_table <- DT::renderDataTable({
    req(length(values$gc_groups) > 0)
    
    summary_data <- data.frame(
      Group = character(),
      Files = integer(),
      Cells = integer(),
      stringsAsFactors = FALSE
    )
    
    for(group_name in names(values$gc_groups)) {
      n_files <- length(values$gc_groups[[group_name]])
      n_cells <- sum(sapply(values$gc_groups[[group_name]], function(x) ncol(x) - 1))
      
      summary_data <- rbind(summary_data, data.frame(
        Group = group_name,
        Files = n_files,
        Cells = n_cells,
        stringsAsFactors = FALSE
      ))
    }
    
    DT::datatable(summary_data, options = list(pageLength = 10, dom = 't'))
  })
  
  # Data summary
  output$gc_data_summary <- renderPrint({
    if(length(values$gc_groups) == 0) {
      cat("No groups loaded\n")
    } else {
      cat("Groups loaded:", length(values$gc_groups), "\n")
      cat("Total files:", sum(sapply(values$gc_groups, length)), "\n")
      cat("Total cells:", sum(sapply(values$gc_groups, function(g) 
        sum(sapply(g, function(x) ncol(x) - 1)))), "\n")
    }
  })
  
  # Status text
  output$gc_status_text <- renderText({
    if(length(values$gc_groups) == 0) {
      "Ready to load data"
    } else {
      paste(length(values$gc_groups), "groups loaded")
    }
  })
  
  # Time course plot (static)
  output$gc_timecourse_plot <- renderPlot({
    req(length(values$gc_groups) > 0)
    
    # Prepare data for plotting
    plot_data <- list()
    
    for(group_name in names(values$gc_groups)) {
      group_data <- values$gc_groups[[group_name]]
      
      # Combine all files in group
      combined_data <- list()
      for(file_name in names(group_data)) {
        dt <- group_data[[file_name]]
        long_dt <- dt %>%
          pivot_longer(cols = -Time, names_to = "Cell", values_to = "Value") %>%
          mutate(Group = group_name, File = file_name)
        combined_data[[file_name]] <- long_dt
      }
      plot_data[[group_name]] <- do.call(rbind, combined_data)
    }
    
    all_data <- do.call(rbind, plot_data)
    
    # Calculate mean and SEM
    summary_data <- all_data %>%
      group_by(Group, Time) %>%
      summarise(
        Mean = mean(Value, na.rm = TRUE),
        SEM = sd(Value, na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      )
    
    # Create plot
    p <- ggplot(summary_data, aes(x = Time, y = Mean, color = Group)) +
      geom_line(size = 1.5)
    
    if(input$gc_time_show_ribbon) {
      p <- p + geom_ribbon(aes(ymin = Mean - SEM, ymax = Mean + SEM, fill = Group), 
                          alpha = 0.3, color = NA)
    }
    
    if(input$gc_time_show_points) {
      p <- p + geom_point(size = 2)
    }
    
    if(input$gc_time_show_individual) {
      p <- p + geom_line(data = all_data, 
                        aes(x = Time, y = Value, group = interaction(Group, File, Cell)),
                        alpha = input$gc_time_alpha, size = 0.5)
    }
    
    # Apply theme
    p <- p + switch(input$gc_time_theme,
      "Classic" = theme_classic(),
      "Minimal" = theme_minimal(),
      "Dark" = theme_dark(),
      "Publication" = theme_bw()
    )
    
    # Customize appearance
    p <- p + theme(
      text = element_text(size = input$gc_time_font_size),
      legend.position = if(input$gc_time_show_legend) input$gc_time_legend_pos else "none"
    )
    
    if(!input$gc_time_show_grid) {
      p <- p + theme(panel.grid = element_blank())
    }
    
    p <- p + labs(
      x = "Time (s)",
      y = expression(Delta*F/F[0]),
      title = "Group Comparison Time Course"
    )
    
    p
  })
  
  # Time course plot (interactive)
  output$gc_timecourse_plotly <- renderPlotly({
    req(length(values$gc_groups) > 0)
    
    # Use the static plot and convert to plotly
    p <- ggplotly(output$gc_timecourse_plot())
    p
  })
  
  # Metrics plot
  output$gc_metrics_plot <- renderPlot({
    metrics_data <- gc_metrics_reactive()
    req(!is.null(metrics_data))
    
    # Filter for selected metric
    metric_col <- input$gc_metric_name
    
    # Calculate summary statistics
    summary_stats <- metrics_data %>%
      group_by(Group) %>%
      summarise(
        Mean = mean(.data[[metric_col]], na.rm = TRUE),
        SEM = sd(.data[[metric_col]], na.rm = TRUE) / sqrt(n()),
        n = n(),
        .groups = "drop"
      )
    
    # Create base plot
    p <- ggplot(summary_stats, aes(x = Group, y = Mean, fill = Group))
    
    # Add geometry based on selection
    if(input$gc_metric_geom == "bar") {
      p <- p + geom_col() +
        geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 0.2)
    } else if(input$gc_metric_geom == "box") {
      p <- p + geom_boxplot(data = metrics_data, aes(x = Group, y = .data[[metric_col]]))
    } else if(input$gc_metric_geom == "violin") {
      p <- p + geom_violin(data = metrics_data, aes(x = Group, y = .data[[metric_col]]))
    } else if(input$gc_metric_geom == "dot") {
      p <- p + geom_dotplot(data = metrics_data, aes(x = Group, y = .data[[metric_col]]),
                           binaxis = "y", stackdir = "center")
    }
    
    # Add individual points if requested
    if(input$gc_metric_points) {
      p <- p + geom_jitter(data = metrics_data, aes(x = Group, y = .data[[metric_col]]),
                          width = 0.1, alpha = 0.5, size = 1)
    }
    
    # Statistical testing
    if(input$gc_metric_stats) {
      n_groups <- length(unique(metrics_data$Group))
      
      if(n_groups == 2) {
        # Two group comparison
        if(input$gc_stat_test == "wilcox") {
          test_result <- wilcox.test(
            metrics_data[[metric_col]][metrics_data$Group == unique(metrics_data$Group)[1]],
            metrics_data[[metric_col]][metrics_data$Group == unique(metrics_data$Group)[2]]
          )
        } else {
          test_result <- t.test(
            metrics_data[[metric_col]][metrics_data$Group == unique(metrics_data$Group)[1]],
            metrics_data[[metric_col]][metrics_data$Group == unique(metrics_data$Group)[2]]
          )
        }
        
        p_value_text <- sprintf("p = %.4f", test_result$p.value)
        p <- p + annotate("text", x = 1.5, y = max(summary_stats$Mean + summary_stats$SEM) * 1.1,
                         label = p_value_text, size = 4)
      } else if(n_groups > 2) {
        # Multiple group comparison
        if(input$gc_stat_test == "anova") {
          test_result <- aov(as.formula(paste(metric_col, "~ Group")), data = metrics_data)
          p_value <- summary(test_result)[[1]][["Pr(>F)"]][1]
        } else {
          test_result <- kruskal.test(as.formula(paste(metric_col, "~ Group")), data = metrics_data)
          p_value <- test_result$p.value
        }
        
        p_value_text <- sprintf("p = %.4f", p_value)
        p <- p + annotate("text", x = n_groups/2, y = max(summary_stats$Mean + summary_stats$SEM) * 1.1,
                         label = p_value_text, size = 4)
      }
    }
    
    # Add mean ± SEM text if requested
    if(input$gc_show_insets) {
      for(i in 1:nrow(summary_stats)) {
        text_label <- sprintf("%.3f ± %.3f\nn = %d", 
                            summary_stats$Mean[i], 
                            summary_stats$SEM[i],
                            summary_stats$n[i])
        
        y_pos <- if(input$gc_inset_pos == "above") {
          summary_stats$Mean[i] + summary_stats$SEM[i] + 
            diff(range(c(0, max(summary_stats$Mean + summary_stats$SEM)))) * 0.05
        } else {
          -diff(range(c(0, max(summary_stats$Mean + summary_stats$SEM)))) * 0.1
        }
        
        p <- p + annotate("text", x = i, y = y_pos, label = text_label, size = 3)
      }
    }
    
    # Customize appearance
    p <- p + theme_classic() +
      theme(text = element_text(size = input$gc_metric_size)) +
      labs(
        x = "",
        y = gsub("_", " ", metric_col),
        title = paste("Group Comparison:", gsub("_", " ", metric_col))
      )
    
    p
  })
  
  # Statistical summary
  output$gc_stats_summary <- renderPrint({
    metrics_data <- gc_metrics_reactive()
    req(!is.null(metrics_data))
    
    metric_col <- input$gc_metric_name
    n_groups <- length(unique(metrics_data$Group))
    
    cat("Statistical Test Results\n")
    cat("========================\n\n")
    cat("Metric:", gsub("_", " ", metric_col), "\n")
    cat("Number of groups:", n_groups, "\n\n")
    
    if(n_groups == 2) {
      groups <- unique(metrics_data$Group)
      group1_data <- metrics_data[[metric_col]][metrics_data$Group == groups[1]]
      group2_data <- metrics_data[[metric_col]][metrics_data$Group == groups[2]]
      
      if(input$gc_stat_test == "wilcox") {
        test_result <- wilcox.test(group1_data, group2_data)
        cat("Test: Wilcoxon rank sum test\n")
      } else {
        test_result <- t.test(group1_data, group2_data)
        cat("Test: Two-sample t-test\n")
      }
      
      cat("P-value:", format.pval(test_result$p.value, digits = 4), "\n")
      
    } else if(n_groups > 2) {
      if(input$gc_stat_test == "anova") {
        test_result <- aov(as.formula(paste(metric_col, "~ Group")), data = metrics_data)
        cat("Test: One-way ANOVA\n")
        print(summary(test_result))
      } else {
        test_result <- kruskal.test(as.formula(paste(metric_col, "~ Group")), data = metrics_data)
        cat("Test: Kruskal-Wallis test\n")
        cat("Chi-squared:", test_result$statistic, "\n")
        cat("P-value:", format.pval(test_result$p.value, digits = 4), "\n")
      }
    }
  })
  
  # Post-hoc table
  output$gc_posthoc_table <- DT::renderDataTable({
    req(input$gc_show_posthoc)
    metrics_data <- gc_metrics_reactive()
    req(!is.null(metrics_data))
    
    metric_col <- input$gc_metric_name
    n_groups <- length(unique(metrics_data$Group))
    
    if(n_groups > 2) {
      # Perform pairwise comparisons
      groups <- unique(metrics_data$Group)
      comparisons <- combn(groups, 2)
      
      results <- data.frame(
        Comparison = character(),
        P_value = numeric(),
        Adjusted_P = numeric(),
        stringsAsFactors = FALSE
      )
      
      for(i in 1:ncol(comparisons)) {
        group1 <- comparisons[1, i]
        group2 <- comparisons[2, i]
        
        group1_data <- metrics_data[[metric_col]][metrics_data$Group == group1]
        group2_data <- metrics_data[[metric_col]][metrics_data$Group == group2]
        
        if(input$gc_stat_test %in% c("kruskal", "wilcox")) {
          test_result <- wilcox.test(group1_data, group2_data)
        } else {
          test_result <- t.test(group1_data, group2_data)
        }
        
        results <- rbind(results, data.frame(
          Comparison = paste(group1, "vs", group2),
          P_value = test_result$p.value,
          Adjusted_P = NA,
          stringsAsFactors = FALSE
        ))
      }
      
      # Apply multiple comparison correction
      if(input$gc_correction_method != "none") {
        results$Adjusted_P <- p.adjust(results$P_value, method = input$gc_correction_method)
      } else {
        results$Adjusted_P <- results$P_value
      }
      
      DT::datatable(results, options = list(pageLength = 10))
    } else {
      DT::datatable(data.frame(Message = "Post-hoc tests require more than 2 groups"))
    }
  })
  
  # Heatmap plot
  output$gc_heatmap_plot <- renderPlot({
    req(length(values$gc_groups) > 0)
    
    # Combine all data
    all_data <- list()
    
    for(group_name in names(values$gc_groups)) {
      group_data <- values$gc_groups[[group_name]]
      
      for(file_name in names(group_data)) {
        dt <- group_data[[file_name]]
        
        # Convert to matrix (cells x time)
        mat <- as.matrix(dt[, -1])
        mat <- t(mat)
        
        # Add group annotation
        rownames(mat) <- paste0(group_name, "_", rownames(mat))
        all_data[[paste(group_name, file_name, sep = "_")]] <- mat
      }
    }
    
    combined_mat <- do.call(rbind, all_data)
    
    # Apply scaling
    if(input$gc_heatmap_scale == "row") {
      combined_mat <- t(scale(t(combined_mat)))
    } else if(input$gc_heatmap_scale == "column") {
      combined_mat <- scale(combined_mat)
    } else if(input$gc_heatmap_scale == "global") {
      combined_mat <- (combined_mat - min(combined_mat, na.rm = TRUE)) / 
                     (max(combined_mat, na.rm = TRUE) - min(combined_mat, na.rm = TRUE))
    }
    
    # Clustering
    if(input$gc_heatmap_cluster_rows) {
      row_order <- hclust(dist(combined_mat))$order
      combined_mat <- combined_mat[row_order, ]
    }
    
    if(input$gc_heatmap_cluster_cols) {
      col_order <- hclust(dist(t(combined_mat)))$order
      combined_mat <- combined_mat[, col_order]
    }
    
    # Convert to long format for ggplot
    heatmap_data <- expand.grid(
      Cell = factor(rownames(combined_mat), levels = rownames(combined_mat)),
      Time = 1:ncol(combined_mat)
    )
    heatmap_data$Value <- as.vector(combined_mat)
    
    # Create heatmap
    p <- ggplot(heatmap_data, aes(x = Time, y = Cell, fill = Value)) +
      geom_tile() +
      scale_fill_viridis_c(option = tolower(input$gc_heatmap_palette)) +
      theme_minimal() +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        text = element_text(size = input$gc_heatmap_font)
      ) +
      labs(
        x = "Time",
        y = "Cells",
        fill = expression(Delta*F/F[0]),
        title = "Activity Heatmap"
      )
    
    # Add group annotations if requested
    if(input$gc_heatmap_annotate_groups) {
      # Extract group info from rownames
      group_info <- sub("_.*", "", rownames(combined_mat))
      unique_groups <- unique(group_info)
      
      # Add colored bars for groups
      group_colors <- scales::hue_pal()(length(unique_groups))
      names(group_colors) <- unique_groups
      
      annotation_data <- data.frame(
        Cell = factor(rownames(combined_mat), levels = rownames(combined_mat)),
        Group = group_info
      )
      
      p_annotation <- ggplot(annotation_data, aes(x = 1, y = Cell, fill = Group)) +
        geom_tile() +
        scale_fill_manual(values = group_colors) +
        theme_void() +
        theme(legend.position = "none")
      
      # Combine plots
      p <- p_annotation + p + plot_layout(widths = c(0.05, 1))
    }
    
    p
  })
  
  # Summary table
  output$gc_summary_table <- DT::renderDataTable({
    metrics_data <- gc_metrics_reactive()
    req(!is.null(metrics_data))
    
    summary_table <- metrics_data %>%
      group_by(Group) %>%
      summarise(
        n_cells = n(),
        Peak_Mean = mean(Peak_dFF0, na.rm = TRUE),
        Peak_SEM = sd(Peak_dFF0, na.rm = TRUE) / sqrt(n()),
        Time_to_Peak_Mean = mean(Time_to_Peak, na.rm = TRUE),
        Time_to_Peak_SEM = sd(Time_to_Peak, na.rm = TRUE) / sqrt(n()),
        AUC_Mean = mean(AUC, na.rm = TRUE),
        AUC_SEM = sd(AUC, na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      )
    
    DT::datatable(summary_table, options = list(pageLength = 10))
  })
  
  # Individual metrics table
  output$gc_metrics_table <- DT::renderDataTable({
    metrics_data <- gc_metrics_reactive()
    req(!is.null(metrics_data))
    
    DT::datatable(metrics_data, options = list(
      pageLength = 25,
      scrollX = TRUE
    ))
  })
  
  # Time course summary table
  output$gc_timecourse_summary_table <- DT::renderDataTable({
    req(length(values$gc_groups) > 0)
    
    # Prepare summary data
    all_summaries <- list()
    
    for(group_name in names(values$gc_groups)) {
      group_data <- values$gc_groups[[group_name]]
      
      # Combine all files in group
      combined_values <- list()
      for(file_name in names(group_data)) {
        dt <- group_data[[file_name]]
        combined_values[[file_name]] <- as.matrix(dt[, -1])
      }
      
      # Calculate mean and SEM at each time point
      all_values <- do.call(cbind, combined_values)
      
      time_vec <- group_data[[1]]$Time
      summary_df <- data.frame(
        Time = time_vec,
        Group = group_name,
        Mean = rowMeans(all_values, na.rm = TRUE),
        SEM = apply(all_values, 1, function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))),
        n = apply(all_values, 1, function(x) sum(!is.na(x)))
      )
      
      all_summaries[[group_name]] <- summary_df
    }
    
    combined_summary <- do.call(rbind, all_summaries)
    
    # Pivot wider for better display
    wide_summary <- combined_summary %>%
      mutate(Mean_SEM = sprintf("%.3f ± %.3f", Mean, SEM)) %>%
      select(Time, Group, Mean_SEM) %>%
      pivot_wider(names_from = Group, values_from = Mean_SEM)
    
    DT::datatable(wide_summary, options = list(
      pageLength = 25,
      scrollX = TRUE
    ))
  })
  
  # ========== INDIVIDUAL ANALYSIS PREPROCESSING ==========
  
  # Apply preprocessing
  observeEvent(input$pp_apply, {
    req(length(values$uploaded_data) > 0)
    
    withProgress(message = "Processing data...", {
      processed <- list()
      
      for(name in names(values$uploaded_data)) {
        dt <- values$uploaded_data[[name]]
        
        # Apply preprocessing based on settings
        if(input$pp_enable) {
          
          # Background subtraction
          if(input$pp_apply_bg && input$pp_bg_col %in% names(dt)) {
            bg_values <- dt[[input$pp_bg_col]]
            cell_cols <- setdiff(names(dt)[-1], input$pp_bg_col)
            
            for(col in cell_cols) {
              dt[[col]] <- dt[[col]] - bg_values
            }
            
            # Remove background column
            dt[[input$pp_bg_col]] <- NULL
          }
          
          # Compute ΔF/F₀
          if(input$pp_compute_dff) {
            cell_cols <- names(dt)[-1]
            
            for(col in cell_cols) {
              values <- dt[[col]]
              
              # Calculate baseline based on method
              if(input$pp_baseline_method == "first_n") {
                baseline <- mean(values[1:min(input$pp_baseline_frames, length(values))], na.rm = TRUE)
              } else if(input$pp_baseline_method == "rolling_min") {
                baseline <- min(rollapply(values, width = input$pp_window_size, 
                                        FUN = mean, na.rm = TRUE, fill = NA), na.rm = TRUE)
              } else if(input$pp_baseline_method == "percentile") {
                baseline <- quantile(values, probs = input$pp_percentile/100, na.rm = TRUE)
              }
              
              # Apply ΔF/F₀
              if(baseline != 0) {
                dt[[col]] <- (values - baseline) / baseline
              }
            }
          }
          
          # Smoothing
          if(input$pp_smooth) {
            cell_cols <- names(dt)[-1]
            
            for(col in cell_cols) {
              if(input$pp_smooth_method == "ma") {
                dt[[col]] <- rollapply(dt[[col]], width = input$pp_smooth_window,
                                      FUN = mean, na.rm = TRUE, fill = NA)
              } else if(input$pp_smooth_method == "gaussian") {
                # Gaussian smoothing implementation
                sigma <- input$pp_smooth_window / 4
                weights <- dnorm(seq(-input$pp_smooth_window/2, input$pp_smooth_window/2), 0, sigma)
                weights <- weights / sum(weights)
                dt[[col]] <- stats::filter(dt[[col]], weights, sides = 2)
              } else if(input$pp_smooth_method == "sg") {
                # Savitzky-Golay filter would require signal package
                # For now, use simple smoothing
                dt[[col]] <- smooth.spline(dt[[col]], spar = 0.5)$y
              }
            }
          }
          
          # Detrending
          if(input$pp_detrend) {
            cell_cols <- names(dt)[-1]
            time_vec <- dt$Time
            
            for(col in cell_cols) {
              values <- dt[[col]]
              
              if(input$pp_detrend_method == "linear") {
                fit <- lm(values ~ time_vec)
                dt[[col]] <- residuals(fit) + mean(values, na.rm = TRUE)
              } else if(input$pp_detrend_method == "poly") {
                fit <- lm(values ~ poly(time_vec, input$pp_poly_order))
                dt[[col]] <- residuals(fit) + mean(values, na.rm = TRUE)
              } else if(input$pp_detrend_method == "exp") {
                # Exponential detrending
                fit <- nls(values ~ a * exp(b * time_vec) + c,
                          start = list(a = 1, b = -0.01, c = mean(values)),
                          control = nls.control(warnOnly = TRUE))
                if(!inherits(fit, "try-error")) {
                  dt[[col]] <- values - predict(fit) + mean(values, na.rm = TRUE)
                }
              }
            }
          }
          
          # Min-max normalization
          if(input$pp_minmax_enable) {
            cell_cols <- names(dt)[-1]
            
            for(col in cell_cols) {
              values <- dt[[col]]
              min_val <- min(values, na.rm = TRUE)
              max_val <- max(values, na.rm = TRUE)
              
              if(max_val != min_val) {
                dt[[col]] <- (values - min_val) / (max_val - min_val)
              }
            }
          }
        }
        
        processed[[name]] <- dt
        incProgress(1/length(values$uploaded_data))
      }
      
      values$processed_data <- processed
      
      # Calculate metrics for processed data
      all_metrics <- list()
      for(name in names(processed)) {
        dt <- processed[[name]]
        cell_cols <- names(dt)[-1]
        
        for(col in cell_cols) {
          metrics <- calculate_cell_metrics(dt[[col]], dt$Time, input$pp_baseline_frames)
          metrics$File <- name
          metrics$Cell <- col
          all_metrics[[paste(name, col, sep = "_")]] <- metrics
        }
      }
      
      values$metrics_data <- do.call(rbind, all_metrics)
      
      showNotification("Processing complete", type = "success")
    })
  })
  
  # Preview plot
  output$pp_preview_plot <- renderPlot({
    req(length(values$processed_data) > 0, input$pp_preview_cell)
    
    # Get selected file
    file_name <- names(values$processed_data)[1]
    dt_processed <- values$processed_data[[file_name]]
    dt_original <- values$uploaded_data[[file_name]]
    
    # Create comparison plot
    if(input$pp_preview_cell %in% names(dt_processed)) {
      plot_data <- data.frame(
        Time = rep(dt_processed$Time, 2),
        Value = c(dt_original[[input$pp_preview_cell]], dt_processed[[input$pp_preview_cell]]),
        Type = rep(c("Original", "Processed"), each = nrow(dt_processed))
      )
      
      ggplot(plot_data, aes(x = Time, y = Value, color = Type)) +
        geom_line(size = 1) +
        theme_minimal() +
        labs(x = "Time (s)", y = "Signal", title = paste("Cell:", input$pp_preview_cell))
    }
  })
  
  # Update preview cell choices
  observe({
    req(length(values$processed_data) > 0)
    
    file_name <- names(values$processed_data)[1]
    dt <- values$processed_data[[file_name]]
    cell_cols <- names(dt)[-1]
    
    updateSelectInput(session, "pp_preview_cell", choices = cell_cols, selected = cell_cols[1])
  })
  
  # Average metrics table
  output$preproc_avg_metrics <- DT::renderDataTable({
    req(!is.null(values$metrics_data))
    
    avg_metrics <- values$metrics_data %>%
      summarise(
        Peak_Mean = mean(Peak_dFF0, na.rm = TRUE),
        Peak_SEM = sd(Peak_dFF0, na.rm = TRUE) / sqrt(n()),
        Time_to_Peak_Mean = mean(Time_to_Peak, na.rm = TRUE),
        Time_to_Peak_SEM = sd(Time_to_Peak, na.rm = TRUE) / sqrt(n()),
        AUC_Mean = mean(AUC, na.rm = TRUE),
        AUC_SEM = sd(AUC, na.rm = TRUE) / sqrt(n()),
        n = n()
      )
    
    DT::datatable(avg_metrics, options = list(dom = 't'))
  })
  
  # ========== INDIVIDUAL ANALYSIS TIME COURSE ==========
  
  output$timecourse_plot <- renderPlot({
    req(length(values$processed_data) > 0)
    
    # Combine all processed data
    all_data <- list()
    for(name in names(values$processed_data)) {
      dt <- values$processed_data[[name]]
      long_dt <- dt %>%
        pivot_longer(cols = -Time, names_to = "Cell", values_to = "Value")
      all_data[[name]] <- long_dt
    }
    
    plot_data <- do.call(rbind, all_data)
    
    # Calculate mean and SEM
    summary_data <- plot_data %>%
      group_by(Time) %>%
      summarise(
        Mean = mean(Value, na.rm = TRUE),
        SEM = sd(Value, na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      )
    
    # Create plot
    p <- ggplot(summary_data, aes(x = Time, y = Mean)) +
      geom_line(color = input$tc_line_color, size = input$tc_line_width)
    
    # Add individual traces if requested
    if(input$tc_show_traces) {
      p <- p + geom_line(data = plot_data, 
                        aes(x = Time, y = Value, group = Cell),
                        alpha = input$tc_trace_transparency/100, 
                        color = input$tc_line_color,
                        size = input$tc_line_width * 0.5)
    }
    
    # Add SEM ribbon if requested
    if(input$tc_show_ribbon) {
      p <- p + geom_ribbon(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                          alpha = 0.3, fill = input$tc_line_color)
    }
    
    # Apply theme
    p <- p + switch(input$tc_theme,
      "classic" = theme_classic(),
      "minimal" = theme_minimal(),
      "light" = theme_light(),
      "dark" = theme_dark()
    )
    
    # Customize appearance
    p <- p + theme(
      plot.title = element_text(size = input$tc_title_size),
      axis.text = element_text(size = input$tc_axis_size),
      axis.title = element_text(size = input$tc_axis_title_size),
      legend.position = input$tc_legend_pos
    )
    
    # Grid lines
    if(!input$tc_grid_major) {
      p <- p + theme(panel.grid.major = element_blank())
    }
    if(!input$tc_grid_minor) {
      p <- p + theme(panel.grid.minor = element_blank())
    }
    
    # Labels
    p <- p + labs(
      x = input$tc_x,
      y = input$tc_y,
      title = if(nchar(input$tc_title) > 0) input$tc_title else NULL,
      subtitle = if(nchar(input$tc_subtitle) > 0) input$tc_subtitle else NULL
    )
    
    # Custom axis limits
    if(input$tc_limits) {
      if(!is.na(input$tc_xmin) && !is.na(input$tc_xmax)) {
        p <- p + xlim(input$tc_xmin, input$tc_xmax)
      }
      if(!is.na(input$tc_ymin) && !is.na(input$tc_ymax)) {
        p <- p + ylim(input$tc_ymin, input$tc_ymax)
      }
    }
    
    # Log scale
    if(input$tc_log_y) {
      p <- p + scale_y_log10()
    }
    
    p
  })
  
  # Time course summary
  output$tc_summary_table <- renderUI({
    req(length(values$processed_data) > 0)
    
    # Calculate summary statistics
    all_values <- list()
    for(name in names(values$processed_data)) {
      dt <- values$processed_data[[name]]
      all_values[[name]] <- as.matrix(dt[, -1])
    }
    
    combined_values <- do.call(cbind, all_values)
    
    n_cells <- ncol(combined_values)
    n_timepoints <- nrow(combined_values)
    
    # Peak statistics
    peaks <- apply(combined_values, 2, max, na.rm = TRUE)
    peak_mean <- mean(peaks, na.rm = TRUE)
    peak_sem <- sd(peaks, na.rm = TRUE) / sqrt(length(peaks))
    
    # Time to peak
    time_vec <- values$processed_data[[1]]$Time
    time_to_peaks <- apply(combined_values, 2, function(x) time_vec[which.max(x)])
    ttp_mean <- mean(time_to_peaks, na.rm = TRUE)
    ttp_sem <- sd(time_to_peaks, na.rm = TRUE) / sqrt(length(time_to_peaks))
    
    HTML(paste0(
      "<h4>Summary Statistics</h4>",
      "<table class='table table-condensed'>",
      "<tr><td>Number of cells:</td><td>", n_cells, "</td></tr>",
      "<tr><td>Number of timepoints:</td><td>", n_timepoints, "</td></tr>",
      "<tr><td>Peak ΔF/F₀:</td><td>", sprintf("%.3f ± %.3f", peak_mean, peak_sem), "</td></tr>",
      "<tr><td>Time to peak (s):</td><td>", sprintf("%.1f ± %.1f", ttp_mean, ttp_sem), "</td></tr>",
      "</table>"
    ))
  })
  
  # ========== INDIVIDUAL ANALYSIS HEATMAP ==========
  
  output$heatmap_plot <- renderPlot({
    req(length(values$processed_data) > 0)
    
    # Combine all data into matrix
    all_matrices <- list()
    for(name in names(values$processed_data)) {
      dt <- values$processed_data[[name]]
      mat <- as.matrix(dt[, -1])
      all_matrices[[name]] <- t(mat)
    }
    
    combined_mat <- do.call(rbind, all_matrices)
    
    # Filter cells if selected
    if(!is.null(input$hmap_cells) && length(input$hmap_cells) > 0) {
      combined_mat <- combined_mat[input$hmap_cells, , drop = FALSE]
    }
    
    # Apply normalization
    if(input$hmap_normalize == "row") {
      combined_mat <- t(scale(t(combined_mat)))
    } else if(input$hmap_normalize == "col") {
      combined_mat <- scale(combined_mat)
    } else if(input$hmap_normalize == "global") {
      combined_mat <- (combined_mat - min(combined_mat, na.rm = TRUE)) / 
                     (max(combined_mat, na.rm = TRUE) - min(combined_mat, na.rm = TRUE))
    }
    
    # Sort cells
    if(input$hmap_sort == "amplitude") {
      row_order <- order(apply(combined_mat, 1, max, na.rm = TRUE), decreasing = TRUE)
      combined_mat <- combined_mat[row_order, ]
    } else if(input$hmap_sort == "time") {
      row_order <- order(apply(combined_mat, 1, which.max))
      combined_mat <- combined_mat[row_order, ]
    } else if(input$hmap_sort == "cluster") {
      row_order <- hclust(dist(combined_mat))$order
      combined_mat <- combined_mat[row_order, ]
    }
    
    # Convert to long format
    heatmap_data <- expand.grid(
      Cell = factor(rownames(combined_mat), levels = rownames(combined_mat)),
      Time = 1:ncol(combined_mat)
    )
    heatmap_data$Value <- as.vector(combined_mat)
    
    # Create heatmap
    p <- ggplot(heatmap_data, aes(x = Time, y = Cell, fill = Value)) +
      geom_tile() +
      scale_fill_viridis_c(option = tolower(input$hmap_palette)) +
      theme_minimal() +
      theme(
        axis.text.y = if(nrow(combined_mat) > 50) element_blank() else element_text(size = 8),
        axis.ticks.y = element_blank(),
        text = element_text(size = input$hmap_font)
      ) +
      labs(
        x = "Time",
        y = "Cells",
        fill = expression(Delta*F/F[0]),
        title = input$hmap_title
      )
    
    p
  })
  
  # Update heatmap cell choices
  observe({
    req(length(values$processed_data) > 0)
    
    all_cells <- c()
    for(name in names(values$processed_data)) {
      dt <- values$processed_data[[name]]
      all_cells <- c(all_cells, names(dt)[-1])
    }
    
    updateSelectInput(session, "hmap_cells", choices = all_cells, selected = all_cells)
  })
  
  # Select all cells
  observeEvent(input$hmap_select_all, {
    all_cells <- c()
    for(name in names(values$processed_data)) {
      dt <- values$processed_data[[name]]
      all_cells <- c(all_cells, names(dt)[-1])
    }
    
    updateSelectInput(session, "hmap_cells", selected = all_cells)
  })
  
  # Clear cell selection
  observeEvent(input$hmap_clear_selection, {
    updateSelectInput(session, "hmap_cells", selected = character(0))
  })
  
  # ========== BATCH PROCESSING ==========
  
  observeEvent(input$run_batch, {
    req(input$batch_files)
    
    withProgress(message = "Running batch analysis...", {
      batch_results <- list()
      
      for(i in seq_along(input$batch_files$datapath)) {
        setProgress(value = i/length(input$batch_files$datapath),
                   message = paste("Processing", input$batch_files$name[i]))
        
        tryCatch({
          # Load file
          dt <- safe_read(input$batch_files$datapath[i])
          dt <- ensure_time_first(dt)
          dt <- coerce_numeric_dt(dt)
          
          # Apply current preprocessing settings if requested
          if(input$batch_use_current) {
            # Apply same preprocessing as individual analysis
            # (Implementation would mirror the preprocessing logic above)
          }
          
          # Calculate metrics
          cell_cols <- names(dt)[-1]
          file_metrics <- list()
          
          for(col in cell_cols) {
            metrics <- calculate_cell_metrics(dt[[col]], dt$Time, values$app_settings$default_baseline)
            metrics$File <- input$batch_files$name[i]
            metrics$Cell <- col
            file_metrics[[col]] <- metrics
          }
          
          batch_results[[input$batch_files$name[i]]] <- do.call(rbind, file_metrics)
          
        }, error = function(e) {
          batch_results[[input$batch_files$name[i]]] <- data.frame(
            Error = e$message,
            stringsAsFactors = FALSE
          )
        })
      }
      
      values$batch_results <- batch_results
      showNotification("Batch processing complete", type = "success")
      
      # Auto-export if requested
      if(input$batch_auto_export) {
        # Export logic here
      }
    })
  })
  
  # Batch progress
  output$batch_progress <- renderPrint({
    if(length(values$batch_results) == 0) {
      cat("No batch processing started\n")
    } else {
      cat("Processed files:", length(values$batch_results), "\n")
      
      successful <- sum(sapply(values$batch_results, function(x) !"Error" %in% names(x)))
      failed <- length(values$batch_results) - successful
      
      cat("Successful:", successful, "\n")
      cat("Failed:", failed, "\n")
    }
  })
  
  # Batch results table
  output$batch_results <- DT::renderDataTable({
    req(length(values$batch_results) > 0)
    
    # Combine all results
    all_results <- list()
    for(name in names(values$batch_results)) {
      if(!"Error" %in% names(values$batch_results[[name]])) {
        all_results[[name]] <- values$batch_results[[name]]
      }
    }
    
    if(length(all_results) > 0) {
      combined_results <- do.call(rbind, all_results)
      DT::datatable(combined_results, options = list(
        pageLength = 25,
        scrollX = TRUE
      ))
    } else {
      DT::datatable(data.frame(Message = "No successful results"))
    }
  })
  
  # ========== CORRELATION ANALYSIS ==========
  
  output$corr_matrix_plot <- renderPlot({
    req(length(values$processed_data) > 0)
    
    # Get first dataset for correlation
    dt <- values$processed_data[[1]]
    mat <- as.matrix(dt[, -1])
    
    # Calculate correlation matrix
    if(input$corr_method == "pearson") {
      cor_mat <- cor(mat, use = "complete.obs", method = "pearson")
    } else if(input$corr_method == "spearman") {
      cor_mat <- cor(mat, use = "complete.obs", method = "spearman")
    } else {
      cor_mat <- cor(mat, use = "complete.obs", method = "kendall")
    }
    
    # Apply threshold
    cor_mat[abs(cor_mat) < input$corr_threshold] <- 0
    
    # Clustering
    if(input$corr_cluster) {
      order <- hclust(as.dist(1 - abs(cor_mat)))$order
      cor_mat <- cor_mat[order, order]
    }
    
    # Plot
    corrplot(cor_mat, method = "color", type = "full",
            tl.cex = 0.7, tl.col = "black",
            col = colorRampPalette(c("blue", "white", "red"))(100))
  })
  
  # ========== CLASSIFICATION ==========
  
  observeEvent(input$run_classification, {
    req(!is.null(values$metrics_data))
    
    withProgress(message = "Running classification...", {
      
      if(input$class_method == "responder") {
        # Classify based on response threshold
        values$metrics_data$Classification <- ifelse(
          values$metrics_data$Peak_dFF0 > input$class_resp_threshold &
          values$metrics_data$SNR > input$class_resp_snr,
          "Responder",
          "Non-responder"
        )
      } else if(input$class_method == "kmeans") {
        # K-means clustering
        features <- values$metrics_data[, c("Peak_dFF0", "Time_to_Peak", "AUC")]
        features <- scale(features)
        
        kmeans_result <- kmeans(features, centers = input$class_kmeans_k)
        values$metrics_data$Classification <- paste("Cluster", kmeans_result$cluster)
      }
      
      showNotification("Classification complete", type = "success")
    })
  })
  
  output$class_results_plot <- renderPlot({
    req(!is.null(values$metrics_data), "Classification" %in% names(values$metrics_data))
    
    ggplot(values$metrics_data, aes(x = Peak_dFF0, y = Time_to_Peak, color = Classification)) +
      geom_point(size = 3, alpha = 0.7) +
      theme_minimal() +
      labs(
        x = "Peak ΔF/F₀",
        y = "Time to Peak (s)",
        title = "Cell Classification Results"
      )
  })
  
  output$class_results_table <- DT::renderDataTable({
    req(!is.null(values$metrics_data), "Classification" %in% names(values$metrics_data))
    
    summary_table <- values$metrics_data %>%
      group_by(Classification) %>%
      summarise(
        n = n(),
        Peak_Mean = mean(Peak_dFF0, na.rm = TRUE),
        Peak_SD = sd(Peak_dFF0, na.rm = TRUE),
        TTP_Mean = mean(Time_to_Peak, na.rm = TRUE),
        TTP_SD = sd(Time_to_Peak, na.rm = TRUE),
        .groups = "drop"
      )
    
    DT::datatable(summary_table, options = list(pageLength = 10))
  })
  
  # ========== MEMORY MANAGEMENT ==========
  
  output$memory_usage <- renderPrint({
    # Get memory info
    mem_used <- pryr::mem_used()
    cat("Memory used:", format(mem_used, units = "MB"), "\n")
    cat("Objects:", length(names(values)), "\n")
    
    # Count data sizes robustly
    uploaded <- values$uploaded_data
    if (!is.list(uploaded) || length(uploaded) == 0) {
      n_files <- 0
      n_cells <- 0
    } else {
      n_files <- length(uploaded)
      n_cells <- sum(vapply(uploaded, function(x) {
        if (is.data.frame(x) || data.table::is.data.table(x)) {
          max(ncol(x) - 1, 0)
        } else if (is.list(x)) {
          # Some loaders store a list of data.frames per group
          sum(vapply(x, function(df) if (is.data.frame(df) || data.table::is.data.table(df)) max(ncol(df) - 1, 0) else 0, numeric(1)))
        } else {
          0
        }
      }, numeric(1)))
    }
    
    cat("Files loaded:", n_files, "\n")
    cat("Total cells:", n_cells, "\n")
  })
  
  observeEvent(input$clear_memory, {
    # Clear non-essential data
    values$metrics_data <- NULL
    gc()
    showNotification("Cache cleared", type = "info")
  })
  
  # ========== SETTINGS ==========
  
  observeEvent(input$save_settings, {
    # Save current settings
    values$app_settings <- list(
      max_cells = input$max_cells,
      chunk_size = input$chunk_size,
      use_parallel = input$use_parallel,
      n_cores = input$n_cores,
      theme = input$theme,
      show_tooltips = input$show_tooltips,
      auto_save = input$auto_save,
      auto_save_interval = input$auto_save_interval,
      default_baseline = input$default_baseline,
      default_norm_method = input$default_norm_method,
      auto_detect_peaks = input$auto_detect_peaks,
      auto_classify_cells = input$auto_classify_cells,
      default_export_format = input$default_export_format,
      plot_dpi = input$plot_dpi
    )
    
    showNotification("Settings saved", type = "success")
  })
  
  observeEvent(input$reset_settings, {
    # Reset to defaults
    updateNumericInput(session, "max_cells", value = 1000)
    updateNumericInput(session, "chunk_size", value = 100)
    updateCheckboxInput(session, "use_parallel", value = FALSE)
    updateNumericInput(session, "n_cores", value = 2)
    updateSelectInput(session, "theme", selected = "default")
    updateCheckboxInput(session, "show_tooltips", value = TRUE)
    updateCheckboxInput(session, "auto_save", value = FALSE)
    updateNumericInput(session, "auto_save_interval", value = 10)
    updateNumericInput(session, "default_baseline", value = 20)
    updateSelectInput(session, "default_norm_method", selected = "mean")
    updateCheckboxInput(session, "auto_detect_peaks", value = TRUE)
    updateCheckboxInput(session, "auto_classify_cells", value = FALSE)
    updateSelectInput(session, "default_export_format", selected = "csv")
    updateNumericInput(session, "plot_dpi", value = 300)
    
    showNotification("Settings reset to defaults", type = "info")
  })
  
  # ========== DOWNLOAD HANDLERS ==========
  
  # Group comparison downloads
  output$gc_dl_metrics_csv <- downloadHandler(
    filename = function() paste0("gc_metrics_", Sys.Date(), ".csv"),
    content = function(file) {
      metrics_data <- gc_metrics_reactive()
      write.csv(metrics_data, file, row.names = FALSE)
    }
  )
  
  output$gc_dl_summary_csv <- downloadHandler(
    filename = function() paste0("gc_summary_", Sys.Date(), ".csv"),
    content = function(file) {
      metrics_data <- gc_metrics_reactive()
      summary_data <- metrics_data %>%
        group_by(Group) %>%
        summarise(across(where(is.numeric), list(
          mean = ~mean(.x, na.rm = TRUE),
          sd = ~sd(.x, na.rm = TRUE),
          sem = ~sd(.x, na.rm = TRUE)/sqrt(n())
        )), .groups = "drop")
      write.csv(summary_data, file, row.names = FALSE)
    }
  )
  
  output$gc_dl_all_excel <- downloadHandler(
    filename = function() paste0("gc_all_data_", Sys.Date(), ".xlsx"),
    content = function(file) {
      metrics_data <- gc_metrics_reactive()
      
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Metrics")
      openxlsx::writeData(wb, "Metrics", metrics_data)
      
      # Add summary sheet
      summary_data <- metrics_data %>%
        group_by(Group) %>%
        summarise(across(where(is.numeric), list(
          mean = ~mean(.x, na.rm = TRUE),
          sd = ~sd(.x, na.rm = TRUE)
        )), .groups = "drop")
      
      openxlsx::addWorksheet(wb, "Summary")
      openxlsx::writeData(wb, "Summary", summary_data)
      
      openxlsx::saveWorkbook(wb, file)
    }
  )
  
  # Individual analysis downloads
  output$dl_processed_wide <- downloadHandler(
    filename = function() paste0("processed_", input$pp_dl_group, "_", Sys.Date(), ".csv"),
    content = function(file) {
      req(input$pp_dl_group %in% names(values$processed_data))
      write.csv(values$processed_data[[input$pp_dl_group]], file, row.names = FALSE)
    }
  )
  
  output$dl_timecourse_plot_local <- downloadHandler(
    filename = function() paste0("timecourse_", Sys.Date(), ".", tolower(input$tc_dl_fmt)),
    content = function(file) {
      p <- output$timecourse_plot()
      ggsave(file, plot = p, width = input$tc_dl_w, height = input$tc_dl_h, 
             dpi = input$tc_dl_dpi, device = tolower(input$tc_dl_fmt))
    }
  )
  
  output$dl_heatmap <- downloadHandler(
    filename = function() paste0("heatmap_", Sys.Date(), ".png"),
    content = function(file) {
      p <- output$heatmap_plot()
      ggsave(file, plot = p, width = 12, height = 8, dpi = 300)
    }
  )
  
  output$save_session <- downloadHandler(
    filename = function() paste0("calcium_session_", Sys.Date(), ".rds"),
    content = function(file) {
      session_data <- list(
        uploaded_data = values$uploaded_data,
        processed_data = values$processed_data,
        metrics_data = values$metrics_data,
        gc_groups = values$gc_groups,
        settings = values$app_settings
      )
      saveRDS(session_data, file)
    }
  )
  
  # Batch download
  output$batch_download_all <- downloadHandler(
    filename = function() paste0("batch_results_", Sys.Date(), ".xlsx"),
    content = function(file) {
      req(length(values$batch_results) > 0)
      
      wb <- openxlsx::createWorkbook()
      
      for(name in names(values$batch_results)) {
        if(!"Error" %in% names(values$batch_results[[name]])) {
          sheet_name <- substr(gsub("[^[:alnum:]]", "_", name), 1, 31)
          openxlsx::addWorksheet(wb, sheet_name)
          openxlsx::writeData(wb, sheet_name, values$batch_results[[name]])
        }
      }
      
      openxlsx::saveWorkbook(wb, file)
    }
  )
  
  # Classification download
  output$class_download <- downloadHandler(
    filename = function() paste0("classification_", Sys.Date(), ".csv"),
    content = function(file) {
      req(!is.null(values$metrics_data), "Classification" %in% names(values$metrics_data))
      write.csv(values$metrics_data, file, row.names = FALSE)
    }
  )
  
  # ========== HELP MODALS ==========
  
  observeEvent(input$show_help, {
    showModal(modalDialog(
      title = "Help - Calcium Imaging Analysis App",
      size = "l",
      tags$h4("Quick Start Guide"),
      tags$ol(
        tags$li("Choose between Group Comparison or Individual Analysis"),
        tags$li("Upload your data files (CSV, Excel, or TXT)"),
        tags$li("Apply preprocessing if needed (ΔF/F₀ calculation, smoothing, etc.)"),
        tags$li("Explore visualizations and calculate metrics"),
        tags$li("Export results in various formats")
      ),
      tags$hr(),
      tags$h4("Data Format"),
      tags$p("Files should have Time in the first column and cell signals in subsequent columns."),
      tags$h4("Support"),
      tags$p("For detailed documentation, visit the Help tab."),
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$show_quickstart, {
    showModal(modalDialog(
      title = "Quick Start",
      size = "m",
      tags$h4("Try Demo Data"),
      tags$p("Click 'Load Demo Data' in either Group Comparison or Individual Analysis to explore features."),
      tags$h4("Your Data"),
      tags$p("Ensure your data has Time in column 1 and cell signals in other columns."),
      tags$h4("Processing Pipeline"),
      tags$ol(
        tags$li("Load Data"),
        tags$li("Preprocess (if needed)"),
        tags$li("Calculate Metrics"),
        tags$li("Visualize Results"),
        tags$li("Export Findings")
      ),
      footer = modalButton("Got it!")
    ))
  })
}

# Run the application
# shinyApp(ui = ui, server = server)