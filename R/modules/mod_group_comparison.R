# Group Comparison Module for Calcium Imaging Analysis App

# Module UI
mod_group_comparison_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # This is embedded in the main app tabs
  )
}

# Module Server
mod_group_comparison_server <- function(id, parent_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values for group comparison
    gcv <- reactiveValues(
      files = NULL,
      groups = NULL,
      dts = list(),
      long = NULL,
      summary = NULL,
      metrics = NULL,
      colors = NULL,
      group_labels = list()
    )
    
    # Helper function for unique labels
    make_unique_label <- function(label, existing) {
      if (!(label %in% existing)) return(label)
      base <- label
      i <- 2
      repeat {
        cand <- paste0(base, "_", i)
        if (!(cand %in% existing)) return(cand)
        i <- i + 1
        if (i > 9999) return(paste0(base, "_", as.integer(Sys.time())))
      }
    }
    
    # Convert data to long format
    to_long <- function(dt, group_label) {
      tv <- dt$Time
      long_list <- list()
      
      for (j in 2:ncol(dt)) {
        col_name <- names(dt)[j]
        cell_id <- paste0(group_label, "_", col_name)
        
        long_list[[j-1]] <- data.frame(
          Group = group_label,
          Cell_ID = cell_id,
          Time = tv,
          dFF0 = dt[[j]],
          stringsAsFactors = FALSE
        )
      }
      
      dplyr::bind_rows(long_list)
    }
    
    # Calculate comprehensive metrics
    calculate_group_metrics <- function(dt, group_label, baseline_frames = 20) {
      tv <- dt$Time
      metrics_list <- list()
      
      for (j in 2:ncol(dt)) {
        signal <- dt[[j]]
        
        # Skip if all NA
        if (all(is.na(signal))) next
        
        # Calculate baseline
        baseline_idx <- min(baseline_frames, floor(length(signal) * 0.1))
        baseline <- mean(signal[1:baseline_idx], na.rm = TRUE)
        baseline_sd <- sd(signal[1:baseline_idx], na.rm = TRUE)
        
        # Find peak
        peak_idx <- which.max(signal)
        peak_value <- signal[peak_idx]
        time_to_peak <- tv[peak_idx]
        
        # Response amplitude
        response_amp <- peak_value - baseline
        
        # SNR
        snr <- if (baseline_sd > 0) response_amp / baseline_sd else NA
        
        # Calculate rise times
        rise_times <- calculate_rise_times_detailed(tv, signal, baseline, peak_idx)
        
        # FWHM
        fwhm_result <- calculate_fwhm_hwhm(tv, signal, baseline)
        
        # AUC
        above_baseline <- pmax(signal - baseline, 0)
        valid_idx <- !is.na(above_baseline)
        if (sum(valid_idx) > 1) {
          x <- tv[valid_idx]
          y <- above_baseline[valid_idx]
          auc <- sum(diff(x) * (y[-1] + y[-length(y)]) / 2)
        } else {
          auc <- NA
        }
        
        # Ca entry rate
        if (length(signal) > 5) {
          smoothed <- stats::filter(signal, rep(1/5, 5), sides = 2)
          rates <- diff(smoothed) / diff(tv)
          ca_entry_rate <- max(rates, na.rm = TRUE)
        } else {
          ca_entry_rate <- NA
        }
        
        # Compile metrics
        metrics_list[[j-1]] <- data.frame(
          Group = group_label,
          Cell_ID = paste0(group_label, "_Cell", j-1),
          Peak_dFF0 = peak_value,
          Time_to_Peak = time_to_peak,
          Time_to_25_Peak = rise_times$time_to_25,
          Time_to_50_Peak = rise_times$time_to_50,
          Time_to_75_Peak = rise_times$time_to_75,
          Rise_Time = rise_times$rise_10_90,
          Half_Width = fwhm_result$hwhm,
          Calcium_Entry_Rate = ca_entry_rate,
          AUC = auc,
          Response_Amplitude = response_amp,
          Baseline_SD = baseline_sd,
          SNR = snr,
          stringsAsFactors = FALSE
        )
      }
      
      dplyr::bind_rows(metrics_list)
    }
    
    # Helper for rise times
    calculate_rise_times_detailed <- function(t, x, baseline, peak_idx) {
      peak_val <- x[peak_idx]
      response_amp <- peak_val - baseline
      
      if (response_amp <= 0 || peak_idx < 2) {
        return(list(
          rise_10_90 = NA_real_,
          time_to_25 = NA_real_,
          time_to_50 = NA_real_,
          time_to_75 = NA_real_
        ))
      }
      
      # Calculate thresholds
      thresh_10 <- baseline + 0.1 * response_amp
      thresh_25 <- baseline + 0.25 * response_amp
      thresh_50 <- baseline + 0.5 * response_amp
      thresh_75 <- baseline + 0.75 * response_amp
      thresh_90 <- baseline + 0.9 * response_amp
      
      # Find crossing times
      find_crossing <- function(threshold) {
        for (i in 1:(peak_idx-1)) {
          if (!is.na(x[i]) && !is.na(x[i+1])) {
            if (x[i] < threshold && x[i+1] >= threshold) {
              # Linear interpolation
              frac <- (threshold - x[i]) / (x[i+1] - x[i])
              return(t[i] + frac * (t[i+1] - t[i]))
            }
          }
        }
        return(NA_real_)
      }
      
      t_10 <- find_crossing(thresh_10)
      t_25 <- find_crossing(thresh_25)
      t_50 <- find_crossing(thresh_50)
      t_75 <- find_crossing(thresh_75)
      t_90 <- find_crossing(thresh_90)
      
      # Calculate metrics
      rise_10_90 <- if (!is.na(t_10) && !is.na(t_90)) t_90 - t_10 else NA_real_
      
      # Time from start
      t0 <- t[1]
      time_to_25 <- if (!is.na(t_25)) t_25 - t0 else NA_real_
      time_to_50 <- if (!is.na(t_50)) t_50 - t0 else NA_real_
      time_to_75 <- if (!is.na(t_75)) t_75 - t0 else NA_real_
      
      list(
        rise_10_90 = rise_10_90,
        time_to_25 = time_to_25,
        time_to_50 = time_to_50,
        time_to_75 = time_to_75
      )
    }
    
    # Process uploaded files
    process_group_files <- function(files, group_label, baseline_frames = 20) {
      withProgress(message = paste("Processing", group_label), value = 0, {
        
        dts <- list()
        
        for (i in seq_len(nrow(files))) {
          incProgress(1/nrow(files), detail = basename(files$name[i]))
          
          # Read file
          dt <- safe_read(files$datapath[i])
          
          # Ensure proper format
          dt <- ensure_time_first(dt)
          dt <- coerce_numeric_dt(dt)
          
          # Store
          dts[[i]] <- dt
        }
        
        # Combine if multiple files
        if (length(dts) > 1) {
          # Merge by time
          combined <- dts[[1]]
          for (i in 2:length(dts)) {
            combined <- merge(combined, dts[[i]], by = "Time", all = TRUE, suffixes = c("", paste0("_", i)))
          }
          return(combined)
        } else {
          return(dts[[1]])
        }
      })
    }
    
    # Statistical analysis functions
    perform_statistical_test <- function(metrics, metric_name, test_type = "anova") {
      
      # Prepare data
      test_data <- metrics %>%
        select(Group, Cell_ID, all_of(metric_name)) %>%
        rename(value = !!metric_name) %>%
        filter(!is.na(value))
      
      groups <- unique(test_data$Group)
      n_groups <- length(groups)
      
      if (n_groups < 2) {
        return(list(
          test = test_type,
          p_value = NA,
          statistic = NA,
          message = "Need at least 2 groups for comparison"
        ))
      }
      
      # Perform test based on type
      if (test_type == "anova" && n_groups >= 2) {
        # One-way ANOVA
        fit <- aov(value ~ Group, data = test_data)
        test_result <- summary(fit)
        
        result <- list(
          test = "One-way ANOVA",
          p_value = test_result[[1]][["Pr(>F)"]][1],
          statistic = test_result[[1]][["F value"]][1],
          df = test_result[[1]][["Df"]],
          post_hoc = NULL
        )
        
        # Post-hoc if significant
        if (result$p_value < 0.05 && n_groups > 2) {
          post_hoc <- TukeyHSD(fit)
          result$post_hoc <- post_hoc$Group
        }
        
      } else if (test_type == "kruskal" && n_groups >= 2) {
        # Kruskal-Wallis test
        test_result <- kruskal.test(value ~ Group, data = test_data)
        
        result <- list(
          test = "Kruskal-Wallis",
          p_value = test_result$p.value,
          statistic = test_result$statistic,
          df = test_result$parameter,
          post_hoc = NULL
        )
        
        # Post-hoc if significant (Dunn test)
        if (result$p_value < 0.05 && n_groups > 2) {
          # Would use dunn.test package here
          result$post_hoc <- "Dunn test would be performed here"
        }
        
      } else if (test_type == "wilcox" && n_groups == 2) {
        # Wilcoxon test
        group1_data <- test_data %>% filter(Group == groups[1]) %>% pull(value)
        group2_data <- test_data %>% filter(Group == groups[2]) %>% pull(value)
        
        test_result <- wilcox.test(group1_data, group2_data)
        
        result <- list(
          test = "Wilcoxon rank-sum",
          p_value = test_result$p.value,
          statistic = test_result$statistic
        )
        
      } else if (test_type == "ttest" && n_groups == 2) {
        # t-test
        group1_data <- test_data %>% filter(Group == groups[1]) %>% pull(value)
        group2_data <- test_data %>% filter(Group == groups[2]) %>% pull(value)
        
        test_result <- t.test(group1_data, group2_data)
        
        result <- list(
          test = "Two-sample t-test",
          p_value = test_result$p.value,
          statistic = test_result$statistic,
          df = test_result$parameter
        )
        
      } else {
        result <- list(
          test = test_type,
          p_value = NA,
          statistic = NA,
          message = "Invalid test for number of groups"
        )
      }
      
      result
    }
    
    # Calculate group summary statistics
    calculate_group_summary <- function(metrics) {
      
      metric_cols <- c("Peak_dFF0", "Time_to_Peak", "Rise_Time", "AUC", 
                      "Half_Width", "SNR", "Response_Amplitude", "Calcium_Entry_Rate")
      
      summary_list <- list()
      
      for (metric in metric_cols) {
        if (metric %in% names(metrics)) {
          summary_list[[metric]] <- metrics %>%
            group_by(Group) %>%
            summarise(
              Mean = mean(.data[[metric]], na.rm = TRUE),
              SD = sd(.data[[metric]], na.rm = TRUE),
              SEM = SD / sqrt(n()),
              Median = median(.data[[metric]], na.rm = TRUE),
              Min = min(.data[[metric]], na.rm = TRUE),
              Max = max(.data[[metric]], na.rm = TRUE),
              N = sum(!is.na(.data[[metric]])),
              .groups = "drop"
            ) %>%
            mutate(Metric = metric)
        }
      }
      
      dplyr::bind_rows(summary_list)
    }
    
    # Return reactive values and functions
    return(list(
      data = reactive(gcv$dts),
      metrics = reactive(gcv$metrics),
      summary = reactive(gcv$summary),
      groups = reactive(gcv$groups),
      colors = reactive(gcv$colors),
      
      # Functions
      add_group = function(files, label) {
        dt <- process_group_files(files, label)
        
        # Make label unique
        label <- make_unique_label(label, names(gcv$dts))
        
        # Add to data
        gcv$dts[[label]] <- dt
        gcv$groups <- names(gcv$dts)
        
        # Update colors
        gcv$colors <- DEFAULT_COLORS[1:length(gcv$groups)]
        
        # Update long format
        gcv$long <- purrr::imap(gcv$dts, ~to_long(.x, .y)) %>% 
          dplyr::bind_rows()
        
        # Calculate summary
        gcv$summary <- gcv$long %>%
          group_by(Group, Time) %>%
          summarise(
            mean_dFF0 = mean(dFF0, na.rm = TRUE),
            sem_dFF0 = sd(dFF0, na.rm = TRUE) / sqrt(n()),
            sd_dFF0 = sd(dFF0, na.rm = TRUE),
            n_cells = n(),
            .groups = "drop"
          )
        
        # Calculate metrics
        metrics_list <- list()
        for (group in gcv$groups) {
          metrics_list[[group]] <- calculate_group_metrics(
            gcv$dts[[group]], 
            group
          )
        }
        gcv$metrics <- dplyr::bind_rows(metrics_list)
        
        return(TRUE)
      },
      
      clear_all = function() {
        gcv$dts <- list()
        gcv$groups <- NULL
        gcv$long <- NULL
        gcv$summary <- NULL
        gcv$metrics <- NULL
        gcv$colors <- NULL
      },
      
      remove_group = function(group_name) {
        if (group_name %in% names(gcv$dts)) {
          gcv$dts[[group_name]] <- NULL
          gcv$groups <- names(gcv$dts)
          
          # Recalculate everything
          if (length(gcv$dts) > 0) {
            gcv$colors <- DEFAULT_COLORS[1:length(gcv$groups)]
            gcv$long <- purrr::imap(gcv$dts, ~to_long(.x, .y)) %>% 
              dplyr::bind_rows()
            
            gcv$summary <- gcv$long %>%
              group_by(Group, Time) %>%
              summarise(
                mean_dFF0 = mean(dFF0, na.rm = TRUE),
                sem_dFF0 = sd(dFF0, na.rm = TRUE) / sqrt(n()),
                sd_dFF0 = sd(dFF0, na.rm = TRUE),
                n_cells = n(),
                .groups = "drop"
              )
            
            # Recalculate metrics
            metrics_list <- list()
            for (group in gcv$groups) {
              metrics_list[[group]] <- calculate_group_metrics(
                gcv$dts[[group]], 
                group
              )
            }
            gcv$metrics <- dplyr::bind_rows(metrics_list)
          } else {
            gcv$long <- NULL
            gcv$summary <- NULL
            gcv$metrics <- NULL
            gcv$colors <- NULL
          }
        }
      },
      
      rename_group = function(old_name, new_name) {
        if (old_name %in% names(gcv$dts) && old_name != new_name) {
          # Make new name unique
          new_name <- make_unique_label(new_name, setdiff(names(gcv$dts), old_name))
          
          # Rename in data
          names(gcv$dts)[names(gcv$dts) == old_name] <- new_name
          gcv$groups <- names(gcv$dts)
          
          # Update long format and metrics
          gcv$long$Group[gcv$long$Group == old_name] <- new_name
          gcv$metrics$Group[gcv$metrics$Group == old_name] <- new_name
          gcv$summary$Group[gcv$summary$Group == old_name] <- new_name
        }
      },
      
      perform_test = perform_statistical_test,
      get_summary = calculate_group_summary
    ))
  })
}