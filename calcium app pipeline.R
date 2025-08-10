#!/usr/bin/env Rscript

# Universal Calcium Imaging Analysis App — refined (v2.3)
# - Metrics (Individual): cell bars labeled by cell #; inset shown on the LEFT when sorted
# - Metrics (Individual): removed global/pairwise statistical tests UI + table
# - Time Course (Individual): compact controls; advanced options collapsed
# - Time Course: legend off by default; line color override; intuitive transparency
# - Pre-processing: average metrics table (Mean, SEM, n) next to controls
# - Metric Guide: Added a dedicated, interactive guide for the dF/F0 calculation itself.
# - Metric Guide: Fixed division-by-zero error when viewing already normalized data.

options(repos = c(CRAN = "https://cran.rstudio.com/"))
options(shiny.maxRequestSize = 200 * 1024^2)

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyjs)
  library(shinyWidgets)
  library(shinycssloaders)
  library(DT)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(data.table)
  library(readxl)
  library(purrr)
  library(cowplot)
  library(corrplot)
  library(rlang)
  library(RColorBrewer)
  library(scales)
  library(colourpicker)
  library(patchwork)
  library(latex2exp)
  library(zoo)
  library(shinyvalidate)
  library(plotly)
})

# ---------- Helpers ----------
safe_read <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) as.data.table(readxl::read_excel(path, .name_repair = "minimal"))
  else data.table::fread(path)
}

ensure_time_first <- function(dt, time_col = NULL) {
  if (!is.null(time_col) && time_col %in% names(dt)) {
    data.table::setcolorder(dt, c(time_col, setdiff(names(dt), time_col)))
  }
  data.table::setnames(dt, 1, "Time")
  dt
}

coerce_numeric_dt <- function(dt) {
  suppressWarnings({ dt[[1]] <- as.numeric(dt[[1]]) })
  keep <- c(TRUE, vapply(dt[, -1], function(col) !is.list(col), logical(1)))
  dt <- dt[, ..keep]
  for (j in seq(2, ncol(dt))) suppressWarnings({ dt[[j]] <- as.numeric(dt[[j]]) })
  dt
}

# Demo data (optional)
simulate_group <- function(label = "GroupA", n_cells = 60, T = 180, sr = 5, peak_t = 50, amp = 0.6, noise = 0.08) {
  time <- seq(0, T, by = 1/sr)
  base <- exp(-((time - peak_t)^2)/(2*(peak_t/6)^2)); base <- base / max(base)
  M <- sapply(seq_len(n_cells), function(i) {
    a <- rlnorm(1, log(amp), 0.3); b <- base * a
    if (runif(1) < 0.15) b <- b * 0.3
    b + stats::filter(rnorm(length(time), 0, noise), rep(1/3, 3), sides = 2)
  })
  dt <- data.table::as.data.table(cbind(Time = time, M))
  data.table::setnames(dt, c("Time", paste0("Cell", seq_len(ncol(dt) - 1))))
  list(label = label, dt = dt)
}

# Correct FWHM/HWHM at 50% around the main peak (with linear interpolation)
fwhm_hwhm <- function(t, x, baseline = NULL) {
  valid <- is.finite(t) & is.finite(x)
  t <- t[valid]; x <- x[valid]; n <- length(x)
  if (n < 4) return(c(FWHM = NA_real_, HWHM = NA_real_))
  
  if (is.null(baseline)) {
    b_len <- min(20, n)
    baseline <- mean(x[1:b_len], na.rm = TRUE)
  }
  
  pk <- which.max(x)
  thr <- baseline + 0.5 * (x[pk] - baseline)  # 50% of (peak - baseline)
  above <- x >= thr
  if (!isTRUE(above[pk])) return(c(FWHM = NA_real_, HWHM = NA_real_))
  
  # Left boundary (prefer true crossing; else truncate to start if already above at t[1])
  tL <- NA_real_
  if (pk > 1) {
    left_idx <- which(x[1:(pk-1)] < thr & x[2:pk] >= thr)
    if (length(left_idx)) {
      i <- max(left_idx)
      denomL <- x[i+1] - x[i]
      tL <- if (is.finite(denomL) && denomL != 0) t[i] + (thr - x[i]) * (t[i+1] - t[i]) / denomL else t[i]
    } else if (above[1]) {
      tL <- t[1]
    }
  } else {
    tL <- t[1]
  }
  
  # Right boundary (prefer true crossing; else truncate to end if still above at t[n])
  tR <- NA_real_
  if (pk < n) {
    right_idx <- which(x[pk:(n-1)] >= thr & x[(pk+1):n] < thr)
    if (length(right_idx)) {
      j <- (pk - 1) + min(right_idx)
      denomR <- x[j+1] - x[j]
      tR <- if (is.finite(denomR) && denomR != 0) t[j] + (thr - x[j]) * (t[j+1] - t[j]) / denomR else t[j]
    } else if (above[n]) {
      tR <- t[n]
    }
  } else {
    tR <- t[n]
  }
  
  if (!is.finite(tL) || !is.finite(tR)) return(c(FWHM = NA_real_, HWHM = NA_real_))
  
  FWHM <- as.numeric(tR - tL)
  c(FWHM = FWHM, HWHM = FWHM/2)
}

calculate_cell_metrics <- function(cell_data, time_vec, baseline_frames = 20) {
  # Validate input
  valid <- is.finite(cell_data) & is.finite(time_vec)
  x <- cell_data[valid]
  t <- time_vec[valid]
  
  # Need minimum data points
  if (length(x) < 10) {
    return(data.frame(
      Peak_dFF0 = NA_real_, 
      Time_to_Peak = NA_real_, 
      Time_to_25_Peak = NA_real_,
      Time_to_50_Peak = NA_real_, 
      Time_to_75_Peak = NA_real_, 
      Rise_Time = NA_real_,
      Calcium_Entry_Rate = NA_real_, 
      AUC = NA_real_, 
      Response_Amplitude = NA_real_,
      Half_Width = NA_real_, 
      Baseline_SD = NA_real_, 
      SNR = NA_real_
    ))
  }
  
  # Calculate baseline (first 20 frames)
  baseline_len <- min(baseline_frames, length(x))
  baseline_vals <- x[1:baseline_len]
  baseline_raw <- mean(baseline_vals, na.rm = TRUE)
  baseline_sd_raw <- stats::sd(baseline_vals, na.rm = TRUE)
  
  # Determine if data is already normalized (ΔF/F₀) or raw fluorescence
  # If baseline is very close to 0, assume already normalized
  if (abs(baseline_raw) < 0.1) {
    # Already normalized as ΔF/F₀
    working_signal <- x
    baseline <- 0
    baseline_sd <- baseline_sd_raw
  } else {
    # Need to calculate ΔF/F₀
    if (baseline_raw != 0) {
      working_signal <- (x - baseline_raw) / baseline_raw
      baseline <- 0
      # Recalculate SD on normalized baseline
      baseline_sd <- stats::sd(working_signal[1:baseline_len], na.rm = TRUE)
    } else {
      # Avoid division by zero
      working_signal <- x
      baseline <- baseline_raw
      baseline_sd <- baseline_sd_raw
    }
  }
  
  # Find peak
  peak_value <- max(working_signal, na.rm = TRUE)
  peak_idx <- which.max(working_signal)
  time_to_peak <- t[peak_idx]
  
  # Response amplitude (peak - baseline)
  response_amplitude <- peak_value - baseline
  
  # Helper function to find first index where signal crosses threshold
  # Starting after baseline period (fixed 20 frames)
  find_threshold_crossing <- function(signal, threshold, start_after = baseline_frames) {
    for (i in (start_after + 1):length(signal)) {
      if (!is.na(signal[i]) && signal[i] >= threshold) {
        return(i)
      }
    }
    return(NA_integer_)
  }
  
  # Initialize time metrics
  tt25 <- tt50 <- tt75 <- rise_time <- ca_entry <- NA_real_
  
  # Only calculate time metrics if we have a positive response
  if (response_amplitude > 0.001) {  # Small threshold to avoid numerical issues
    
    # Time to X% of peak calculations
    p25 <- baseline + 0.25 * response_amplitude
    p50 <- baseline + 0.50 * response_amplitude
    p75 <- baseline + 0.75 * response_amplitude
    
    i25 <- find_threshold_crossing(working_signal, p25)
    i50 <- find_threshold_crossing(working_signal, p50)
    i75 <- find_threshold_crossing(working_signal, p75)
    
    tt25 <- if (!is.na(i25)) t[i25] else NA_real_
    tt50 <- if (!is.na(i50)) t[i50] else NA_real_
    tt75 <- if (!is.na(i75)) t[i75] else NA_real_
    
    # Rise time (10% to 90%)
    r10 <- baseline + 0.1 * response_amplitude
    r90 <- baseline + 0.9 * response_amplitude
    
    i10 <- find_threshold_crossing(working_signal, r10)
    i90 <- find_threshold_crossing(working_signal, r90)
    
    if (!is.na(i10) && !is.na(i90) && i90 > i10) {
      rise_time <- t[i90] - t[i10]
      # Calcium entry rate
      ca_entry <- if (rise_time > 0) {
        (0.8 * response_amplitude) / rise_time  # 80% of amplitude over rise time
      } else NA_real_
    }
  }
  
  # AUC calculation (using normalized signal)
  auc <- if (length(t) > 1) {
    # Trapezoidal integration
    dt_vals <- diff(t)
    heights <- (working_signal[-1] + working_signal[-length(working_signal)]) / 2
    sum(dt_vals * heights, na.rm = TRUE)
  } else NA_real_
  
  # SNR calculation
  snr <- if (!is.na(baseline_sd) && baseline_sd > 0) {
    response_amplitude / baseline_sd
  } else NA_real_
  
  # Half-width (HWHM) calculation
  half_width <- NA_real_
  if (response_amplitude > 0.001) {
    threshold_half <- baseline + 0.5 * response_amplitude
    
    # Find left crossing (after baseline)
    idx_left <- find_threshold_crossing(working_signal, threshold_half)
    
    # Find right crossing (search from peak forward for where it drops below)
    idx_right <- NA_integer_
    if (!is.na(idx_left) && peak_idx < length(working_signal)) {
      # Look for where signal drops back below threshold
      for (i in peak_idx:length(working_signal)) {
        if (!is.na(working_signal[i]) && i < length(working_signal)) {
          if (working_signal[i] >= threshold_half && 
              !is.na(working_signal[i+1]) && 
              working_signal[i+1] < threshold_half) {
            idx_right <- i
            break
          }
        }
      }
      
      # If no drop found, check if signal stays above threshold
      if (is.na(idx_right) && !is.na(working_signal[length(working_signal)])) {
        if (working_signal[length(working_signal)] >= threshold_half) {
          # Signal doesn't return to 50%, use end point
          idx_right <- length(working_signal)
        }
      }
    }
    
    # Calculate FWHM and HWHM
    if (!is.na(idx_left) && !is.na(idx_right) && idx_right > idx_left) {
      fwhm <- t[idx_right] - t[idx_left]
      half_width <- fwhm / 2
    }
  }
  
  # Return all metrics
  data.frame(
    Peak_dFF0 = peak_value,
    Time_to_Peak = time_to_peak,
    Time_to_25_Peak = tt25,
    Time_to_50_Peak = tt50,
    Time_to_75_Peak = tt75,
    Rise_Time = rise_time,
    Calcium_Entry_Rate = ca_entry,
    AUC = auc,
    Response_Amplitude = response_amplitude,
    Half_Width = half_width,
    Baseline_SD = baseline_sd,
    SNR = snr
  )
}

compute_metrics_for_dt <- function(dt, group_label, baseline_frames = 20) {
  tv <- dt$Time
  
  # Get all cell columns (exclude Time and any non-numeric columns)
  cell_cols <- setdiff(names(dt), c("Time", "Label", "label"))
  
  # Filter to only numeric columns that contain actual data
  valid_cols <- character()
  for (col in cell_cols) {
    if (is.numeric(dt[[col]]) && any(is.finite(dt[[col]]))) {
      valid_cols <- c(valid_cols, col)
    }
  }
  
  if (length(valid_cols) == 0) {
    return(data.frame())
  }
  
  # Calculate metrics for each valid cell
  out <- lapply(valid_cols, function(col_name) {
    m <- calculate_cell_metrics(dt[[col_name]], tv, baseline_frames)
    
    # Create a proper cell ID
    # Extract cell number if column name has a number
    cell_num <- gsub("[^0-9]", "", col_name)
    if (nzchar(cell_num)) {
      m$Cell_ID <- paste0(group_label, "_Cell", cell_num)
    } else {
      # Use column name if no number found
      m$Cell_ID <- paste0(group_label, "_", col_name)
    }
    
    m$Group <- group_label
    m$Original_Column <- col_name  # Keep track of original column name
    m
  })
  
  # Combine results
  result <- dplyr::bind_rows(out)
  
  # Remove rows where all metrics are NA (except Cell_ID and Group)
  metric_cols <- c("Peak_dFF0", "Time_to_Peak", "Time_to_25_Peak", 
                   "Time_to_50_Peak", "Time_to_75_Peak", "Rise_Time",
                   "Calcium_Entry_Rate", "AUC", "Response_Amplitude", 
                   "Half_Width", "Baseline_SD", "SNR")
  
  # Keep rows that have at least one non-NA metric
  has_data <- apply(result[metric_cols], 1, function(row) any(!is.na(row)))
  result[has_data, ]
}

to_long <- function(dt, group_label) {
  time_vec <- dt$Time
  mat <- as.matrix(dt[, -1])
  as.data.frame(mat) |>
    dplyr::mutate(Time = time_vec) |>
    tidyr::pivot_longer(cols = -Time, names_to = "Cell", values_to = "dFF0") |>
    dplyr::mutate(dFF0 = suppressWarnings(as.numeric(dFF0))) |>
    dplyr::mutate(Group = group_label)
}

metric_label <- function(metric) {
  if (metric == "Peak_dFF0") return(latex2exp::TeX("$\\Delta F/F_0$"))
  if (metric == "Response_Amplitude") return(latex2exp::TeX("Response Amplitude ($\\Delta F/F_0$)"))
  if (metric == "Calcium_Entry_Rate") return("Ca²⁺ Entry Rate")
  if (metric %in% c("Time_to_Peak","Time_to_25_Peak","Time_to_50_Peak","Time_to_75_Peak","Rise_Time","Half_Width")) return("Time (s)")
  if (metric == "AUC") return("AUC")
  if (metric == "SNR") return("SNR")
  metric
}

metric_title <- function(metric) {
  switch(metric,
         dFF0_calc = "ΔF/F₀ Calculation",
         Peak_dFF0 = "Peak ΔF/F₀",
         Response_Amplitude = "Response Amplitude (ΔF/F₀)",
         Calcium_Entry_Rate = "Ca²⁺ Entry Rate",
         Time_to_Peak = "Time to Peak (s)",
         Time_to_25_Peak = "Time to 25% Peak (s)",
         Time_to_50_Peak = "Time to 50% Peak (s)",
         Time_to_75_Peak = "Time to 75% Peak (s)",
         Rise_Time = "Rise Time (s)",
         Half_Width = "Half Width (HWHM, s)",
         
         AUC = "AUC",
         SNR = "SNR",
         metric)
}

default_group_colors <- function(groups) {
  n <- length(groups)
  cols <- if (n <= 8) RColorBrewer::brewer.pal(max(3, n), "Set2") else scales::hue_pal()(n)
  stats::setNames(cols[seq_len(n)], groups)
}

# ---------- UI ----------
header <- dashboardHeader(title = "Universal Calcium Imaging Analysis")
sidebar <- dashboardSidebar(
  sidebarMenu(id = "sidebar_tabs",
              menuItem("Individual Analysis", icon = icon("microscope"), startExpanded = TRUE,
                       menuSubItem("Load", tabName = "ind_data", icon = icon("database")),
                       menuSubItem("Data Processing", tabName = "preproc", icon = icon("sliders")),
                       menuSubItem("Time Course", tabName = "time", icon = icon("chart-line")),
                       menuSubItem("Metrics", tabName = "metrics", icon = icon("chart-bar")),
                       menuSubItem("Heatmap", tabName = "heatmap", icon = icon("th")),
                       menuSubItem("Metric Guide", tabName = "guide", icon = icon("circle-info")),
                       menuSubItem("Tables", tabName = "tables", icon = icon("table")),
                       menuSubItem("Export", tabName = "export", icon = icon("download"))
              ),
              menuItem("Group Comparison", icon = icon("layer-group"), startExpanded = TRUE,
                       menuSubItem("Load", tabName = "gc_data", icon = icon("database")),
                       menuSubItem("Time Course", tabName = "gc_time", icon = icon("chart-line")),
                       menuSubItem("Metrics", tabName = "gc_metrics", icon = icon("chart-bar")),
                       menuSubItem("Heatmap", tabName = "gc_heatmap", icon = icon("th")),
                       menuSubItem("Classification", tabName = "classify", icon = icon("braille")),
                       menuSubItem("Correlation", tabName = "corr", icon = icon("project-diagram")),
                       menuSubItem("Tables", tabName = "gc_tables", icon = icon("table")),
                       menuSubItem("Export", tabName = "gc_export", icon = icon("download"))
              ),
              menuItem("Help", tabName = "help", icon = icon("circle-question"))
  )
)

body <- dashboardBody(
  useShinyjs(),
  tags$head(tags$style(HTML("
    .small-help {color:#6c757d;font-size:12px;margin-top:4px}
    .box-title {font-weight:600}
    details > summary {cursor:pointer;font-weight:600;margin-top:8px}
  "))),
  tabItems(
    
    # ---- GC LOAD ----
    tabItem(tabName = "gc_data",
            fluidRow(
              box(title = "Load Processed Data (for Group Comparison)", status = "primary", solidHeader = TRUE, width = 5,
                  fileInput("gc_files","Upload processed CSV/Excel (first col = Time)", multiple = TRUE,
                            accept = c(".csv",".xlsx",".xls")),
                  actionButton("gc_load_btn","Load Files", class = "btn-primary", width = "100%"),
                  br(), actionButton("gc_demo_btn","Load Demo Groups", class = "btn-secondary", width = "100%"),
                  br(), div(class="small-help","Each file becomes one group. Rename and recolor below.")
              ),
              box(title = "Group Labels & Summary", status = "info", solidHeader = TRUE, width = 7,
                  uiOutput("gc_group_label_ui"),
                  div(style="margin-top:10px;", DTOutput("gc_data_summary_table"))
              )
            ),
            fluidRow(
              valueBoxOutput("vb_gc_groups", width = 4),
              valueBoxOutput("vb_gc_cells", width = 4),
              valueBoxOutput("vb_gc_timepoints", width = 4)
            ),
            fluidRow(
              box(title = "Quick Metrics Summary (Mean ± SEM, n per Group)", status = "info", solidHeader = TRUE, width = 12,
                  div(class = "small-help", "Computed from per-cell metrics; updates after you apply labels."),
                  DTOutput("gc_quick_metrics"))
            )
    ),
    
    # ---- IND LOAD ----
    tabItem(tabName = "ind_data",
            fluidRow(
              box(title = "Load Data (Individual Analysis)", status = "primary", solidHeader = TRUE, width = 8,
                  fileInput("data_files","Upload CSV or Excel (wide; first column = Time)", multiple = TRUE,
                            accept = c(".csv",".xlsx",".xls")),
                  actionButton("load_btn","Load Files", class = "btn-primary"),
                  actionButton("demo_btn","Load Demo Data", class = "btn-secondary"),
                  br(), div(class="small-help","Upload raw/pre-processed traces; compute ΔF/F₀ in Pre-processing if needed.")
              ),
              box(title = "At a glance", status = "info", solidHeader = TRUE, width = 4,
                  valueBoxOutput("vb_groups", width = 12),
                  valueBoxOutput("vb_cells", width = 12),
                  valueBoxOutput("vb_timepoints", width = 12)
              ),
              box(title = "Processing Status", status = "info", solidHeader = TRUE, width = 12,
                  div(id = "status_panel",
                      tags$div(class = "row",
                               tags$div(class = "col-sm-3", 
                                        icon("file-import", class = "fa-2x"),
                                        h4("Files Loaded"),
                                        textOutput("status_files_loaded")
                               ),
                               tags$div(class = "col-sm-3",
                                        icon("check-circle", class = "fa-2x"),
                                        h4("Processing"),
                                        textOutput("status_processing")
                               ),
                               tags$div(class = "col-sm-3",
                                        icon("calculator", class = "fa-2x"),
                                        h4("Metrics"),
                                        textOutput("status_metrics")
                               ),
                               tags$div(class = "col-sm-3",
                                        icon("chart-line", class = "fa-2x"),
                                        h4("Ready"),
                                        textOutput("status_ready")
                               )
                      )
                  ),
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;"
              )
            )
    ),
    
    # ---- GC TIME ----
    tabItem(tabName = "gc_time",
            fluidRow(
              box(title = "Controls", status = "primary", solidHeader = TRUE, width = 3,
                  selectInput("gc_time_plot_type", "Plot Type",
                              choices = c("Static (ggplot2)" = "static",
                                          "Interactive (plotly)" = "plotly"),
                              selected = "static"),
                  checkboxInput("gc_time_show_ribbon", "Show SEM ribbon", TRUE),
                  checkboxInput("gc_time_show_points", "Show mean points", TRUE),
                  checkboxInput("gc_time_show_individual", "Show individual traces", FALSE),
                  sliderInput("gc_time_alpha", "Individual trace transparency", 
                              min = 0.1, max = 1, value = 0.3, step = 0.1),
                  sliderInput("gc_time_font_size", "Font size", 
                              min = 8, max = 20, value = 12, step = 1),
                  div(class="small-help", "Interactive plot allows zooming, panning, and hover tooltips.")
              ),
              box(title = "Time Course Plot (Processed Data)", status = "primary", solidHeader = TRUE, width = 9,
                  conditionalPanel(
                    condition = "input.gc_time_plot_type == 'static'",
                    withSpinner(plotOutput("gc_timecourse_plot", height = "620px"), type = 4)
                  ),
                  conditionalPanel(
                    condition = "input.gc_time_plot_type == 'plotly'",
                    withSpinner(plotlyOutput("gc_timecourse_plotly", height = "620px"), type = 4)
                  )
              )
            )
    ),
    
    # ---- GC METRICS ----
    tabItem(tabName = "gc_metrics",
            fluidRow(
              box(title = "Controls", status = "success", solidHeader = TRUE, width = 4,
                  selectInput("gc_metric_name","Metric",
                              choices = c("Peak ΔF/F₀"="Peak_dFF0","Time to Peak (s)"="Time_to_Peak",
                                          "Time to 25% Peak (s)"="Time_to_25_Peak","Time to 50% Peak (s)"="Time_to_50_Peak",
                                          "Time to 75% Peak (s)"="Time_to_75_Peak","Rise Time (s)"="Rise_Time",
                                          "Half Width (HWHM)"="Half_Width",
                                          "Ca²⁺ Entry Rate"="Calcium_Entry_Rate","AUC"="AUC",
                                          "Response Amplitude"="Response_Amplitude","SNR"="SNR"),
                              selected="Peak_dFF0"),
                  selectInput("gc_metric_geom","Plot type",
                              choices = c("Bar"="bar","Box"="box","Violin"="violin","Dot"="dot"), selected="bar"),
                  checkboxInput("gc_metric_points","Show individual points", TRUE),
                  checkboxInput("gc_metric_stats","Show statistical tests", TRUE),
                  selectInput("gc_stat_test", "Test", 
                              choices = c("One-way ANOVA" = "anova",
                                          "Kruskal-Wallis" = "kruskal",
                                          "Wilcoxon (2 groups)" = "wilcox",
                                          "t-test (2 groups)" = "ttest"), 
                              selected = "anova"),
                  checkboxInput("gc_show_posthoc", "Show post-hoc comparisons (ANOVA)", TRUE),
                  radioButtons("gc_inset_pos", "Inset position", inline = TRUE,
                               choices = c("Above bars" = "above", "Below axis" = "below"),
                               selected = "above"),
                  checkboxInput("gc_show_brackets", "Show pairwise comparison brackets", TRUE),
                  checkboxInput("gc_show_insets", "Show mean ± SEM, n insets", TRUE),
                  sliderInput("gc_metric_size","Base font size", 8, 22, 14, 1),
                  div(class="small-help","ANOVA/Kruskal-Wallis for multiple groups; Wilcoxon/t-test for 2 groups only.")
              ),
              box(title = "Metrics Plot (Processed Data)", status = "success", solidHeader = TRUE, width = 8,
                  withSpinner(plotOutput("gc_metrics_plot", height = "640px"), type = 4))
            )
    ),
    
    # ---- GC HEATMAP ----
    tabItem(tabName = "gc_heatmap",
            fluidRow(
              box(title = "Heatmap (Processed Data)", status = "warning", solidHeader = TRUE, width = 12,
                  withSpinner(plotOutput("gc_heatmap_plot", height = "760px"), type = 4))
            )
    ),
    
    # ---- GC TABLES ----
    tabItem(tabName = "gc_tables",
            fluidRow(
              box(title = "Per-cell Metrics (Processed Data)", status = "primary", solidHeader = TRUE, width = 12,
                  DTOutput("gc_metrics_table"))
            )
    ),
    
    # ---- GC EXPORT ----
    tabItem(tabName = "gc_export",
            fluidRow(
              box(title = "Export (Processed Data)", status = "primary", solidHeader = TRUE, width = 12,
                  downloadButton("gc_dl_metrics_csv","Download Metrics CSV")
              )
            )
    ),
    
    # ---- PREPROC ----
    tabItem(tabName = "preproc",
            fluidRow(
              column(width = 5,
                     box(title = "Data Processing Controls", status = "warning", solidHeader = TRUE, width = 12,
                         switchInput("pp_enable","Enable pre-processing", onLabel="Yes", offLabel="No", value=TRUE),
                         checkboxInput("pp_compute_dff","Compute ΔF/F₀ per cell", TRUE),
                         selectInput("pp_baseline_method","Baseline (F₀) method",
                                     choices = c("First N frames"="first_n","Rolling minimum"="rolling_min","Percentile"="percentile"),
                                     selected="first_n"),
                         conditionalPanel("input.pp_baseline_method == 'first_n'",
                                          numericInput("pp_baseline_frames","N frames for baseline (F₀)", value=20, min=1, step=1)),
                         conditionalPanel("input.pp_baseline_method == 'rolling_min'",
                                          numericInput("pp_window_size","Rolling window (frames)", value=50, min=5, step=1)),
                         conditionalPanel("input.pp_baseline_method == 'percentile'",
                                          numericInput("pp_percentile","Baseline percentile", value=10, min=1, max=50, step=1)),
                         checkboxInput("pp_minmax_enable","Per-cell min-max normalization (0-1)", FALSE),
                         tags$hr(),
                         checkboxInput("pp_apply_bg","Background subtraction (single column)", FALSE),
                         textInput("pp_bg_col","Background column name (exact)", value=""),
                         tags$hr(),
                         numericInput("pp_sampling_rate","Sampling rate (Hz) if Time missing/invalid", value=1, min=0.0001, step=0.1),
                         div(class="small-help","ΔF/F₀ = (F - F₀)/F₀. Operations apply per uploaded file.")
                     )
              ),
              column(width = 7,
                     box(title = "Notes", status = "info", solidHeader = TRUE, width = 12,
                         tags$ul(
                           tags$li("Baseline uses per-cell F₀ (method above)."),
                           tags$li("Min-max normalization rescales each cell independently to [0,1]."),
                           tags$li("Turn pre-processing off to analyze raw uploaded values.")
                         )
                     ),
                     box(title = "Average Metrics (All Cells)", status = "info", solidHeader = TRUE, width = 12,
                         DTOutput("preproc_avg_metrics")),
                     box(title = "Download Processed Data", status = "primary", solidHeader = TRUE, width = 12,
                         tags$p("Download the processed data in the original wide format (first column = Time; subsequent columns = cells)."),
                         selectInput("pp_dl_group", "Select file", choices = NULL),
                         downloadButton("dl_processed_wide", "Download Processed File (CSV)")
                     )
              )
            )
    ),
    
    # ---- IND TIME (compact controls) ----
    tabItem(tabName = "time",
            fluidRow(
              box(title = "Controls", status = "primary", solidHeader = TRUE, width = 3,
                  textInput("tc_title","Title",""),
                  textInput("tc_subtitle","Subtitle","ΔF/F₀ over time"),
                  checkboxInput("tc_show_traces","Show individual cell traces", TRUE),
                  sliderInput("tc_trace_transparency","Traces transparency (%)", 0, 100, 65, 1),
                  checkboxInput("tc_show_ribbon","Show SEM ribbon", TRUE),
                  sliderInput("tc_line_width","Line width", 0.5, 4, 1.6, 0.1),
                  colourpicker::colourInput("tc_line_color","Line color override", value = "#1b9e77"),
                  sliderInput("tc_title_size","Title size", 10, 28, 18, 1),
                  sliderInput("tc_axis_size","Axis text size", 8, 28, 12, 1),
                  sliderInput("tc_axis_title_size","Axis title size", 8, 28, 14, 1),
                  selectInput("tc_legend_pos","Legend position", choices = c("none","bottom","right","top","left"), selected="none"),
                  tags$details(
                    tags$summary("Advanced formatting"),
                    textInput("tc_x","X label","Time (s)"),
                    textInput("tc_y","Y label","ΔF/F₀"),
                    selectInput("tc_theme","Theme", choices=c("classic","minimal","light","dark"), selected="classic"),
                    selectInput("tc_font","Font", choices=c("Arial","Helvetica","Times","Courier"), selected="Arial"),
                    checkboxInput("tc_grid_major","Show major gridlines", TRUE),
                    checkboxInput("tc_grid_minor","Show minor gridlines", FALSE),
                    checkboxInput("tc_log_y","Log10 Y", FALSE),
                    checkboxInput("tc_limits","Custom axis limits", FALSE),
                    conditionalPanel("input.tc_limits == true",
                                     numericInput("tc_xmin","X min", NA), numericInput("tc_xmax","X max", NA),
                                     numericInput("tc_ymin","Y min", NA), numericInput("tc_ymax","Y max", NA)
                    ),
                    tags$hr(),
                    h4("Axis ticks"),
                    textInput("tc_x_breaks","X breaks (comma)",""),
                    textInput("tc_y_breaks","Y breaks (comma)",""),
                    selectInput("tc_tick_format","Tick format", choices=c("number","scientific","percent"), selected="number"),
                    NULL
                  )
              ),
              box(title = "Time Course", status = "primary", solidHeader = TRUE, width = 5,
                  withSpinner(plotOutput("timecourse_plot", height = "620px"), type = 4),
                  tags$hr(),
                  fluidRow(
                    column(3, selectInput("tc_dl_fmt","Format", choices = c("PNG"="png","PDF"="pdf","TIFF"="tiff","SVG"="svg"), selected = "png")),
                    column(3, numericInput("tc_dl_w","Width (in)", 12, min = 4, max = 30)),
                    column(3, numericInput("tc_dl_h","Height (in)", 8, min = 4, max = 30)),
                    column(3, numericInput("tc_dl_dpi","DPI", 300, min = 72, max = 600))
                  ),
                  div(style = "margin-top:8px;", downloadButton("dl_timecourse_plot_local","Download Time Course"))
              ),
              box(title = "Time Course Summary", status = "info", solidHeader = TRUE, width = 4,
                  htmlOutput("tc_summary_table"))
            )
    ),
    
    # ---- IND METRICS (no stats) ----
    tabItem(tabName = "metrics",
            fluidRow(
              box(title = "Controls", status = "success", solidHeader = TRUE, width = 4,
                  selectInput("metric_name","Metric",
                              choices = c("Peak ΔF/F₀"="Peak_dFF0","Time to Peak (s)"="Time_to_Peak",
                                          "Time to 25% Peak (s)"="Time_to_25_Peak","Time to 50% Peak (s)"="Time_to_50_Peak",
                                          "Time to 75% Peak (s)"="Time_to_75_Peak","Rise Time (s)"="Rise_Time",
                                          "Half Width (HWHM)"="Half_Width",
                                          "Ca²⁺ Entry Rate"="Calcium_Entry_Rate","AUC"="AUC",
                                          "Response Amplitude"="Response_Amplitude","SNR"="SNR"),
                              selected="Peak_dFF0"),
                  # Only cell bars supported now
                  shinyjs::hidden(selectInput("metric_geom","Plot type", choices = c("Cell bars"="cellbar"), selected="cellbar")),
                  checkboxInput("metric_sort_cells","Sort cell bars within group", TRUE),
                  sliderInput("metric_inset_scale","Inset size", min = 0.5, max = 3, value = 1, step = 0.1),
                  textInput("metric_title","Custom title (optional)",""),
                  checkboxInput("metric_auto_y","Auto y-label (use metric units)", TRUE),
                  conditionalPanel("!input.metric_auto_y", textInput("metric_y_label","Y label","Value")),
                  sliderInput("metric_size","Base font size", 8, 22, 14, 1)
              ),
              box(title = "Metrics Plot", status = "success", solidHeader = TRUE, width = 8,
                  withSpinner(plotOutput("metrics_plot", height = "640px"), type = 4))
            )
    ),
    
    # ---- IND HEATMAP ----
    tabItem(tabName = "heatmap",
            fluidRow(
              box(title = "Controls", status = "warning", solidHeader = TRUE, width = 4,
                  checkboxInput("hm_norm_per_cell","Display normalization 0-1 per cell (heatmap only)", FALSE),
                  selectInput("hm_sort","Sort cells by", choices = c("Time to Peak"="tpeak","Peak Amplitude"="amp","Original"="orig"), selected="tpeak"),
                  selectInput("hm_palette","Color palette", choices = c("plasma","viridis","magma","inferno","cividis"), selected = "plasma"),
                  sliderInput("hm_legend_text_size","Legend text size", min = 6, max = 24, value = 10, step = 1),
                  tags$hr(),
                  textInput("hm_title","Plot title","Population Heatmap"),
                  textInput("hm_x_label","X label","Time (s)"),
                  textInput("hm_y_label","Y label","Cell"),
                  sliderInput("hm_title_size","Title size", 10, 28, 16, 1),
                  sliderInput("hm_axis_title_size","Axis title size", 8, 28, 14, 1),
                  sliderInput("hm_axis_text_size","Axis text size", 8, 28, 12, 1)
              ),
              box(title = "Heatmap", status = "warning", solidHeader = TRUE, width = 8,
                  withSpinner(plotOutput("heatmap_plot", height = "760px"), type = 4),
                  tags$hr(),
                  fluidRow(
                    column(3, selectInput("hm_dl_fmt","Format", choices = c("PNG"="png","PDF"="pdf","TIFF"="tiff","SVG"="svg"), selected = "png")),
                    column(3, numericInput("hm_dl_w","Width (in)", 12, min = 4, max = 30)),
                    column(3, numericInput("hm_dl_h","Height (in)", 8, min = 4, max = 30)),
                    column(3, numericInput("hm_dl_dpi","DPI", 300, min = 72, max = 600))
                  ),
                  div(style = "margin-top:8px;", downloadButton("dl_heatmap_plot_local","Download Heatmap"))
              )
            )
    ),
    
    # ---- IND TABLES ----
    tabItem(tabName = "tables",
            fluidRow(
              box(title = "Per-cell Metrics", status = "primary", solidHeader = TRUE, width = 12,
                  DTOutput("metrics_table"))
            ),
            fluidRow(
              box(title = "Time-course Summary (Mean ± SEM)", status = "primary", solidHeader = TRUE, width = 12,
                  DTOutput("summary_table"))
            )
    ),
    
    # ---- IND GUIDE ----
    tabItem(tabName = "guide",
            fluidRow(
              box(title = "Metric Guide (Live Examples)", status = "info", solidHeader = TRUE, width = 12,
                  withMathJax(),
                  p("Select a metric to see how it is computed using your uploaded data, with detailed explanations and an annotated example trace."),
                  fluidRow(
                    column(4,
                           selectInput("guide_metric", "Metric",
                                       choices = c(
                                         "ΔF/F₀ Calculation" = "dFF0_calc",
                                         "Peak ΔF/F₀" = "Peak_dFF0",
                                         "Response Amplitude" = "Response_Amplitude",
                                         "AUC" = "AUC",
                                         "Time to Peak" = "Time_to_Peak",
                                         "Time to 25% Peak" = "Time_to_25_Peak",
                                         "Time to 50% Peak" = "Time_to_50_Peak",
                                         "Time to 75% Peak" = "Time_to_75_Peak",
                                         "Rise Time (10→90%)" = "Rise_Time",
                                         "Ca²⁺ Entry Rate" = "Calcium_Entry_Rate",
                                         "Half Width" = "Half_Width",
                                         "SNR" = "SNR"
                                       ),
                                       selected = "dFF0_calc"),
                           uiOutput("guide_cell_picker"),
                           uiOutput("guide_timepoint_ui")
                    ),
                    column(8,
                           uiOutput("guide_math_explanation"),
                           tags$hr(),
                           withSpinner(plotOutput("guide_plot", height = "560px"), type = 4))
                  )
              )
            )
    ),
    
    # ---- IND EXPORT ----
    tabItem(tabName = "export",
            fluidRow(
              box(title = "Save", status = "primary", solidHeader = TRUE, width = 4,
                  radioButtons("exp_fmt","Format", choices = c("PNG"="png","PDF"="pdf","TIFF"="tiff","SVG"="svg"),
                               inline = TRUE, selected="png"),
                  numericInput("exp_w","Width (in)", 12, min=4, max=30),
                  numericInput("exp_h","Height (in)", 8, min=4, max=30),
                  numericInput("exp_dpi","DPI (for raster)", 300, min=72, max=600),
                  conditionalPanel("input.exp_fmt == 'tiff'", selectInput("tiff_comp","TIFF compression", choices=c("lzw","zip","none"), selected="lzw")),
                  tags$hr(),
                  h4("Figure Composer"),
                  checkboxGroupInput("compose_panels","Include panels", choices=c("Time Course"="tc","Metrics"="mp","Heatmap"="hm"), selected=c("tc","mp")),
                  selectInput("compose_layout","Layout", choices=c("Columns"="col","Rows"="row"), selected="col"),
                  downloadButton("dl_composite","Download Composite Figure"),
                  downloadButton("dl_metrics_csv","Download Metrics CSV"),
                  br(), br(), downloadButton("dl_summary_csv","Download Summary CSV"),
                  br(), br(), downloadButton("dl_timecourse_plot","Download Time Course Plot"),
                  br(), br(), downloadButton("dl_heatmap_plot","Download Heatmap Plot"),
                  br(), br(), downloadButton("dl_metrics_plot","Download Current Metrics Plot"),
                  tags$hr(),
                  h4("Processed Data"),
                  selectInput("exp_dl_group", "Select file", choices = NULL),
                  downloadButton("dl_processed_wide_exp", "Download Processed Data (CSV)")
              ),
              box(title = "Notes", status = "info", solidHeader = TRUE, width = 8,
                  tags$ul(
                    tags$li("PNG/TIFF recommended for slides/publication; use 300–600 DPI"),
                    tags$li("PDF/SVG preserve vector graphics")
                  ),
                  verbatimTextOutput("export_info")
              )
            )
    ),
    
    tabItem(tabName = "help",
            fluidRow(
              box(title = "Documentation & Tips", status = "primary", solidHeader = TRUE, width = 12,
                  tags$h4("Getting Started"),
                  tags$ol(
                    tags$li("Group Comparison: upload processed ΔF/F₀ files and append multiple groups."),
                    tags$li("Individual Analysis: upload wide matrices, compute ΔF/F₀ in Pre-processing if needed, then analyze."),
                    
                  )
              )
            )
    )
  )
)

ui <- dashboardPage(header, sidebar, body)

# ---------- Server ----------
server <- function(input, output, session) {
  iv <- InputValidator$new()
  iv$add_rule("metric_name", sv_required())
  iv$add_rule("gc_metric_name", sv_required())
  iv$enable()
  
  # Global baseline setting (single source of truth)
  baseline_frames <- reactive({
    if (isTRUE(input$pp_enable) && identical(input$pp_baseline_method, "first_n")) {
      val <- as.integer(input$pp_baseline_frames %||% 20)
      return(max(1, val))
    }
    20L
  })
  
  rv <- reactiveValues(files=NULL, groups=NULL, dts=list(), long=NULL, summary=NULL, metrics=NULL, colors=NULL)
  gcv <- reactiveValues(files=NULL, groups=NULL, dts=list(), long=NULL, summary=NULL, metrics=NULL, colors=NULL)
  
  make_unique_label <- function(label, existing) {
    if (!(label %in% existing)) return(label)
    base <- label; i <- 2
    repeat {
      cand <- paste0(base, "_", i)
      if (!(cand %in% existing)) return(cand)
      i <- i + 1
      if (i > 9999) return(paste0(base, "_", as.integer(Sys.time())))
    }
  }
  
  # Demo loaders (optional)
  observeEvent(input$demo_btn, {
    withProgress(message="Generating demo data...", value=0, {
      g1 <- simulate_group("Ctrl", n_cells=60, T=180, sr=5, peak_t=40, amp=0.5)
      g2 <- simulate_group("Drug", n_cells=60, T=180, sr=5, peak_t=55, amp=0.7)
      rv$dts <- list(Ctrl=g1$dt, Drug=g2$dt)
      updateSelectInput(session, "pp_dl_group", choices = names(rv$dts), selected = names(rv$dts)[1])
      rv$groups <- names(rv$dts); rv$colors <- default_group_colors(rv$groups)
      rv$long <- purrr::imap(rv$dts, ~to_long(.x, .y)) |> dplyr::bind_rows()
      updateTextInput(session, "tc_title", value = paste(rv$groups, collapse = " vs "))
      updateSelectInput(session, "exp_dl_group", choices = names(rv$dts), selected = names(rv$dts)[1])
      rv$summary <- rv$long |>
        dplyr::group_by(Group, Time) |>
        dplyr::summarise(mean_dFF0=mean(dFF0, na.rm=TRUE),
                         sem_dFF0=stats::sd(dFF0, na.rm=TRUE)/sqrt(dplyr::n()),
                         sd_dFF0=stats::sd(dFF0, na.rm=TRUE),
                         n_cells=dplyr::n(), .groups="drop")
      incProgress(0, detail = "Calculating metrics (demo)...")
      rv$metrics <- purrr::imap(rv$dts, ~compute_metrics_for_dt(.x, .y, baseline_frames())) |> dplyr::bind_rows()
    })
  })
  observeEvent(input$gc_demo_btn, {
    withProgress(message="Generating demo groups...", value=0, {
      g1 <- simulate_group("WT", n_cells=50, T=120, sr=4, peak_t=35, amp=0.5)
      g2 <- simulate_group("KO", n_cells=50, T=120, sr=4, peak_t=50, amp=0.65)
      gcv$dts <- list(WT=g1$dt, KO=g2$dt)
      gcv$groups <- names(gcv$dts); gcv$colors <- default_group_colors(gcv$groups)
      gcv$long <- purrr::imap(gcv$dts, ~to_long(.x, .y)) |> dplyr::bind_rows()
      gcv$summary <- gcv$long |>
        dplyr::group_by(Group, Time) |>
        dplyr::summarise(mean_dFF0=mean(dFF0, na.rm=TRUE),
                         sem_dFF0=stats::sd(dFF0, na.rm=TRUE)/sqrt(dplyr::n()),
                         sd_dFF0=stats::sd(dFF0, na.rm=TRUE),
                         n_cells=dplyr::n(), .groups="drop")
      out <- data.frame()
      for (nm in names(gcv$dts)) {
        tv <- gcv$dts[[nm]]$Time
        for (j in 2:ncol(gcv$dts[[nm]])) {
          m <- calculate_cell_metrics(gcv$dts[[nm]][[j]], tv, baseline_frames())
          m$Group <- nm; m$Cell_ID <- paste0(nm, "_Cell", j-1)
          out <- rbind(out, m)
        }
      }
      gcv$metrics <- out[stats::complete.cases(out[,1:9]), ]
    })
  })
  
  # Individual loaders
  observeEvent(input$load_btn, {
    req(input$data_files)
    withProgress(message="Processing data...", value=0, {
      files <- input$data_files
      rv$files <- files
      labels <- tools::file_path_sans_ext(basename(files$name))
      rv$groups <- labels
      rv$colors <- default_group_colors(labels)
      dts <- list()
      n_files <- nrow(files)
      for (i in seq_len(nrow(files))) {
        incProgress(1/n_files, detail = paste("Loading file", i, "of", n_files, ":", basename(files$name[i])))
        path <- files$datapath[i]
        dt <- safe_read(path)
        if (ncol(dt) < 2) next
        incProgress(0, detail = paste("Processing", basename(files$name[i]), "..."))
        dt <- ensure_time_first(dt) |> coerce_numeric_dt()
        if (!all(is.finite(dt[[1]])) || any(diff(dt[[1]]) <= 0, na.rm=TRUE)) {
          sr <- as.numeric(input$pp_sampling_rate %||% 1)
          dt[[1]] <- seq(0, by=1/sr, length.out=nrow(dt))
        }
        if (isTRUE(input$pp_enable)) {
          if (isTRUE(input$pp_compute_dff)) {
            if (isTRUE(input$pp_apply_bg) && nzchar(input$pp_bg_col) && input$pp_bg_col %in% names(dt)) {
              bg <- dt[[input$pp_bg_col]]
              for (j in 2:ncol(dt)) if (names(dt)[j] != input$pp_bg_col) dt[[j]] <- dt[[j]] - bg
            }
            if (identical(input$pp_baseline_method,"first_n")) {
              n_bl <- max(1, as.integer(input$pp_baseline_frames %||% 20))
              F0 <- vapply(seq(2, ncol(dt)), function(j) mean(dt[[j]][seq_len(min(n_bl, nrow(dt)))], na.rm=TRUE), numeric(1))
            } else if (identical(input$pp_baseline_method,"rolling_min")) {
              win <- max(5, as.integer(input$pp_window_size %||% 50))
              F0 <- vapply(seq(2, ncol(dt)), function(j) {
                x <- dt[[j]]; if (length(x) < win) return(min(x, na.rm=TRUE))
                rm <- zoo::rollmean(x, k=win, fill=NA); min(rm, na.rm=TRUE)
              }, numeric(1))
            } else {
              pct <- max(1, min(50, as.integer(input$pp_percentile %||% 10)))
              F0 <- vapply(seq(2, ncol(dt)), function(j) stats::quantile(dt[[j]], probs=pct/100, na.rm=TRUE, names=FALSE), numeric(1))
            }
            for (k in seq_along(F0)) {
              j <- k+1; f0 <- F0[[k]]
              dt[[j]] <- if (is.finite(f0) && f0 != 0) (dt[[j]] - f0) / f0 else NA_real_
            }
          }
          if (isTRUE(input$pp_minmax_enable)) {
            for (j in 2:ncol(dt)) {
              x <- dt[[j]]; xr <- range(x, na.rm=TRUE)
              if (is.finite(xr[1]) && is.finite(xr[2]) && xr[2] > xr[1]) dt[[j]] <- (x - xr[1])/(xr[2]-xr[1])
            }
          }
        }
        dts[[labels[i]]] <- dt
      }
      rv$dts <- dts
      updateSelectInput(session, "pp_dl_group", choices = names(dts), selected = names(dts)[1])
      updateSelectInput(session, "exp_dl_group", choices = names(dts), selected = names(dts)[1])
      
      rv$long <- purrr::imap(dts, ~to_long(.x, .y)) |> dplyr::bind_rows()
      rv$summary <- if (nrow(rv$long) > 0) rv$long |>
        dplyr::group_by(Group, Time) |>
        dplyr::summarise(mean_dFF0=mean(dFF0, na.rm=TRUE),
                         sem_dFF0=stats::sd(dFF0, na.rm=TRUE)/sqrt(dplyr::n()),
                         sd_dFF0=stats::sd(dFF0, na.rm=TRUE),
                         n_cells=dplyr::n(), .groups="drop") else NULL
      
      incProgress(0, detail = "Calculating metrics...")
      rv$metrics <- purrr::imap(dts, ~compute_metrics_for_dt(.x, .y, baseline_frames())) |> dplyr::bind_rows()
      if (length(labels) > 0) updateTextInput(session, "tc_title", value = paste(labels, collapse = ", "))
    })
  })
  
  # Group comparison loaders (unchanged)
  observeEvent(input$gc_load_btn, {
    req(input$gc_files)
    withProgress(message="Loading processed data...", value=0, {
      files <- input$gc_files
      new_dts <- isolate(gcv$dts); if (is.null(new_dts)) new_dts <- list()
      for (i in seq_len(nrow(files))) {
        path <- files$datapath[i]
        dt <- safe_read(path)
        if (ncol(dt) < 2) next
        dt <- ensure_time_first(dt) |> coerce_numeric_dt()
        lab <- tools::file_path_sans_ext(basename(files$name[i]))
        lab <- make_unique_label(lab, names(new_dts))
        new_dts[[lab]] <- dt
      }
      gcv$dts <- new_dts
      gcv$groups <- names(new_dts)
      cols <- isolate(gcv$colors); if (is.null(cols)) cols <- setNames(character(0), character(0))
      missing <- setdiff(names(new_dts), names(cols))
      if (length(missing) > 0) cols <- c(cols, default_group_colors(missing))
      gcv$colors <- cols
      
      gcv$long <- purrr::imap(new_dts, ~to_long(.x, .y)) |> dplyr::bind_rows()
      gcv$summary <- gcv$long |>
        dplyr::group_by(Group, Time) |>
        dplyr::summarise(mean_dFF0=mean(dFF0, na.rm=TRUE),
                         sem_dFF0=stats::sd(dFF0, na.rm=TRUE)/sqrt(dplyr::n()),
                         sd_dFF0=stats::sd(dFF0, na.rm=TRUE),
                         n_cells=dplyr::n(), .groups="drop")
      out <- data.frame()
      for (nm in names(new_dts)) {
        tv <- new_dts[[nm]]$Time
        for (j in 2:ncol(new_dts[[nm]])) {
          m <- calculate_cell_metrics(new_dts[[nm]][[j]], tv, baseline_frames())
          m$Group <- nm; m$Cell_ID <- paste0(nm, "_Cell", j-1)
          out <- rbind(out, m)
        }
      }
      gcv$metrics <- out[stats::complete.cases(out[,1:9]), ]
    })
  })
  
  # Value boxes (mini summaries)
  output$vb_groups <- renderValueBox({
    g <- length(rv$dts); shinydashboard::valueBox(value=g, subtitle="Files loaded", icon=icon("layer-group"), color="teal")
  })
  output$vb_cells <- renderValueBox({
    n <- sum(purrr::map_int(rv$dts, ~max(0, ncol(.x)-1)))
    shinydashboard::valueBox(value=n, subtitle="Total cells", icon=icon("circle"), color="purple")
  })
  output$vb_timepoints <- renderValueBox({
    tp <- sum(purrr::map_int(rv$dts, nrow))
    shinydashboard::valueBox(value=tp, subtitle="Total timepoints", icon=icon("clock"), color="olive")
  })
  output$vb_gc_groups <- renderValueBox({
    g <- length(gcv$dts); shinydashboard::valueBox(value=g, subtitle="Groups (processed)", icon=icon("layer-group"), color="aqua")
  })
  output$vb_gc_cells <- renderValueBox({
    n <- sum(purrr::map_int(gcv$dts, ~max(0, ncol(.x)-1)))
    shinydashboard::valueBox(value=n, subtitle="Cells (processed)", icon=icon("braille"), color="maroon")
  })
  output$vb_gc_timepoints <- renderValueBox({
    tp <- sum(purrr::map_int(gcv$dts, nrow))
    shinydashboard::valueBox(value=tp, subtitle="Timepoints (processed)", icon=icon("clock"), color="yellow")
  })
  
  # Validation helpers and observers
  show_error <- function(message, duration = 5) { showNotification(message, type = "error", duration = duration) }
  show_warning <- function(message, duration = 4) { showNotification(message, type = "warning", duration = duration) }
  show_success <- function(message, duration = 3) { showNotification(message, type = "success", duration = duration) }
  
  observeEvent(input$pp_baseline_frames, {
    if (!is.null(input$pp_baseline_frames)) {
      if (input$pp_baseline_frames < 1) {
        updateNumericInput(session, "pp_baseline_frames", value = 20)
        show_error("Baseline must be at least 1 frame. Reset to 20.")
      } else if (input$pp_baseline_frames > 100) {
        show_warning("Large baseline (>100 frames) may not leave enough data for analysis.")
      }
    }
  })
  
  observeEvent(input$pp_sampling_rate, {
    if (!is.null(input$pp_sampling_rate) && input$pp_sampling_rate <= 0) {
      updateNumericInput(session, "pp_sampling_rate", value = 1)
      show_error("Sampling rate must be positive. Reset to 1 Hz.")
    }
  })
  
  # Status dashboard outputs
  output$status_files_loaded <- renderText({
    if (is.null(rv$files)) "No files" else paste(nrow(rv$files), "file(s)")
  })
  output$status_processing <- renderText({
    if (is.null(rv$dts) || length(rv$dts) == 0) "Not started" else "Complete"
  })
  output$status_metrics <- renderText({
    if (is.null(rv$metrics)) "Not calculated" else paste(nrow(rv$metrics), "cells analyzed")
  })
  output$status_ready <- renderText({
    if (!is.null(rv$metrics) && nrow(rv$metrics) > 0) "✓ Ready for analysis" else "Awaiting data"
  })
  
  # Group comparison UI helpers
  output$gc_group_label_ui <- renderUI({
    req(gcv$dts)
    labs <- names(gcv$dts)
    tagList(
      lapply(seq_along(labs), function(i) {
        fluidRow(
          column(8, textInput(paste0("gc_group_label_", i), paste0("Group ", i, " label"), value=labs[i])),
          column(4, colourpicker::colourInput(paste0("gc_group_color_", i), "Color", value = if (!is.null(gcv$colors)) gcv$colors[[labs[i]]] else NULL))
        )
      }),
      fluidRow(
        column(6, actionButton("gc_apply_labels","Apply Labels", class="btn-success", width="100%")),
        column(6, actionButton("gc_apply_colors","Apply Colors", class="btn-info", width="100%"))
      )
    )
  })
  observeEvent(input$gc_apply_labels, {
    req(gcv$dts)
    labs <- sapply(seq_len(length(gcv$dts)), function(i) input[[paste0("gc_group_label_", i)]])
    if (any(is.na(labs) | labs == "")) return()
    new_dts <- stats::setNames(unname(gcv$dts), labs)
    gcv$groups <- labs; gcv$dts <- new_dts
    if (is.null(gcv$colors) || length(gcv$colors) != length(labs)) gcv$colors <- default_group_colors(labs) else names(gcv$colors) <- labs
    gcv$long <- purrr::imap(new_dts, ~to_long(.x, .y)) |> dplyr::bind_rows()
    gcv$summary <- gcv$long |>
      dplyr::group_by(Group, Time) |>
      dplyr::summarise(mean_dFF0=mean(dFF0, na.rm=TRUE),
                       sem_dFF0=stats::sd(dFF0, na.rm=TRUE)/sqrt(dplyr::n()),
                       sd_dFF0=stats::sd(dFF0, na.rm=TRUE),
                       n_cells=dplyr::n(), .groups="drop")
    out <- data.frame()
    for (nm in names(new_dts)) {
      tv <- new_dts[[nm]]$Time
      for (j in 2:ncol(new_dts[[nm]])) {
        m <- calculate_cell_metrics(new_dts[[nm]][[j]], tv)
        m$Group <- nm; m$Cell_ID <- paste0(nm, "_Cell", j-1)
        out <- rbind(out, m)
      }
    }
    gcv$metrics <- out[stats::complete.cases(out[,1:9]), ]
  })
  observeEvent(input$gc_apply_colors, {
    req(gcv$groups)
    cols <- sapply(seq_along(gcv$groups), function(i) input[[paste0("gc_group_color_", i)]])
    names(cols) <- gcv$groups; gcv$colors <- cols
  })
  output$gc_data_summary_table <- renderDT({
    req(gcv$dts)
    df <- purrr::imap(gcv$dts, ~data.frame(Group=.y, n_time=nrow(.x), n_cells=ncol(.x)-1)) |> dplyr::bind_rows()
    datatable(df, rownames=FALSE, options=list(dom='t', pageLength=100))
  })
  
  output$gc_timecourse_plot <- renderPlot({
    req(gcv$summary)
    
    # Base plot setup
    base_size <- input$gc_time_font_size %||% 12
    p <- ggplot(gcv$summary, aes(Time, mean_dFF0, color=Group, fill=Group))
    
    # Add individual traces if requested
    if (input$gc_time_show_individual %||% FALSE) {
      # Create long format data for individual traces
      individual_data <- purrr::imap(gcv$dts, function(dt, group_name) {
        dt_long <- dt %>%
          tidyr::pivot_longer(cols = -Time, names_to = "Cell", values_to = "dFF0") %>%
          dplyr::mutate(Group = group_name)
        return(dt_long)
      }) %>% dplyr::bind_rows()
      
      alpha_val <- input$gc_time_alpha %||% 0.3
      p <- p + geom_line(data = individual_data, 
                         aes(x = Time, y = dFF0, group = interaction(Group, Cell)), 
                         alpha = alpha_val, linewidth = 0.3)
    }
    
    # Add ribbon if requested
    if (input$gc_time_show_ribbon %||% TRUE) {
      p <- p + geom_ribbon(aes(ymin=mean_dFF0 - sem_dFF0, ymax=mean_dFF0 + sem_dFF0), 
                           alpha=0.25, color=NA)
    }
    
    # Add main line
    p <- p + geom_line(linewidth=1.4)
    
    # Add points if requested
    if (input$gc_time_show_points %||% TRUE) {
      p <- p + geom_point(size=0.8, alpha=0.7)
    }
    
    # Apply theme and labels
    p <- p + theme_classic(base_size=base_size) + 
      theme(legend.position="bottom") +
      labs(title="Group Time Course (Processed Data)", x="Time (s)", y=expression(Delta*"F/F"[0]))
    
    # Apply colors
    cols <- gcv$colors
    if (!is.null(cols)) {
      p <- p + scale_color_manual(values=cols) + scale_fill_manual(values=cols)
    }
    
    p
  }) %>% bindCache(gcv$summary, input$gc_time_show_ribbon, input$gc_time_show_points, 
                   input$gc_time_show_individual, input$gc_time_alpha, input$gc_time_font_size)
  
  # Interactive plotly version
  output$gc_timecourse_plotly <- renderPlotly({
    req(gcv$summary)
    
    # Create base ggplot
    base_size <- input$gc_time_font_size %||% 12
    p <- ggplot(gcv$summary, aes(Time, mean_dFF0, color=Group, fill=Group))
    
    # Add individual traces if requested
    if (input$gc_time_show_individual %||% FALSE) {
      individual_data <- purrr::imap(gcv$dts, function(dt, group_name) {
        dt_long <- dt %>%
          tidyr::pivot_longer(cols = -Time, names_to = "Cell", values_to = "dFF0") %>%
          dplyr::mutate(Group = group_name)
        return(dt_long)
      }) %>% dplyr::bind_rows()
      
      alpha_val <- input$gc_time_alpha %||% 0.3
      p <- p + geom_line(data = individual_data, 
                         aes(x = Time, y = dFF0, group = interaction(Group, Cell),
                             text = paste("Group:", Group, "<br>Cell:", Cell, 
                                          "<br>Time:", round(Time, 2), "s",
                                          "<br>ΔF/F₀:", round(dFF0, 4))), 
                         alpha = alpha_val, linewidth = 0.3)
    }
    
    # Add ribbon if requested
    if (input$gc_time_show_ribbon %||% TRUE) {
      p <- p + geom_ribbon(aes(ymin=mean_dFF0 - sem_dFF0, ymax=mean_dFF0 + sem_dFF0,
                               text = paste("Group:", Group, 
                                            "<br>Time:", round(Time, 2), "s",
                                            "<br>Mean:", round(mean_dFF0, 4),
                                            "<br>SEM:", round(sem_dFF0, 4))), 
                           alpha=0.25, color=NA)
    }
    
    # Add main line with enhanced tooltips
    p <- p + geom_line(aes(text = paste("Group:", Group, 
                                        "<br>Time:", round(Time, 2), "s",
                                        "<br>Mean ΔF/F₀:", round(mean_dFF0, 4),
                                        "<br>SEM:", round(sem_dFF0, 4),
                                        "<br>n cells:", n_cells)), 
                       linewidth=1.4)
    
    # Add points if requested
    if (input$gc_time_show_points %||% TRUE) {
      p <- p + geom_point(aes(text = paste("Group:", Group, 
                                           "<br>Time:", round(Time, 2), "s",
                                           "<br>Mean ΔF/F₀:", round(mean_dFF0, 4),
                                           "<br>SEM:", round(sem_dFF0, 4),
                                           "<br>n cells:", n_cells)), 
                          size=0.8, alpha=0.7)
    }
    
    # Apply theme
    p <- p + theme_classic(base_size=base_size) + 
      theme(legend.position="bottom") +
      labs(title="Interactive Group Time Course (Processed Data)", 
           x="Time (s)", y="ΔF/F₀")
    
    # Apply colors
    cols <- gcv$colors
    if (!is.null(cols)) {
      p <- p + scale_color_manual(values=cols) + scale_fill_manual(values=cols)
    }
    
    # Convert to plotly with enhanced interactivity
    gg <- ggplotly(p, tooltip = "text") %>%
      plotly::layout(
        title = list(text = "Interactive Group Time Course (Processed Data)", font = list(size = base_size + 2)),
        xaxis = list(title = "Time (s)", titlefont = list(size = base_size)),
        yaxis = list(title = "ΔF/F₀", titlefont = list(size = base_size)),
        hovermode = 'closest',
        legend = list(orientation = "h", x = 0.5, xanchor = 'center', y = -0.1)
      ) %>%
      plotly::config(
        displayModeBar = TRUE,
        modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
        displaylogo = FALSE,
        toImageButtonOptions = list(
          format = "png",
          filename = "group_timecourse",
          height = 600,
          width = 900,
          scale = 2
        )
      )
    
    gg
  }) %>% bindCache(gcv$summary, input$gc_time_show_ribbon, input$gc_time_show_points, 
                   input$gc_time_show_individual, input$gc_time_alpha, input$gc_time_font_size)
  
  output$gc_metrics_plot <- renderPlot({
    req(gcv$metrics)
    df <- gcv$metrics
    metric <- input$gc_metric_name
    df <- dplyr::filter(df, is.finite(.data[[metric]]))
    validate(need(nrow(df) > 0, "No finite values for this metric."))
    
    # Calculate summary statistics
    sm <- df |>
      dplyr::group_by(Group) |>
      dplyr::summarise(
        mean_val = mean(.data[[metric]], na.rm=TRUE), 
        sem_val = stats::sd(.data[[metric]], na.rm=TRUE)/sqrt(dplyr::n()),
        n = dplyr::n(),
        .groups="drop"
      )
    
    # Ensure Group is an ordered factor so x positions are stable
    df$Group <- factor(df$Group, levels = sm$Group)
    
    n_groups <- length(unique(df$Group))
    base <- theme_classic(base_size=input$gc_metric_size) +
      theme(legend.position="none", axis.text.x = element_text(angle = 30, hjust = 1))
    
    # Create base plot based on geometry
    if (input$gc_metric_geom == "bar") {
      p <- ggplot(sm, aes(Group, mean_val, fill=Group)) +
        geom_col(alpha=0.85, width=0.6, color="black") +
        geom_errorbar(aes(ymin=mean_val - sem_val, ymax=mean_val + sem_val), width=0.2) + base
      
      # Add individual points if requested
      if (input$gc_metric_points) {
        p <- p + geom_jitter(data=df, aes(Group, .data[[metric]]), 
                             width=0.15, alpha=0.6, size=1.5, color="black")
      }
      
    } else if (input$gc_metric_geom == "box") {
      p <- ggplot(df, aes(Group, .data[[metric]], fill=Group)) + 
        geom_boxplot(alpha=0.8, outlier.alpha=0.4) + base
      
      # Add individual points if requested
      if (input$gc_metric_points) {
        p <- p + geom_jitter(width=0.15, alpha=0.6, size=1.5)
      }
      
    } else if (input$gc_metric_geom == "violin") {
      p <- ggplot(df, aes(Group, .data[[metric]], fill=Group)) +
        geom_violin(trim=FALSE, alpha=0.8) + 
        geom_boxplot(width=0.12, alpha=0.9, outlier.alpha=0) + base
      
      # Add individual points if requested
      if (input$gc_metric_points) {
        p <- p + geom_jitter(width=0.08, alpha=0.6, size=1.5)
      }
      
    } else { # dot plot
      p <- ggplot(sm, aes(Group, mean_val, color=Group)) +
        geom_point(size=3) + 
        geom_errorbar(aes(ymin=mean_val - sem_val, ymax=mean_val + sem_val), width=0.12) + base
      
      # Add individual points if requested
      if (input$gc_metric_points) {
        p <- p + geom_jitter(data=df, aes(Group, .data[[metric]]), 
                             width=0.15, alpha=0.6, size=1.5)
      }
    }
    
    # Apply colors
    cols <- gcv$colors
    if (!is.null(cols)) { 
      if (input$gc_metric_geom == "dot") {
        p <- p + scale_color_manual(values=cols)
      } else {
        p <- p + scale_fill_manual(values=cols)
      }
    }
    
    # Utility to convert p to stars
    p_to_stars <- function(p) {
      if (is.na(p)) return("ns")
      if (p < 0.0001) return("****")
      if (p < 0.001)  return("***")
      if (p < 0.01)   return("**")
      if (p < 0.05)   return("*")
      "ns"
    }
    
    # Add statistical tests if requested
    if (input$gc_metric_stats && n_groups >= 2) {
      y_range <- range(df[[metric]], na.rm=TRUE)
      y_max <- y_range[2]
      y_span <- diff(y_range)
      
      if (n_groups == 2) {
        # Two-group tests
        groups <- levels(df$Group)
        group1_data <- df[df$Group == groups[1], metric]
        group2_data <- df[df$Group == groups[2], metric]
        
        if (input$gc_stat_test == "ttest") {
          test_result <- tryCatch({ t.test(group1_data, group2_data) }, error = function(e) NULL)
          test_name <- "t-test"
        } else if (input$gc_stat_test == "wilcox") {
          test_result <- tryCatch({ wilcox.test(group1_data, group2_data) }, error = function(e) NULL)
          test_name <- "Wilcoxon"
        } else { test_result <- NULL }
        
        if (!is.null(test_result)) {
          p_val <- test_result$p.value
          sig_text <- if (p_val < 0.001) paste0(test_name, ": p < 0.001")
          else if (p_val < 0.01) paste0(test_name, ": p < 0.01")
          else if (p_val < 0.05) paste0(test_name, ": p < 0.05")
          else paste0(test_name, ": p = ", signif(p_val, 3))
          p <- p + annotate("text", x = mean(1:2), y = y_max + 0.15*y_span, 
                            label = sig_text, size = input$gc_metric_size/3.5, 
                            fontface = ifelse(p_val < 0.05, "bold", "plain"))
          
          # Draw bracket between the two groups if requested
          if (isTRUE(input$gc_show_brackets)) {
            p <- p +
              annotate("segment", x=1, xend=1, y=y_max + 0.05*y_span, yend=y_max + 0.10*y_span) +
              annotate("segment", x=2, xend=2, y=y_max + 0.05*y_span, yend=y_max + 0.10*y_span) +
              annotate("segment", x=1, xend=2, y=y_max + 0.10*y_span, yend=y_max + 0.10*y_span) +
              annotate("text", x=1.5, y=y_max + 0.12*y_span, label=p_to_stars(p_val), fontface="bold")
          }
        }
      } else if (input$gc_stat_test %in% c("anova","kruskal")) {
        if (input$gc_stat_test == "anova") {
          # One-way ANOVA + Tukey HSD
          anova_result <- tryCatch({
            aov_model <- aov(as.formula(paste(metric, "~ Group")), data = df)
            anova_summary <- summary(aov_model)
            list(p.value = anova_summary[[1]][["Pr(>F)"]][1], f.value = anova_summary[[1]][["F value"]][1], model = aov_model)
          }, error = function(e) NULL)
          if (!is.null(anova_result)) {
            p_val <- anova_result$p.value; f_val <- anova_result$f.value
            p <- p + annotate("text", x = mean(seq_len(n_groups)), y = y_max + 0.15*y_span,
                              label = sprintf("ANOVA: F=%.2f, p=%s", f_val, ifelse(p_val<0.001,"<0.001", signif(p_val,3))),
                              size = input$gc_metric_size/3.5, fontface = ifelse(p_val < 0.05, "bold", "plain"))
            
            if (isTRUE(input$gc_show_posthoc)) {
              tuk <- tryCatch({ TukeyHSD(anova_result$model)$Group }, error=function(e) NULL)
              if (!is.null(tuk)) {
                tuk_df <- as.data.frame(tuk)
                tuk_df$pair <- rownames(tuk_df)
                # Parse pairs as group1-group2
                parsed <- do.call(rbind, strsplit(tuk_df$pair, "-", fixed = TRUE))
                if (!is.null(parsed) && ncol(parsed) == 2) {
                  colnames(parsed) <- c("g1","g2")
                  tuk_df <- cbind(tuk_df, parsed)
                  # Keep significant first, then best p-values
                  tuk_df <- tuk_df[order(tuk_df$`p adj`, decreasing = FALSE), , drop=FALSE]
                  # Limit number of brackets to avoid clutter
                  max_br <- 6L
                  shown <- head(tuk_df, max_br)
                  # Compute y positions
                  base_y <- y_max + 0.05*y_span
                  step_y <- 0.07*y_span
                  for (i in seq_len(nrow(shown))) {
                    g1 <- as.character(shown$g1[i]); g2 <- as.character(shown$g2[i])
                    x1 <- match(g1, levels(df$Group)); x2 <- match(g2, levels(df$Group))
                    if (is.na(x1) || is.na(x2)) next
                    yy <- base_y + (i-1)*step_y
                    # bracket
                    p <- p +
                      annotate("segment", x=x1, xend=x1, y=yy - 0.02*y_span, yend=yy) +
                      annotate("segment", x=x2, xend=x2, y=yy - 0.02*y_span, yend=yy) +
                      annotate("segment", x=min(x1,x2), xend=max(x1,x2), y=yy, yend=yy)
                    # label stars
                    stars <- p_to_stars(shown$`p adj`[i])
                    label_txt <- if (stars == "ns") paste0("p=", signif(shown$`p adj`[i], 2)) else stars
                    p <- p + annotate("text", x=mean(c(x1,x2)), y=yy + 0.02*y_span, label=label_txt, fontface="bold", size = input$gc_metric_size/4)
                  }
                }
              }
            }
          }
        } else {
          # Kruskal-Wallis (no brackets by default)
          kw_result <- tryCatch({ kruskal.test(as.formula(paste(metric, "~ Group")), data = df) }, error=function(e) NULL)
          if (!is.null(kw_result)) {
            p_val <- kw_result$p.value; chi_val <- unname(kw_result$statistic)
            p <- p + annotate("text", x = mean(seq_len(n_groups)), y = y_max + 0.15*y_span,
                              label = sprintf("Kruskal-Wallis: chi^2 = %.2f, p = %s", chi_val, ifelse(p_val<0.001,"<0.001", signif(p_val,3))),
                              size = input$gc_metric_size/3.5, fontface = ifelse(p_val < 0.05, "bold", "plain"))
          }
        }
      }
    }
    
    # Add mean ± SEM, n insets if requested (robust to empty/constant data)
    if (isTRUE(input$gc_show_insets) && nrow(sm) > 0) {
      # Use summary stats to derive sensible y-limits
      y_low  <- min(sm$mean_val - sm$sem_val, na.rm = TRUE)
      y_high <- max(sm$mean_val + sm$sem_val, na.rm = TRUE)
      if (!is.finite(y_low) || !is.finite(y_high)) {
        y_low <- min(df[[metric]], na.rm = TRUE)
        y_high <- max(df[[metric]], na.rm = TRUE)
      }
      if (!is.finite(y_low) || !is.finite(y_high) || y_low == y_high) {
        y_low <- y_low - 0.5
        y_high <- y_high + 0.5
      }
      y_span <- max(1e-6, y_high - y_low)
      for (i in seq_len(nrow(sm))) {
        inset_text <- sprintf("%.3g ± %.3g\nn = %d", sm$mean_val[i], sm$sem_val[i], sm$n[i])
        if ((input$gc_inset_pos %||% "above") == "below") {
          p <- p + annotate("text", x = i, y = y_low - 0.12*y_span, label = inset_text,
                            size = input$gc_metric_size/4, hjust = 0.5, vjust = 1)
        } else {
          ypos <- if (input$gc_metric_geom %in% c("bar","dot")) sm$mean_val[i] + sm$sem_val[i] + 0.08*y_span else (y_high - 0.05*y_span)
          p <- p + annotate("text", x = i, y = ypos, label = inset_text,
                            size = input$gc_metric_size/4, hjust = 0.5, vjust = 0)
        }
      }
      # Expand limits to ensure insets are visible
      if ((input$gc_inset_pos %||% "above") == "below") {
        p <- p + expand_limits(y = y_low - 0.22*y_span)
      } else {
        p <- p + expand_limits(y = y_high + 0.22*y_span)
      }
    }
    
    p + labs(title = metric_title(metric), x=NULL, y=metric_label(metric))
  }) %>% bindCache(gcv$metrics, input$gc_metric_name, input$gc_metric_geom, input$gc_metric_size, input$gc_metric_points, input$gc_metric_stats, input$gc_stat_test, input$gc_show_posthoc, input$gc_show_brackets, input$gc_show_insets, input$gc_inset_pos)
  
  output$gc_heatmap_plot <- renderPlot({
    req(gcv$dts)
    build_hm <- function(dt, label) {
      time_vec <- dt$Time
      dnum <- coerce_numeric_dt(dt)
      mat <- as.matrix(dnum[, -1])
      valid <- apply(mat, 2, function(x) !all(is.na(x)))
      mat <- mat[, valid, drop=FALSE]
      if (ncol(mat) == 0) return(NULL)
      hm <- expand.grid(Time=time_vec, Cell=seq_len(ncol(mat)))
      hm$Value <- as.vector(mat); hm$Group <- label
      hm
    }
    all_hm <- purrr::imap(gcv$dts, ~build_hm(.x, .y)) |> purrr::compact() |> dplyr::bind_rows()
    ggplot(all_hm, aes(Time, Cell, fill=Value)) + geom_tile() + facet_wrap(~ Group, ncol=1, scales="free_y") +
      scale_fill_viridis_c(name=expression(Delta*"F/F"[0]), option="plasma", na.value="white") +
      labs(title="Group Heatmap (Processed)", x="Time (s)", y="Cell") + theme_classic(base_size=14)
  }) %>% bindCache(names(gcv$dts))
  
  output$gc_metrics_table <- renderDT({
    req(gcv$metrics)
    datatable(gcv$metrics, options=list(scrollX=TRUE, pageLength=25, deferRender=TRUE, processing=TRUE), rownames=FALSE)
  })
  
  # Quick metrics summary table on GC Load page
  output$gc_quick_metrics <- renderDT({
    req(gcv$metrics)
    cols <- c("Peak_dFF0","Response_Amplitude","AUC","Half_Width","Calcium_Entry_Rate",
              "Time_to_Peak","Time_to_25_Peak","Time_to_50_Peak","Time_to_75_Peak","Rise_Time","SNR")
    present <- intersect(cols, names(gcv$metrics))
    sm <- gcv$metrics |>
      dplyr::group_by(Group) |>
      dplyr::summarise(dplyr::across(all_of(present),
                                     list(Mean = ~mean(.x, na.rm = TRUE),
                                          SEM = ~stats::sd(.x, na.rm = TRUE)/sqrt(sum(is.finite(.x))),
                                          n = ~sum(is.finite(.x))), .names = "{.col}_{.fn}"), .groups = "drop")
    datatable(sm, options=list(scrollX=TRUE, pageLength=20), rownames=FALSE) |>
      formatRound(grep("_Mean$", names(sm), value=TRUE), 4) |>
      formatRound(grep("_SEM$", names(sm), value=TRUE), 4)
  })
  output$gc_dl_metrics_csv <- downloadHandler(
    filename = function() sprintf("gc_metrics_%s.csv", Sys.Date()),
    content  = function(file) data.table::fwrite(gcv$metrics, file)
  )
  
  # ---- Individual: Time course ----
  output$timecourse_plot <- renderPlot({
    req(rv$summary)
    p <- ggplot()
    if (isTRUE(input$tc_show_traces) && !is.null(rv$long) && nrow(rv$long) > 0) {
      alpha_traces <- 1 - (as.numeric(input$tc_trace_transparency) %||% 65) / 100
      p <- p + geom_line(data=rv$long, aes(x=Time, y=dFF0, group=interaction(Group, Cell), color=Group),
                         inherit.aes=FALSE, alpha=alpha_traces, linewidth=0.35)
    }
    p <- p + geom_ribbon(data=rv$summary,
                         aes(x=Time, ymin=mean_dFF0 - sem_dFF0, ymax=mean_dFF0 + sem_dFF0, fill=Group),
                         alpha=if (isTRUE(input$tc_show_ribbon)) 0.25 else 0, color=NA) +
      geom_line(data=rv$summary, aes(x=Time, y=mean_dFF0, color=Group), linewidth=input$tc_line_width)
    
    groups <- unique(rv$summary$Group)
    cols <- rv$colors
    if (!is.null(input$tc_line_color) && nzchar(input$tc_line_color)) {
      cols <- stats::setNames(rep(input$tc_line_color, length(groups)), groups)
    }
    if (!is.null(cols)) { p <- p + scale_color_manual(values=cols) + scale_fill_manual(values=cols) }
    
    p <- p + labs(title=input$tc_title, subtitle=input$tc_subtitle,
                  x=input$tc_x %||% "Time (s)", y=input$tc_y %||% "ΔF/F₀")
    base_theme <- switch(input$tc_theme, classic=theme_classic(), minimal=theme_minimal(),
                         light=theme_light(), dark=theme_dark())
    p <- p + base_theme + theme(
      plot.title = element_text(hjust=0.5, size=input$tc_title_size, face="bold", family=input$tc_font),
      plot.subtitle = element_text(hjust=0.5, size=max(8, input$tc_title_size - 4), family=input$tc_font),
      axis.title = element_text(size=input$tc_axis_title_size, face="bold", family=input$tc_font),
      axis.text = element_text(size=input$tc_axis_size, family=input$tc_font),
      legend.position = input$tc_legend_pos
    )
    if (isTRUE(input$tc_log_y)) p <- p + scale_y_log10()
    if (nzchar(input$tc_x_breaks)) {
      xb <- suppressWarnings(as.numeric(strsplit(input$tc_x_breaks, ",")[[1]])); xb <- xb[is.finite(xb)]
      if (length(xb) > 0) p <- p + scale_x_continuous(breaks=xb)
    }
    if (nzchar(input$tc_y_breaks)) {
      yb <- suppressWarnings(as.numeric(strsplit(input$tc_y_breaks, ",")[[1]])); yb <- yb[is.finite(yb)]
      if (length(yb) > 0) {
        lab_fun <- switch(input$tc_tick_format,
                          scientific = scales::label_scientific(digits=2),
                          percent    = scales::label_percent(accuracy=0.01),
                          scales::label_number(accuracy=0.01)
        )
        p <- p + scale_y_continuous(breaks=yb, labels=lab_fun)
      }
    }
    if (isTRUE(input$tc_grid_major) || isTRUE(input$tc_grid_minor)) {
      p <- p + theme(
        panel.grid.major = if (input$tc_grid_major) element_line(color="grey90", linewidth=0.3) else element_blank(),
        panel.grid.minor = if (input$tc_grid_minor) element_line(color="grey95", linewidth=0.2) else element_blank()
      )
    } else p <- p + theme(panel.grid = element_blank())
    if (isTRUE(input$tc_limits)) {
      if (!is.na(input$tc_xmin) && !is.na(input$tc_xmax)) p <- p + coord_cartesian(xlim=c(input$tc_xmin, input$tc_xmax))
      if (!is.na(input$tc_ymin) && !is.na(input$tc_ymax)) p <- p + coord_cartesian(ylim=c(input$tc_ymin, input$tc_ymax))
    }
    # Faceting removed as requested
    p
  }) %>% bindCache(rv$summary, input$tc_show_traces, input$tc_trace_transparency, input$tc_show_ribbon, input$tc_line_width, input$tc_title, input$tc_subtitle, input$tc_x, input$tc_y, input$tc_line_color, input$tc_legend_pos, input$tc_title_size, input$tc_axis_size, input$tc_axis_title_size)
  
  # Individual: average tables next to controls (already placed in UI)
  output$ind_avg_metrics <- renderDT({
    req(rv$metrics)
    cols <- c("Peak_dFF0","Response_Amplitude","AUC","Half_Width","Calcium_Entry_Rate",
              "Time_to_Peak","Time_to_25_Peak","Time_to_50_Peak","Time_to_75_Peak","Rise_Time","SNR")
    present <- intersect(cols, names(rv$metrics))
    sm <- lapply(present, function(cl) {
      vals <- rv$metrics[[cl]]
      c(Metric = cl,
        Mean = mean(vals, na.rm=TRUE),
        SEM  = stats::sd(vals, na.rm=TRUE)/sqrt(sum(is.finite(vals))),
        n    = sum(is.finite(vals)))
    })
    df <- as.data.frame(do.call(rbind, sm), stringsAsFactors = FALSE)
    df$Mean <- as.numeric(df$Mean); df$SEM <- as.numeric(df$SEM); df$n <- as.integer(df$n)
    datatable(df, options=list(dom='t', pageLength=20), rownames=FALSE) |>
      formatRound(c("Mean","SEM"), 4)
  })
  # Polished summary table next to time course
  output$tc_summary_table <- renderUI({
    req(rv$metrics)
    metric_cols <- c("Peak_dFF0","Response_Amplitude","AUC","Half_Width","Calcium_Entry_Rate",
                     "Time_to_Peak","Time_to_25_Peak","Time_to_50_Peak","Time_to_75_Peak","Rise_Time","SNR")
    present <- intersect(metric_cols, names(rv$metrics))
    if (length(present) == 0) return(NULL)
    nice_name <- function(cl){
      switch(cl,
             Peak_dFF0 = "Peak ΔF/F₀",
             Response_Amplitude = "Response Amplitude (ΔF/F₀)",
             Calcium_Entry_Rate = "Ca²⁺ Entry Rate",
             Time_to_Peak = "Time to Peak (s)",
             Time_to_25_Peak = "Time to 25% Peak (s)",
             Time_to_50_Peak = "Time to 50% Peak (s)",
             Time_to_75_Peak = "Time to 75% Peak (s)",
             Rise_Time = "Rise Time (s)",
             Half_Width = "Half Width (s)",
             FWHM = "FWHM (s)",
             AUC = "AUC",
             SNR = "SNR",
             cl)
    }
    rows <- lapply(present, function(cl){
      vals <- rv$metrics[[cl]]
      n <- sum(is.finite(vals))
      data.frame(
        Metric = nice_name(cl),
        Mean = mean(vals, na.rm = TRUE),
        SEM  = stats::sd(vals, na.rm = TRUE)/max(1, sqrt(n)),
        n    = n,
        check.names = FALSE
      )
    })
    df <- dplyr::bind_rows(rows)
    tb <- knitr::kable(df, format = "html", digits = 4, col.names = c("Metric","Mean","SEM","n")) |>
      kableExtra::kable_styling(full_width = TRUE, bootstrap_options = c("condensed", "striped", "hover"))
    htmltools::HTML(tb)
  })
  output$preproc_avg_metrics <- renderDT({
    req(rv$metrics)
    cols <- c("Peak_dFF0","Response_Amplitude","AUC","Half_Width","Calcium_Entry_Rate",
              "Time_to_Peak","Time_to_25_Peak","Time_to_50_Peak","Time_to_75_Peak","Rise_Time","SNR")
    present <- intersect(cols, names(rv$metrics))
    sm <- lapply(present, function(cl) {
      vals <- rv$metrics[[cl]]
      label <- if (cl == "Peak_dFF0") "Peak ΔF/F₀" else cl
      c(Metric = label,
        Mean = mean(vals, na.rm=TRUE),
        SEM  = stats::sd(vals, na.rm=TRUE)/sqrt(sum(is.finite(vals))),
        n    = sum(is.finite(vals)))
    })
    df <- as.data.frame(do.call(rbind, sm), stringsAsFactors = FALSE)
    df$Mean <- as.numeric(df$Mean); df$SEM <- as.numeric(df$SEM); df$n <- as.integer(df$n)
    datatable(df, options=list(dom='t', pageLength=20), rownames=FALSE) |>
      formatRound(c("Mean","SEM"), 4)
  })
  
  # ---- Individual: Metrics (no stats) ----
  output$metrics_plot <- renderPlot({
    req(rv$metrics)
    metric <- input$metric_name
    df <- dplyr::filter(rv$metrics, is.finite(.data[[metric]]))
    validate(need(nrow(df) > 0, "No finite values for this metric."))
    base <- theme_classic(base_size=input$metric_size) +
      theme(
        legend.position = "none",
        axis.title = element_text(size = input$metric_size, face = "bold"),
        axis.text  = element_text(size = input$metric_size * 0.9)
      )
    y_lab <- if (isTRUE(input$metric_auto_y)) metric_label(metric) else input$metric_y_label
    title_txt <- if (nzchar(input$metric_title)) input$metric_title else metric_title(metric)
    
    if (input$metric_geom == "cellbar") {
      df2 <- df
      if (isTRUE(input$metric_sort_cells)) {
        df2 <- df2 |>
          dplyr::group_by(Group) |>
          dplyr::arrange(.data[[metric]], .by_group = TRUE) |>
          dplyr::mutate(Cell_Idx = dplyr::row_number()) |>
          dplyr::ungroup()
      } else df2$Cell_Idx <- seq_len(nrow(df2))
      
      # per-group inset stats + left-side placement so it doesn't overlap tallest bars
      stats_g <- df2 |>
        dplyr::group_by(Group) |>
        dplyr::summarise(
          mean_val = mean(.data[[metric]], na.rm = TRUE),
          sem_val  = stats::sd(.data[[metric]], na.rm = TRUE)/sqrt(dplyr::n()),
          n        = dplyr::n(),
          xpos     = 1.5,                                        # << left side
          ypos     = max(.data[[metric]], na.rm = TRUE) * 0.98,  # top
          .groups = "drop"
        ) |>
        dplyr::mutate(label = sprintf("Mean ± SEM: %.3g ± %.3g\nn = %d", mean_val, sem_val, n))
      lab_size_val <- max(3, input$metric_size * 0.18) * input$metric_inset_scale
      
      p <- ggplot(df2, aes(x = Cell_Idx, y = .data[[metric]], fill = Group)) +
        geom_col(width = 0.85, alpha = 0.9, color = "black", linewidth = 0.2) +
        facet_wrap(~ Group, scales = "free_x", ncol = 1, strip.position = "top") +
        labs(x = "Cell number", y = y_lab, title = title_txt) + base +
        scale_x_continuous(breaks = function(lims) seq(1, floor(lims[2]), by = 1)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = max(7, input$metric_size * 0.6)))
      
      cols <- rv$colors; if (!is.null(cols)) p <- p + scale_fill_manual(values = cols)
      
      p <- p +
        geom_label(data = stats_g,
                   aes(x = xpos, y = ypos, label = label),
                   inherit.aes = FALSE, size = lab_size_val,
                   label.size = 0.15, alpha = 0.9, hjust = 0) +  # left-anchored
        coord_cartesian(clip = "off") +
        theme(plot.margin = margin(10, 25, 10, 10))
      return(p)
    }
    
    # Other geoms (keep inset above bars)
    stats_simple <- df |>
      dplyr::group_by(Group) |>
      dplyr::summarise(
        mean_val = mean(.data[[metric]], na.rm = TRUE),
        sem_val  = stats::sd(.data[[metric]], na.rm = TRUE)/sqrt(dplyr::n()),
        n        = dplyr::n(),
        ymax     = max(.data[[metric]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(label = sprintf("μ±SEM: %.3g ± %.3g\nn=%d", mean_val, sem_val, n))
    
    if (input$metric_geom == "bar") {
      sm <- df |>
        dplyr::group_by(Group) |>
        dplyr::summarise(mean_val = mean(.data[[metric]], na.rm = TRUE),
                         sem_val = stats::sd(.data[[metric]], na.rm = TRUE) / sqrt(dplyr::n()),
                         .groups = "drop")
      p <- ggplot(sm, aes(Group, mean_val, fill = Group)) +
        geom_col(alpha = 0.85, width = 0.6, color = "black") +
        geom_errorbar(aes(ymin = mean_val - sem_val, ymax = mean_val + sem_val), width = 0.2) +
        labs(x = NULL, y = y_lab, title = title_txt) + base
    } else if (input$metric_geom == "box") {
      p <- ggplot(df, aes(Group, .data[[metric]], fill = Group)) +
        geom_boxplot(alpha = 0.8, outlier.alpha = 0.4) +
        labs(x = NULL, y = y_lab, title = title_txt) + base
    } else if (input$metric_geom == "violin") {
      p <- ggplot(df, aes(Group, .data[[metric]], fill = Group)) +
        geom_violin(trim = FALSE, alpha = 0.8) +
        geom_boxplot(width = 0.12, alpha = 0.9, outlier.alpha = 0) +
        labs(x = NULL, y = y_lab, title = title_txt) + base
    } else { # dot
      sm <- df |>
        dplyr::group_by(Group) |>
        dplyr::summarise(mean_val = mean(.data[[metric]], na.rm = TRUE),
                         sem_val = stats::sd(.data[[metric]], na.rm = TRUE) / sqrt(dplyr::n()),
                         .groups = "drop")
      p <- ggplot(sm, aes(Group, mean_val, color = Group)) +
        geom_point(size = 3) +
        geom_errorbar(aes(ymin = mean_val - sem_val, ymax = mean_val + sem_val), width = 0.12) +
        labs(x = NULL, y = y_lab, title = title_txt) + base
    }
    cols <- rv$colors; if (!is.null(cols)) { if (input$metric_geom == "dot") p <- p + scale_color_manual(values = cols) else p <- p + scale_fill_manual(values = cols) }
    p <- p +
      geom_label(data = stats_simple,
                 aes(x = Group, y = ymax * 1.02, label = label),
                 inherit.aes = FALSE, size = max(3, input$metric_size * 0.18),
                 label.size = 0.15, alpha = 0.9) +
      coord_cartesian(clip = "off") +
      theme(plot.margin = margin(10, 25, 10, 10))
    p
  }) %>% bindCache(rv$metrics, input$metric_name, input$metric_size, input$metric_auto_y, input$metric_sort_cells, input$metric_title, input$metric_inset_scale)
  
  # Heatmap
  output$heatmap_plot <- renderPlot({
    req(rv$dts)
    build_hm <- function(dt, label) {
      time_vec <- dt$Time
      dnum <- coerce_numeric_dt(dt)
      mat <- as.matrix(dnum[, -1])
      valid <- apply(mat, 2, function(x) !all(is.na(x)))
      mat <- mat[, valid, drop=FALSE]
      if (ncol(mat) == 0) return(NULL)
      ord <- seq_len(ncol(mat))
      if (input$hm_sort == "tpeak") {
        tpk <- apply(mat, 2, function(x) if (all(is.na(x))) Inf else which.max(x))
        ord <- order(tpk)
      } else if (input$hm_sort == "amp") {
        amp <- apply(mat, 2, function(x) if (all(is.na(x))) -Inf else max(x, na.rm = TRUE))
        ord <- order(amp, decreasing = TRUE)
      }
      mat <- mat[, ord, drop=FALSE]
      if (isTRUE(input$hm_norm_per_cell)) {
        mat <- apply(mat, 2, function(x) {
          xr <- range(x, na.rm = TRUE)
          if (xr[2] > xr[1]) (x - xr[1])/(xr[2]-xr[1]) else rep(0.5, length(x))
        })
      }
      hm <- expand.grid(Time = time_vec, Cell = seq_len(ncol(mat)))
      hm$Value <- as.vector(mat); hm$Group <- label
      hm
    }
    all_hm <- purrr::imap(rv$dts, ~build_hm(.x, .y)) |> purrr::compact() |> dplyr::bind_rows()
    validate(need(nrow(all_hm) > 0, "No valid data for heatmap"))
    ggplot(all_hm, aes(Time, Cell, fill = Value)) +
      geom_tile() +
      facet_wrap(~ Group, ncol = 1, scales = "free_y") +
      scale_fill_viridis_c(name = expression(Delta*"F/F"[0]), option = input$hm_palette, na.value = "white") +
      guides(fill = guide_colorbar(frame.colour = "black", frame.linewidth = 0.3)) +
      labs(title = "Population Heatmap", x = "Time (s)", y = "Cell") +
      theme_classic(base_size = 14) +
      theme(
        legend.text  = element_text(size = input$hm_legend_text_size),
        legend.title = element_text(size = max(6, input$hm_legend_text_size + 2))
      )
  }) %>% bindCache(names(rv$dts), lapply(rv$dts, dim), input$hm_norm_per_cell, input$hm_sort, input$hm_palette, input$hm_legend_text_size)
  
  # Tables
  output$metrics_table <- renderDT({
    req(rv$metrics)
    datatable(rv$metrics,
              options = list(scrollX = TRUE, pageLength = 25, deferRender = TRUE, processing = TRUE),
              rownames = FALSE) |>
      formatRound(c("Peak_dFF0","Response_Amplitude","Calcium_Entry_Rate","AUC","Half_Width","SNR","Baseline_SD"), 3)
  })
  output$summary_table <- renderDT({
    req(rv$summary)
    datatable(rv$summary,
              options = list(scrollX = TRUE, pageLength = 25, deferRender = TRUE, processing = TRUE),
              rownames = FALSE) |>
      formatRound(c("mean_dFF0","sem_dFF0","sd_dFF0"), 5)
  })
  
  # Export
  output$export_info <- renderText({
    paste0("Format: ", toupper(input$exp_fmt), "\n",
           "Size: ", input$exp_w, " x ", input$exp_h, " in\n",
           "DPI: ", input$exp_dpi)
  })
  output$dl_metrics_csv <- downloadHandler(
    filename = function() sprintf("metrics_%s.csv", Sys.Date()),
    content  = function(file) data.table::fwrite(rv$metrics, file)
  )
  
  # ---- Metric Guide server ----
  output$guide_cell_picker <- renderUI({
    req(rv$dts)
    # Build choices as Group:Cell column names from first dataset by default
    group_choices <- names(rv$dts)
    if (length(group_choices) == 0) return(NULL)
    first_group <- group_choices[1]
    dt <- rv$dts[[first_group]]
    cols <- setdiff(names(dt), "Time")
    # Exclude non-signal columns (all-NA or explicitly named 'Label')
    cols <- cols[vapply(cols, function(nm) any(is.finite(dt[[nm]])), logical(1))]
    cols <- setdiff(cols, c("Label", "label", "Labels", "labels"))
    if (length(cols) == 0) return(NULL)
    selected <- if (!is.null(input$guide_cell) && input$guide_cell %in% cols) input$guide_cell else cols[1]
    selectInput("guide_cell", "Example cell", choices = setNames(cols, paste0(first_group, ": ", cols)), selected = selected)
  })
  
  output$guide_timepoint_ui <- renderUI({
    req(input$guide_metric == "dFF0_calc")
    req(rv$dts)
    group <- names(rv$dts)[1]
    dt <- rv$dts[[group]]
    req(nrow(dt) > 0)
    max_timepoint <- nrow(dt)
    selected <- min(max_timepoint %/% 2, max_timepoint)
    sliderInput(
      inputId = "guide_timepoint",
      label   = "Timepoint",
      min     = 1,
      max     = max_timepoint,
      value   = selected,
      step    = 1,
      ticks   = FALSE,
      animate = animationOptions(interval = 200, loop = FALSE)
    )
  })
  
  output$guide_math_explanation <- renderUI({
    req(rv$dts)
    group <- names(rv$dts)[1]
    dt <- rv$dts[[group]]
    
    cell <- input$guide_cell
    req(cell)
    
    metric <- input$guide_metric
    req(metric)
    
    # Get the raw signal data (which may have been processed)
    signal <- dt[[cell]]
    time <- dt$Time
    req(length(signal) > 0, length(time) > 0)
    
    if (metric == "dFF0_calc") {
      req(input$guide_timepoint)
      timepoint <- input$guide_timepoint
      
      b_len <- min(20, length(signal))
      baseline_from_signal <- mean(signal[seq_len(b_len)], na.rm = TRUE)
      
      # Check if data appears to be already normalized
      if (abs(baseline_from_signal) < 0.1) {
        dFF0_val <- signal[timepoint]
        return(tagList(
          h3("ΔF/F₀ Value"),
          p("The loaded data appears to be already normalized (i.e., it is already in ΔF/F₀ format). This is assumed because the baseline value is very close to zero."),
          hr(),
          div(class = "alert alert-success", style = "margin: 10px 0;",
              h4(paste("Value at Timepoint:", timepoint, "(", round(time[timepoint], 2), "s)"), style = "margin-top: 0;"),
              p(strong("ΔF/F₀ = "), code(round(dFF0_val, 4))),
              p(em("No calculation is needed as the signal is already normalized."))
          )
        ))
      } else {
        # Data appears to be raw, so show the calculation
        F0_val <- baseline_from_signal
        raw_F <- signal[timepoint]
        dFF0_val <- if (F0_val != 0) (raw_F - F0_val) / F0_val else Inf
        
        return(tagList(
          h3("ΔF/F₀ Calculation at a Specific Timepoint"),
          p(strong("Definition:"), " The change in fluorescence (ΔF) at a given timepoint relative to the baseline fluorescence (F₀). It normalizes the signal, making it comparable across different cells or experiments."),
          hr(),
          div(class = "alert alert-info", style = "margin: 10px 0;",
              h4(paste("Calculation at Timepoint:", timepoint, "(", round(time[timepoint], 2), "s)"), style = "margin-top: 0;"),
              p("1. ", strong("Baseline Fluorescence (F₀): "), "The average of the first", b_len, "raw fluorescence values."),
              p(strong("   F₀ = "), code(round(F0_val, 4))),
              p("2. ", strong("Raw Fluorescence (F) at selected timepoint: ")),
              p(strong("   F = "), code(round(raw_F, 4))),
              p("3. ", strong("ΔF/F₀ Calculation: ")),
              withMathJax(
                if (is.finite(dFF0_val)) {
                  sprintf("\\[ \\frac{F - F_0}{F_0} = \\frac{%.4f - %.4f}{%.4f} = \\mathbf{%.4f} \\]", raw_F, F0_val, F0_val, dFF0_val)
                } else {
                  sprintf("\\[ \\frac{F - F_0}{F_0} = \\frac{%.4f - %.4f}{%.4f} = \\text{Undefined (division by zero)} \\]", raw_F, F0_val, F0_val)
                }
              )
          )
        ))
      }
    }
    
    # --- Logic for other metrics ---
    # Determine normalization status for other metrics
    baseline_raw <- mean(signal[1:min(20, length(signal))], na.rm = TRUE)
    if (abs(baseline_raw) < 0.1) {
      dff_signal <- signal; F0_val <- 0; baseline <- 0
    } else {
      F0_val <- baseline_raw
      dff_signal <- if(F0_val != 0) (signal - F0_val) / F0_val else signal
      baseline <- 0
    }
    
    peak_val <- max(dff_signal, na.rm = TRUE)
    peak_idx <- which.max(dff_signal)
    peak_time <- time[peak_idx]
    amplitude <- peak_val - baseline
    
    if (metric == "Peak_dFF0") {
      tagList(
        h3("Peak ΔF/F₀"),
        p(strong("Definition:"), "Maximum signal change relative to baseline"),
        div(class = "alert alert-info", style = "margin: 10px 0;",
            h4("Your Data Results:", style = "margin-top: 0;"),
            p(strong("Peak ΔF/F₀:"), round(peak_val, 5)),
            p(strong("Peak Time:"), round(peak_time, 3), "seconds")
        )
      )
    } else {
      tagList(
        h3(metric_title(metric)),
        p("Detailed explanation coming soon.")
      )
    }
  })
  
  output$guide_plot <- renderPlot({
    req(rv$dts, input$guide_metric)
    group <- names(rv$dts)[1]
    dt <- rv$dts[[group]]
    req(ncol(dt) > 1)
    candidates <- setdiff(names(dt), "Time")
    candidates <- candidates[vapply(candidates, function(nm) any(is.finite(dt[[nm]])), logical(1))]
    candidates <- setdiff(candidates, c("Label", "label", "Labels", "labels"))
    req(length(candidates) >= 1)
    cell_col <- if (!is.null(input$guide_cell) && input$guide_cell %in% candidates) input$guide_cell else candidates[1]
    
    time <- dt$Time
    signal <- suppressWarnings(as.numeric(dt[[cell_col]]))
    
    b_len <- min(20, length(signal))
    baseline_raw <- mean(signal[seq_len(b_len)], na.rm = TRUE)
    
    y <- if (abs(baseline_raw) < 0.1 || baseline_raw == 0) {
      signal
    } else {
      (signal - baseline_raw) / baseline_raw
    }
    
    df <- data.frame(Time = time, y = y)
    metric <- input$guide_metric
    
    p <- ggplot(df, aes(Time, y)) +
      geom_line(linewidth = 0.9, color = "#2c7fb8") +
      theme_classic(base_size = 14) +
      labs(x = "Time (s)", y = "ΔF/F₀")
    
    if (metric == "dFF0_calc") {
      req(input$guide_timepoint)
      timepoint_idx <- input$guide_timepoint
      time_at_point <- df$Time[timepoint_idx]
      dFF0_at_point <- df$y[timepoint_idx]
      
      if (abs(baseline_raw) < 0.1) {
        # Data is already normalized
        p <- p +
          labs(title = "Visualizing ΔF/F₀ Value",
               subtitle = "Data appears to be already normalized. The slider shows the value at each timepoint.") +
          geom_vline(xintercept = time_at_point, linetype = "dotted", color = "orange", linewidth = 1.2) +
          geom_point(aes(x = time_at_point, y = dFF0_at_point), color = "orange", size = 4) +
          annotate("label", x = time_at_point, y = dFF0_at_point,
                   label = sprintf("At t = %.2fs:\nΔF/F₀ = %.3g", time_at_point, dFF0_at_point),
                   hjust = if(time_at_point > mean(range(df$Time))) 1.1 else -0.1,
                   vjust = if(dFF0_at_point > mean(range(df$y, na.rm=TRUE))) 1.2 else -0.2,
                   fill = "#E3F2FD", color = "#0D47A1")
      } else {
        # Data is raw, show calculation
        raw_F_at_point <- signal[timepoint_idx]
        F0_val <- baseline_raw
        
        p <- p +
          labs(title = "Visualizing ΔF/F₀ Calculation",
               subtitle = "Use the slider to see how ΔF/F₀ (blue trace) is calculated from raw values.") +
          annotate("rect", xmin = df$Time[1], xmax = df$Time[b_len], ymin = -Inf, ymax = Inf, fill = "#17a2b8", alpha = 0.07) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "firebrick") +
          geom_vline(xintercept = time_at_point, linetype = "dotted", color = "orange", linewidth = 1.2) +
          geom_point(aes(x = time_at_point, y = dFF0_at_point), color = "orange", size = 4) +
          annotate("label", x = time_at_point, y = dFF0_at_point,
                   label = sprintf("At t = %.2fs:\n  F = %.4g\n  F₀ = %.4g\n  ΔF/F₀ = %.3g", 
                                   time_at_point, raw_F_at_point, F0_val, dFF0_at_point),
                   hjust = if(time_at_point > mean(range(df$Time))) 1.1 else -0.1,
                   vjust = if(dFF0_at_point > mean(range(df$y, na.rm=TRUE))) 1.2 else -0.2,
                   fill = "#FFF3E0", color = "#E65100", label.padding = unit(0.4, "lines")) +
          annotate("text", x = df$Time[b_len / 2], y = max(df$y, na.rm=TRUE)*0.9, 
                   label="Baseline Region\n(for F₀ calculation)", color="grey30", size=3.5) +
          annotate("text", x = max(df$Time), y = 0, 
                   label=sprintf("F₀ = %.4g", F0_val), color="firebrick", hjust=1, vjust=-0.5, size=3.5)
      }
    } else {
      # Fallback plotting logic for other metrics
      p <- p + labs(title = paste0("Metric Guide: ", metric_title(metric)))
      # Additional annotations for other metrics would go here...
    }
    
    p
  })
  
  output$guide_explanation <- renderUI({
    metric <- input$guide_metric %||% "Peak_dFF0"
    txt <- switch(metric,
                  Peak_dFF0 = "Peak ΔF/F₀: maximum of the trace.",
                  Response_Amplitude = "Response amplitude: peak minus baseline (first 10% of frames).",
                  AUC = "Area under the curve (trapezoidal rule).",
                  Time_to_Peak = "Time to Peak: time at which the signal first reaches its maximum.",
                  Rise_Time = "Rise Time: time between 10% and 90% of the response amplitude.",
                  FWHM = "Full Width at Half Maximum: duration between the two crossings at 50% of (peak - baseline).",
                  Half_Width = "Half Width (HWHM): Duration of the calcium transient measured at 50% of peak amplitude above baseline. Calculated as half of the Full Width at Half Maximum (FWHM) - the time between when the signal rises to 50% and falls back to 50% of peak, divided by 2. Smaller values indicate faster, more transient responses.",
                  SNR = "Signal-to-noise ratio: response amplitude divided by baseline SD.",
                  "Metric description")
    htmltools::HTML(paste0("<div class='small-help' style='margin-top:8px;'>", txt, "</div>"))
  })
  
  output$guide_equations <- renderUI({
    metric <- input$guide_metric %||% "Peak_dFF0"
    eq <- switch(metric,
                 Peak_dFF0 = "\\[\\text{Peak } \\Delta F/F_0 = \\max_t\\; y(t), \\quad y(t)=\\frac{F(t)-F_0}{F_0}\\]",
                 Response_Amplitude = "\\[\\text{Amplitude} = y_{peak} - 0 = y_{peak} \\text{ (when baseline is } 0)\\]",
                 AUC = "\\[\\mathrm{AUC} \\approx \\sum_i \\tfrac{y_{i+1}+y_i}{2}\\,\\Delta t_i\\]",
                 Time_to_Peak = "\\[t_{peak} = \\operatorname{argmax}_t\\; y(t)\\]",
                 Time_to_25_Peak = "\\[t_{25}: y(t_{25}) = F_0 + 0.25(F_{peak}-F_0)\\]",
                 Time_to_50_Peak = "\\[t_{50}: y(t_{50}) = F_0 + 0.50(F_{peak}-F_0)\\]",
                 Time_to_75_Peak = "\\[t_{75}: y(t_{75}) = F_0 + 0.75(F_{peak}-F_0)\\]",
                 Rise_Time = "\\[t_{rise} = t_{90} - t_{10}\\]",
                 Calcium_Entry_Rate = "\\[\\text{Ca}^{2+} \\text{ entry rate} = \\dfrac{F_{peak}-F_0}{t_{90}-t_{10}}\\]",
                 Half_Width = "\\[\\mathrm{HWHM} = \\frac{1}{2} \\times \\mathrm{FWHM} = \\frac{1}{2} \\times (t_{\\text{right}} - t_{\\text{left}})\\]",
                 SNR = "\\[\\mathrm{SNR} = \\dfrac{F_{peak}-F_0}{\\sigma_{baseline}}\\]",
                 "")
    if (!nzchar(eq)) return(NULL)
    htmltools::HTML(paste0("<div style='margin-top:8px;'><span><b>Equation</b></span><br/>", eq, "</div>"))
  })
  output$dl_summary_csv <- downloadHandler(
    filename = function() sprintf("timecourse_summary_%s.csv", Sys.Date()),
    content  = function(file) data.table::fwrite(rv$summary, file)
  )
  # Export tab: processed data (wide) per selected file
  output$dl_processed_wide_exp <- downloadHandler(
    filename = function() sprintf("processed_%s_%s.csv", input$exp_dl_group %||% "data", Sys.Date()),
    content = function(file) {
      req(rv$dts, input$exp_dl_group)
      dt <- rv$dts[[ input$exp_dl_group ]]
      req(!is.null(dt), ncol(dt) >= 2)
      data.table::fwrite(dt, file)
    }
  )
  # Export processed data in the original wide (Time + cells) format, per uploaded file
  output$dl_processed_wide <- downloadHandler(
    filename = function() sprintf("processed_%s_%s.csv", input$pp_dl_group %||% "data", Sys.Date()),
    content = function(file) {
      req(rv$dts, input$pp_dl_group)
      grp <- input$pp_dl_group
      dt <- rv$dts[[grp]]
      req(!is.null(dt), ncol(dt) >= 2)
      data.table::fwrite(dt, file)
    }
  )
  output$dl_timecourse_plot <- downloadHandler(
    filename = function() sprintf("timecourse_%s.%s", Sys.Date(), input$exp_fmt),
    content = function(file) {
      req(rv$summary)
      p <- ggplot(rv$summary, aes(Time, mean_dFF0, color = Group, fill = Group)) +
        geom_ribbon(aes(ymin = mean_dFF0 - sem_dFF0, ymax = mean_dFF0 + sem_dFF0), alpha = 0.3, color = NA) +
        geom_line(linewidth = 1.4) + theme_classic(base_size = 14) + theme(legend.position = "none") +
        labs(title = "Calcium Time Course (Mean ± SEM)", x = "Time (s)", y = expression(Delta*"F/F"[0]))
      ggplot2::ggsave(file, plot = p, width = input$exp_w, height = input$exp_h, dpi = input$exp_dpi)
    }
  )
  # Local downloads (Time Course & Heatmap)
  output$dl_timecourse_plot_local <- downloadHandler(
    filename = function() sprintf("timecourse_%s.%s", Sys.Date(), input$tc_dl_fmt),
    content = function(file) {
      req(rv$summary)
      # Rebuild current time course with user-selected formatting
      p <- ggplot()
      if (isTRUE(input$tc_show_traces) && !is.null(rv$long) && nrow(rv$long) > 0) {
        alpha_traces <- 1 - (as.numeric(input$tc_trace_transparency) %||% 65) / 100
        p <- p + geom_line(data=rv$long, aes(x=Time, y=dFF0, group=interaction(Group, Cell), color=Group),
                           inherit.aes=FALSE, alpha=alpha_traces, linewidth=0.35)
      }
      p <- p + geom_ribbon(data=rv$summary,
                           aes(x=Time, ymin=mean_dFF0 - sem_dFF0, ymax=mean_dFF0 + sem_dFF0, fill=Group),
                           alpha=if (isTRUE(input$tc_show_ribbon)) 0.25 else 0, color=NA) +
        geom_line(data=rv$summary, aes(x=Time, y=mean_dFF0, color=Group), linewidth=input$tc_line_width)
      groups <- unique(rv$summary$Group)
      cols <- rv$colors
      if (!is.null(input$tc_line_color) && nzchar(input$tc_line_color)) {
        cols <- stats::setNames(rep(input$tc_line_color, length(groups)), groups)
      }
      if (!is.null(cols)) { p <- p + scale_color_manual(values=cols) + scale_fill_manual(values=cols) }
      p <- p + labs(title=input$tc_title, subtitle=input$tc_subtitle,
                    x=input$tc_x %||% "Time (s)", y=input$tc_y %||% "ΔF/F₀")
      base_theme <- switch(input$tc_theme, classic=theme_classic(), minimal=theme_minimal(),
                           light=theme_light(), dark=theme_dark())
      p <- p + base_theme + theme(
        plot.title = element_text(hjust=0.5, size=input$tc_title_size, face="bold", family=input$tc_font),
        plot.subtitle = element_text(hjust=0.5, size=max(8, input$tc_title_size - 4), family=input$tc_font),
        axis.title = element_text(size=input$tc_axis_title_size, face="bold", family=input$tc_font),
        axis.text = element_text(size=input$tc_axis_size, family=input$tc_font),
        legend.position = input$tc_legend_pos
      )
      if (isTRUE(input$tc_log_y)) p <- p + scale_y_log10()
      ggplot2::ggsave(file, plot = p, width = input$tc_dl_w, height = input$tc_dl_h, dpi = input$tc_dl_dpi)
    }
  )
  output$dl_heatmap_plot_local <- downloadHandler(
    filename = function() sprintf("heatmap_%s.%s", Sys.Date(), input$hm_dl_fmt),
    content = function(file) {
      req(rv$dts)
      p <- isolate({
        build_hm <- function(dt, label) {
          time_vec <- dt$Time
          dnum <- coerce_numeric_dt(dt)
          mat <- as.matrix(dnum[, -1])
          valid <- apply(mat, 2, function(x) !all(is.na(x)))
          mat <- mat[, valid, drop = FALSE]
          if (ncol(mat) == 0) return(NULL)
          ord <- seq_len(ncol(mat))
          tpk <- apply(mat, 2, function(x) if (all(is.na(x))) Inf else which.max(x))
          ord <- order(tpk)
          mat <- mat[, ord, drop = FALSE]
          hm <- expand.grid(Time = time_vec, Cell = seq_len(ncol(mat)))
          hm$Value <- as.vector(mat); hm$Group <- label
          hm
        }
        all_hm <- purrr::imap(rv$dts, ~build_hm(.x, .y)) |> purrr::compact() |> dplyr::bind_rows()
        ggplot(all_hm, aes(Time, Cell, fill = Value)) +
          geom_tile() +
          facet_wrap(~ Group, ncol = 1, scales = "free_y") +
          scale_fill_viridis_c(name = expression(Delta*"F/F"[0]), option = input$hm_palette, na.value = "white") +
          guides(fill = guide_colorbar(frame.colour = "black", frame.linewidth = 0.3)) +
          labs(title = "Population Heatmap", x = "Time (s)", y = "Cell") +
          theme_classic(base_size = 14) +
          theme(
            legend.text  = element_text(size = input$hm_legend_text_size),
            legend.title = element_text(size = max(6, input$hm_legend_text_size + 2))
          )
      })
      ggplot2::ggsave(file, plot = p, width = input$hm_dl_w, height = input$hm_dl_h, dpi = input$hm_dl_dpi)
    }
  )
  output$dl_heatmap_plot <- downloadHandler(
    filename = function() sprintf("heatmap_%s.%s", Sys.Date(), input$exp_fmt),
    content = function(file) {
      req(rv$dts)
      p <- isolate({
        build_hm <- function(dt, label) {
          time_vec <- dt$Time
          mat <- as.matrix(dt[, -1])
          valid <- apply(mat, 2, function(x) !all(is.na(x)))
          mat <- mat[, valid, drop = FALSE]
          if (ncol(mat) == 0) return(NULL)
          ord <- seq_len(ncol(mat))
          tpk <- apply(mat, 2, function(x) if (all(is.na(x))) Inf else which.max(x))
          ord <- order(tpk)
          mat <- mat[, ord, drop = FALSE]
          hm <- expand.grid(Time = time_vec, Cell = seq_len(ncol(mat)))
          hm$Value <- as.vector(mat); hm$Group <- label
          hm
        }
        all_hm <- purrr::imap(rv$dts, ~build_hm(.x, .y)) |> purrr::compact() |> dplyr::bind_rows()
        ggplot(all_hm, aes(Time, Cell, fill = Value)) +
          geom_tile() +
          facet_wrap(~ Group, ncol = 1, scales = "free_y") +
          scale_fill_viridis_c(name = expression(Delta*"F/F"[0]), option = input$hm_palette, na.value = "white") +
          guides(fill = guide_colorbar(frame.colour = "black", frame.linewidth = 0.3)) +
          labs(title = "Population Heatmap", x = "Time (s)", y = "Cell") +
          theme_classic(base_size = 14) +
          theme(
            legend.text  = element_text(size = input$hm_legend_text_size),
            legend.title = element_text(size = max(6, input$hm_legend_text_size + 2))
          )
      })
      ggplot2::ggsave(file, plot = p, width = input$exp_w, height = input$exp_h, dpi = input$exp_dpi)
    }
  )
  output$dl_metrics_plot <- downloadHandler(
    filename = function() sprintf("metric_%s_%s.%s", input$metric_name, Sys.Date(), input$exp_fmt),
    content = function(file) {
      req(rv$metrics)
      metric <- input$metric_name
      df <- dplyr::filter(rv$metrics, is.finite(.data[[metric]]))
      base <- theme_classic(base_size = input$metric_size) + theme(legend.position = "none")
      y_lab <- if (isTRUE(input$metric_auto_y)) metric_label(metric) else input$metric_y_label
      title_txt <- if (nzchar(input$metric_title)) input$metric_title else metric_title(metric)
      
      if (input$metric_geom == "cellbar") {
        df2 <- df
        if (isTRUE(input$metric_sort_cells)) {
          df2 <- df2 |>
            dplyr::group_by(Group) |>
            dplyr::arrange(.data[[metric]], .by_group = TRUE) |>
            dplyr::mutate(Cell_Idx = dplyr::row_number()) |>
            dplyr::ungroup()
        } else df2$Cell_Idx <- seq_len(nrow(df2))
        
        stats_g <- df2 |>
          dplyr::group_by(Group) |>
          dplyr::summarise(
            mean_val = mean(.data[[metric]], na.rm = TRUE),
            sem_val  = stats::sd(.data[[metric]], na.rm = TRUE)/sqrt(dplyr::n()),
            n        = dplyr::n(),
            xpos     = 1.5,                                        # left side
            ypos     = max(.data[[metric]], na.rm = TRUE) * 0.98,
            .groups = "drop"
          ) |>
          dplyr::mutate(label = sprintf("Mean ± SEM: %.3g ± %.3g\nn = %d", mean_val, sem_val, n))
        
        p <- ggplot(df2, aes(x = Cell_Idx, y = .data[[metric]], fill = Group)) +
          geom_col(width = 0.85, alpha = 0.9, color = "black", linewidth = 0.2) +
          facet_wrap(~ Group, scales = "free_x", ncol = 1) +
          labs(x = "Cell number", y = y_lab, title = title_txt) + base +
          scale_x_continuous(breaks = function(lims) seq(1, floor(lims[2]), by = 1)) +
          theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = max(7, input$metric_size * 0.6)))
        cols <- rv$colors; if (!is.null(cols)) p <- p + scale_fill_manual(values = cols)
        p <- p + geom_label(data = stats_g, aes(x = xpos, y = ypos, label = label),
                            inherit.aes = FALSE, size = max(3, input$metric_size * 0.18),
                            label.size = 0.15, alpha = 0.9, hjust = 0) +
          coord_cartesian(clip = "off") +
          theme(plot.margin = margin(10, 25, 10, 10))
        ggplot2::ggsave(file, plot = p, width = input$exp_w, height = input$exp_h, dpi = input$exp_dpi)
        return(invisible())
      }
      
      stats_simple <- df |>
        dplyr::group_by(Group) |>
        dplyr::summarise(
          mean_val = mean(.data[[metric]], na.rm = TRUE),
          sem_val  = stats::sd(.data[[metric]], na.rm = TRUE)/sqrt(dplyr::n()),
          n        = dplyr::n(),
          ymax     = max(.data[[metric]], na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::mutate(label = sprintf("μ±SEM: %.3g ± %.3g\nn=%d", mean_val, sem_val, n))
      
      if (input$metric_geom == "bar") {
        sm <- df |>
          dplyr::group_by(Group) |>
          dplyr::summarise(mean_val = mean(.data[[metric]], na.rm = TRUE),
                           sem_val = stats::sd(.data[[metric]], na.rm = TRUE) / sqrt(dplyr::n()),
                           .groups = "drop")
        p <- ggplot(sm, aes(Group, mean_val, fill = Group)) +
          geom_col(alpha = 0.85, width = 0.6, color = "black") +
          geom_errorbar(aes(ymin = mean_val - sem_val, ymax = mean_val + sem_val), width = 0.2) +
          labs(x = NULL, y = y_lab, title = title_txt) + base
      } else if (input$metric_geom == "box") {
        p <- ggplot(df, aes(Group, .data[[metric]], fill = Group)) +
          geom_boxplot(alpha = 0.8, outlier.alpha = 0.4) +
          labs(x = NULL, y = y_lab, title = title_txt) + base
      } else if (input$metric_geom == "violin") {
        p <- ggplot(df, aes(Group, .data[[metric]], fill = Group)) +
          geom_violin(trim = FALSE, alpha = 0.8) +
          geom_boxplot(width = 0.12, alpha = 0.9, outlier.alpha = 0) +
          labs(x = NULL, y = y_lab, title = title_txt) + base
      } else {
        sm <- df |>
          dplyr::group_by(Group) |>
          dplyr::summarise(mean_val = mean(.data[[metric]], na.rm = TRUE),
                           sem_val = stats::sd(.data[[metric]], na.rm = TRUE) / sqrt(dplyr::n()),
                           .groups = "drop")
        p <- ggplot(sm, aes(Group, mean_val, color = Group)) +
          geom_point(size = 3) +
          geom_errorbar(aes(ymin = mean_val - sem_val, ymax = mean_val + sem_val), width = 0.12) +
          labs(x = NULL, y = y_lab, title = title_txt) + base
      }
      cols <- rv$colors; if (!is.null(cols)) { if (input$metric_geom == "dot") p <- p + scale_color_manual(values = cols) else p <- p + scale_fill_manual(values = cols) }
      p <- p + geom_label(data = stats_simple, aes(x = Group, y = ymax * 1.02, label = label),
                          inherit.aes = FALSE, size = max(3, input$metric_size * 0.18),
                          label.size = 0.15, alpha = 0.9) +
        coord_cartesian(clip = "off") +
        theme(plot.margin = margin(10, 25, 10, 10))
      ggplot2::ggsave(file, plot = p, width = input$exp_w, height = input$exp_h, dpi = input$exp_dpi)
    }
  )
  
  # Keyboard shortcuts for Time Course (kept)
  # Removed keyboard shortcuts and related observers for simpler UX
}

cat("Starting Universal Calcium Imaging Analysis App...\n")
shinyApp(ui, server)