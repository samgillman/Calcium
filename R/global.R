# Global configuration and shared utilities for Calcium Imaging Analysis App
# Version 2.4 - Modularized

# Package loading
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
  library(pheatmap)
  library(openxlsx)
  library(pryr)
  library(viridis)
})

# Global options
options(repos = c(CRAN = "https://cran.rstudio.com/"))
options(shiny.maxRequestSize = 200 * 1024^2)

# Theme configuration
APP_THEME <- theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    panel.grid.minor = element_blank()
  )

# Color palettes
DEFAULT_COLORS <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
)

# Metric definitions
METRIC_INFO <- list(
  Peak_dFF0 = list(
    label = "Peak ΔF/F₀",
    description = "Maximum fluorescence change relative to baseline",
    unit = "ΔF/F₀"
  ),
  Time_to_Peak = list(
    label = "Time to Peak",
    description = "Time from stimulus to peak response",
    unit = "seconds"
  ),
  Rise_Time = list(
    label = "Rise Time",
    description = "Time from 10% to 90% of peak",
    unit = "seconds"
  ),
  AUC = list(
    label = "Area Under Curve",
    description = "Total integrated response",
    unit = "ΔF/F₀ × s"
  ),
  Half_Width = list(
    label = "Half Width (HWHM)",
    description = "Half width at half maximum",
    unit = "seconds"
  ),
  SNR = list(
    label = "Signal-to-Noise Ratio",
    description = "Peak amplitude divided by baseline noise",
    unit = "ratio"
  ),
  Response_Amplitude = list(
    label = "Response Amplitude",
    description = "Peak minus baseline",
    unit = "ΔF/F₀"
  ),
  Calcium_Entry_Rate = list(
    label = "Ca²⁺ Entry Rate",
    description = "Maximum rate of fluorescence increase",
    unit = "ΔF/F₀/s"
  )
)

# Get metric label
get_metric_label <- function(metric_name) {
  if (metric_name %in% names(METRIC_INFO)) {
    return(METRIC_INFO[[metric_name]]$label)
  }
  return(metric_name)
}

# Get metric description
get_metric_description <- function(metric_name) {
  if (metric_name %in% names(METRIC_INFO)) {
    return(METRIC_INFO[[metric_name]]$description)
  }
  return("")
}

# File reading helpers
safe_read <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    as.data.table(readxl::read_excel(path, .name_repair = "minimal"))
  } else {
    data.table::fread(path)
  }
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
  for (j in seq(2, ncol(dt))) {
    suppressWarnings({ dt[[j]] <- as.numeric(dt[[j]]) })
  }
  dt
}

# Demo data generation
simulate_group <- function(label = "GroupA", n_cells = 60, T = 180, sr = 5, 
                          peak_t = 50, amp = 0.6, noise = 0.08) {
  time <- seq(0, T, by = 1/sr)
  base <- exp(-((time - peak_t)^2)/(2*(peak_t/6)^2))
  base <- base / max(base)
  
  M <- sapply(seq_len(n_cells), function(i) {
    a <- rlnorm(1, log(amp), 0.3)
    b <- base * a
    if (runif(1) < 0.15) b <- b * 0.3
    b + stats::filter(rnorm(length(time), 0, noise), rep(1/3, 3), sides = 2)
  })
  
  dt <- data.table::as.data.table(cbind(Time = time, M))
  data.table::setnames(dt, c("Time", paste0("Cell", seq_len(ncol(dt) - 1))))
  list(label = label, dt = dt)
}

# FWHM/HWHM calculation
fwhm_hwhm <- function(t, x, baseline = NULL) {
  valid <- is.finite(t) & is.finite(x)
  t <- t[valid]
  x <- x[valid]
  n <- length(x)
  
  if (n < 4) return(c(FWHM = NA_real_, HWHM = NA_real_))
  
  if (is.null(baseline)) {
    b_len <- min(20, n)
    baseline <- mean(x[1:b_len], na.rm = TRUE)
  }
  
  pk <- which.max(x)
  thr <- baseline + 0.5 * (x[pk] - baseline)
  above <- x >= thr
  
  if (!isTRUE(above[pk])) return(c(FWHM = NA_real_, HWHM = NA_real_))
  
  # Find left boundary
  tL <- NA_real_
  if (pk > 1) {
    left_idx <- which(x[1:(pk-1)] < thr & x[2:pk] >= thr)
    if (length(left_idx)) {
      i <- max(left_idx)
      denomL <- x[i+1] - x[i]
      tL <- if (is.finite(denomL) && denomL != 0) {
        t[i] + (thr - x[i]) * (t[i+1] - t[i]) / denomL
      } else {
        t[i]
      }
    } else if (above[1]) {
      tL <- t[1]
    }
  } else {
    tL <- t[1]
  }
  
  # Find right boundary
  tR <- NA_real_
  if (pk < n) {
    right_idx <- which(x[pk:(n-1)] >= thr & x[(pk+1):n] < thr)
    if (length(right_idx)) {
      j <- (pk - 1) + min(right_idx)
      denomR <- x[j+1] - x[j]
      tR <- if (is.finite(denomR) && denomR != 0) {
        t[j] + (thr - x[j]) * (t[j+1] - t[j]) / denomR
      } else {
        t[j]
      }
    } else if (above[n]) {
      tR <- t[n]
    }
  } else {
    tR <- t[n]
  }
  
  if (!is.finite(tL) || !is.finite(tR)) {
    return(c(FWHM = NA_real_, HWHM = NA_real_))
  }
  
  FWHM <- as.numeric(tR - tL)
  c(FWHM = FWHM, HWHM = FWHM/2)
}

# Cell metrics calculation
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
  
  # Calculate baseline
  baseline_len <- min(baseline_frames, length(x))
  baseline_vals <- x[1:baseline_len]
  baseline_raw <- mean(baseline_vals, na.rm = TRUE)
  baseline_sd_raw <- stats::sd(baseline_vals, na.rm = TRUE)
  
  # Determine if already normalized
  if (abs(baseline_raw) < 0.1) {
    # Already normalized
    working_signal <- x
    baseline <- 0
    baseline_sd <- baseline_sd_raw
  } else {
    # Calculate ΔF/F₀
    if (baseline_raw != 0) {
      working_signal <- (x - baseline_raw) / baseline_raw
      baseline <- 0
      baseline_sd <- stats::sd(working_signal[1:baseline_len], na.rm = TRUE)
    } else {
      working_signal <- x
      baseline <- baseline_raw
      baseline_sd <- baseline_sd_raw
    }
  }
  
  # Find peak
  peak_value <- max(working_signal, na.rm = TRUE)
  peak_idx <- which.max(working_signal)
  time_to_peak <- t[peak_idx]
  
  # Response amplitude
  response_amplitude <- peak_value - baseline
  
  # Helper function for threshold crossing
  find_threshold_crossing <- function(signal, threshold, start_after = baseline_frames) {
    for (i in (start_after + 1):length(signal)) {
      if (!is.na(signal[i]) && signal[i] >= threshold) {
        return(i)
      }
    }
    return(NA_integer_)
  }
  
  # Time to percentage of peak
  time_to_25 <- NA_real_
  time_to_50 <- NA_real_
  time_to_75 <- NA_real_
  
  if (response_amplitude > 0) {
    idx_25 <- find_threshold_crossing(working_signal, baseline + 0.25 * response_amplitude)
    idx_50 <- find_threshold_crossing(working_signal, baseline + 0.50 * response_amplitude)
    idx_75 <- find_threshold_crossing(working_signal, baseline + 0.75 * response_amplitude)
    
    if (!is.na(idx_25)) time_to_25 <- t[idx_25]
    if (!is.na(idx_50)) time_to_50 <- t[idx_50]
    if (!is.na(idx_75)) time_to_75 <- t[idx_75]
  }
  
  # Rise time (10% to 90%)
  rise_time <- NA_real_
  if (response_amplitude > 0) {
    idx_10 <- find_threshold_crossing(working_signal, baseline + 0.10 * response_amplitude)
    idx_90 <- find_threshold_crossing(working_signal, baseline + 0.90 * response_amplitude)
    
    if (!is.na(idx_10) && !is.na(idx_90)) {
      rise_time <- t[idx_90] - t[idx_10]
    }
  }
  
  # Calcium entry rate (max derivative)
  if (length(x) > 1) {
    dt <- diff(t)
    dx <- diff(working_signal)
    derivatives <- dx / dt
    calcium_entry_rate <- max(derivatives, na.rm = TRUE)
  } else {
    calcium_entry_rate <- NA_real_
  }
  
  # AUC using trapezoidal rule
  auc <- NA_real_
  if (length(t) > 1) {
    # Only integrate positive values above baseline
    positive_signal <- pmax(working_signal - baseline, 0)
    auc <- sum(diff(t) * (positive_signal[-length(positive_signal)] + 
                         positive_signal[-1]) / 2, na.rm = TRUE)
  }
  
  # Half width calculation
  width_results <- fwhm_hwhm(t, working_signal, baseline)
  half_width <- width_results["HWHM"]
  
  # SNR
  snr <- if (baseline_sd > 0) response_amplitude / baseline_sd else NA_real_
  
  # Return metrics
  data.frame(
    Peak_dFF0 = peak_value,
    Time_to_Peak = time_to_peak,
    Time_to_25_Peak = time_to_25,
    Time_to_50_Peak = time_to_50,
    Time_to_75_Peak = time_to_75,
    Rise_Time = rise_time,
    Calcium_Entry_Rate = calcium_entry_rate,
    AUC = auc,
    Response_Amplitude = response_amplitude,
    Half_Width = half_width,
    Baseline_SD = baseline_sd,
    SNR = snr
  )
}