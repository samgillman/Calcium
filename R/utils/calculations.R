# Calculation utilities for Calcium Imaging Analysis App

# Safe file reading with error handling
safe_read <- function(path) {
  tryCatch({
    ext <- tolower(tools::file_ext(path))
    if (ext %in% c("xlsx", "xls")) {
      as.data.table(readxl::read_excel(path, .name_repair = "minimal"))
    } else {
      data.table::fread(path)
    }
  }, error = function(e) {
    stop(paste("Error reading file:", e$message))
  })
}

# Ensure time column is first
ensure_time_first <- function(dt, time_col = NULL) {
  if (!is.null(time_col) && time_col %in% names(dt)) {
    data.table::setcolorder(dt, c(time_col, setdiff(names(dt), time_col)))
  }
  data.table::setnames(dt, 1, "Time")
  dt
}

# Coerce all columns to numeric
coerce_numeric_dt <- function(dt) {
  suppressWarnings({
    dt[[1]] <- as.numeric(dt[[1]])
  })
  
  # Remove list columns
  keep <- c(TRUE, vapply(dt[, -1], function(col) !is.list(col), logical(1)))
  dt <- dt[, ..keep]
  
  # Convert remaining columns to numeric
  for (j in seq(2, ncol(dt))) {
    suppressWarnings({
      dt[[j]] <- as.numeric(dt[[j]])
    })
  }
  dt
}

# Improved FWHM/HWHM calculation with edge case handling
calculate_fwhm_hwhm <- function(t, x, baseline = NULL) {
  valid <- is.finite(t) & is.finite(x)
  t <- t[valid]
  x <- x[valid]
  n <- length(x)
  
  if (n < 4) {
    return(list(fwhm = NA_real_, hwhm = NA_real_))
  }
  
  # Calculate baseline if not provided
  if (is.null(baseline)) {
    b_len <- min(20, floor(n * 0.1))  # Use 10% of signal or 20 points
    baseline <- mean(x[1:b_len], na.rm = TRUE)
  }
  
  # Find peak
  pk <- which.max(x)
  peak_val <- x[pk]
  
  # Calculate threshold at 50% of peak height
  threshold <- baseline + 0.5 * (peak_val - baseline)
  
  # Find left crossing with interpolation
  left_time <- NA_real_
  if (pk > 1) {
    for (i in 1:(pk-1)) {
      if (x[i] < threshold && x[i+1] >= threshold) {
        # Linear interpolation
        frac <- (threshold - x[i]) / (x[i+1] - x[i])
        left_time <- t[i] + frac * (t[i+1] - t[i])
        break
      }
    }
  }
  
  # Find right crossing with interpolation
  right_time <- NA_real_
  if (pk < n) {
    for (i in pk:(n-1)) {
      if (x[i] >= threshold && x[i+1] < threshold) {
        # Linear interpolation
        frac <- (threshold - x[i]) / (x[i+1] - x[i])
        right_time <- t[i] + frac * (t[i+1] - t[i])
        break
      }
    }
    
    # Handle case where signal doesn't return to baseline
    if (is.na(right_time) && x[n] >= threshold) {
      # Estimate using exponential decay assumption
      right_time <- t[n]
    }
  }
  
  # Calculate FWHM and HWHM
  fwhm <- NA_real_
  hwhm <- NA_real_
  
  if (!is.na(left_time) && !is.na(right_time)) {
    fwhm <- right_time - left_time
    hwhm <- fwhm / 2
  } else if (!is.na(left_time) && is.na(right_time)) {
    # Only left side found - estimate HWHM from left side
    hwhm <- t[pk] - left_time
  }
  
  list(fwhm = fwhm, hwhm = hwhm)
}

# Calculate comprehensive rise time metrics
calculate_rise_times <- function(t, x, baseline, peak_idx) {
  peak_val <- x[peak_idx]
  response_amp <- peak_val - baseline
  
  if (response_amp <= 0 || peak_idx < 2) {
    return(list(
      rise_10_90 = NA_real_,
      time_to_25 = NA_real_,
      time_to_50 = NA_real_,
      time_to_75 = NA_real_,
      time_to_10 = NA_real_,
      time_to_90 = NA_real_
    ))
  }
  
  # Calculate thresholds
  thresh_10 <- baseline + 0.1 * response_amp
  thresh_25 <- baseline + 0.25 * response_amp
  thresh_50 <- baseline + 0.5 * response_amp
  thresh_75 <- baseline + 0.75 * response_amp
  thresh_90 <- baseline + 0.9 * response_amp
  
  # Find crossing times before peak
  find_crossing_time <- function(threshold, start_idx = 1) {
    for (i in start_idx:(peak_idx-1)) {
      if (x[i] < threshold && x[i+1] >= threshold) {
        # Linear interpolation
        frac <- (threshold - x[i]) / (x[i+1] - x[i])
        return(t[i] + frac * (t[i+1] - t[i]))
      }
    }
    return(NA_real_)
  }
  
  t_10 <- find_crossing_time(thresh_10)
  t_25 <- find_crossing_time(thresh_25)
  t_50 <- find_crossing_time(thresh_50)
  t_75 <- find_crossing_time(thresh_75)
  t_90 <- find_crossing_time(thresh_90)
  
  # Calculate rise time
  rise_10_90 <- if (!is.na(t_10) && !is.na(t_90)) t_90 - t_10 else NA_real_
  
  # Time to percentage of peak (from start)
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

# Vectorized metrics calculation for performance
compute_metrics_vectorized <- function(dt, time_vec, baseline_frames = 20) {
  # Convert to matrix for vectorized operations
  cell_names <- setdiff(names(dt), "Time")
  mat <- as.matrix(dt[, ..cell_names])
  n_cells <- ncol(mat)
  n_time <- nrow(mat)
  
  # Pre-allocate results
  results <- data.frame(
    Cell_ID = cell_names,
    Peak_dFF0 = numeric(n_cells),
    Time_to_Peak = numeric(n_cells),
    Rise_Time = numeric(n_cells),
    AUC = numeric(n_cells),
    Half_Width = numeric(n_cells),
    SNR = numeric(n_cells),
    Response_Amplitude = numeric(n_cells),
    Calcium_Entry_Rate = numeric(n_cells),
    stringsAsFactors = FALSE
  )
  
  # Vectorized baseline calculation
  baseline_idx <- min(baseline_frames, floor(n_time * 0.1))
  baselines <- colMeans(mat[1:baseline_idx, , drop = FALSE], na.rm = TRUE)
  baseline_sds <- apply(mat[1:baseline_idx, , drop = FALSE], 2, sd, na.rm = TRUE)
  
  # Process each cell
  for (i in 1:n_cells) {
    signal <- mat[, i]
    
    # Skip if all NA
    if (all(is.na(signal))) next
    
    # Find peak
    peak_idx <- which.max(signal)
    peak_val <- signal[peak_idx]
    
    # Calculate metrics
    results$Peak_dFF0[i] <- peak_val
    results$Time_to_Peak[i] <- time_vec[peak_idx]
    results$Response_Amplitude[i] <- peak_val - baselines[i]
    results$SNR[i] <- if (baseline_sds[i] > 0) results$Response_Amplitude[i] / baseline_sds[i] else NA
    
    # AUC (trapezoidal integration using base R)
    valid_idx <- !is.na(signal)
    if (sum(valid_idx) > 1) {
      # Simple trapezoidal integration
      x <- time_vec[valid_idx]
      y <- signal[valid_idx]
      results$AUC[i] <- sum(diff(x) * (y[-1] + y[-length(y)]) / 2)
    }
    
    # Calculate rate (using smoothed derivative)
    if (n_time > 5) {
      smoothed <- stats::filter(signal, rep(1/5, 5), sides = 2)
      rates <- diff(smoothed) / diff(time_vec)
      results$Calcium_Entry_Rate[i] <- max(rates, na.rm = TRUE)
    }
    
    # FWHM
    fwhm_result <- calculate_fwhm_hwhm(time_vec, signal, baselines[i])
    results$Half_Width[i] <- fwhm_result$hwhm
    
    # Rise time
    rise_result <- calculate_rise_times(time_vec, signal, baselines[i], peak_idx)
    results$Rise_Time[i] <- rise_result$rise_10_90
  }
  
  results
}

# Spike detection algorithm
detect_spikes <- function(signal, threshold_sd = 3, min_spike_distance = 10) {
  # Calculate baseline and noise
  baseline <- median(signal, na.rm = TRUE)
  noise_sd <- mad(signal, na.rm = TRUE)
  
  # Threshold for spike detection
  threshold <- baseline + threshold_sd * noise_sd
  
  # Find peaks above threshold
  above_threshold <- signal > threshold
  spike_indices <- c()
  
  i <- 1
  while (i <= length(signal)) {
    if (above_threshold[i]) {
      # Find local maximum in this region
      j <- i
      while (j < length(signal) && above_threshold[j+1]) {
        j <- j + 1
      }
      
      # Find peak in this window
      peak_idx <- i + which.max(signal[i:j]) - 1
      spike_indices <- c(spike_indices, peak_idx)
      
      # Skip ahead to avoid detecting same spike
      i <- j + min_spike_distance
    } else {
      i <- i + 1
    }
  }
  
  spike_indices
}

# Deconvolution for spike inference (simplified)
deconvolve_calcium <- function(signal, tau = 1.0, threshold = 0.1) {
  n <- length(signal)
  if (n < 3) return(rep(0, n))
  
  # Simple exponential kernel deconvolution
  # This is a placeholder - real implementation would use more sophisticated methods
  deconvolved <- numeric(n)
  
  for (i in 2:n) {
    # Estimate spike as positive derivative adjusted for decay
    spike_est <- signal[i] - signal[i-1] * exp(-1/tau)
    deconvolved[i] <- max(0, spike_est)
  }
  
  # Threshold small values
  deconvolved[deconvolved < threshold] <- 0
  
  deconvolved
}