# Metric Guide Module - Interactive visualization of metric calculations

# Module UI
mod_metric_guide_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Metric Guide (Live Examples)",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        
        withMathJax(),
        
        p("Select a metric to see how it is computed using your uploaded data, with detailed explanations and an annotated example trace."),
        
        fluidRow(
          column(4,
            selectInput(
              ns("guide_metric"),
              "Metric",
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
              selected = "dFF0_calc"
            ),
            
            uiOutput(ns("guide_cell_picker")),
            
            uiOutput(ns("guide_timepoint_ui")),
            
            hr(),
            
            h4("Display Options"),
            
            checkboxInput(
              ns("show_annotations"),
              "Show annotations",
              value = TRUE
            ),
            
            checkboxInput(
              ns("show_formula"),
              "Show mathematical formula",
              value = TRUE
            ),
            
            checkboxInput(
              ns("show_calculation"),
              "Show step-by-step calculation",
              value = TRUE
            ),
            
            selectInput(
              ns("color_scheme"),
              "Color scheme",
              choices = c(
                "Default" = "default",
                "Educational" = "educational",
                "Publication" = "publication"
              ),
              selected = "educational"
            )
          ),
          
          column(8,
            uiOutput(ns("guide_math_explanation")),
            
            tags$hr(),
            
            withSpinner(plotOutput(ns("guide_plot"), height = "560px"), type = 4),
            
            br(),
            
            uiOutput(ns("guide_calculation_details"))
          )
        )
      )
    )
  )
}

# Module Server
mod_metric_guide_server <- function(id, data_module) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Get current data
    current_data <- reactive({
      data <- data_module$data()
      if (!is.null(data) && length(data) > 0) {
        return(data[[1]])  # Use first dataset
      }
      return(NULL)
    })
    
    # Update cell picker
    output$guide_cell_picker <- renderUI({
      req(current_data())
      
      cells <- setdiff(names(current_data()), "Time")
      
      selectInput(
        ns("guide_cell"),
        "Select Cell",
        choices = cells,
        selected = cells[1]
      )
    })
    
    # Update timepoint UI based on metric
    output$guide_timepoint_ui <- renderUI({
      if (input$guide_metric == "dFF0_calc") {
        sliderInput(
          ns("guide_timepoint"),
          "Time point (for ΔF/F₀ demo)",
          min = 1,
          max = ifelse(!is.null(current_data()), nrow(current_data()), 100),
          value = ifelse(!is.null(current_data()), min(50, nrow(current_data())), 50),
          step = 1
        )
      }
    })
    
    # Mathematical explanation
    output$guide_math_explanation <- renderUI({
      
      explanations <- list(
        dFF0_calc = tagList(
          h4("ΔF/F₀ Normalization"),
          p("The fundamental calculation for calcium imaging analysis:"),
          if (input$show_formula) {
            withMathJax(
              "$$\\Delta F/F_0 = \\frac{F(t) - F_0}{F_0}$$"
            )
          },
          p(strong("Where:")),
          tags$ul(
            tags$li(HTML("F(t) = Fluorescence at time t")),
            tags$li(HTML("F₀ = Baseline fluorescence (average of first N frames)")),
            tags$li(HTML("ΔF/F₀ = Fractional change in fluorescence"))
          ),
          p("This normalization accounts for differences in baseline fluorescence between cells and experiments.")
        ),
        
        Peak_dFF0 = tagList(
          h4("Peak ΔF/F₀"),
          p("The maximum fluorescence change during the response:"),
          if (input$show_formula) {
            withMathJax(
              "$$Peak = \\max_{t} \\left( \\Delta F/F_0(t) \\right)$$"
            )
          },
          p("This metric captures the maximum response amplitude and is a key indicator of calcium transient magnitude.")
        ),
        
        Response_Amplitude = tagList(
          h4("Response Amplitude"),
          p("The difference between peak and baseline:"),
          if (input$show_formula) {
            withMathJax(
              "$$Amplitude = Peak - Baseline$$"
            )
          },
          p("This measures the actual change in signal from resting state to peak response.")
        ),
        
        AUC = tagList(
          h4("Area Under Curve (AUC)"),
          p("The integrated response over time:"),
          if (input$show_formula) {
            withMathJax(
              "$$AUC = \\int_{t_0}^{t_{end}} \\max(0, \\Delta F/F_0(t) - baseline) \\, dt$$"
            )
          },
          p("AUC represents the total calcium influx and is calculated using trapezoidal integration."),
          p("Only values above baseline contribute to the AUC.")
        ),
        
        Time_to_Peak = tagList(
          h4("Time to Peak"),
          p("The time from stimulus onset to peak response:"),
          if (input$show_formula) {
            withMathJax(
              "$$T_{peak} = t_{peak} - t_0$$"
            )
          },
          p("This metric indicates the speed of response initiation.")
        ),
        
        Time_to_25_Peak = tagList(
          h4("Time to 25% of Peak"),
          p("Time to reach 25% of the peak amplitude:"),
          if (input$show_formula) {
            withMathJax(
              "$$T_{25\\%} = t_{25\\%} - t_0$$",
              "$$where \\; F(t_{25\\%}) = baseline + 0.25 \\times (peak - baseline)$$"
            )
          },
          p("Early response kinetics indicator.")
        ),
        
        Time_to_50_Peak = tagList(
          h4("Time to 50% of Peak"),
          p("Time to reach half-maximum response:"),
          if (input$show_formula) {
            withMathJax(
              "$$T_{50\\%} = t_{50\\%} - t_0$$",
              "$$where \\; F(t_{50\\%}) = baseline + 0.5 \\times (peak - baseline)$$"
            )
          },
          p("Mid-response kinetics indicator.")
        ),
        
        Time_to_75_Peak = tagList(
          h4("Time to 75% of Peak"),
          p("Time to reach 75% of peak amplitude:"),
          if (input$show_formula) {
            withMathJax(
              "$$T_{75\\%} = t_{75\\%} - t_0$$",
              "$$where \\; F(t_{75\\%}) = baseline + 0.75 \\times (peak - baseline)$$"
            )
          },
          p("Late response kinetics indicator.")
        ),
        
        Rise_Time = tagList(
          h4("Rise Time (10% to 90%)"),
          p("Time taken to rise from 10% to 90% of peak amplitude:"),
          if (input$show_formula) {
            withMathJax(
              "$$Rise\\;Time = t_{90\\%} - t_{10\\%}$$"
            )
          },
          p("This metric characterizes the speed of calcium influx, excluding the initial lag and final approach to peak.")
        ),
        
        Calcium_Entry_Rate = tagList(
          h4("Calcium Entry Rate"),
          p("Maximum rate of fluorescence increase:"),
          if (input$show_formula) {
            withMathJax(
              "$$Ca^{2+}\\;Entry\\;Rate = \\max_{t} \\left( \\frac{d(\\Delta F/F_0)}{dt} \\right)$$"
            )
          },
          p("Calculated as the maximum derivative of the smoothed signal."),
          p("Indicates the fastest rate of calcium influx.")
        ),
        
        Half_Width = tagList(
          h4("Half Width at Half Maximum (HWHM)"),
          p("Duration of the response at 50% of peak amplitude:"),
          if (input$show_formula) {
            withMathJax(
              "$$HWHM = \\frac{FWHM}{2} = \\frac{t_{right,50\\%} - t_{left,50\\%}}{2}$$"
            )
          },
          p("HWHM characterizes the temporal extent of the calcium transient."),
          p("Useful for comparing response durations between conditions.")
        ),
        
        SNR = tagList(
          h4("Signal-to-Noise Ratio"),
          p("Ratio of response amplitude to baseline noise:"),
          if (input$show_formula) {
            withMathJax(
              "$$SNR = \\frac{Response\\;Amplitude}{\\sigma_{baseline}}$$"
            )
          },
          p("Where σ_baseline is the standard deviation of the baseline period."),
          p("Higher SNR indicates better signal quality and more reliable detection.")
        )
      )
      
      explanations[[input$guide_metric]]
    })
    
    # Create annotated plot
    output$guide_plot <- renderPlot({
      req(current_data(), input$guide_cell)
      
      df <- current_data()
      
      if (!input$guide_cell %in% names(df)) return(NULL)
      
      # Get signal
      time_vec <- df$Time
      signal <- df[[input$guide_cell]]
      
      # Calculate baseline (first 20 frames or 10%)
      baseline_frames <- min(20, floor(length(signal) * 0.1))
      baseline <- mean(signal[1:baseline_frames], na.rm = TRUE)
      baseline_sd <- sd(signal[1:baseline_frames], na.rm = TRUE)
      
      # Find peak
      peak_idx <- which.max(signal)
      peak_value <- signal[peak_idx]
      peak_time <- time_vec[peak_idx]
      
      # Create base plot
      plot_df <- data.frame(
        Time = time_vec,
        Signal = signal
      )
      
      p <- ggplot(plot_df, aes(x = Time, y = Signal)) +
        geom_line(size = 1.5, color = "black") +
        theme_minimal(base_size = 14) +
        labs(
          title = paste("Metric Demonstration:", input$guide_cell),
          x = "Time (s)",
          y = "Signal (ΔF/F₀)"
        )
      
      # Add metric-specific annotations
      if (input$show_annotations) {
        p <- add_metric_annotations(
          p, input$guide_metric, time_vec, signal, 
          baseline, baseline_sd, peak_idx, peak_value, peak_time,
          baseline_frames, input$guide_timepoint
        )
      }
      
      # Apply color scheme
      p <- apply_color_scheme(p, input$color_scheme)
      
      p
    })
    
    # Calculation details
    output$guide_calculation_details <- renderUI({
      req(current_data(), input$guide_cell)
      
      if (!input$show_calculation) return(NULL)
      
      df <- current_data()
      if (!input$guide_cell %in% names(df)) return(NULL)
      
      # Get signal
      time_vec <- df$Time
      signal <- df[[input$guide_cell]]
      
      # Calculate metrics
      baseline_frames <- min(20, floor(length(signal) * 0.1))
      baseline <- mean(signal[1:baseline_frames], na.rm = TRUE)
      baseline_sd <- sd(signal[1:baseline_frames], na.rm = TRUE)
      
      peak_idx <- which.max(signal)
      peak_value <- signal[peak_idx]
      peak_time <- time_vec[peak_idx]
      
      # Create calculation details based on metric
      details <- create_calculation_details(
        input$guide_metric, time_vec, signal, baseline, baseline_sd,
        peak_idx, peak_value, peak_time, baseline_frames,
        input$guide_timepoint
      )
      
      div(
        class = "well",
        h4("Step-by-Step Calculation"),
        details
      )
    })
    
    # Helper function to add annotations
    add_metric_annotations <- function(p, metric, time_vec, signal, baseline, 
                                       baseline_sd, peak_idx, peak_value, 
                                       peak_time, baseline_frames, timepoint = NULL) {
      
      if (metric == "dFF0_calc" && !is.null(timepoint)) {
        # Show calculation at specific timepoint
        if (timepoint <= length(signal)) {
          current_val <- signal[timepoint]
          current_time <- time_vec[timepoint]
          
          p <- p +
            # Baseline region
            annotate("rect", xmin = time_vec[1], xmax = time_vec[baseline_frames],
                    ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "blue") +
            # Baseline line
            geom_hline(yintercept = baseline, linetype = "dashed", color = "blue", size = 1) +
            # Current point
            geom_point(x = current_time, y = current_val, size = 5, color = "red") +
            # Annotations
            annotate("text", x = time_vec[baseline_frames/2], y = baseline,
                    label = paste("F₀ =", round(baseline, 3)),
                    vjust = -1, color = "blue", size = 4) +
            annotate("text", x = current_time, y = current_val,
                    label = paste("F(t) =", round(current_val, 3)),
                    vjust = -1, color = "red", size = 4)
          
          if (!is.na(baseline) && baseline != 0) {
            dff0 <- (current_val - baseline) / baseline
            p <- p +
              annotate("text", x = current_time, y = current_val,
                      label = paste("ΔF/F₀ =", round(dff0, 3)),
                      vjust = 2, color = "red", size = 4, fontface = "bold")
          }
        }
        
      } else if (metric == "Peak_dFF0") {
        p <- p +
          # Peak point
          geom_point(x = peak_time, y = peak_value, size = 5, color = "red") +
          # Peak annotation
          annotate("text", x = peak_time, y = peak_value,
                  label = paste("Peak =", round(peak_value, 3)),
                  vjust = -1, color = "red", size = 4, fontface = "bold") +
          # Baseline
          geom_hline(yintercept = baseline, linetype = "dashed", color = "blue", alpha = 0.5)
        
      } else if (metric == "Response_Amplitude") {
        p <- p +
          # Baseline
          geom_hline(yintercept = baseline, linetype = "dashed", color = "blue", size = 1) +
          # Peak
          geom_point(x = peak_time, y = peak_value, size = 5, color = "red") +
          # Amplitude arrow
          annotate("segment", x = peak_time, xend = peak_time,
                  y = baseline, yend = peak_value,
                  arrow = arrow(ends = "both", length = unit(0.3, "cm")),
                  color = "green", size = 1.5) +
          # Amplitude text
          annotate("text", x = peak_time, y = (baseline + peak_value) / 2,
                  label = paste("Amplitude =", round(peak_value - baseline, 3)),
                  hjust = -0.2, color = "green", size = 4, fontface = "bold")
        
      } else if (metric == "AUC") {
        # Create polygon for AUC
        above_baseline <- pmax(signal - baseline, 0)
        auc_df <- data.frame(
          x = c(time_vec, rev(time_vec)),
          y = c(rep(baseline, length(time_vec)), rev(baseline + above_baseline))
        )
        
        p <- p +
          geom_polygon(data = auc_df, aes(x = x, y = y),
                      fill = "green", alpha = 0.3) +
          geom_hline(yintercept = baseline, linetype = "dashed", color = "blue") +
          annotate("text", x = mean(time_vec), y = baseline + max(above_baseline) / 2,
                  label = "AUC", color = "green", size = 6, fontface = "bold")
        
      } else if (metric %in% c("Time_to_Peak", "Time_to_25_Peak", "Time_to_50_Peak", "Time_to_75_Peak")) {
        
        # Determine threshold
        pct <- switch(metric,
                     "Time_to_Peak" = 1.0,
                     "Time_to_25_Peak" = 0.25,
                     "Time_to_50_Peak" = 0.50,
                     "Time_to_75_Peak" = 0.75)
        
        threshold <- baseline + pct * (peak_value - baseline)
        
        # Find crossing point
        crossing_idx <- which(signal >= threshold)[1]
        if (!is.na(crossing_idx)) {
          crossing_time <- time_vec[crossing_idx]
          
          p <- p +
            # Threshold line
            geom_hline(yintercept = threshold, linetype = "dotted", color = "orange", size = 1) +
            # Crossing point
            geom_point(x = crossing_time, y = signal[crossing_idx], size = 5, color = "orange") +
            # Time arrow
            annotate("segment", x = time_vec[1], xend = crossing_time,
                    y = threshold - (peak_value - baseline) * 0.1, 
                    yend = threshold - (peak_value - baseline) * 0.1,
                    arrow = arrow(length = unit(0.3, "cm")),
                    color = "orange", size = 1.5) +
            # Time annotation
            annotate("text", x = (time_vec[1] + crossing_time) / 2,
                    y = threshold - (peak_value - baseline) * 0.1,
                    label = paste(round(crossing_time - time_vec[1], 2), "s"),
                    vjust = 2, color = "orange", size = 4, fontface = "bold")
        }
        
      } else if (metric == "Rise_Time") {
        # 10% and 90% thresholds
        thresh_10 <- baseline + 0.1 * (peak_value - baseline)
        thresh_90 <- baseline + 0.9 * (peak_value - baseline)
        
        idx_10 <- which(signal >= thresh_10)[1]
        idx_90 <- which(signal >= thresh_90)[1]
        
        if (!is.na(idx_10) && !is.na(idx_90)) {
          p <- p +
            # Threshold lines
            geom_hline(yintercept = thresh_10, linetype = "dotted", color = "purple", alpha = 0.5) +
            geom_hline(yintercept = thresh_90, linetype = "dotted", color = "purple", alpha = 0.5) +
            # Points
            geom_point(x = time_vec[idx_10], y = signal[idx_10], size = 4, color = "purple") +
            geom_point(x = time_vec[idx_90], y = signal[idx_90], size = 4, color = "purple") +
            # Rise time region
            annotate("rect", xmin = time_vec[idx_10], xmax = time_vec[idx_90],
                    ymin = thresh_10, ymax = thresh_90,
                    alpha = 0.2, fill = "purple") +
            # Annotation
            annotate("text", x = (time_vec[idx_10] + time_vec[idx_90]) / 2,
                    y = (thresh_10 + thresh_90) / 2,
                    label = paste("Rise Time\n", round(time_vec[idx_90] - time_vec[idx_10], 2), "s"),
                    color = "purple", size = 4, fontface = "bold")
        }
        
      } else if (metric == "Half_Width") {
        # Half maximum threshold
        half_max <- baseline + 0.5 * (peak_value - baseline)
        
        # Find left and right crossing points
        left_idx <- which(signal[1:peak_idx] >= half_max)[1]
        right_idx <- peak_idx + which(signal[(peak_idx+1):length(signal)] <= half_max)[1] - 1
        
        if (!is.na(left_idx) && !is.na(right_idx) && right_idx > left_idx) {
          fwhm <- time_vec[right_idx] - time_vec[left_idx]
          hwhm <- fwhm / 2
          
          p <- p +
            # Half max line
            geom_hline(yintercept = half_max, linetype = "dashed", color = "brown", size = 1) +
            # Crossing points
            geom_point(x = time_vec[left_idx], y = signal[left_idx], size = 4, color = "brown") +
            geom_point(x = time_vec[right_idx], y = signal[right_idx], size = 4, color = "brown") +
            # FWHM arrow
            annotate("segment", x = time_vec[left_idx], xend = time_vec[right_idx],
                    y = half_max, yend = half_max,
                    arrow = arrow(ends = "both", length = unit(0.3, "cm")),
                    color = "brown", size = 1.5) +
            # Annotation
            annotate("text", x = (time_vec[left_idx] + time_vec[right_idx]) / 2,
                    y = half_max,
                    label = paste("HWHM =", round(hwhm, 2), "s"),
                    vjust = -1, color = "brown", size = 4, fontface = "bold")
        }
        
      } else if (metric == "SNR") {
        # Baseline noise region
        p <- p +
          # Baseline region
          annotate("rect", xmin = time_vec[1], xmax = time_vec[baseline_frames],
                  ymin = baseline - 2*baseline_sd, ymax = baseline + 2*baseline_sd,
                  alpha = 0.2, fill = "gray") +
          # Baseline line
          geom_hline(yintercept = baseline, linetype = "solid", color = "gray", size = 1) +
          # Noise bounds
          geom_hline(yintercept = baseline + baseline_sd, linetype = "dotted", color = "gray") +
          geom_hline(yintercept = baseline - baseline_sd, linetype = "dotted", color = "gray") +
          # Peak
          geom_point(x = peak_time, y = peak_value, size = 5, color = "red") +
          # SNR annotation
          annotate("text", x = time_vec[baseline_frames/2], y = baseline,
                  label = paste("σ =", round(baseline_sd, 3)),
                  vjust = 2, color = "gray", size = 3) +
          annotate("text", x = peak_time, y = peak_value,
                  label = paste("SNR =", round((peak_value - baseline) / baseline_sd, 1)),
                  vjust = -1, color = "red", size = 4, fontface = "bold")
      }
      
      p
    }
    
    # Helper function to apply color schemes
    apply_color_scheme <- function(p, scheme) {
      if (scheme == "educational") {
        p <- p + theme_minimal(base_size = 14) +
          theme(
            plot.background = element_rect(fill = "#f8f9fa", color = NA),
            panel.grid.major = element_line(color = "#dee2e6"),
            panel.grid.minor = element_blank()
          )
      } else if (scheme == "publication") {
        p <- p + theme_classic(base_size = 12) +
          theme(
            axis.line = element_line(size = 0.5),
            axis.ticks = element_line(size = 0.5)
          )
      }
      p
    }
    
    # Helper function to create calculation details
    create_calculation_details <- function(metric, time_vec, signal, baseline, 
                                          baseline_sd, peak_idx, peak_value, 
                                          peak_time, baseline_frames, timepoint = NULL) {
      
      if (metric == "dFF0_calc" && !is.null(timepoint)) {
        if (timepoint <= length(signal)) {
          current_val <- signal[timepoint]
          dff0 <- if (!is.na(baseline) && baseline != 0) {
            (current_val - baseline) / baseline
          } else NA
          
          return(tagList(
            tags$ol(
              tags$li(paste("Baseline (F₀) = mean of first", baseline_frames, "frames =", round(baseline, 4))),
              tags$li(paste("Current value F(t) at frame", timepoint, "=", round(current_val, 4))),
              tags$li(paste("ΔF = F(t) - F₀ =", round(current_val, 4), "-", round(baseline, 4), "=", round(current_val - baseline, 4))),
              tags$li(paste("ΔF/F₀ = ΔF / F₀ =", round(current_val - baseline, 4), "/", round(baseline, 4), "=", round(dff0, 4)))
            )
          ))
        }
        
      } else if (metric == "Peak_dFF0") {
        return(tagList(
          tags$ol(
            tags$li(paste("Scan all time points for maximum value")),
            tags$li(paste("Peak found at time", round(peak_time, 2), "s (frame", peak_idx, ")")),
            tags$li(paste("Peak value =", round(peak_value, 4)))
          )
        ))
        
      } else if (metric == "Response_Amplitude") {
        return(tagList(
          tags$ol(
            tags$li(paste("Baseline =", round(baseline, 4))),
            tags$li(paste("Peak =", round(peak_value, 4))),
            tags$li(paste("Response Amplitude = Peak - Baseline =", round(peak_value - baseline, 4)))
          )
        ))
        
      } else if (metric == "AUC") {
        above_baseline <- pmax(signal - baseline, 0)
        valid_idx <- !is.na(above_baseline)
        if (sum(valid_idx) > 1) {
          x <- time_vec[valid_idx]
          y <- above_baseline[valid_idx]
          auc <- sum(diff(x) * (y[-1] + y[-length(y)]) / 2)
        } else {
          auc <- NA
        }
        
        return(tagList(
          tags$ol(
            tags$li(paste("Baseline =", round(baseline, 4))),
            tags$li("Subtract baseline from all values"),
            tags$li("Set negative values to 0"),
            tags$li("Apply trapezoidal integration"),
            tags$li(paste("AUC =", round(auc, 4), "ΔF/F₀ × s"))
          )
        ))
        
      } else if (metric == "Rise_Time") {
        thresh_10 <- baseline + 0.1 * (peak_value - baseline)
        thresh_90 <- baseline + 0.9 * (peak_value - baseline)
        idx_10 <- which(signal >= thresh_10)[1]
        idx_90 <- which(signal >= thresh_90)[1]
        
        rise_time <- if (!is.na(idx_10) && !is.na(idx_90)) {
          time_vec[idx_90] - time_vec[idx_10]
        } else NA
        
        return(tagList(
          tags$ol(
            tags$li(paste("Response amplitude =", round(peak_value - baseline, 4))),
            tags$li(paste("10% threshold =", round(thresh_10, 4))),
            tags$li(paste("90% threshold =", round(thresh_90, 4))),
            tags$li(paste("Time at 10% =", 
                         ifelse(!is.na(idx_10), round(time_vec[idx_10], 2), "NA"), "s")),
            tags$li(paste("Time at 90% =", 
                         ifelse(!is.na(idx_90), round(time_vec[idx_90], 2), "NA"), "s")),
            tags$li(paste("Rise Time = t(90%) - t(10%) =", 
                         ifelse(!is.na(rise_time), round(rise_time, 3), "NA"), "s"))
          )
        ))
        
      } else if (metric == "SNR") {
        snr <- if (baseline_sd > 0) (peak_value - baseline) / baseline_sd else NA
        
        return(tagList(
          tags$ol(
            tags$li(paste("Baseline standard deviation (σ) =", round(baseline_sd, 4))),
            tags$li(paste("Response amplitude =", round(peak_value - baseline, 4))),
            tags$li(paste("SNR = Amplitude / σ =", round(peak_value - baseline, 4), "/", 
                         round(baseline_sd, 4), "=", round(snr, 2)))
          )
        ))
      }
      
      return(p("Select a metric to see calculation details."))
    }
  })
}