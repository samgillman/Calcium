# Export Module for Calcium Imaging Analysis App

# Module UI
mod_export_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Export Options",
        status = "primary",
        solidHeader = TRUE,
        width = 4,
        
        radioButtons(
          ns("exp_fmt"),
          "Format",
          choices = c("PNG" = "png", "PDF" = "pdf", "TIFF" = "tiff", "SVG" = "svg"),
          inline = TRUE,
          selected = "png"
        ),
        
        numericInput(ns("exp_w"), "Width (in)", 12, min = 4, max = 30),
        numericInput(ns("exp_h"), "Height (in)", 8, min = 4, max = 30),
        numericInput(ns("exp_dpi"), "DPI (for raster)", 300, min = 72, max = 600),
        
        conditionalPanel(
          condition = "input.exp_fmt == 'tiff'",
          ns = ns,
          selectInput(
            ns("tiff_comp"),
            "TIFF compression",
            choices = c("lzw", "zip", "none"),
            selected = "lzw"
          )
        ),
        
        tags$hr(),
        
        h4("Figure Composer"),
        
        checkboxGroupInput(
          ns("compose_panels"),
          "Include panels",
          choices = c(
            "Time Course" = "tc",
            "Metrics" = "mp",
            "Heatmap" = "hm",
            "Correlation" = "cor"
          ),
          selected = c("tc", "mp")
        ),
        
        selectInput(
          ns("compose_layout"),
          "Layout",
          choices = c("Columns" = "col", "Rows" = "row", "Grid (2x2)" = "grid"),
          selected = "col"
        ),
        
        numericInput(
          ns("compose_spacing"),
          "Panel spacing",
          value = 0.05,
          min = 0,
          max = 0.2,
          step = 0.01
        ),
        
        checkboxInput(
          ns("compose_labels"),
          "Add panel labels (A, B, C...)",
          value = TRUE
        ),
        
        downloadButton(
          ns("dl_composite"),
          "Download Composite Figure",
          class = "btn-warning"
        ),
        
        tags$hr(),
        
        h4("Data Export"),
        
        downloadButton(ns("dl_metrics_csv"), "Download Metrics CSV"),
        br(), br(),
        downloadButton(ns("dl_summary_csv"), "Download Summary CSV"),
        br(), br(),
        downloadButton(ns("dl_timecourse_plot"), "Download Time Course Plot"),
        br(), br(),
        downloadButton(ns("dl_heatmap_plot"), "Download Heatmap Plot"),
        br(), br(),
        downloadButton(ns("dl_metrics_plot"), "Download Current Metrics Plot"),
        
        tags$hr(),
        
        h4("Processed Data"),
        
        selectInput(ns("exp_dl_group"), "Select file", choices = NULL),
        
        downloadButton(ns("dl_processed_wide_exp"), "Download Processed Data (CSV)")
      ),
      
      box(
        title = "Export Preview",
        status = "info",
        solidHeader = TRUE,
        width = 8,
        
        h4("Export Information"),
        
        tags$ul(
          tags$li("PNG/TIFF recommended for slides/publication; use 300–600 DPI"),
          tags$li("PDF/SVG preserve vector graphics for scaling"),
          tags$li("Composite figures combine multiple panels into publication-ready layouts"),
          tags$li("Panel labels help reference specific plots in manuscripts")
        ),
        
        verbatimTextOutput(ns("export_info")),
        
        tags$hr(),
        
        h4("Composite Figure Preview"),
        
        plotOutput(ns("composite_preview"), height = "600px")
      )
    )
  )
}

# Module Server
mod_export_server <- function(id, data_module, metrics_module, viz_module = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values for plots
    plot_storage <- reactiveValues(
      timecourse = NULL,
      metrics = NULL,
      heatmap = NULL,
      correlation = NULL,
      composite = NULL
    )
    
    # Update file selector
    observe({
      data <- data_module$data()
      if (!is.null(data)) {
        updateSelectInput(
          session,
          "exp_dl_group",
          choices = names(data),
          selected = names(data)[1]
        )
      }
    })
    
    # Generate plots for export
    observe({
      req(data_module$data())
      
      # Time course plot
      plot_storage$timecourse <- create_timecourse_plot_export(
        data_module$data(),
        metrics_module$metrics()
      )
      
      # Metrics plot
      if (!is.null(metrics_module$metrics())) {
        plot_storage$metrics <- create_metrics_plot_export(
          metrics_module$metrics()
        )
      }
      
      # Heatmap
      plot_storage$heatmap <- create_heatmap_export(
        data_module$data()
      )
    })
    
    # Composite figure preview
    output$composite_preview <- renderPlot({
      req(input$compose_panels)
      
      plots <- list()
      
      if ("tc" %in% input$compose_panels && !is.null(plot_storage$timecourse)) {
        plots$tc <- plot_storage$timecourse
      }
      
      if ("mp" %in% input$compose_panels && !is.null(plot_storage$metrics)) {
        plots$mp <- plot_storage$metrics
      }
      
      if ("hm" %in% input$compose_panels && !is.null(plot_storage$heatmap)) {
        plots$hm <- plot_storage$heatmap
      }
      
      if ("cor" %in% input$compose_panels && !is.null(plot_storage$correlation)) {
        plots$cor <- plot_storage$correlation
      }
      
      if (length(plots) > 0) {
        composite <- create_composite_figure(
          plots,
          layout = input$compose_layout,
          spacing = input$compose_spacing,
          add_labels = input$compose_labels
        )
        
        plot_storage$composite <- composite
        composite
      }
    })
    
    # Export info
    output$export_info <- renderPrint({
      cat("Export Settings:\n")
      cat("Format:", input$exp_fmt, "\n")
      cat("Dimensions:", input$exp_w, "x", input$exp_h, "inches\n")
      cat("DPI:", input$exp_dpi, "\n")
      cat("File size estimate:", estimate_file_size(
        input$exp_fmt, input$exp_w, input$exp_h, input$exp_dpi
      ), "\n")
      
      if (length(input$compose_panels) > 0) {
        cat("\nComposite Figure:\n")
        cat("Panels:", paste(input$compose_panels, collapse = ", "), "\n")
        cat("Layout:", input$compose_layout, "\n")
      }
    })
    
    # Download handlers
    output$dl_composite <- downloadHandler(
      filename = function() {
        paste0("composite_figure_", Sys.Date(), ".", input$exp_fmt)
      },
      content = function(file) {
        if (!is.null(plot_storage$composite)) {
          ggsave(
            file,
            plot = plot_storage$composite,
            width = input$exp_w,
            height = input$exp_h,
            dpi = input$exp_dpi,
            device = input$exp_fmt
          )
        }
      }
    )
    
    output$dl_timecourse_plot <- downloadHandler(
      filename = function() {
        paste0("timecourse_", Sys.Date(), ".", input$exp_fmt)
      },
      content = function(file) {
        if (!is.null(plot_storage$timecourse)) {
          ggsave(
            file,
            plot = plot_storage$timecourse,
            width = input$exp_w,
            height = input$exp_h,
            dpi = input$exp_dpi,
            device = input$exp_fmt
          )
        }
      }
    )
    
    output$dl_heatmap_plot <- downloadHandler(
      filename = function() {
        paste0("heatmap_", Sys.Date(), ".", input$exp_fmt)
      },
      content = function(file) {
        if (!is.null(plot_storage$heatmap)) {
          ggsave(
            file,
            plot = plot_storage$heatmap,
            width = input$exp_w,
            height = input$exp_h,
            dpi = input$exp_dpi,
            device = input$exp_fmt
          )
        }
      }
    )
    
    output$dl_metrics_plot <- downloadHandler(
      filename = function() {
        paste0("metrics_", Sys.Date(), ".", input$exp_fmt)
      },
      content = function(file) {
        if (!is.null(plot_storage$metrics)) {
          ggsave(
            file,
            plot = plot_storage$metrics,
            width = input$exp_w,
            height = input$exp_h,
            dpi = input$exp_dpi,
            device = input$exp_fmt
          )
        }
      }
    )
    
    output$dl_metrics_csv <- downloadHandler(
      filename = function() {
        paste0("metrics_", Sys.Date(), ".csv")
      },
      content = function(file) {
        if (!is.null(metrics_module$metrics())) {
          write.csv(metrics_module$metrics(), file, row.names = FALSE)
        }
      }
    )
    
    output$dl_summary_csv <- downloadHandler(
      filename = function() {
        paste0("summary_", Sys.Date(), ".csv")
      },
      content = function(file) {
        if (!is.null(metrics_module$group_stats())) {
          write.csv(metrics_module$group_stats(), file, row.names = FALSE)
        }
      }
    )
    
    output$dl_processed_wide_exp <- downloadHandler(
      filename = function() {
        paste0("processed_", input$exp_dl_group, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        data_list <- data_module$data()
        if (input$exp_dl_group %in% names(data_list)) {
          write.csv(data_list[[input$exp_dl_group]], file, row.names = FALSE)
        }
      }
    )
  })
}

# Helper functions for creating export plots

create_timecourse_plot_export <- function(data_list, metrics = NULL) {
  # Combine all data
  long_list <- list()
  
  for (name in names(data_list)) {
    df <- data_list[[name]]
    time_vec <- df$Time
    cell_cols <- setdiff(names(df), "Time")
    
    for (col in cell_cols) {
      long_list[[paste0(name, "_", col)]] <- data.frame(
        Group = name,
        Cell = col,
        Time = time_vec,
        Signal = df[[col]],
        stringsAsFactors = FALSE
      )
    }
  }
  
  plot_data <- dplyr::bind_rows(long_list)
  
  # Calculate mean and SEM
  summary_data <- plot_data %>%
    group_by(Group, Time) %>%
    summarise(
      Mean = mean(Signal, na.rm = TRUE),
      SEM = sd(Signal, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  # Create plot
  ggplot(summary_data, aes(x = Time, y = Mean, color = Group, fill = Group)) +
    geom_ribbon(aes(ymin = Mean - SEM, ymax = Mean + SEM), alpha = 0.3) +
    geom_line(size = 1.5) +
    scale_color_manual(values = DEFAULT_COLORS) +
    scale_fill_manual(values = DEFAULT_COLORS) +
    labs(
      title = "Calcium Imaging Time Course",
      x = "Time (s)",
      y = "Signal (ΔF/F₀)"
    ) +
    theme_classic(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )
}

create_metrics_plot_export <- function(metrics) {
  # Select key metric for display
  metric_name <- "Peak_dFF0"
  
  if (metric_name %in% names(metrics)) {
    ggplot(metrics, aes(x = Group, y = .data[[metric_name]], fill = Group)) +
      geom_boxplot(alpha = 0.7) +
      geom_jitter(width = 0.2, alpha = 0.3) +
      scale_fill_manual(values = DEFAULT_COLORS) +
      labs(
        title = "Peak ΔF/F₀ by Group",
        x = "",
        y = "Peak ΔF/F₀"
      ) +
      theme_classic(base_size = 12) +
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold")
      )
  }
}

create_heatmap_export <- function(data_list) {
  # Use first dataset
  df <- data_list[[1]]
  
  # Convert to matrix
  cell_cols <- setdiff(names(df), "Time")
  mat <- as.matrix(df[, ..cell_cols])
  
  # Scale rows (per cell)
  mat_scaled <- t(scale(t(mat)))
  
  # Create heatmap data
  heatmap_data <- expand.grid(
    Time = df$Time,
    Cell = cell_cols
  )
  heatmap_data$Signal <- as.vector(mat_scaled)
  
  ggplot(heatmap_data, aes(x = Time, y = Cell, fill = Signal)) +
    geom_tile() +
    scale_fill_viridis_c(name = "Z-score") +
    labs(
      title = "Calcium Signal Heatmap",
      x = "Time (s)",
      y = "Cell"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.y = element_text(size = 6),
      plot.title = element_text(face = "bold")
    )
}

create_composite_figure <- function(plots, layout = "col", spacing = 0.05, add_labels = TRUE) {
  
  # Add labels if requested
  if (add_labels) {
    labels <- LETTERS[1:length(plots)]
    for (i in seq_along(plots)) {
      plots[[i]] <- plots[[i]] +
        labs(tag = labels[i]) +
        theme(plot.tag = element_text(size = 16, face = "bold"))
    }
  }
  
  # Combine plots based on layout
  if (layout == "col") {
    composite <- patchwork::wrap_plots(plots, ncol = length(plots))
  } else if (layout == "row") {
    composite <- patchwork::wrap_plots(plots, nrow = length(plots))
  } else if (layout == "grid") {
    composite <- patchwork::wrap_plots(plots, ncol = 2)
  }
  
  composite + plot_layout(guides = "collect")
}

estimate_file_size <- function(format, width, height, dpi) {
  pixels <- width * height * dpi * dpi
  
  if (format == "png") {
    # PNG ~3 bytes per pixel
    size_bytes <- pixels * 3
  } else if (format == "tiff") {
    # TIFF ~4 bytes per pixel
    size_bytes <- pixels * 4
  } else if (format %in% c("pdf", "svg")) {
    # Vector formats, size depends on complexity
    size_bytes <- 100000  # ~100KB estimate
  } else {
    size_bytes <- pixels * 3
  }
  
  # Format size
  if (size_bytes < 1024) {
    paste0(round(size_bytes), " bytes")
  } else if (size_bytes < 1024^2) {
    paste0(round(size_bytes / 1024, 1), " KB")
  } else {
    paste0(round(size_bytes / 1024^2, 1), " MB")
  }
}