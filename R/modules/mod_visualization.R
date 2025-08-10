# Visualization Module for Calcium Imaging Analysis App

# Module UI
mod_visualization_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
        h3("Data Visualization", icon("chart-area")),
        hr()
      )
    ),
    
    fluidRow(
      # Left column - Controls
      column(3,
        wellPanel(
          h4("Plot Controls"),
          
          # Plot type selector
          radioButtons(
            ns("plot_type"),
            "Plot Type",
            choices = list(
              "Time Course" = "timecourse",
              "Average Response" = "average",
              "Heatmap" = "heatmap",
              "3D Surface" = "surface3d",
              "Interactive" = "interactive"
            ),
            selected = "timecourse"
          ),
          
          hr(),
          
          # Data selection
          h4("Data Selection"),
          
          selectInput(
            ns("selected_group"),
            "Select Group",
            choices = NULL,
            multiple = TRUE,
            width = "100%"
          ),
          
          selectInput(
            ns("selected_cells"),
            "Select Cells",
            choices = NULL,
            multiple = TRUE,
            width = "100%"
          ),
          
          actionButton(
            ns("select_all_cells"),
            "Select All",
            class = "btn-sm"
          ),
          
          actionButton(
            ns("clear_cells"),
            "Clear",
            class = "btn-sm"
          ),
          
          hr(),
          
          # Display options
          h4("Display Options"),
          
          checkboxInput(
            ns("show_mean"),
            "Show Mean ± SEM",
            value = TRUE
          ),
          
          checkboxInput(
            ns("show_individual"),
            "Show Individual Traces",
            value = FALSE
          ),
          
          checkboxInput(
            ns("show_legend"),
            "Show Legend",
            value = TRUE
          ),
          
          checkboxInput(
            ns("show_grid"),
            "Show Grid",
            value = TRUE
          ),
          
          hr(),
          
          # Color options
          h4("Color Settings"),
          
          radioButtons(
            ns("color_scheme"),
            "Color Scheme",
            choices = list(
              "Default" = "default",
              "Viridis" = "viridis",
              "Heat" = "heat",
              "Cool" = "cool",
              "Custom" = "custom"
            ),
            selected = "default"
          ),
          
          conditionalPanel(
            condition = "input.color_scheme == 'custom'",
            ns = ns,
            colourInput(
              ns("custom_color1"),
              "Color 1",
              value = "#1f77b4"
            ),
            colourInput(
              ns("custom_color2"),
              "Color 2",
              value = "#ff7f0e"
            )
          ),
          
          sliderInput(
            ns("line_width"),
            "Line Width",
            min = 0.5,
            max = 3,
            value = 1,
            step = 0.5
          ),
          
          sliderInput(
            ns("transparency"),
            "Transparency",
            min = 0.1,
            max = 1,
            value = 0.8,
            step = 0.1
          )
        )
      ),
      
      # Main plot area
      column(9,
        tabsetPanel(
          id = ns("viz_tabs"),
          
          # Main plot tab
          tabPanel(
            "Main Plot",
            icon = icon("chart-line"),
            br(),
            conditionalPanel(
              condition = "input.plot_type != 'interactive'",
              ns = ns,
              plotOutput(ns("main_plot"), height = "600px") %>%
                withSpinner(type = 4)
            ),
            conditionalPanel(
              condition = "input.plot_type == 'interactive'",
              ns = ns,
              plotlyOutput(ns("interactive_plot"), height = "600px") %>%
                withSpinner(type = 4)
            )
          ),
          
          # Comparison tab
          tabPanel(
            "Group Comparison",
            icon = icon("balance-scale"),
            br(),
            plotOutput(ns("comparison_plot"), height = "600px") %>%
              withSpinner(type = 4)
          ),
          
          # Animation tab
          tabPanel(
            "Animation",
            icon = icon("play-circle"),
            br(),
            actionButton(
              ns("play_animation"),
              "Play Animation",
              icon = icon("play"),
              class = "btn-success"
            ),
            br(), br(),
            plotOutput(ns("animation_plot"), height = "500px") %>%
              withSpinner(type = 4)
          ),
          
          # Export tab
          tabPanel(
            "Export",
            icon = icon("download"),
            br(),
            h4("Export Options"),
            
            fluidRow(
              column(6,
                selectInput(
                  ns("export_format"),
                  "Format",
                  choices = list(
                    "PNG" = "png",
                    "PDF" = "pdf",
                    "SVG" = "svg",
                    "TIFF" = "tiff"
                  ),
                  selected = "png"
                )
              ),
              column(6,
                numericInput(
                  ns("export_dpi"),
                  "DPI",
                  value = 300,
                  min = 72,
                  max = 600,
                  step = 50
                )
              )
            ),
            
            fluidRow(
              column(6,
                numericInput(
                  ns("export_width"),
                  "Width (inches)",
                  value = 8,
                  min = 4,
                  max = 20,
                  step = 1
                )
              ),
              column(6,
                numericInput(
                  ns("export_height"),
                  "Height (inches)",
                  value = 6,
                  min = 3,
                  max = 15,
                  step = 1
                )
              )
            ),
            
            br(),
            
            downloadButton(
              ns("download_plot"),
              "Download Plot",
              class = "btn-primary"
            ),
            
            br(), br(),
            
            h4("Preview"),
            plotOutput(ns("export_preview"), height = "400px")
          )
        )
      )
    )
  )
}

# Module Server
mod_visualization_server <- function(id, preprocessed_module, metrics_module) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      current_plot = NULL,
      animation_frame = 1,
      animation_playing = FALSE
    )
    
    # Update group and cell selections
    observe({
      data <- preprocessed_module$data()
      if (!is.null(data)) {
        # Update group selector
        updateSelectInput(
          session,
          "selected_group",
          choices = names(data),
          selected = names(data)[1]
        )
        
        # Update cell selector
        if (length(data) > 0) {
          first_df <- data[[1]]
          cell_cols <- setdiff(names(first_df), "Time")
          updateSelectInput(
            session,
            "selected_cells",
            choices = cell_cols,
            selected = cell_cols[1:min(5, length(cell_cols))]
          )
        }
      }
    })
    
    # Select all cells
    observeEvent(input$select_all_cells, {
      data <- preprocessed_module$data()
      if (!is.null(data) && length(data) > 0) {
        first_df <- data[[1]]
        cell_cols <- setdiff(names(first_df), "Time")
        updateSelectInput(
          session,
          "selected_cells",
          selected = cell_cols
        )
      }
    })
    
    # Clear cell selection
    observeEvent(input$clear_cells, {
      updateSelectInput(
        session,
        "selected_cells",
        selected = character(0)
      )
    })
    
    # Generate color palette
    get_colors <- reactive({
      n_colors <- length(input$selected_cells)
      
      if (input$color_scheme == "default") {
        colors <- DEFAULT_COLORS[1:min(n_colors, length(DEFAULT_COLORS))]
        if (n_colors > length(DEFAULT_COLORS)) {
          colors <- c(colors, rainbow(n_colors - length(DEFAULT_COLORS)))
        }
      } else if (input$color_scheme == "viridis") {
        colors <- viridis::viridis(n_colors)
      } else if (input$color_scheme == "heat") {
        colors <- heat.colors(n_colors)
      } else if (input$color_scheme == "cool") {
        colors <- topo.colors(n_colors)
      } else if (input$color_scheme == "custom") {
        colors <- colorRampPalette(c(input$custom_color1, input$custom_color2))(n_colors)
      }
      
      colors
    })
    
    # Main plot
    output$main_plot <- renderPlot({
      req(preprocessed_module$data(), input$selected_cells)
      
      plot <- create_main_plot(
        data = preprocessed_module$data(),
        selected_groups = input$selected_group,
        selected_cells = input$selected_cells,
        plot_type = input$plot_type,
        colors = get_colors(),
        show_mean = input$show_mean,
        show_individual = input$show_individual,
        show_legend = input$show_legend,
        show_grid = input$show_grid,
        line_width = input$line_width,
        alpha = input$transparency
      )
      
      values$current_plot <- plot
      plot
    })
    
    # Interactive plot
    output$interactive_plot <- renderPlotly({
      req(preprocessed_module$data(), input$selected_cells)
      
      create_interactive_plot(
        data = preprocessed_module$data(),
        selected_groups = input$selected_group,
        selected_cells = input$selected_cells,
        colors = get_colors()
      )
    })
    
    # Comparison plot
    output$comparison_plot <- renderPlot({
      req(preprocessed_module$data())
      
      create_comparison_plot(
        data = preprocessed_module$data(),
        metrics = metrics_module$metrics(),
        show_mean = input$show_mean,
        alpha = input$transparency
      )
    })
    
    # Animation
    observeEvent(input$play_animation, {
      values$animation_playing <- !values$animation_playing
      
      if (values$animation_playing) {
        updateActionButton(
          session,
          "play_animation",
          label = "Stop Animation",
          icon = icon("stop")
        )
      } else {
        updateActionButton(
          session,
          "play_animation",
          label = "Play Animation",
          icon = icon("play")
        )
      }
    })
    
    # Animation plot
    output$animation_plot <- renderPlot({
      req(preprocessed_module$data())
      
      if (values$animation_playing) {
        invalidateLater(100)  # Update every 100ms
        values$animation_frame <- values$animation_frame + 1
        
        data <- preprocessed_module$data()[[1]]
        max_frame <- nrow(data)
        
        if (values$animation_frame > max_frame) {
          values$animation_frame <- 1
        }
      }
      
      create_animation_frame(
        data = preprocessed_module$data(),
        frame = values$animation_frame,
        selected_cells = input$selected_cells,
        colors = get_colors()
      )
    })
    
    # Export preview
    output$export_preview <- renderPlot({
      values$current_plot
    })
    
    # Download handler
    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("calcium_plot_", Sys.Date(), ".", input$export_format)
      },
      content = function(file) {
        ggsave(
          file,
          plot = values$current_plot,
          width = input$export_width,
          height = input$export_height,
          dpi = input$export_dpi,
          device = input$export_format
        )
      }
    )
  })
}

# Plotting functions

create_main_plot <- function(data, selected_groups, selected_cells, plot_type,
                            colors, show_mean, show_individual, show_legend,
                            show_grid, line_width, alpha) {
  
  # Prepare data
  plot_data <- prepare_plot_data(data, selected_groups, selected_cells)
  
  if (plot_type == "timecourse") {
    p <- create_timecourse_plot(
      plot_data, colors, show_mean, show_individual,
      line_width, alpha
    )
  } else if (plot_type == "average") {
    p <- create_average_plot(
      plot_data, colors, show_mean, line_width, alpha
    )
  } else if (plot_type == "heatmap") {
    p <- create_heatmap_plot(plot_data)
  } else if (plot_type == "surface3d") {
    p <- create_surface_plot(plot_data)
  }
  
  # Apply theme and options
  p <- p + APP_THEME
  
  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  }
  
  if (!show_grid) {
    p <- p + theme(panel.grid = element_blank())
  }
  
  p
}

prepare_plot_data <- function(data, selected_groups, selected_cells) {
  # Combine selected data
  plot_list <- list()
  
  for (group in selected_groups) {
    if (group %in% names(data)) {
      df <- data[[group]]
      
      # Select cells
      cols_to_keep <- c("Time", intersect(selected_cells, names(df)))
      df_subset <- df[, ..cols_to_keep]
      
      # Convert to long format
      df_long <- pivot_longer(
        df_subset,
        cols = -Time,
        names_to = "Cell",
        values_to = "Signal"
      )
      
      df_long$Group <- group
      plot_list[[group]] <- df_long
    }
  }
  
  dplyr::bind_rows(plot_list)
}

create_timecourse_plot <- function(data, colors, show_mean, show_individual,
                                   line_width, alpha) {
  
  p <- ggplot(data, aes(x = Time))
  
  if (show_individual) {
    p <- p + geom_line(
      aes(y = Signal, color = Cell, group = interaction(Cell, Group)),
      size = line_width * 0.5,
      alpha = alpha * 0.5
    )
  }
  
  if (show_mean) {
    mean_data <- data %>%
      group_by(Time, Group) %>%
      summarise(
        Mean = mean(Signal, na.rm = TRUE),
        SEM = sd(Signal, na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      )
    
    p <- p +
      geom_ribbon(
        data = mean_data,
        aes(ymin = Mean - SEM, ymax = Mean + SEM, fill = Group),
        alpha = 0.3
      ) +
      geom_line(
        data = mean_data,
        aes(y = Mean, color = Group),
        size = line_width
      )
  }
  
  p + 
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    labs(
      title = "Calcium Imaging Time Course",
      x = "Time (s)",
      y = "Signal (ΔF/F₀)"
    )
}

create_average_plot <- function(data, colors, show_mean, line_width, alpha) {
  
  # Calculate average response
  avg_data <- data %>%
    group_by(Time, Group) %>%
    summarise(
      Mean = mean(Signal, na.rm = TRUE),
      SEM = sd(Signal, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  ggplot(avg_data, aes(x = Time, y = Mean, color = Group, fill = Group)) +
    geom_ribbon(aes(ymin = Mean - SEM, ymax = Mean + SEM), alpha = 0.3) +
    geom_line(size = line_width) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    labs(
      title = "Average Calcium Response",
      x = "Time (s)",
      y = "Mean Signal ± SEM (ΔF/F₀)"
    )
}

create_heatmap_plot <- function(data) {
  
  # Prepare matrix for heatmap
  heatmap_data <- data %>%
    pivot_wider(names_from = Time, values_from = Signal) %>%
    select(-Group) %>%
    column_to_rownames("Cell") %>%
    as.matrix()
  
  # Create heatmap
  ggplot(data, aes(x = Time, y = Cell, fill = Signal)) +
    geom_tile() +
    scale_fill_viridis_c(name = "Signal\n(ΔF/F₀)") +
    labs(
      title = "Calcium Signal Heatmap",
      x = "Time (s)",
      y = "Cell"
    ) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 6))
}

create_surface_plot <- function(data) {
  # Placeholder for 3D surface plot
  # Would use plotly or rgl for actual implementation
  ggplot(data, aes(x = Time, y = as.numeric(factor(Cell)), fill = Signal)) +
    geom_tile() +
    scale_fill_viridis_c(name = "Signal") +
    labs(
      title = "3D Surface View",
      x = "Time (s)",
      y = "Cell Index"
    )
}

create_interactive_plot <- function(data, selected_groups, selected_cells, colors) {
  
  plot_data <- prepare_plot_data(data, selected_groups, selected_cells)
  
  p <- plot_ly()
  
  for (i in seq_along(selected_cells)) {
    cell_data <- plot_data %>%
      filter(Cell == selected_cells[i])
    
    p <- p %>%
      add_trace(
        data = cell_data,
        x = ~Time,
        y = ~Signal,
        type = 'scatter',
        mode = 'lines',
        name = selected_cells[i],
        line = list(color = colors[i])
      )
  }
  
  p %>%
    layout(
      title = "Interactive Calcium Imaging Data",
      xaxis = list(title = "Time (s)"),
      yaxis = list(title = "Signal (ΔF/F₀)"),
      hovermode = 'x unified'
    )
}

create_comparison_plot <- function(data, metrics, show_mean, alpha) {
  
  if (is.null(metrics)) {
    return(ggplot() + 
             labs(title = "Calculate metrics first for comparison"))
  }
  
  # Create comparison of key metrics
  comparison_data <- metrics %>%
    select(Group, Peak_dFF0, AUC, Half_Width, SNR) %>%
    pivot_longer(cols = -Group, names_to = "Metric", values_to = "Value")
  
  ggplot(comparison_data, aes(x = Metric, y = Value, fill = Group)) +
    geom_boxplot(alpha = alpha) +
    facet_wrap(~Metric, scales = "free_y") +
    scale_fill_manual(values = DEFAULT_COLORS) +
    labs(
      title = "Group Comparison of Key Metrics",
      x = "",
      y = "Value"
    ) +
    theme(axis.text.x = element_blank())
}

create_animation_frame <- function(data, frame, selected_cells, colors) {
  
  # Get data up to current frame
  df <- data[[1]]
  
  if (frame > nrow(df)) {
    frame <- nrow(df)
  }
  
  # Subset data
  frame_data <- df[1:frame, ]
  
  # Prepare for plotting
  plot_data <- frame_data %>%
    select(Time, all_of(selected_cells)) %>%
    pivot_longer(cols = -Time, names_to = "Cell", values_to = "Signal")
  
  ggplot(plot_data, aes(x = Time, y = Signal, color = Cell)) +
    geom_line(size = 1) +
    scale_color_manual(values = colors) +
    labs(
      title = paste("Time:", round(df$Time[frame], 2), "s"),
      x = "Time (s)",
      y = "Signal (ΔF/F₀)"
    ) +
    xlim(range(df$Time)) +
    APP_THEME
}