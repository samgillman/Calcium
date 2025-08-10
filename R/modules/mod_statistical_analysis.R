# Statistical Analysis Module for Calcium Imaging Analysis App

# Module UI
mod_statistical_analysis_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Statistical Analysis",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        
        fluidRow(
          column(4,
            h4("Test Selection"),
            
            selectInput(
              ns("metric_for_test"),
              "Select Metric",
              choices = c(
                "Peak ΔF/F₀" = "Peak_dFF0",
                "Time to Peak" = "Time_to_Peak",
                "Rise Time" = "Rise_Time",
                "AUC" = "AUC",
                "Half Width" = "Half_Width",
                "SNR" = "SNR",
                "Response Amplitude" = "Response_Amplitude",
                "Ca²⁺ Entry Rate" = "Calcium_Entry_Rate"
              ),
              selected = "Peak_dFF0"
            ),
            
            radioButtons(
              ns("test_type"),
              "Statistical Test",
              choices = c(
                "Automatic (based on data)" = "auto",
                "Parametric" = "parametric",
                "Non-parametric" = "nonparametric",
                "Custom" = "custom"
              ),
              selected = "auto"
            ),
            
            conditionalPanel(
              condition = "input.test_type == 'parametric'",
              ns = ns,
              selectInput(
                ns("parametric_test"),
                "Test",
                choices = c(
                  "One-way ANOVA" = "anova",
                  "Two-way ANOVA" = "anova2",
                  "t-test" = "ttest",
                  "Paired t-test" = "paired_ttest",
                  "Linear regression" = "lm"
                ),
                selected = "anova"
              )
            ),
            
            conditionalPanel(
              condition = "input.test_type == 'nonparametric'",
              ns = ns,
              selectInput(
                ns("nonparametric_test"),
                "Test",
                choices = c(
                  "Kruskal-Wallis" = "kruskal",
                  "Mann-Whitney U" = "wilcox",
                  "Wilcoxon signed-rank" = "wilcox_paired",
                  "Friedman test" = "friedman"
                ),
                selected = "kruskal"
              )
            ),
            
            hr(),
            
            h4("Post-hoc Analysis"),
            
            checkboxInput(
              ns("do_posthoc"),
              "Perform post-hoc tests",
              value = TRUE
            ),
            
            conditionalPanel(
              condition = "input.do_posthoc",
              ns = ns,
              
              selectInput(
                ns("posthoc_method"),
                "Post-hoc method",
                choices = c(
                  "Tukey HSD" = "tukey",
                  "Bonferroni" = "bonferroni",
                  "Dunnett" = "dunnett",
                  "Dunn test" = "dunn",
                  "Pairwise t-tests" = "pairwise_t",
                  "Pairwise Wilcoxon" = "pairwise_wilcox"
                ),
                selected = "tukey"
              ),
              
              selectInput(
                ns("p_adjust_method"),
                "P-value adjustment",
                choices = c(
                  "None" = "none",
                  "Bonferroni" = "bonferroni",
                  "Holm" = "holm",
                  "Hochberg" = "hochberg",
                  "Hommel" = "hommel",
                  "BH (FDR)" = "BH",
                  "BY" = "BY"
                ),
                selected = "BH"
              )
            ),
            
            hr(),
            
            h4("Analysis Options"),
            
            checkboxInput(
              ns("check_assumptions"),
              "Check test assumptions",
              value = TRUE
            ),
            
            checkboxInput(
              ns("calculate_effect_size"),
              "Calculate effect sizes",
              value = TRUE
            ),
            
            checkboxInput(
              ns("bootstrap_ci"),
              "Bootstrap confidence intervals",
              value = FALSE
            ),
            
            conditionalPanel(
              condition = "input.bootstrap_ci",
              ns = ns,
              numericInput(
                ns("n_bootstrap"),
                "Bootstrap iterations",
                value = 1000,
                min = 100,
                max = 10000
              )
            ),
            
            br(),
            
            actionButton(
              ns("run_analysis"),
              "Run Statistical Analysis",
              icon = icon("calculator"),
              class = "btn-primary btn-lg"
            )
          ),
          
          column(8,
            tabsetPanel(
              id = ns("stats_tabs"),
              
              tabPanel(
                "Results Summary",
                icon = icon("chart-bar"),
                br(),
                verbatimTextOutput(ns("test_results")),
                br(),
                plotOutput(ns("results_plot"), height = "500px")
              ),
              
              tabPanel(
                "Assumptions",
                icon = icon("check-square"),
                br(),
                conditionalPanel(
                  condition = "input.check_assumptions",
                  ns = ns,
                  h4("Normality Test"),
                  verbatimTextOutput(ns("normality_results")),
                  plotOutput(ns("qq_plots"), height = "400px"),
                  
                  br(),
                  
                  h4("Homogeneity of Variance"),
                  verbatimTextOutput(ns("variance_results")),
                  plotOutput(ns("variance_plot"), height = "300px")
                )
              ),
              
              tabPanel(
                "Post-hoc",
                icon = icon("sitemap"),
                br(),
                conditionalPanel(
                  condition = "input.do_posthoc",
                  ns = ns,
                  h4("Pairwise Comparisons"),
                  DT::dataTableOutput(ns("posthoc_table")),
                  br(),
                  plotOutput(ns("posthoc_plot"), height = "500px")
                )
              ),
              
              tabPanel(
                "Effect Sizes",
                icon = icon("ruler"),
                br(),
                conditionalPanel(
                  condition = "input.calculate_effect_size",
                  ns = ns,
                  h4("Effect Size Measures"),
                  DT::dataTableOutput(ns("effect_size_table")),
                  br(),
                  plotOutput(ns("effect_size_plot"), height = "400px")
                )
              ),
              
              tabPanel(
                "Report",
                icon = icon("file-alt"),
                br(),
                h4("Statistical Report"),
                uiOutput(ns("stats_report")),
                br(),
                downloadButton(
                  ns("download_report"),
                  "Download Report (HTML)",
                  class = "btn-primary"
                )
              )
            )
          )
        )
      )
    )
  )
}

# Module Server
mod_statistical_analysis_server <- function(id, metrics_module) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values for results
    analysis_results <- reactiveValues(
      test_result = NULL,
      normality = NULL,
      variance = NULL,
      posthoc = NULL,
      effect_sizes = NULL,
      plots = list()
    )
    
    # Run analysis
    observeEvent(input$run_analysis, {
      req(metrics_module$metrics())
      
      withProgress(message = "Running statistical analysis...", value = 0, {
        
        metrics_data <- metrics_module$metrics()
        metric_name <- input$metric_for_test
        
        # Prepare data
        test_data <- metrics_data %>%
          select(Group, Cell_ID, all_of(metric_name)) %>%
          rename(value = !!metric_name) %>%
          filter(!is.na(value))
        
        incProgress(0.2, detail = "Checking assumptions...")
        
        # Check assumptions if requested
        if (input$check_assumptions) {
          analysis_results$normality <- check_normality(test_data)
          analysis_results$variance <- check_homogeneity(test_data)
        }
        
        incProgress(0.4, detail = "Running main test...")
        
        # Determine test type
        if (input$test_type == "auto") {
          test_to_use <- determine_test(test_data, analysis_results$normality)
        } else if (input$test_type == "parametric") {
          test_to_use <- input$parametric_test
        } else if (input$test_type == "nonparametric") {
          test_to_use <- input$nonparametric_test
        } else {
          test_to_use <- "anova"  # Default
        }
        
        # Run main test
        analysis_results$test_result <- run_statistical_test(test_data, test_to_use)
        
        incProgress(0.6, detail = "Running post-hoc tests...")
        
        # Post-hoc analysis if significant and requested
        if (input$do_posthoc && analysis_results$test_result$p_value < 0.05) {
          analysis_results$posthoc <- run_posthoc(
            test_data,
            method = input$posthoc_method,
            p_adjust = input$p_adjust_method
          )
        }
        
        incProgress(0.8, detail = "Calculating effect sizes...")
        
        # Effect sizes if requested
        if (input$calculate_effect_size) {
          analysis_results$effect_sizes <- calculate_effect_sizes(test_data)
        }
        
        incProgress(1.0, detail = "Generating plots...")
        
        # Generate plots
        analysis_results$plots$main <- create_stats_plot(test_data, analysis_results$test_result)
        analysis_results$plots$qq <- create_qq_plots(test_data)
        analysis_results$plots$variance <- create_variance_plot(test_data)
        
        if (!is.null(analysis_results$posthoc)) {
          analysis_results$plots$posthoc <- create_posthoc_plot(analysis_results$posthoc)
        }
        
        if (!is.null(analysis_results$effect_sizes)) {
          analysis_results$plots$effect_size <- create_effect_size_plot(analysis_results$effect_sizes)
        }
      })
    })
    
    # Display results
    output$test_results <- renderPrint({
      req(analysis_results$test_result)
      
      cat("Statistical Test Results\n")
      cat("========================\n\n")
      
      result <- analysis_results$test_result
      
      cat("Test:", result$test_name, "\n")
      cat("Statistic:", round(result$statistic, 4), "\n")
      
      if (!is.null(result$df)) {
        cat("Degrees of freedom:", result$df, "\n")
      }
      
      cat("P-value:", format.pval(result$p_value), "\n")
      
      cat("\nInterpretation:\n")
      if (result$p_value < 0.001) {
        cat("*** Highly significant difference between groups (p < 0.001)\n")
      } else if (result$p_value < 0.01) {
        cat("** Very significant difference between groups (p < 0.01)\n")
      } else if (result$p_value < 0.05) {
        cat("* Significant difference between groups (p < 0.05)\n")
      } else {
        cat("No significant difference between groups (p >= 0.05)\n")
      }
    })
    
    output$results_plot <- renderPlot({
      req(analysis_results$plots$main)
      analysis_results$plots$main
    })
    
    # Assumptions outputs
    output$normality_results <- renderPrint({
      req(analysis_results$normality)
      
      cat("Shapiro-Wilk Normality Test\n")
      cat("===========================\n\n")
      
      for (group in names(analysis_results$normality)) {
        result <- analysis_results$normality[[group]]
        cat("Group:", group, "\n")
        cat("  W statistic:", round(result$statistic, 4), "\n")
        cat("  P-value:", format.pval(result$p.value), "\n")
        cat("  Normal:", ifelse(result$p.value > 0.05, "Yes", "No"), "\n\n")
      }
    })
    
    output$qq_plots <- renderPlot({
      req(analysis_results$plots$qq)
      analysis_results$plots$qq
    })
    
    output$variance_results <- renderPrint({
      req(analysis_results$variance)
      
      cat("Levene's Test for Homogeneity of Variance\n")
      cat("==========================================\n\n")
      
      cat("F statistic:", round(analysis_results$variance$statistic, 4), "\n")
      cat("P-value:", format.pval(analysis_results$variance$p_value), "\n")
      cat("Equal variances:", ifelse(analysis_results$variance$p_value > 0.05, "Yes", "No"), "\n")
    })
    
    output$variance_plot <- renderPlot({
      req(analysis_results$plots$variance)
      analysis_results$plots$variance
    })
    
    # Post-hoc outputs
    output$posthoc_table <- DT::renderDataTable({
      req(analysis_results$posthoc)
      
      DT::datatable(
        analysis_results$posthoc,
        options = list(
          pageLength = 10,
          dom = 'Bfrtip'
        ),
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = c("diff", "lwr", "upr", "p_adj"), digits = 4) %>%
        DT::formatStyle(
          "p_adj",
          backgroundColor = styleInterval(c(0.001, 0.01, 0.05), 
                                         c("#d32f2f", "#f57c00", "#ffc107", "white"))
        )
    })
    
    output$posthoc_plot <- renderPlot({
      req(analysis_results$plots$posthoc)
      analysis_results$plots$posthoc
    })
    
    # Effect sizes outputs
    output$effect_size_table <- DT::renderDataTable({
      req(analysis_results$effect_sizes)
      
      DT::datatable(
        analysis_results$effect_sizes,
        options = list(
          pageLength = 10,
          dom = 'Bfrtip'
        ),
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = c("cohens_d", "hedges_g", "glass_delta"), digits = 3)
    })
    
    output$effect_size_plot <- renderPlot({
      req(analysis_results$plots$effect_size)
      analysis_results$plots$effect_size
    })
    
    # Statistical report
    output$stats_report <- renderUI({
      req(analysis_results$test_result)
      
      generate_stats_report(
        analysis_results,
        metric_name = input$metric_for_test
      )
    })
    
    # Download report
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("statistical_report_", Sys.Date(), ".html")
      },
      content = function(file) {
        # Generate HTML report
        report_html <- create_html_report(analysis_results, input$metric_for_test)
        writeLines(report_html, file)
      }
    )
  })
}

# Helper functions

check_normality <- function(data) {
  groups <- unique(data$Group)
  normality_results <- list()
  
  for (group in groups) {
    group_data <- data %>% filter(Group == group) %>% pull(value)
    
    if (length(group_data) >= 3) {
      normality_results[[group]] <- shapiro.test(group_data)
    }
  }
  
  normality_results
}

check_homogeneity <- function(data) {
  # Levene's test for homogeneity of variance
  # Using base R approximation
  groups <- split(data$value, data$Group)
  group_means <- sapply(groups, mean, na.rm = TRUE)
  
  # Calculate deviations from group means
  deviations <- list()
  for (i in seq_along(groups)) {
    deviations[[i]] <- abs(groups[[i]] - group_means[i])
  }
  
  # ANOVA on deviations
  dev_data <- data.frame(
    deviation = unlist(deviations),
    group = rep(names(groups), sapply(groups, length))
  )
  
  fit <- aov(deviation ~ group, data = dev_data)
  summary_fit <- summary(fit)
  
  list(
    statistic = summary_fit[[1]][["F value"]][1],
    p_value = summary_fit[[1]][["Pr(>F)"]][1]
  )
}

determine_test <- function(data, normality_results) {
  n_groups <- length(unique(data$Group))
  
  # Check if data is normal
  all_normal <- TRUE
  if (!is.null(normality_results)) {
    for (result in normality_results) {
      if (result$p.value < 0.05) {
        all_normal <- FALSE
        break
      }
    }
  }
  
  if (n_groups == 2) {
    if (all_normal) "ttest" else "wilcox"
  } else {
    if (all_normal) "anova" else "kruskal"
  }
}

run_statistical_test <- function(data, test_type) {
  groups <- unique(data$Group)
  n_groups <- length(groups)
  
  if (test_type == "anova") {
    fit <- aov(value ~ Group, data = data)
    summary_fit <- summary(fit)
    
    list(
      test_name = "One-way ANOVA",
      statistic = summary_fit[[1]][["F value"]][1],
      df = paste(summary_fit[[1]][["Df"]], collapse = ", "),
      p_value = summary_fit[[1]][["Pr(>F)"]][1]
    )
    
  } else if (test_type == "kruskal") {
    test_result <- kruskal.test(value ~ Group, data = data)
    
    list(
      test_name = "Kruskal-Wallis rank sum test",
      statistic = test_result$statistic,
      df = test_result$parameter,
      p_value = test_result$p.value
    )
    
  } else if (test_type == "ttest" && n_groups == 2) {
    group1 <- data %>% filter(Group == groups[1]) %>% pull(value)
    group2 <- data %>% filter(Group == groups[2]) %>% pull(value)
    
    test_result <- t.test(group1, group2)
    
    list(
      test_name = "Welch Two Sample t-test",
      statistic = test_result$statistic,
      df = test_result$parameter,
      p_value = test_result$p.value
    )
    
  } else if (test_type == "wilcox" && n_groups == 2) {
    group1 <- data %>% filter(Group == groups[1]) %>% pull(value)
    group2 <- data %>% filter(Group == groups[2]) %>% pull(value)
    
    test_result <- wilcox.test(group1, group2)
    
    list(
      test_name = "Wilcoxon rank sum test",
      statistic = test_result$statistic,
      df = NULL,
      p_value = test_result$p.value
    )
    
  } else {
    list(
      test_name = "Test not implemented",
      statistic = NA,
      df = NA,
      p_value = NA
    )
  }
}

run_posthoc <- function(data, method = "tukey", p_adjust = "BH") {
  
  if (method == "tukey") {
    fit <- aov(value ~ Group, data = data)
    posthoc <- TukeyHSD(fit)
    
    # Convert to data frame
    posthoc_df <- as.data.frame(posthoc$Group)
    posthoc_df$comparison <- rownames(posthoc_df)
    names(posthoc_df) <- c("diff", "lwr", "upr", "p_adj", "comparison")
    
  } else if (method %in% c("pairwise_t", "pairwise_wilcox")) {
    
    # Get all pairwise combinations
    groups <- unique(data$Group)
    comparisons <- combn(groups, 2)
    
    posthoc_df <- data.frame()
    
    for (i in 1:ncol(comparisons)) {
      group1 <- comparisons[1, i]
      group2 <- comparisons[2, i]
      
      data1 <- data %>% filter(Group == group1) %>% pull(value)
      data2 <- data %>% filter(Group == group2) %>% pull(value)
      
      if (method == "pairwise_t") {
        test_result <- t.test(data1, data2)
      } else {
        test_result <- wilcox.test(data1, data2)
      }
      
      posthoc_df <- rbind(posthoc_df, data.frame(
        comparison = paste(group1, "-", group2),
        diff = mean(data1) - mean(data2),
        lwr = if (method == "pairwise_t") test_result$conf.int[1] else NA,
        upr = if (method == "pairwise_t") test_result$conf.int[2] else NA,
        p_adj = test_result$p.value
      ))
    }
    
    # Adjust p-values
    posthoc_df$p_adj <- p.adjust(posthoc_df$p_adj, method = p_adjust)
    
  } else {
    posthoc_df <- data.frame(
      comparison = "Not implemented",
      diff = NA,
      lwr = NA,
      upr = NA,
      p_adj = NA
    )
  }
  
  posthoc_df
}

calculate_effect_sizes <- function(data) {
  groups <- unique(data$Group)
  
  if (length(groups) == 2) {
    group1 <- data %>% filter(Group == groups[1]) %>% pull(value)
    group2 <- data %>% filter(Group == groups[2]) %>% pull(value)
    
    # Cohen's d
    pooled_sd <- sqrt(((length(group1) - 1) * var(group1) + 
                      (length(group2) - 1) * var(group2)) / 
                     (length(group1) + length(group2) - 2))
    cohens_d <- (mean(group1) - mean(group2)) / pooled_sd
    
    # Hedges' g (corrected Cohen's d)
    n <- length(group1) + length(group2)
    hedges_g <- cohens_d * (1 - 3 / (4 * n - 9))
    
    # Glass's delta
    glass_delta <- (mean(group1) - mean(group2)) / sd(group2)
    
    data.frame(
      comparison = paste(groups[1], "vs", groups[2]),
      cohens_d = cohens_d,
      hedges_g = hedges_g,
      glass_delta = glass_delta,
      interpretation = interpret_effect_size(abs(cohens_d))
    )
  } else {
    # For multiple groups, calculate eta-squared
    fit <- aov(value ~ Group, data = data)
    ss_total <- sum((data$value - mean(data$value))^2)
    ss_between <- sum(summary(fit)[[1]][["Sum Sq"]][1])
    eta_squared <- ss_between / ss_total
    
    data.frame(
      comparison = "Overall",
      eta_squared = eta_squared,
      interpretation = interpret_effect_size_eta(eta_squared)
    )
  }
}

interpret_effect_size <- function(d) {
  if (d < 0.2) "Negligible"
  else if (d < 0.5) "Small"
  else if (d < 0.8) "Medium"
  else "Large"
}

interpret_effect_size_eta <- function(eta) {
  if (eta < 0.01) "Negligible"
  else if (eta < 0.06) "Small"
  else if (eta < 0.14) "Medium"
  else "Large"
}

# Plotting functions

create_stats_plot <- function(data, test_result) {
  p <- ggplot(data, aes(x = Group, y = value, fill = Group)) +
    geom_boxplot(alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.3) +
    scale_fill_manual(values = DEFAULT_COLORS) +
    labs(
      title = paste("Statistical Analysis:", test_result$test_name),
      subtitle = paste("p =", format.pval(test_result$p_value)),
      y = "Value"
    ) +
    theme_classic(base_size = 12) +
    theme(legend.position = "none")
  
  # Add significance stars
  if (test_result$p_value < 0.001) {
    p <- p + annotate("text", x = Inf, y = Inf, label = "***", 
                     hjust = 1.1, vjust = 1.1, size = 8)
  } else if (test_result$p_value < 0.01) {
    p <- p + annotate("text", x = Inf, y = Inf, label = "**", 
                     hjust = 1.1, vjust = 1.1, size = 8)
  } else if (test_result$p_value < 0.05) {
    p <- p + annotate("text", x = Inf, y = Inf, label = "*", 
                     hjust = 1.1, vjust = 1.1, size = 8)
  }
  
  p
}

create_qq_plots <- function(data) {
  groups <- unique(data$Group)
  plots <- list()
  
  for (group in groups) {
    group_data <- data %>% filter(Group == group)
    
    p <- ggplot(group_data, aes(sample = value)) +
      stat_qq() +
      stat_qq_line() +
      labs(
        title = paste("Q-Q Plot:", group),
        x = "Theoretical Quantiles",
        y = "Sample Quantiles"
      ) +
      theme_classic()
    
    plots[[group]] <- p
  }
  
  patchwork::wrap_plots(plots)
}

create_variance_plot <- function(data) {
  ggplot(data, aes(x = Group, y = value, fill = Group)) +
    geom_violin(alpha = 0.7) +
    geom_boxplot(width = 0.2, alpha = 0.9) +
    scale_fill_manual(values = DEFAULT_COLORS) +
    labs(
      title = "Variance Comparison",
      y = "Value"
    ) +
    theme_classic() +
    theme(legend.position = "none")
}

create_posthoc_plot <- function(posthoc_df) {
  posthoc_df$significant <- posthoc_df$p_adj < 0.05
  
  ggplot(posthoc_df, aes(x = comparison, y = diff)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
    geom_point(aes(color = significant), size = 4) +
    scale_color_manual(values = c("FALSE" = "gray50", "TRUE" = "red")) +
    coord_flip() +
    labs(
      title = "Pairwise Comparisons",
      x = "",
      y = "Mean Difference",
      color = "Significant"
    ) +
    theme_classic()
}

create_effect_size_plot <- function(effect_df) {
  if ("cohens_d" %in% names(effect_df)) {
    ggplot(effect_df, aes(x = comparison, y = cohens_d, fill = interpretation)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c(
        "Negligible" = "gray80",
        "Small" = "lightblue",
        "Medium" = "orange",
        "Large" = "red"
      )) +
      labs(
        title = "Effect Sizes (Cohen's d)",
        x = "",
        y = "Cohen's d",
        fill = "Magnitude"
      ) +
      theme_classic()
  } else {
    ggplot()  # Empty plot if no effect sizes
  }
}

generate_stats_report <- function(results, metric_name) {
  tagList(
    h5("Analysis Summary"),
    
    p(strong("Metric analyzed:"), metric_name),
    p(strong("Statistical test:"), results$test_result$test_name),
    p(strong("Test statistic:"), round(results$test_result$statistic, 4)),
    p(strong("P-value:"), format.pval(results$test_result$p_value)),
    
    if (!is.null(results$normality)) {
      tagList(
        hr(),
        h5("Assumptions"),
        p("Normality tests performed for each group (Shapiro-Wilk test)"),
        p("Homogeneity of variance tested (Levene's test)")
      )
    },
    
    if (!is.null(results$posthoc)) {
      tagList(
        hr(),
        h5("Post-hoc Analysis"),
        p("Pairwise comparisons performed with", 
          ifelse(is.null(results$posthoc), "no", nrow(results$posthoc)),
          "comparisons"),
        p("P-value adjustment method: specified by user")
      )
    },
    
    if (!is.null(results$effect_sizes)) {
      tagList(
        hr(),
        h5("Effect Sizes"),
        p("Effect size measures calculated to quantify practical significance")
      )
    }
  )
}

create_html_report <- function(results, metric_name) {
  # Generate a complete HTML report
  # This would create a full HTML document with all results
  # Simplified version here
  paste0(
    "<html><head><title>Statistical Report</title></head><body>",
    "<h1>Statistical Analysis Report</h1>",
    "<h2>Metric: ", metric_name, "</h2>",
    "<p>Test: ", results$test_result$test_name, "</p>",
    "<p>P-value: ", format.pval(results$test_result$p_value), "</p>",
    "</body></html>"
  )
}