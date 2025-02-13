#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/

# Define server logic required to draw a histogram

function(input, output, session) {
  
  # Reactive data based on selected Order Class and Sales Percentile Range
  plot_data <- reactive({
    # Start with all data
    plot_data <- sales
    
    # Filter based on Order Class selection
    if (input$S_Cons_Order_Class != "All Sales") {
      plot_data <- plot_data |> 
        filter(S_Cons_Order_Class == input$S_Cons_Order_Class)
    }
    
    # Get lower and upper bounds from the percentile slider
    lower_percentile <- input$slider2[1]
    upper_percentile <- input$slider2[2]
    
    # Calculate the cutoffs for the range
    lower_cutoff <- quantile(plot_data$`Total Rev`, lower_percentile / 100, na.rm = TRUE)
    upper_cutoff <- quantile(plot_data$`Total Rev`, upper_percentile / 100, na.rm = TRUE)
    
    # Filter data based on the selected percentile range
    plot_data <- plot_data |> 
      filter(`Total Rev` >= lower_cutoff & `Total Rev` <= upper_cutoff)
    
    return(plot_data)
  })
  
  # GDP Sector Tab: Distribution of Total Rev for GDP (Filtered by Percentile Range)
  output$distPlotGDP <- renderPlot({
    req(input$tabs == "Quarterly Economic Indicators")  # Ensure the tab is active
    
    title <- glue("Distribution of {input$S_Cons_Order_Class} within Sales Percentile Range: {input$slider2[1]}% - {input$slider2[2]}%")
    
    plot_data() |> 
      ggplot(aes(x = `Total Rev`)) +  
      geom_histogram(bins = 50, fill = "#4C9F70", color = "white", alpha = 0.7) +  # Color with border
      ggtitle(title) +
      theme_minimal(base_size = 15) +  # Clean theme
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16, color = "#333333"),
        axis.title.x = element_text(size = 14, color = "#333333"),
        axis.title.y = element_text(size = 14, color = "#333333"),
        axis.text.x = element_text(size = 12, color = "#333333"),
        axis.text.y = element_text(size = 12, color = "#333333"),
        panel.grid.major = element_line(color = "#eeeeee"),  # Lighter grid lines
        panel.grid.minor = element_blank()  # No minor grid lines
      ) + 
      scale_x_continuous(
        labels = scales::comma  # Format x-axis
      ) +
      scale_y_continuous(labels = scales::comma)  # Format y-axis numbers with commas
  })
  
  # GDP Sector: Data for the table and scatter plot (Merged Data)
  merged_data_gdp <- reactive({
    # agg
    qtr_rev_agg_0 <- plot_data() |> 
      group_by(`Year-Qtr`) |> 
      summarize(
        Total_Rev = sum(`Total Rev`, na.rm = TRUE), 
        .groups = "drop"
      )
    
    gdp$`Year-Qtr` <- as.character(gdp$`Year-Qtr`)
    qtr_rev_agg_0$`Year-Qtr` <- as.character(qtr_rev_agg_0$`Year-Qtr`)
    
    merged_data_gdp <- gdp |> 
      inner_join(qtr_rev_agg_0, by = "Year-Qtr")
    
    # Second aggregation(qtr_rev_agg_1)
    qtr_rev_agg_1 <- plot_data() |> 
      group_by(`Year-Qtr_Offset1`) |> 
      summarize(
        Total_Rev = sum(`Total Rev`, na.rm = TRUE), 
        .groups = "drop"
      ) %>%
      rename(Total_Rev_Offset1 = Total_Rev)
    
    merged_data_gdp$`Year-Qtr_Offset1` <- as.character(merged_data_gdp$`Year-Qtr_Offset1`)
    qtr_rev_agg_1$`Year-Qtr_Offset1` <- as.character(qtr_rev_agg_1$`Year-Qtr_Offset1`)
    
    merged_data_gdp <- merged_data_gdp |> 
      inner_join(qtr_rev_agg_1, by = "Year-Qtr_Offset1")
    
    # Third aggregation step (qtr_rev_agg_2) for merged data
    qtr_rev_agg_2 <- plot_data() |> 
      group_by(`Year-Qtr_Offset2`) |> 
      summarize(
        Total_Rev = sum(`Total Rev`, na.rm = TRUE), 
        .groups = "drop"
      ) |> 
      rename(Total_Rev_Offset2 = Total_Rev)
    
    merged_data_gdp$`Year-Qtr_Offset2` <- as.character(merged_data_gdp$`Year-Qtr_Offset2`)
    qtr_rev_agg_2$`Year-Qtr_Offset2` <- as.character(qtr_rev_agg_2$`Year-Qtr_Offset2`)
    
    merged_data_gdp <- merged_data_gdp |> 
      inner_join(qtr_rev_agg_2, by = "Year-Qtr_Offset2")
    
    return(merged_data_gdp)
  })
  
  # GDP Sector Tab: Scatter Plot (using merged_data_gdp)
  output$scatterPlotGDP <- renderPlot({
    # Ensure merged data is available
    merged_data_gdp_scatter <- merged_data_gdp()
    
    # Get selected variables for X and Y axes
    x_var <- input$scatter_x_var_gdp
    y_var <- input$scatter_y_var_gdp
    
    # Plot the selected X and Y axis variables
    ggplot(merged_data_gdp_scatter, aes_string(x = x_var, y = y_var)) +
      geom_point(color = "darkblue") +
      labs(
        title = glue("{x_var} vs {y_var}"),
        x = x_var,
        y = y_var
      ) +
      theme_minimal() +
      geom_smooth(method = lm, color = "forestgreen") +
      theme(plot.title = element_text(face = "bold"))
  })
  
  # GDP Sector Tab: Table with merged data
  
  gdp_exclude <- c("Year","Year-Qtr-Float","Year-Qtr_Offset1", "Year-Qtr_Offset2")
  
  output$AggregatedDataTableGDP <- DT::renderDataTable({
    DT::datatable(
      merged_data_gdp() |> 
        select(-all_of(gdp_exclude)) |> 
        mutate(across(where(is.numeric), ~ round(.x, 2))) |> 
        mutate(across(where(is.numeric), ~ scales::comma(.x))),  # Add commas to numeric values
      options = list(
        pageLength = 5,
        scrollX = TRUE   
      ),
      width = "100%"
    )
  })
  
  
  # GDP Tab: Linear Regression Summary
  
  output$lmSummaryGDP <- renderPrint({
    req(input$scatter_x_var_gdp, input$scatter_y_var_gdp)  # Ensure both variables are selected
    
    # Get merged data (this will trigger the reactive block to compute it)
    merged_data_gdp <- merged_data_gdp()
    
    lm_model_gdp <- lm(as.formula(paste(input$scatter_y_var_gdp, "~", input$scatter_x_var_gdp)), data = merged_data_gdp)
    
    summary(lm_model_gdp)
  })
  
  # Economic Indicator Tab: Distribution of Total Rev (Filtered by Percentile Range)
  output$distPlot <- renderPlot({
    title <- glue("Distribution of {input$S_Cons_Order_Class} within Sales Percentile Range: {input$slider2[1]}% - {input$slider2[2]}%")
    
    plot_data() |> 
      ggplot(aes(x = `Total Rev`)) +  
      geom_histogram(bins = 50, fill = "#4682B4", color = "white", alpha = 0.7) +  # Color with border
      ggtitle(title) +
      theme_minimal(base_size = 15) +  # Clean theme
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16, color = "#333333"),
        axis.title.x = element_text(size = 14, color = "#333333"),
        axis.title.y = element_text(size = 14, color = "#333333"),
        axis.text.x = element_text(size = 12, color = "#333333"),
        axis.text.y = element_text(size = 12, color = "#333333"),
        panel.grid.major = element_line(color = "#eeeeee"),  # Lighter grid lines
        panel.grid.minor = element_blank()  # No minor grid lines
      ) + 
      scale_x_continuous(
        #breaks = seq(0, max(plot_data()$`Total Rev`, na.rm = TRUE), by = 2500),
        labels = scales::comma  # Format x-axis numbers with commas for better readability
      ) +
      scale_y_continuous(labels = scales::comma)  # Format y-axis numbers with commas
  })
  
  # Economic Indicator Tab: Line Plot of Total Rev by Month (Grouped by Year)
  output$linePlot <- renderPlot({
    title <- glue("Line Plot of ({input$S_Cons_Order_Class}) by Month within Sales Percentile Range: {input$slider2[1]}% - {input$slider2[2]}%")
    
    # Group data by Year, Month, and Order Class
    aggregated_data <- plot_data() |> 
      group_by(Year, Month, S_Cons_Order_Class) %>%
      summarize(Total_Sales = sum(`Total Rev`, na.rm = TRUE), .groups = "drop")
    
    ggplot(aggregated_data, aes(x = Month, y = Total_Sales, group = S_Cons_Order_Class, color = as.factor(S_Cons_Order_Class))) + 
      geom_line() +
      geom_point() +
      labs(
        title = title,
        y = "Total Rev",
        x = "Month",
        color = "Order Class" 
      ) +
      theme(
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 14, face = "bold", color = "darkblue")  # Customize Year label (facet label)
      ) +
      facet_wrap(~ Year, scales = "free_y")  # Add facet grid by Year and set free y-axis scale
  })
  
  # Economic Indicator Tab: Scatter Plot with Dynamic X and Y-Axis Variables
  output$scatterPlot <- renderPlot({
    # Create month_rev from plot_data, filtered by Order Class and Percentile Range
    month_rev <- plot_data() |> 
      group_by(`Year-Month`) |> 
      summarize(Total_Rev = sum(`Total Rev`, na.rm = TRUE), .groups = "drop")
    
    # Merge month_rev with non_gdp dataset using Year-Month as common key
    merged_data_nongdp <- non_gdp |> 
      inner_join(month_rev, by = "Year-Month", suffix = c("_non_gdp", "_month_rev"))
    merged_data_nongdp <- merged_data_nongdp |> 
      inner_join(month_rev, by = c("Year-Month_Offset1" = "Year-Month"), suffix = c("", "_Offset1")) %>%
      rename(Total_Rev_Offset1 = Total_Rev_Offset1)
    merged_data_nongdp <- merged_data_nongdp |> 
      inner_join(month_rev, by = c("Year-Month_Offset2" = "Year-Month"), suffix = c("", "_Offset2")) %>%
      rename(Total_Rev_Offset2 = Total_Rev_Offset2)
    
    # Get selected variables for X and Y axes
    x_var <- input$scatter_x_var
    y_var <- input$scatter_y_var
    
    # Plot the selected X and Y axis variables
    ggplot(merged_data_nongdp, aes_string(x = x_var, y = y_var)) +
      geom_point(color = "darkblue") +
      labs(
        title = glue("{x_var} vs {y_var}"),
        x = x_var,
        y = y_var
      ) +
      theme_minimal() +
      geom_smooth(method = lm) +
      theme(plot.title = element_text(face = "bold"))
  })
  
  # Economic Indicator Tab: Underlying Data Table for Scatter Plot
  output$aggregatedDataTable <- DT::renderDataTable({
    month_rev <- plot_data() |> 
      group_by(`Year-Month`) |> 
      summarize(Total_Rev = sum(`Total Rev`, na.rm = TRUE), .groups = "drop")
    
    merged_data_nongdp <- non_gdp |> 
      inner_join(month_rev, by = "Year-Month", suffix = c("_non_gdp", "_month_rev"))
    merged_data_nongdp <- merged_data_nongdp |> 
      inner_join(month_rev, by = c("Year-Month_Offset1" = "Year-Month"), suffix = c("", "_Offset1")) %>%
      rename(Total_Rev_Offset1 = Total_Rev_Offset1)
    merged_data_nongdp <- merged_data_nongdp |> 
      inner_join(month_rev, by = c("Year-Month_Offset2" = "Year-Month"), suffix = c("", "_Offset2")) %>%
      rename(Total_Rev_Offset2 = Total_Rev_Offset2)
    
    nongdp__exclude_columns <- c("Year-Month_Offset1", "Year-Month_Offset2")
    
    merged_data_nongdp <- merged_data_nongdp |> 
      select(-all_of(nongdp__exclude_columns))
    
    DT::datatable(
      merged_data_nongdp,  
      options = list(pageLength = 5, scrollX = TRUE), 
      width = "100%"  
    )
  })

  # Economic Indicator Tab: Linear Regression Summary
  output$lmSummary <- renderPrint({
    req(input$scatter_x_var, input$scatter_y_var)  # Ensure both variables are selected
    
    # Get the selected variables for X and Y from the user input
    x_var <- input$scatter_x_var
    y_var <- input$scatter_y_var
    
    # Create month-level revenue summary
    month_rev <- plot_data() |> 
      group_by(`Year-Month`) |> 
      summarize(Total_Rev = sum(`Total Rev`, na.rm = TRUE), .groups = "drop")
    
    # Merge the data for revenue and GDP offsets
    merged_data <- non_gdp |> 
      inner_join(month_rev, by = "Year-Month", suffix = c("_non_gdp", "_month_rev")) %>%
      inner_join(month_rev, by = c("Year-Month_Offset1" = "Year-Month"), suffix = c("", "_Offset1")) %>%
      rename(Total_Rev_Offset1 = Total_Rev_Offset1) %>%
      inner_join(month_rev, by = c("Year-Month_Offset2" = "Year-Month"), suffix = c("", "_Offset2")) %>%
      rename(Total_Rev_Offset2 = Total_Rev_Offset2)
    
    # lm model
    lm_model <- lm(as.formula(paste(y_var, "~", x_var)), data = merged_data)
    
    # Return lm model
    summary(lm_model)
  })
  
  # Quarterly ARIMA Tab: Arima Model Results_Plot
  output$arimaPlot <- renderPlot({
    req(input$scatter_x_reg)  # Ensure both variables are selected
    
    # Get the selected variables for X and Y from the user input
    x_var <- input$scatter_x_reg
    
    # Create qtr-level revenue summary
    qtr_rev_arima <- plot_data() |> 
      group_by(`Year-Qtr`) |> 
      summarize(
        Total_Rev = sum(`Total Rev`, na.rm = TRUE), 
        .groups = "drop"
      ) 
    
    gdp_qtr_rev <- gdp |> 
      inner_join(qtr_rev_arima, by = "Year-Qtr")
    
    cutoff_year = 2023
    
    # Use data before the cutoff year to fit the model
    model_tib_gdp <- gdp_qtr_rev |> 
      arrange(Year) |> 
      filter(Year < cutoff_year)
    
    total_rev_ts <- ts(model_tib_gdp$Total_Rev, start = min(model_tib_gdp$Year), frequency = 4)
    gdp_total_ts <- ts(model_tib_gdp[[input$scatter_x_reg]], start = min(model_tib_gdp$Year), frequency = 4)
    
    # Fit Quarterly ARIMA model with GDP as an external regressor
    gdp_sales_model <- auto.arima(
      total_rev_ts, 
      xreg = log(gdp_total_ts)
    )
    
    future_GDP <- gdp_qtr_rev |> 
      arrange(Year) |> 
      filter(Year >= cutoff_year) |>
      filter(Year < 2024) |> 
      pull(input$scatter_x_reg) # Dynamic x
    
    forecasted_values <- forecast(gdp_sales_model, 
                                  xreg = log(future_GDP)
    )
    
    start_year <- 2023
    years <- seq(start_year, start_year + length(future_GDP) / 4 - 1)
    quarters <- rep(c("Q1", "Q2", "Q3", "Q4"), length.out = length(future_GDP))
    Year_Qtr <- paste(rep(years, each = 4), quarters, sep = "-")
    
    forecast_tib_qtr <- tibble(
      Year_Qtr,
      Total_Rev_Exp_Pred = forecasted_values$mean,
      Lower_80 = forecasted_values$lower[,1],
      Upper_80 = forecasted_values$upper[,1],
      Lower_95 = forecasted_values$lower[,2],
      Upper_95 = forecasted_values$upper[,2]
    )
    
    Year_Qtr_Float <- rep(years, each = 4) + match(quarters, c("Q1", "Q2", "Q3", "Q4")) / 4
    
    forecast_tib_qtr$`Year-Qtr-Float` <- Year_Qtr_Float
    
    forecast_tib_qtr <- forecast_tib_qtr |> 
      mutate(`Year-Qtr-Float` = Year_Qtr_Float)
    
    print(gdp_qtr_rev)
    
    ggplot() +
      geom_line(data = gdp_qtr_rev |> 
                  filter(Year < 2024),
                aes(x = `Year-Qtr-Float`, y = Total_Rev)
      ) +
      geom_line(data = forecast_tib_qtr, 
                aes(x = Year_Qtr_Float, y = Total_Rev_Exp_Pred ), 
                linetype = "dashed") +
      geom_ribbon(data = forecast_tib_qtr, 
                  aes(x = Year_Qtr_Float, ymin = Lower_95, ymax = Upper_95), 
                  alpha = 0.5,
                  fill = "gray80") +
      geom_ribbon(data = forecast_tib_qtr, 
                  aes(x = Year_Qtr_Float, ymin = Lower_80, ymax = Upper_80),
                  alpha = 0.5,
                  fill = "gray70")
    #scale_y_continuous(labels = label_number(big.mark = ","))
    
  })
  
  # Display Quarterly ARIMA Model Summary below the plot
  output$arimaSummary <- renderPrint({
    req(input$scatter_x_reg)  # Ensure both variables are selected
    
    # Get the selected variables for X and Y from the user input
    x_var <- input$scatter_x_reg
    
    # Create qtr-level revenue summary
    qtr_rev_arima <- plot_data() |> 
      group_by(`Year-Qtr`) |> 
      summarize(
        Total_Rev = sum(`Total Rev`, na.rm = TRUE), 
        .groups = "drop"
      ) 
    
    gdp_qtr_rev <- gdp |> 
      inner_join(qtr_rev_arima, by = "Year-Qtr")
    
    cutoff_year = 2023
    
    # Use data before the cutoff year to fit the model
    model_tib_gdp <- gdp_qtr_rev |> 
      arrange(Year) |> 
      filter(Year < cutoff_year)
    
    total_rev_ts <- ts(model_tib_gdp$Total_Rev, start = min(model_tib_gdp$Year), frequency = 4)
    gdp_total_ts <- ts(model_tib_gdp[[input$scatter_x_reg]], start = min(model_tib_gdp$Year), frequency = 4)
    
    # Fit ARIMA model with GDP as an external regressor
    gdp_sales_model <- auto.arima(
      total_rev_ts, 
      xreg = log(gdp_total_ts)
    )
    # Return ARIMA model summary
    coeftest(gdp_sales_model)
  })
  
  # Quarterly Arima Model Tab: Table with underlying Arima data
  
  output$AggregatedDataTable_GDP_Arima <- DT::renderDataTable({
    
    # Create qtr-level revenue summary
    qtr_rev_arima <- plot_data() |>
      group_by(`Year-Qtr`) |>
      summarize(
        Total_Rev = sum(`Total Rev`, na.rm = TRUE),
        .groups = "drop"
      )

    # filter
    exclude_columns <- c("Year", "Year-Qtr-Float", "Year-Qtr_Offset1", "Year-Qtr_Offset2")
    
    # Join
    gdp_qtr_rev <- gdp |> 
      inner_join(qtr_rev_arima, by = "Year-Qtr")
    
    gdp_qtr_rev_filtered <- gdp_qtr_rev |> 
      select(-all_of(exclude_columns))
      # mutate_all(~ ifelse(is.numeric(.),
      #                     scales::comma(round(., 2)),
      #                     .))
      # 
    DT::datatable(
      gdp_qtr_rev_filtered, 
      options = list(
        pageLength = 5,
        scrollX = TRUE  
      ),
      width = "100%"
    )
  })
    # Monthly ARIMA Tab: Arima Model Results_Plot
  output$Arima_Monthly_Plot <- renderPlot({
    req(input$monthly_x_reg)  # Ensure input is available
    
    # Debugging print statement (optional)
    print(input$monthly_x_reg)  # Print to check input
    
    # Get the selected variable for X from user input
    x_var_monthly <- input$monthly_x_reg
    
    # Create month-level revenue summary
    monthly_rev <- plot_data() |> 
      group_by(`Year-Month`) |> 
      summarize(
        Total_Rev = sum(`Total Rev`, na.rm = TRUE), 
        .groups = "drop"
      ) 
    
    merged_nongdp <- non_gdp |> 
      inner_join(monthly_rev, by = "Year-Month")
    
    # Convert Year-Month column to Date object
    merged_nongdp$`Year-Month-No` <- as.Date(paste(merged_nongdp$`Year-Month-No`, "01", sep = "-"), format = "%Y-%m-%d")
    
    # Use data before the cutoff year to fit the model
    cutoff_year = 2023
    
    model_tib_ngdp <- merged_nongdp |> 
      arrange(Year, MonthNo) |> 
      filter(Year < cutoff_year)
    
    m_total_rev_ts <- ts(model_tib_ngdp$Total_Rev, start = min(model_tib_ngdp$Year), frequency = 12)
    x_reg_ts <- ts(model_tib_ngdp[[input$monthly_x_reg]], start = min(model_tib_ngdp$Year), frequency = 12)
    
    # Fit Monthly ARIMA model
    n_gdp_sales_model <- auto.arima(
      m_total_rev_ts, 
      xreg = log(x_reg_ts)
    )
    
    future_x_reg <- merged_nongdp |> 
      arrange(Year) |> 
      filter(Year >= cutoff_year) |>
      filter(Year <= 2024) |> 
      pull(input$monthly_x_reg)
    
    forecasted_values <- forecast(n_gdp_sales_model, 
                                  xreg = log(future_x_reg)
    )
    
    start_year <- 2023
    
    months <- c("2023-1", "2023-2", "2023-3", "2023-4", "2023-5", "2023-6", "2023-7", "2023-8", "2023-9", "2023-10", 
                "2023-11", "2023-12", "2024-1", "2024-2", "2024-3", "2024-4", "2024-5", "2024-6", "2024-7", "2024-8", 
                "2024-9", "2024-10", "2024-11", "2024-12")
    
    forecast_tib_monthly <- tibble(
      months,
      Total_Rev_Exp_Pred = forecasted_values$mean,
      Lower_80 = forecasted_values$lower[,1],
      Upper_80 = forecasted_values$upper[,1],
      Lower_95 = forecasted_values$lower[,2],
      Upper_95 = forecasted_values$upper[,2]
    )
    
    # Convert Year-Month column to Date object.
    forecast_tib_monthly$months <- as.Date(paste(forecast_tib_monthly$months, "01", sep = "-"), format = "%Y-%m-%d")
    
    # Create the plot
    ggplot() +
      geom_line(data = merged_nongdp |> 
                  filter(Year <= 2024),
                aes(x = `Year-Month-No`, y = `Total_Rev`)
      ) +
      geom_line(data = forecast_tib_monthly, 
                aes(x = months, y = `Total_Rev_Exp_Pred`), 
                linetype = "dashed") +
      geom_ribbon(data = forecast_tib_monthly, 
                  aes(x = months, ymin = Lower_95, ymax = Upper_95), 
                  alpha = 0.5, fill = "gray80") +
      geom_ribbon(data = forecast_tib_monthly, 
                  aes(x = months, ymin = Lower_80, ymax = Upper_80),
                  alpha = 0.5, fill = "gray70") +
      scale_y_continuous(labels = label_number(big.mark = ","))
  })
  
  # Display Monthly ARIMA Model Summary below the plot
  output$ArimaMonthlySummary <- renderPrint({
    
    # Create month-level revenue summary
    monthly_rev <- plot_data() |> 
      group_by(`Year-Month`) |> 
      summarize(
        Total_Rev = sum(`Total Rev`, na.rm = TRUE), 
        .groups = "drop"
      ) 
    
    merged_nongdp <- non_gdp |> 
      inner_join(monthly_rev, by = "Year-Month")
    
    # Convert Year-Month column to Date object
    merged_nongdp$`Year-Month-No` <- as.Date(paste(merged_nongdp$`Year-Month-No`, "01", sep = "-"), format = "%Y-%m-%d")
    
    
    #Use data before the cutoff year to fit the model
    
    cutoff_year = 2023
    
    model_tib_ngdp <- merged_nongdp |> 
      arrange(Year, MonthNo) |> 
      filter(Year < cutoff_year)
    
    m_total_rev_ts <- ts(model_tib_ngdp$Total_Rev, start = min(model_tib_ngdp$Year), frequency = 12)
    x_reg_ts <- ts(model_tib_ngdp[[input$monthly_x_reg]], start = min(model_tib_ngdp$Year), frequency = 12)
    
    # Fit Monthly monthly ARIMA model
    n_gdp_sales_model <- auto.arima(
      m_total_rev_ts, 
      xreg = log(x_reg_ts)
    )
    
    coeftest(n_gdp_sales_model)
  })
  
  # Monthly Arima Model Tab: Table with underlying Arima data
  
  output$AggregatedDataTable_Monthly_Arima <- DT::renderDataTable({
    
    # Create month-level revenue summary
    monthly_rev <- plot_data() |> 
      group_by(`Year-Month`) |> 
      summarize(
        Total_Rev = sum(`Total Rev`, na.rm = TRUE), 
        .groups = "drop"
      ) 
    
    merged_nongdp <- non_gdp |> 
      inner_join(monthly_rev, by = "Year-Month")
    
    
    DT::datatable(
      merged_nongdp, 
      options = list(
        pageLength = 5,
        scrollX = TRUE  
      ),
      width = "100%"
    )
  })
}





