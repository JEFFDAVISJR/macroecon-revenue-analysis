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
    req(input$tabs == "Linear Regression (Quarterly)")  # Ensure the tab is active
    
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

  # GDP Sector Tab: LM Line Plot 
  output$linePlotGDP <- renderPlot({
    qtr_rev <- plot_data() |> 
      group_by(`Year-Qtr`) |> 
      summarize(
        Total_Rev = sum(`Total Rev`, na.rm = TRUE), 
        .groups = "drop"
      ) 
    
    merged_gdp <- gdp |> 
      inner_join(qtr_rev, by = "Year-Qtr")
    
    merged_gdp
    
    # Get selected variables for X and Y axes
    x_var <- input$line_x_var_gdp
    
    # 1. Fit the Linear Regression Model using the full dataset
    lm_model <- lm(as.formula(paste("Total_Rev ~", x_var)), data = merged_gdp)
    
    # 2. Predicted values with confidence intervals for the entire dataset
    predicted_vals <- predict(lm_model, merged_gdp, 
                              interval = "confidence", level = 0.95)
    
    # Add predicted values and confidence intervals to the tibble
    merged_gdp$Predicted_Rev <- predicted_vals[,1]  # Predicted values
    merged_gdp$Lower_95 <- predicted_vals[,2]      # Lower bound of 95% CI
    merged_gdp$Upper_95 <- predicted_vals[,3]      # Upper bound of 95% CI
    
    # 3. Create filtered tibble
    merged_gdp_filtered <- merged_gdp |> 
      filter(Year %in% c("2023", "2024"))
    
    # 4. Plot the Actual vs Predicted 
    ggplot(merged_gdp, aes_string(x = "`Year-Qtr-Float`")) +  # Dynamically refer to x_var here
      # Actual values for all data
      geom_line(aes(y = Total_Rev), color = "blue", size = 1, alpha = 0.7, label = "Actual Total Revenue") +
      # Predicted values starting at 2023
      geom_line(data = merged_gdp_filtered, aes(y = Predicted_Rev), color = "red", linetype = "dashed", size = 1, alpha = 0.7, label = "Predicted Total Revenue") +
      # Confidence intervals for the predicted values starting at 2023
      geom_ribbon(data = merged_gdp_filtered, aes(ymin = Lower_95, ymax = Upper_95), alpha = 0.3, fill = "gray70") +
      labs(
        title = glue("Actual Revenue vs Predicted Revenue by Quarter (Predictor Var: {input$line_x_var})"),
           x = "Year-Month", y = "Total Revenue") +
      theme_minimal() +
      theme(legend.position = "top", plot.title = element_text(face = "bold", size = 16)) +
      scale_y_continuous(labels = scales::label_number(big.mark = ","))  # Format y-axis with commas for readability
   
  })
  
  # GDP Sector Tab: Table with merged data
  output$AggregatedDataTableGDP <- DT::renderDataTable({
    
    # Aggregating revenue by quarter
    qtr_rev <- plot_data() |> 
      group_by(`Year-Qtr`) |> 
      summarize(
        Total_Rev = sum(`Total Rev`, na.rm = TRUE), 
        .groups = "drop"
      ) 
    
    gdp_exclude <- c("Year", "Year-Qtr-Float", "Year-Qtr_Offset1", "Year-Qtr_Offset2")
    
    # Merging the GDP data with aggregated quarterly revenue data
    merged_gdp <- gdp |> 
      inner_join(qtr_rev, by = "Year-Qtr")
    
    # Render the DataTable with proper formatting
    DT::datatable(
      merged_gdp |> 
        select(-all_of(gdp_exclude)),  # Exclude unnecessary columns
      options = list(
        pageLength = 5,
        scrollX = TRUE
      ),
      width = "100%"
    )
  })
  
  
  # GDP Tab: Linear Regression Summary
  
  output$lmSummaryGDP <- renderPrint({
    qtr_rev <- plot_data() |> 
      group_by(`Year-Qtr`) |> 
      summarize(
        Total_Rev = sum(`Total Rev`, na.rm = TRUE), 
        .groups = "drop"
      ) 
    
    merged_gdp <- gdp |> 
      inner_join(qtr_rev, by = "Year-Qtr")
    
    merged_gdp
    
    # Get selected variables for X and Y axes
    x_var <- input$line_x_var_gdp
    
    # 1. Fit the Linear Regression Model using the full dataset
    lm_model <- lm(as.formula(paste("Total_Rev ~", x_var)), data = merged_gdp)
    
    summary(lm_model)
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
  
  # Economic Indicator Tab: LM Line plot with Dynamic X and Y-Axis Variables
  output$linePlot <- renderPlot({
    monthly_rev <- plot_data() |> 
      group_by(`Year-Month`) |> 
      summarize(
        Total_Rev = sum(`Total Rev`, na.rm = TRUE), 
        .groups = "drop"
      ) 
    
    merged_nongdp <- non_gdp |> 
      inner_join(monthly_rev, by = "Year-Month")
    
    merged_nongdp$`Year-Month-No` <- as.Date(paste0(merged_nongdp$`Year-Month-No`, "-01"), format="%Y-%m-%d")
    
    merged_nongdp <- merged_nongdp |> 
      mutate(Year = year(`Year-Month-No`))
    
    # Get selected variables for X and Y axes
    line_x_var <- input$line_x_var
    
    # 1. Fit the lm
    lm_model <- lm(as.formula(paste("Total_Rev ~", line_x_var)), data = merged_nongdp)
    
    # 2. Predicted values with confidence intervals for the entire dataset (95% CI)
    predicted_vals_95 <- predict(lm_model, merged_nongdp, 
                                 interval = "prediction", level = 0.95)
    
    # Add 95% confidence intervals to the tibble
    merged_nongdp$Predicted_Rev <- predicted_vals_95[,1]  # Predicted values
    merged_nongdp$Lower_95 <- predicted_vals_95[,2]      # Lower bound of 95% CI
    merged_nongdp$Upper_95 <- predicted_vals_95[,3]      # Upper bound of 95% CI
    
    # 2. Predicted values with confidence intervals for the entire dataset (80% CI)
    predicted_vals_80 <- predict(lm_model, merged_nongdp, 
                                 interval = "prediction", level = 0.80)
    
    # Add 80% confidence intervals to the tibble
    merged_nongdp$Lower_80 <- predicted_vals_80[,2]      # Lower bound of 80% CI
    merged_nongdp$Upper_80 <- predicted_vals_80[,3]      # Upper bound of 80% CI
    
    # 3. Create a new tibble for the plot, where we filter for 2023 and 2024 for predictions
    merged_nongdp_filtered <- merged_nongdp %>%
      filter(Year %in% c("2023", "2024"))
    
    # 4. Plot the Actual vs Predicted values
    ggplot() +
      # Plot actual
      geom_line(data = merged_nongdp |> 
                  filter(Year <= 2024),
                aes(x = `Year-Month-No`, y = Total_Rev),
                color = "blue", size = 1) +  # Set color and line thickness for clarity
      
      # Plot predicted
      geom_line(data = merged_nongdp_filtered, 
                aes(x = `Year-Month-No`, y = Predicted_Rev), 
                linetype = "dashed", color = "red", size = 1) +  # Dashed line for predicted values
      
      # Confidence intervals (95%)
      geom_ribbon(data = merged_nongdp_filtered, 
                  aes(x = `Year-Month-No`, ymin = Lower_95, ymax = Upper_95), 
                  alpha = 0.3, fill = "gray80") +  # 95% confidence interval
      
      # Confidence intervals (80%)
      geom_ribbon(data = merged_nongdp_filtered, 
                  aes(x = `Year-Month-No`, ymin = Lower_80, ymax = Upper_80), 
                  alpha = 0.3, fill = "gray70") +  # 80% confidence interval
      
      labs(
        title = glue("Actual Revenue vs Predicted Revenue by Month (Predictor Var: {input$line_x_var})"),
        x = "Year-Month", y = "Total Revenue"
      ) +
      theme_minimal() +
      theme(legend.position = "top", plot.title = element_text(face = "bold", size = 16)) +
      scale_y_continuous(labels = scales::label_number(big.mark = ","), 
                         limits = c(200000, NA)) +  # Set the lower limit to 200,000
      
      # Formatting x-axis 
      scale_x_date(labels = date_format("%b %Y"), breaks = "3 months") +
      
      # Theme
      theme_minimal(base_size = 15) +
      theme(
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.title.x = element_blank(),  # Remove x-axis label
        axis.title.y = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better fit
        axis.text.y = element_text(size = 12),
        legend.position = "bottom"  # Position legend at the bottom if applicable
      ) +
      labs(
        title = glue("Actual Revenue vs Predicted Revenue by Month (Predictor Var: {input$line_x_var})"),
        x = "Month",
        y = "Total Revenue (USD)"
      )
    
  })
  
  # Economic Indicator Tab: Underlying Data Table for Scatter Plot
  output$aggregatedDataTable <- DT::renderDataTable({
    month_rev <- plot_data() |> 
      group_by(`Year-Month`) |> 
      summarize(Total_Rev = sum(`Total Rev`, na.rm = TRUE), .groups = "drop")
    
    merged_data_nongdp <- non_gdp |> 
      inner_join(month_rev, by = "Year-Month", suffix = c("_non_gdp", "_month_rev"))
    
    nongdp__exclude_columns <- c("Year","Month","Year-Month-No","MonthNo","Year-Month-No-Float","Year-Month_Offset1", "Year-Month_Offset2")
    
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
    monthly_rev <- plot_data() |> 
      group_by(`Year-Month`) |> 
      summarize(
        Total_Rev = sum(`Total Rev`, na.rm = TRUE), 
        .groups = "drop"
      ) 
    
    merged_nongdp <- non_gdp |> 
      inner_join(monthly_rev, by = "Year-Month")
    
    merged_nongdp$`Year-Month-No` <- as.Date(paste0(merged_nongdp$`Year-Month-No`, "-01"), format="%Y-%m-%d")
    
    merged_nongdp <- merged_nongdp |> 
      mutate(Year = year(`Year-Month-No`))
    
    # Get selected variables for X and Y axes
    line_x_var <- input$line_x_var
    
    # 1. Fit the Linear Regression Model using the full dataset
    lm_model <- lm(as.formula(paste("Total_Rev ~", line_x_var)), data = merged_nongdp)
    
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
    
    library(ggplot2)
    library(scales)
    library(glue)
    
    # Assuming 'gdp_qtr_rev' and 'forecast_tib_qtr' are your data frames
    
    ggplot() +
      # Plot actual
      geom_line(data = gdp_qtr_rev |> 
                  filter(Year < 2024),
                aes(x = `Year-Qtr-Float`, y = Total_Rev),
                color = "blue", size = 1) +  # Blue line for actual values
      
      # Plot predicted
      geom_line(data = forecast_tib_qtr, 
                aes(x = Year_Qtr_Float, y = Total_Rev_Exp_Pred), 
                linetype = "dashed", color = "red", size = 1) +  # Dashed red line for predictions
      
      # Confidence intervals
      geom_ribbon(data = forecast_tib_qtr, 
                  aes(x = Year_Qtr_Float, ymin = Lower_95, ymax = Upper_95), 
                  alpha = 0.3, fill = "gray80") +  # 95% confidence interval
      
      geom_ribbon(data = forecast_tib_qtr, 
                  aes(x = Year_Qtr_Float, ymin = Lower_80, ymax = Upper_80),
                  alpha = 0.3, fill = "gray70") +  # 80% confidence interval
      
      # 
      scale_y_continuous(labels = label_number(big.mark = ",")) +
      
      # 
      theme_minimal(base_size = 15) +
      theme(
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.title.x = element_blank(),  # Remove x-axis label
        axis.title.y = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better fit
        axis.text.y = element_text(size = 12),
        legend.position = "bottom"  # Position legend at the bottom if applicable
      ) +
      
      # Title
      labs(
        title = glue("Actual Revenue vs Predicted Revenue by Quarter (Predictor Var: {input$scatter_x_reg})"),
        y = "Total Revenue (USD)"
      )
    
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
    req(input$line_x_var)  # Ensure input is available
    
    # Debugging print statement (optional)
    print(input$line_x_var)  # Print to check input
    
    # Get the selected variable for X from user input
    line_x_var <- input$line_x_var
    
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
    
    # Create time series data using the selected variable
    m_total_rev_ts <- ts(model_tib_ngdp$Total_Rev, start = min(model_tib_ngdp$Year), frequency = 12)
    x_reg_ts <- ts(model_tib_ngdp[[line_x_var]], start = min(model_tib_ngdp$Year), frequency = 12)
    
    # Fit Monthly ARIMA model
    n_gdp_sales_model <- auto.arima(
      m_total_rev_ts, 
      xreg = x_reg_ts
    )
    
    future_x_reg <- merged_nongdp |> 
      arrange(Year) |> 
      filter(Year >= cutoff_year) |>
      filter(Year <= 2024) |> 
      pull(line_x_var)  # Make sure to use the selected variable here
    
    forecasted_values <- forecast(n_gdp_sales_model, 
                                  xreg = future_x_reg
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
      # Plot actual
      geom_line(data = merged_nongdp |> 
                  filter(Year <= 2024),
                aes(x = `Year-Month-No`, y = `Total_Rev`),
                color = "blue", size = 1) +  # Set color and line thickness for clarity
      
      # Plot predicted
      geom_line(data = forecast_tib_monthly, 
                aes(x = months, y = `Total_Rev_Exp_Pred`), 
                linetype = "dashed", color = "red", size = 1) +  # Dashed line for predicted values
      
      # Confidence intervals
      geom_ribbon(data = forecast_tib_monthly, 
                  aes(x = months, ymin = Lower_95, ymax = Upper_95), 
                  alpha = 0.3, fill = "gray80") +  # 95% confidence interval
      
      geom_ribbon(data = forecast_tib_monthly, 
                  aes(x = months, ymin = Lower_80, ymax = Upper_80),
                  alpha = 0.3, fill = "gray70") +  # 80% confidence interval
      
      # Format Y axis with commas for readability
      scale_y_continuous(labels = label_number(big.mark = ",")) +
      
      # Format X axis with monthly breaks
      scale_x_date(labels = date_format("%b %Y"), breaks = "3 months") +  
      
      # Apply theme settings
      theme_minimal(base_size = 15) +
      theme(
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.title.x = element_blank(),  # Remove x-axis label
        axis.title.y = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better fit
        axis.text.y = element_text(size = 12),
        legend.position = "bottom"  # Position legend at the bottom if applicable
      ) +
      
      labs(
        title = glue("Actual Revenue vs Predicted Revenue by Month (Predictor Var: {line_x_var})"),
        x = "Month",
        y = "Total Revenue (USD)"
      )
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
    
    # 
    model_tib_ngdp <- merged_nongdp |> 
      arrange(Year, MonthNo)
    
    # Prepare time series
    m_total_rev_ts <- ts(model_tib_ngdp$Total_Rev, start = min(model_tib_ngdp$Year), frequency = 12)
    x_reg_ts <- ts(model_tib_ngdp[[input$line_x_var]], start = min(model_tib_ngdp$Year), frequency = 12)
    
    # Fit the ARIMA model on the full dataset
    n_gdp_sales_model <- auto.arima(
      m_total_rev_ts, 
      xreg = x_reg_ts
    )
    
    # Get the ARIMA model coefficients
    coeftest_output <- coeftest(n_gdp_sales_model)
    
    # Get the ARIMA model summary
    model_summary <- summary(n_gdp_sales_model)
    
    # Print both outputs as a list
    list(
      "ARIMA Model Coefficients" = coeftest_output,
      "ARIMA Model Summary" = model_summary
    )
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
    
    
    exclude_columns <- c("Year","Month","MonthNo","Year-Month-No","Year-Month-No-Float", "Year-Month_Offset1", "Year-Month_Offset2")
    
    merged_nongdp_filtered <- merged_nongdp |> 
      select(-all_of(exclude_columns))
    
    
    DT::datatable(
      merged_nongdp_filtered, 
      options = list(
        pageLength = 5,
        scrollX = TRUE  
      ),
      width = "100%"
    )
  })
}





