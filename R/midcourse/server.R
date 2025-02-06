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
      plot_data <- plot_data %>%
        filter(S_Cons_Order_Class == input$S_Cons_Order_Class)
    }
    
    # Get lower and upper bounds from the percentile slider
    lower_percentile <- input$slider2[1]
    upper_percentile <- input$slider2[2]
    
    # Calculate the cutoffs for the range
    lower_cutoff <- quantile(plot_data$`Total Rev`, lower_percentile / 100, na.rm = TRUE)
    upper_cutoff <- quantile(plot_data$`Total Rev`, upper_percentile / 100, na.rm = TRUE)
    
    # Filter data based on the selected percentile range
    plot_data <- plot_data %>%
      filter(`Total Rev` >= lower_cutoff & `Total Rev` <= upper_cutoff)
    
    return(plot_data)
  })
  
  # GDP Sector Tab: Distribution of Total Rev for GDP (Filtered by Percentile Range)
  output$distPlotGDP <- renderPlot({
    req(input$tabs == "GDP Sector Comparison")  # Ensure the tab is active
    
    title <- glue("Distribution of {input$S_Cons_Order_Class} within Sales Percentile Range: {input$slider2[1]}% - {input$slider2[2]}%")
    
    plot_data() %>%
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
        labels = scales::comma  # Format x-axis numbers with commas for better readability
      ) +
      scale_y_continuous(labels = scales::comma)  # Format y-axis numbers with commas
  })
  
  # GDP Sector: Data for the table and scatter plot (Merged Data)
  merged_data_gdp <- reactive({
    # Same aggregation steps as before
    qtr_rev_agg_0 <- plot_data() %>%
      group_by(`Year-Qtr`) %>%
      summarize(
        Total_Rev = sum(`Total Rev`, na.rm = TRUE), 
        .groups = "drop"
      )
    
    gdp$`Year-Qtr` <- as.character(gdp$`Year-Qtr`)
    qtr_rev_agg_0$`Year-Qtr` <- as.character(qtr_rev_agg_0$`Year-Qtr`)
    
    merged_data_gdp <- gdp %>%
      inner_join(qtr_rev_agg_0, by = "Year-Qtr")
    
    # Second aggregation step (qtr_rev_agg_1) for merged data
    qtr_rev_agg_1 <- plot_data() %>%
      group_by(`Year-Qtr_Offset1`) %>%
      summarize(
        Total_Rev = sum(`Total Rev`, na.rm = TRUE), 
        .groups = "drop"
      ) %>%
      rename(Total_Rev_Offset1 = Total_Rev)
    
    merged_data_gdp$`Year-Qtr_Offset1` <- as.character(merged_data_gdp$`Year-Qtr_Offset1`)
    qtr_rev_agg_1$`Year-Qtr_Offset1` <- as.character(qtr_rev_agg_1$`Year-Qtr_Offset1`)
    
    merged_data_gdp <- merged_data_gdp %>%
      inner_join(qtr_rev_agg_1, by = "Year-Qtr_Offset1")
    
    # Third aggregation step (qtr_rev_agg_2) for merged data
    qtr_rev_agg_2 <- plot_data() %>%
      group_by(`Year-Qtr_Offset2`) %>%
      summarize(
        Total_Rev = sum(`Total Rev`, na.rm = TRUE), 
        .groups = "drop"
      ) %>%
      rename(Total_Rev_Offset2 = Total_Rev)
    
    merged_data_gdp$`Year-Qtr_Offset2` <- as.character(merged_data_gdp$`Year-Qtr_Offset2`)
    qtr_rev_agg_2$`Year-Qtr_Offset2` <- as.character(qtr_rev_agg_2$`Year-Qtr_Offset2`)
    
    merged_data_gdp <- merged_data_gdp %>%
      inner_join(qtr_rev_agg_2, by = "Year-Qtr_Offset2")
    
    return(merged_data_gdp)
  })
  
  # GDP Sector Tab: Scatter Plot (using merged_data_gdp)
  output$scatterPlotGDP <- renderPlot({
    # Ensure merged data is available
    merged_data <- merged_data_gdp()
    
    # Get selected variables for X and Y axes
    x_var <- input$scatter_x_var_gdp
    y_var <- input$scatter_y_var_gdp
    
    # Plot the selected X and Y axis variables
    ggplot(merged_data, aes_string(x = x_var, y = y_var)) +
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
  output$AggregatedDataTableGDP <- DT::renderDataTable({
    DT::datatable(
      merged_data_gdp(), 
      options = list(
        pageLength = 10,
        scrollX = TRUE   # Horizontal scrolling
      ),
      width = "100%"
    )
  })
  
  # GDP Tab: Linear Regression Summary
  
  output$lmSummaryGDP <- renderPrint({
    req(input$scatter_x_var_gdp, input$scatter_y_var_gdp)  # Ensure both variables are selected
    
    # Get merged data (this will trigger the reactive block to compute it)
    merged_data_gdp <- merged_data_gdp()
    
    # Build the linear model using the merged data
    lm_model_gdp <- lm(as.formula(paste(input$scatter_y_var_gdp, "~", input$scatter_x_var_gdp)), data = merged_data_gdp)
    
    # Return the summary of the regression model
    summary(lm_model_gdp)
  })
  
  # Economic Indicator Tab: Distribution of Total Rev (Filtered by Percentile Range)
  output$distPlot <- renderPlot({
    title <- glue("Distribution of {input$S_Cons_Order_Class} within Sales Percentile Range: {input$slider2[1]}% - {input$slider2[2]}%")
    
    plot_data() %>%
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
    aggregated_data <- plot_data() %>%
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
    month_rev <- plot_data() %>%
      group_by(`Year-Month`) %>%
      summarize(Total_Rev = sum(`Total Rev`, na.rm = TRUE), .groups = "drop")
    
    # Merge month_rev with non_gdp dataset using Year-Month as common key
    merged_data <- non_gdp %>%
      inner_join(month_rev, by = "Year-Month", suffix = c("_non_gdp", "_month_rev"))
    merged_data <- merged_data %>%
      inner_join(month_rev, by = c("Year-Month_Offset1" = "Year-Month"), suffix = c("", "_Offset1")) %>%
      rename(Total_Rev_Offset1 = Total_Rev_Offset1)
    merged_data <- merged_data %>%
      inner_join(month_rev, by = c("Year-Month_Offset2" = "Year-Month"), suffix = c("", "_Offset2")) %>%
      rename(Total_Rev_Offset2 = Total_Rev_Offset2)
    
    # Get selected variables for X and Y axes
    x_var <- input$scatter_x_var
    y_var <- input$scatter_y_var
    
    # Plot the selected X and Y axis variables
    ggplot(merged_data, aes_string(x = x_var, y = y_var)) +
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
    month_rev <- plot_data() %>%
      group_by(`Year-Month`) %>%
      summarize(Total_Rev = sum(`Total Rev`, na.rm = TRUE), .groups = "drop")
    
    merged_data <- non_gdp %>%
      inner_join(month_rev, by = "Year-Month", suffix = c("_non_gdp", "_month_rev"))
    merged_data <- merged_data %>%
      inner_join(month_rev, by = c("Year-Month_Offset1" = "Year-Month"), suffix = c("", "_Offset1")) %>%
      rename(Total_Rev_Offset1 = Total_Rev_Offset1)
    merged_data <- merged_data %>%
      inner_join(month_rev, by = c("Year-Month_Offset2" = "Year-Month"), suffix = c("", "_Offset2")) %>%
      rename(Total_Rev_Offset2 = Total_Rev_Offset2)
    
    DT::datatable(
      merged_data,  # The merged data to be displayed
      options = list(pageLength = 10, scrollX = TRUE),  # Table options
      width = "100%"  # Set table width
    )
  })

  # Economic Indicator Tab: Linear Regression Summary
  output$lmSummary <- renderPrint({
    req(input$scatter_x_var, input$scatter_y_var)  # Ensure both variables are selected
    
    # Get the selected variables for X and Y from the user input
    x_var <- input$scatter_x_var
    y_var <- input$scatter_y_var
    
    # Create month-level revenue summary
    month_rev <- plot_data() %>%
      group_by(`Year-Month`) %>%
      summarize(Total_Rev = sum(`Total Rev`, na.rm = TRUE), .groups = "drop")
    
    # Merge the data for revenue and GDP offsets
    merged_data <- non_gdp %>%
      inner_join(month_rev, by = "Year-Month", suffix = c("_non_gdp", "_month_rev")) %>%
      inner_join(month_rev, by = c("Year-Month_Offset1" = "Year-Month"), suffix = c("", "_Offset1")) %>%
      rename(Total_Rev_Offset1 = Total_Rev_Offset1) %>%
      inner_join(month_rev, by = c("Year-Month_Offset2" = "Year-Month"), suffix = c("", "_Offset2")) %>%
      rename(Total_Rev_Offset2 = Total_Rev_Offset2)
    
    # Build the linear model using the merged data
    lm_model <- lm(as.formula(paste(y_var, "~", x_var)), data = merged_data)
    
    # Return the summary of the regression model
    summary(lm_model)
  })
}





