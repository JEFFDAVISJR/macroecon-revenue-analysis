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
      geom_histogram(bins = 50) +
      ggtitle(title) +
      theme(plot.title = element_text(face = "bold")) +  # Make title bold
      scale_x_continuous(breaks = seq(0, max(plot_data()$`Total Rev`, na.rm = TRUE), by = 2500))  # Adjust x-tick interval
  })
  
  # GDP Sector Tab: New Table with Total_Rev, Total_Rev_Offset1, and Total_Rev_Offset2
  output$AggregatedDataTableGDP <- DT::renderDataTable({
    
    # first aggregation: qtr_rev_agg_0
    qtr_rev_agg_0 <- plot_data() %>%
      group_by(`Year-Qtr`) %>%
      summarize(
        Total_Rev = sum(`Total Rev`, na.rm = TRUE), 
        .groups = "drop"
      )
    
    # ensure data types are the same for merging
    gdp$`Year-Qtr` <- as.character(gdp$`Year-Qtr`)
    qtr_rev_agg_0$`Year-Qtr` <- as.character(qtr_rev_agg_0$`Year-Qtr`)
    
    # merge 1: qtr_rev_agg_0 and gdp by Year-Qtr
    merged_data_gdp <- gdp %>%
      inner_join(qtr_rev_agg_0, by = "Year-Qtr")
    
    # second aggregation: qtr_rev_agg_1 (grouped by Year-Qtr_Offset1)
    qtr_rev_agg_1 <- plot_data() %>%
      group_by(`Year-Qtr_Offset1`) %>%
      summarize(
        Total_Rev = sum(`Total Rev`, na.rm = TRUE), 
        .groups = "drop"
      ) %>%
      rename(Total_Rev_Offset1 = Total_Rev)
    
    # ensure data types are the same for merging
    merged_data_gdp$`Year-Qtr_Offset1` <- as.character(merged_data_gdp$`Year-Qtr_Offset1`)
    qtr_rev_agg_1$`Year-Qtr_Offset1` <- as.character(qtr_rev_agg_1$`Year-Qtr_Offset1`)
    
    # merge 2: qtr_rev_agg_1 and merged_data_gdp by Year-Qtr_Offset1
    merged_data_gdp <- merged_data_gdp %>%
      inner_join(qtr_rev_agg_1, by = "Year-Qtr_Offset1")
    
    # third aggregation: qtr_rev_agg_2 (grouped by Year-Qtr_Offset2)
    qtr_rev_agg_2 <- plot_data() %>%
      group_by(`Year-Qtr_Offset2`) %>%
      summarize(
        Total_Rev = sum(`Total Rev`, na.rm = TRUE), 
        .groups = "drop"
      ) %>%
      rename(Total_Rev_Offset2 = Total_Rev)
    
    # ensure data types are the same for merging
    merged_data_gdp$`Year-Qtr_Offset2` <- as.character(merged_data_gdp$`Year-Qtr_Offset2`)
    qtr_rev_agg_2$`Year-Qtr_Offset2` <- as.character(qtr_rev_agg_2$`Year-Qtr_Offset2`)
    
    # merge 3: qtr_rev_agg_2 and merged_data_gdp by Year-Qtr_Offset2
    merged_data_gdp <- merged_data_gdp %>%
      inner_join(qtr_rev_agg_2, by = "Year-Qtr_Offset2")
    
    # display the resulting merged data table for the third merge
    DT::datatable(merged_data_gdp, options = list(pageLength = 10))
  })

  # Economic Indicator Tab: Distribution of Total Rev (Filtered by Percentile Range)
  output$distPlot <- renderPlot({
    title <- glue("Distribution of {input$S_Cons_Order_Class} within Sales Percentile Range: {input$slider2[1]}% - {input$slider2[2]}%")
    
    plot_data() %>%
      ggplot(aes(x = `Total Rev`)) +  
      geom_histogram(bins = 50) +
      ggtitle(title) +
      theme(plot.title = element_text(face = "bold")) +  # Make title bold
      scale_x_continuous(breaks = seq(0, max(plot_data()$`Total Rev`, na.rm = TRUE), by = 2500))  # Adjust x-tick interval
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
      left_join(month_rev, by = "Year-Month", suffix = c("", "_month_rev"))
    
    # Merge for Total_Rev_Offset1 and Total_Rev_Offset2
    merged_data <- merged_data %>%
      left_join(month_rev, by = c("Year-Month_Offset1" = "Year-Month"), suffix = c("", "_Offset1"))
    merged_data <- merged_data %>%
      left_join(month_rev, by = c("Year-Month_Offset2" = "Year-Month"), suffix = c("", "_Offset2"))
    
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
    
    month_rev$`Year-Month` <- as.Date(paste0(month_rev$`Year-Month`, "-01"), format = "%Y-%b-%d")
    non_gdp$`Year-Month` <- as.Date(paste0(non_gdp$`Year-Month`, "-01"), format = "%Y-%b-%d")
    non_gdp$`Year-Month_Offset1` <- as.Date(paste0(non_gdp$`Year-Month_Offset1`, "-01"), format = "%Y-%b-%d")
    non_gdp$`Year-Month_Offset2` <- as.Date(paste0(non_gdp$`Year-Month_Offset2`, "-01"), format = "%Y-%b-%d")
    
    merged_data <- non_gdp %>%
      inner_join(month_rev, by = "Year-Month", suffix = c("_non_gdp", "_month_rev"))
    merged_data <- merged_data %>%
      inner_join(month_rev, by = c("Year-Month_Offset1" = "Year-Month"), suffix = c("", "_Offset1")) %>%
      rename(Total_Rev_Offset1 = Total_Rev_Offset1)
    merged_data <- merged_data %>%
      inner_join(month_rev, by = c("Year-Month_Offset2" = "Year-Month"), suffix = c("", "_Offset2")) %>%
      rename(Total_Rev_Offset2 = Total_Rev_Offset2)
    
    DT::datatable(merged_data, options = list(pageLength = 10))
  })
}







