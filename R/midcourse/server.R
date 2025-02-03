#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/

# Define server logic required to draw a histogram

function(input, output, session) {
  
  # Reactive data based on the selected Order Class and Sales Percentile Range
  plot_data <- reactive({
    
    # Start with everything
    plot_data <- sales
    
    # Filter on order class
    if (input$S_Cons_Order_Class != "All Sales") {
      plot_data <- plot_data %>%
        filter(S_Cons_Order_Class == input$S_Cons_Order_Class)
    }
    
    # Get the lower and upper bounds from the slider
    lower_percentile <- input$slider2[1]
    upper_percentile <- input$slider2[2]
    
    # Calculate the cutoffs
    lower_cutoff <- quantile(plot_data$`Total Rev`, lower_percentile / 100, na.rm = TRUE)
    upper_cutoff <- quantile(plot_data$`Total Rev`, upper_percentile / 100, na.rm = TRUE)
    
    # Filter the data to include only those sales within the range
    plot_data <- plot_data %>%
      filter(`Total Rev` >= lower_cutoff & `Total Rev` <= upper_cutoff)
    
    return(plot_data)
  })
  
  # First plot: Distribution of Total Rev (Filtered by Percentile Range)
  output$distPlot <- renderPlot({
    
    title <- glue("Distribution of {input$S_Cons_Order_Class} within Sales Percentile Range: {input$slider2[1]}% - {input$slider2[2]}%")
    
    plot_data() %>%
      ggplot(aes(x = `Total Rev`)) +  
      geom_histogram(bins = 50) +
      ggtitle(title) +
      theme(plot.title = element_text(face = "bold"))  # Make title bold
    
  })
  
  # Second plot: facet of sales by year
  output$linePlot <- renderPlot({
    
    title <- glue("Line Plot of ({input$S_Cons_Order_Class}) by Month within Sales Percentile Range: {input$slider2[1]}% - {input$slider2[2]}%")
    
    # group data by year, month, order class
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
  
  # Third plot: scatter. goal is to be able to test all variables
  output$scatterPlot <- renderPlot({
    
    # Load the 'non_gdp' dataframe
    non_gdp_data <- non_gdp 
    
    # testing scatter plot will use different variables later
    ggplot(non_gdp_data, aes(x = New_Jobs, y = Unemployment)) +
      geom_point(color = "green") +
      labs(
        title = "Testing a scatter plot: New Jobs vs. Unemployment",
        x = "New Jobs",
        y = "Unemployment Rate"
      ) +
      theme_minimal()  
  })
  
  # Render the aggregated data as an interactive table using DT
  output$aggregatedDataTable <- renderDT({
    # group data by year, month, order class
    aggregated_data <- plot_data() %>%
      group_by(Year, Month, S_Cons_Order_Class) %>%
      summarize(Total_Sales = sum(`Total Rev`, na.rm = TRUE), .groups = "drop")
    
    datatable(aggregated_data)  # Use datatable() for an interactive table
  })
}

  



