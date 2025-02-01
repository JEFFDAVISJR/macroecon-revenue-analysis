#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Define server logic required to draw a histogram

function(input, output, session) {
  
  plot_data <- reactive({
    
    plot_data <- sales
    
    # Apply the filter only when "All" is not selected
    if (input$S_Cons_Order_Class != "All Sales") {
      plot_data <- plot_data %>%
        filter(S_Cons_Order_Class == input$S_Cons_Order_Class)
    }
    
    return(plot_data)
  })
  
  # First plot  
  output$distPlot <- renderPlot({
    
    title <- glue("Distribution of ({input$S_Cons_Order_Class})")
    
    plot_data() %>%
      ggplot(aes(x = `Total Rev`)) +  
      geom_histogram(bins = 50) +
      ggtitle(title) +
      theme(plot.title = element_text(face = "bold"))  # Make title bold
    
  })
  
  # Second plot
  output$linePlot <- renderPlot({
    
    title <- glue("Line Plot of ({input$S_Cons_Order_Class}) by Year")  
    
    if (input$S_Cons_Order_Class != "All") {
      title <- paste0(title, "")  
    }
    
    # Aggregate the data by Month (and optionally by S_Cons_Order_Class)
    aggregated_data <- plot_data() %>%
      group_by(Month, S_Cons_Order_Class) %>%
      summarize(Total_Sales = sum(`Total Rev`, na.rm = TRUE), .groups = "drop")
    
    ggplot(aggregated_data, aes(x = Month, y = Total_Sales, group = S_Cons_Order_Class, color = as.factor(S_Cons_Order_Class))) + 
      geom_line() +
      geom_point() +  # Add points to the line plot
      labs(
        title = title,
        y = "Total Rev",
        x = "Month",
        color = "Order Class" 
      ) +
      theme(
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
}