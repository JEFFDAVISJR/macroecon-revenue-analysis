#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/

# Define server logic required to draw a histogram

function(input, output, session) {
  
      # reactive data based on the selected Order Class and Sales Percentile Range
    plot_data <- reactive({
      
      # start with everything
      plot_data <- sales
      
      # filter on order class
      if (input$S_Cons_Order_Class != "All Sales") {
        plot_data <- plot_data %>%
          filter(S_Cons_Order_Class == input$S_Cons_Order_Class)
      }
      
      # lower and upper bounds from the slider
      lower_percentile <- input$slider2[1]
      upper_percentile <- input$slider2[2]
      
      # calculate the cutoffs
      lower_cutoff <- quantile(plot_data$`Total Rev`, lower_percentile / 100, na.rm = TRUE)
      upper_cutoff <- quantile(plot_data$`Total Rev`, upper_percentile / 100, na.rm = TRUE)
      
      # filter the data to include only those sales within the range
      plot_data <- plot_data %>%
        filter(`Total Rev` >= lower_cutoff & `Total Rev` <= upper_cutoff)
      
      return(plot_data)
    })
    
    # first plot: Distribution of Total Rev (Filtered by Percentile Range)
    output$distPlot <- renderPlot({
      
      title <- glue("Distribution of {input$S_Cons_Order_Class} within Sales Percentile Range: {input$slider2[1]}% - {input$slider2[2]}%")
      
      plot_data() %>%
        ggplot(aes(x = `Total Rev`)) +  
        geom_histogram(bins = 50) +
        ggtitle(title) +
        theme(plot.title = element_text(face = "bold"))  # Make title bold
      
    })
    
    # second plot: facet of sales by year
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
    
    # third plot: scatter plot with dynamic X and fixed Y-axis
    output$scatterPlot <- renderPlot({
      
      # 1. Create month_rev from plot_data, filtered by Order Class and Percentile Range
      month_rev <- plot_data() %>%
        group_by(`Year-Month`) %>%
        summarize(Total_Rev = sum(`Total Rev`, na.rm = TRUE), .groups = "drop")
      
      # 2. Merge month_rev with non_gdp using the standardized 'Year-Month' column
      merged_data <- non_gdp %>%
        left_join(month_rev, by = "Year-Month")  # We join on the 'Year-Month' column, which has been aligned
      
      # Debug: Check merged data
      print(head(merged_data))  # Print the first few rows of the merged data
      
      # 3. Get selected variable for the x-axis 
      x_var <- input$scatter_x_var
      
      # 4. Fixed Y-axis variable "Total_Rev"
      y_var <- "Total_Rev"
      
      # 5. Plot the selected X and fixed Y axis variables
      ggplot(merged_data, aes_string(x = x_var, y = y_var)) +
        geom_point(color = "darkblue") +
        labs(
          title = glue("Scatter Plot: {x_var} vs {y_var}"),
          x = x_var,
          y = y_var
        ) +
        theme_minimal() +
        geom_smooth(method = lm)
    })
    
    output$aggregatedDataTable <- DT::renderDataTable({
      
      # create month_rev from plot_data, group by Year-Month and sum Total Rev
      month_rev <- plot_data() %>%
        group_by(`Year-Month`) %>%
        summarize(Total_Rev = sum(`Total Rev`, na.rm = TRUE), .groups = "drop")
      
      # convert Year-Month in month_rev to Date format (using first day of month)
      month_rev$`Year-Month` <- as.Date(paste0(month_rev$`Year-Month`, "-01"), format = "%Y-%b-%d")
      
      # convert Year-Month and Year-Month_Offset1 in non_gdp to Date format (using first day of month)
      non_gdp$`Year-Month` <- as.Date(paste0(non_gdp$`Year-Month`, "-01"), format = "%Y-%b-%d")
      non_gdp$`Year-Month_Offset1` <- as.Date(paste0(non_gdp$`Year-Month_Offset1`, "-01"), format = "%Y-%b-%d")
      non_gdp$`Year-Month_Offset2` <- as.Date(paste0(non_gdp$`Year-Month_Offset2`, "-01"), format = "%Y-%b-%d")
      
      # 1. merge month_rev with non_gdp to get the Total_Rev use ij
      merged_data <- non_gdp %>%
        inner_join(month_rev, by = "Year-Month", suffix = c("_non_gdp", "_month_rev"))
      
      # 2. merge for Total_Rev_Offset1 use ij and rename columns 
      merged_data <- merged_data %>%
        inner_join(month_rev, by = c("Year-Month_Offset1" = "Year-Month"), suffix = c("", "_Offset1")) %>%
        rename(Total_Rev_Offset1 = Total_Rev_Offset1)
      
      # 3. merge for Total_Rev_Offset2 use ij and rename columns
      merged_data <- merged_data %>%
        inner_join(month_rev, by = c("Year-Month_Offset2" = "Year-Month"), suffix = c("", "_Offset2")) %>%
        rename(Total_Rev_Offset2 = Total_Rev_Offset2)
      
      # return the merged data with the new columns
      DT::datatable(merged_data, options = list(pageLength = 10))
    })
  }
  