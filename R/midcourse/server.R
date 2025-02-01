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
    
    if (input$S_Cons_Order_Class != "All"){
      plot_data <- plot_data %>%
        filter(S_Cons_Order_Class == input$S_Cons_Order_Class)
    }
    
    return(plot_data)
  })
  
  # First plot  
  output$distPlot <- renderPlot({
    
    plot_data() %>%
      ggplot(aes(x = .data[[input$OrderClass]])) +
      geom_histogram(bins = 50) 
    
  })  # <-- closes renderPlot
  
}  # <-- closes the function

