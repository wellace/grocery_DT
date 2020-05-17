# This is a shiny app for explorng Australia grocery data
library(shiny)
library(tidyverse)
library(ggbeeswarm)


# Loading data ------------------------------------------------------------
time_data <- read_tsv(file = "shiny/02_time.tsv")

selections <- time_data %>%
  colnames()

ui <- fluidPage(
  titlePanel("GroceryX: Trends by Time Analysis"),
  
  # checkboxes for log scale
  checkboxInput("logarithmicX", "Show x-axis in log10", FALSE),
  checkboxInput("logarithmicY", "Show y-axis in log10", TRUE),
  
  br(),
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    # Inputs
    sidebarPanel(
      # Select choices to put in the x-axis
      selectInput(inputId = "x", label = "X-axis",
                  choices = selections ,
                  selected = "DATE"
      ),
      # Select choices to put in the y-axis
      selectInput(inputId = "y", label = "Y-axis",
                  choices = selections ,
                  selected = "AVG_SALES"
      ),
      
      # Select choices to put in the x-axis
      selectInput(inputId = "size", label = "Size",
                  choices = selections ,
                  selected = "TOTAL_VOLUME"
      ),
      
      # select choices to color stuff
      selectInput(inputId = "ColorVar", label = "Color",
                  choices = selections ,
                  selected = "PAYMENT"
      ),
      # select choises for the alpha scale
      selectInput(inputId = "alpha", label = "Alpha",
                  choices = selections,
                  selected = "PAYMENT"
      ),
      # select choises for facet
      selectInput(inputId = "facet", label = "Facet",
                  choices = c("none", selections ) ,
                  selected = "none"),
      
      # select choises for plot type
      selectInput("plot.type","Plot Type:",
                  list(Boxplot = "Boxplot", Scatterplot = "Scatterplot"),
                  selected = "Scatterplot"
      ),
      
      width = 6
    ),
    
    # Output:
    mainPanel(
      # Create a container for tab panels
      tabsetPanel(
        
        tabPanel(
          title = "Analysis Panel",
          # Show Scatterplot
          plotOutput(outputId = "plots")
        )
        
      ),
      
      width = 6
    )
  )
)
# Define server function required to create the Scatterplot
server <- function(input, output) {
  
  # Create Scatterplot object the plotOutput function is expecting
  output$plots <- renderPlot({
    
    # switch for plot type
    renderText({
      switch(input$plot.type,
             "Boxplot" 	= 	"Boxplot",
             "Scatterplot" =	"Scatterplot")
    })
    
    # if statement for duing facet or not
    if(input$facet=="none")
      plot.type<-switch(input$plot.type,
                        "Boxplot" 	= time_data %>%
                          ggplot(mapping = aes_string(x = input$x, y = input$y)) +
                          geom_quasirandom(aes_string(color = input$ColorVar, size = input$size, alpha= input$alpha)) +
                          geom_boxplot(aes_string(fill = input$ColorVar),
                                       alpha = .5, outlier.shape = NA, colour = '#525252') +
                          theme_bw() ,
                        
                        "Scatterplot" = time_data %>%
                          ggplot(mapping = aes_string(x = input$x, y = input$y)) +
                          geom_point(aes_string(color = input$ColorVar, alpha= input$alpha, size = input$size)) +
                          theme_bw() )
    if(input$facet!="none")
      plot.type<-switch(input$plot.type,
                        "Boxplot" 	= time_data %>%
                          ggplot(mapping = aes_string(x = input$x, y = input$y)) +
                          geom_quasirandom(aes_string(color = input$ColorVar, size = input$size, alpha= input$alpha)) +
                          geom_boxplot(aes_string(fill = input$ColorVar),
                                       alpha = .5, outlier.shape = NA, colour = '#525252') +
                          theme_bw() +
                          facet_grid(~get(input$facet)),
                        
                        "Scatterplot" = time_data %>%
                          ggplot(mapping = aes_string(x = input$x, y = input$y)) +
                          geom_point(aes_string(color = input$ColorVar, alpha= input$alpha, size = input$size)) +
                          theme_bw() +
                          facet_grid(~get(input$facet)))
    
    
    # if statement for making log scales
    if(input$logarithmicX)
      plot.type <- plot.type + scale_x_log10(breaks = c(0.01, 0.10, 1.00, 2.00, 10))
    
    if(input$logarithmicY)
      plot.type <- plot.type + scale_y_log10(breaks = c(0.01, 0.10, 1.00, 2.00, 10))
    
    return(plot.type)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)

###########################################
