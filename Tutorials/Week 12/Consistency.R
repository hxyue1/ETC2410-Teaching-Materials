library(shiny)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  sidebarPanel(
    numericInput(inputId="n", 
                label = "Sample size", 
                value = 2, min = 1, max = 10000), 
    
    numericInput(inputId = "mu",
                 label = "Mean", 
                 value = 10, min = -50, max = 50), 
    
    numericInput(inputId = "sigma",
                 label = "Standard Deviation", 
                 value = 2, min = 0, max = 100)
  ),
  
  mainPanel(plotOutput(outputId = "xhist"),plotOutput(outputId = "xbarhist"))
)


server <- function(input,output){
  observeEvent(input$refresh, {
    shinyjs::reset("form")
  })
  
  output$xhist <- renderPlot({
    x <-rnorm(round(input$n),input$mu,input$sigma)
    hist(x,
         col = "#75AADB", 
         border = "white",
         xlab= "",
         main = "Histogram of sample",
         xlim = c(input$mu-2.5*input$sigma,input$mu+2.5*input$sigma)
    ) 
    abline(v=input$mu)
    abline(v=mean(x), lty=2)
    }) 
  
  output$xbarhist <- renderPlot({
    x <- rnorm(100000,input$mu,input$sigma/sqrt(input$n))
    hist(x,
         col = "#75AADB", 
         border = "white",
         xlab= "",
         main = "Distribution of sample mean",
         xlim = c(input$mu-2.5*input$sigma,input$mu+2.5*input$sigma)
    ) 
    abline(v=input$mu)
  })
}

shinyApp(ui=ui, server=server)