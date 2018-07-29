#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
#install.packages("data.table")
library(shiny)
# library(data.table)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Send a pre-rendered image, and don't delete the image after sending it
  output$preImage <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- normalizePath(file.path('./images',
                                        paste('image', input$n, '.jpeg', sep='')))
  
    # Return a list containing the filename and alt textxs
    list(src = filename,
         width = 260,
         height = 260,
         alt = paste("Image number", input$n))
    
  }, deleteFile = FALSE)
  
  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')

  })
})
