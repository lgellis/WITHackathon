#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  br(),
  # Application title
  headerPanel("Fire Disaster Prediction System"),
  
  # df_counties <-fread('https://raw.githubusercontent.com/lgellis/WITHackathon/master/files/counties.csv', stringsAsFactors = FALSE),
  df_counties <-fread('./data/counties.csv', stringsAsFactors = FALSE),
  df_regions <-fread('./data/regions.csv', stringsAsFactors = FALSE),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput('region_id', label = 'Select Region', c(Choose = '', df_regions[1]), selectize = FALSE),
      selectInput('element_id', label = 'Select County', c(Choose = '', df_counties[1]), selectize = FALSE),
      dateInput(
        'date_input_id',
        label = 'Disaster Start Date',
        value = NULL,
        min = NULL,
        max = NULL,
        format = "yyyy-mm-dd",
        startview = "month",
        weekstart = 0,
        language = "en",
        width = NULL,
        autoclose = TRUE
      )
    ),
    # Show a plot of the generated distribution
    mainPanel(
      uiOutput("toColumns"),
      # h1('The title of some text'),
      imageOutput("preImage"),
      p('Recommendation will be here...')
      # ,plotOutput("distPlot")
    )
  )
))
