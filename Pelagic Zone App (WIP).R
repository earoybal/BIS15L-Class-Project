library(tidyverse)
library(shiny)
library(shinydashboard)

#make sure you have pelagic_org.csv inside your working directory

pelagic_org <- read.csv('pelagic_org.csv')

ui <- dashboardPage(
  dashboardHeader(title = 'San Joaquin River Delta Data'),
  dashboardSidebar(disable = T),
  dashboardBody(
    fluidRow(
      box(title = 'Plot Options', width = 3,
          selectInput('y', 'Select Analyte', choices = c('pH', 'Oxygen, Dissolved, Not Recorded', 'Hardness as CaCO3, Not Recorded', 'Temperature', 'Ammonia as NH3, Unionized, Not Recorded'),
                      selected = 'pH')),
      box(title = 'Plot of Analyte', width = 7,
          plotOutput('plot', width = '400px', height = '400px')))
  ))

server <- function(input, output, session) {
  output$plot <- renderPlot({
    pelagic_org %>%
      filter(analyte == input$y, sample_date <= '2010-12-31') %>%
      ggplot(aes_string(x = 'sample_date', y = 'result')) +
      geom_point(alpha = .5) +
      geom_smooth(method = lm, color = 'yellow', alpha = .5) +
      geom_smooth(color = 'red', alpha = .5) +
      theme_light() +
      labs(title = 'Analyte Change Over Time',
           x = 'Date',
           y = if (input$y == 'pH') {
             'pH'
           } else if (input$y == 'Temperature') {
             'Temperature (Deg C)'
           } else if (input$y == 'Oxygen, Dissolved, Not Recorded') {
             'Dissolve Oxygen (mg/L)'
           } else if (input$y == 'Hardness as CaCO3, Not Recorded') {
             'Water Hardness Measured by CaCO3 Concentrations (mg/L)'
           } else if (input$y == 'Ammonia as NH3, Unionized, Not Recorded') {
             'Amount of NH3 in Water (mg/L)'
           }
      )
    # session$onSessionEnded(stopApp)
    #I don't know why but commenting this out seems to make it work 
  })
}

shinyApp(ui, server)
