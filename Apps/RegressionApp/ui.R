library(shiny)
library(ggplot2)

# The  trend is initaly chosen at random 
dynamicValue = function(){
  runif(1, min = -1, max =1.5)
}


# Define the UI
ui <- fluidPage(
  # Application title
  titlePanel("Playing with regression line"),
  tags$h4("JICA-TAPOS Course"),
  tags$h5("Christophe Bontemps (SIAP)"),
  tags$h5("August- December 2023"),
  
  #titlePanel("Scatter Plot with Regression Line"),
  sidebarLayout(
    sidebarPanel(
      HTML("<h4><font color='#2874A6'> Play with Y = a X +b </font></h4>"),
      sliderInput("coef_a", "Coefficient a", min = -1, max = 2, value = dynamicValue(), step = 0.1),
      sliderInput("coef_b", "Coefficient b", min = -2, max = 2, value = -0.5, step = 0.1), 
      br(),
      HTML("<h4><font color='#2874A6'> Let's see the \"right\" result </font></h4>"),
      checkboxInput("show_regression", "Show Regression Line", value = FALSE)
    ),
    mainPanel(
      plotOutput("scatterplot")
    )
  )
)
