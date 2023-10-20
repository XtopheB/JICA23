library(shiny)
library(ggplot2)

# The  trend is initaly chosen at random 
dynamicValue = function(){
  runif(1, min = -1, max =1.5)
}


# Define the UI
ui <- fluidPage(
  # Application title
  titlePanel( HTML("<h3><font color='#2874A6'>Playing with the regression line</font></h4>")),
  tags$h5("Christophe Bontemps (SIAP)"),
  
  #titlePanel("Scatter Plot with Regression Line"),
  sidebarLayout(
    sidebarPanel(
      HTML("<h4><font color='#2874A6'>Let's adjust a line! </font></h4>"),
      checkboxInput("show_line", "Play with Y = a X +b", value = FALSE), 
      conditionalPanel(
        condition = "input.show_line == true",
        sliderInput("coef_a", "Coefficient a", min = -1, max = 2, value = dynamicValue(), step = 0.1),
        sliderInput("coef_b", "Coefficient b", min = -2, max = 2, value = -0.5, step = 0.1), 
        HTML("<h5><em > The first line you see is randomly chosen! </em></h4>"),
        br()
        
        ),
      br(),
      HTML("<h4><font color='#2874A6'> Let's see the \"right\" result </font></h4>"),
      checkboxInput("show_regression", "Show Regression Line", value = FALSE)
    ),
    mainPanel(
      plotOutput("scatterplot")
    )
  )
)
