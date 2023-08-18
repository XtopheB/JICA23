#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Global 
## Libraries 
library(readxl)
library(tidyverse)
library(viridis)
library(ggthemes)
library(ggplot2)

## Options and datasets 
# My colors:
SIAP.color <- "#0385a8"
# Data extracted from  https://unstats.un.org/SDGS/Indicators/Database/?area=TTO
# Only countries with letters A-C selected to avoid uneccessary heavy files

SDGdata1<- readxl::read_xlsx("data/SDGDataSample.xlsx", col_names = TRUE, sheet = "Goal1")

SDGdata1$Poverty <- as.numeric(SDGdata1$Value)
SDGPov <- subset(SDGdata1, Poverty >5 & Sex =="BOTHSEX" )
SDGPov.caption <- paste("Proportion of population below international
                           poverty line (%)")

# --- Here we create a dataset
set.seed(2512)
MyDataNumCat <- data.frame(
    Country=c( rep("A",500), rep("B",500), rep("B",500), rep("C",20), rep('D', 100)  ),
    Poverty=c( rnorm(500, 20, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(20, 25, 4), rnorm(100, 12, 1) )
)

#  Avg by category 
MyDataNumCatAvg <- MyDataNumCat %>%
    group_by(Country) %>%
    summarize(
        Poverty.Avg = mean(Poverty)
    )




# Define UI for application that draws the points
ui <- fluidPage(
  # Application title
  titlePanel("How jitter helps visualizing univariate data"),
  tags$h4("JICA-TAPOS Course"),
  tags$h5("Christophe Bontemps (SIAP)"),
  tags$h5("August- December 2023"),

  # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("Jitter",
                        "Jitter height:",
                        min = 0,
                        max = 0.2,
                        step = 0.02,
                        value = 0, 
                        animate = TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        set.seed(2512)
        ggplot(data= SDGPov) +
            aes(x = Poverty, y ="") +
            geom_jitter(color=SIAP.color, size=0.9, alpha=0.9,  height = input$Jitter) +
            ggtitle("Points (jitter)")+ 
            xlab(SDGPov.caption)+
            theme_tufte()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
