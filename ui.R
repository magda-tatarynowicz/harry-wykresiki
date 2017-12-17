library(shiny)
require(rCharts)

spells <- read.csv("spells_frame.csv")[,1]



shinyUI(fluidPage(
  
  titlePanel("Harry Potter"),
  
  sidebarLayout(
    sidebarPanel(
       selectInput("spells", "Zaklęcia", spells, selected=c("Expelliarmus", "Avada Kedavra"), multiple = TRUE)
    ),
    
    
    mainPanel(
      showOutput("spellMoviePlot", "nvd3"),
      showOutput("spellBookPlot", "nvd3")
    )
  )
))
