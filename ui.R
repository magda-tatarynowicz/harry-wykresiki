library(shiny)
require(rCharts)

spells <- read.csv("spells_frame.csv")[,1]

shinyUI(fluidPage(
  
  h1("Harry Potter", align="center"),
  
  fluidRow(
    column(12, align="center",
           selectInput("spells", "Zaklęcia", spells, selected=c("Expelliarmus", "Avada Kedavra"), multiple = TRUE, width = '100%'))
  ),
    
  fluidRow(
    column(6,
            h3("Filmy",align="center"),
            showOutput("spellMoviePlot", "nvd3")),
    column(6,
            h3("Książki",align="center"),
            showOutput("spellBookPlot", "nvd3"))
    )
  )
)
