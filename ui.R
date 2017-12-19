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
    column(12, align="center", radioButtons("filter", "",
                 c("Własne" = "custom",
                   "Niewybaczalne" = "forbidden",
                   "Używane w pojedynkach" = "duels",
                   "Popularne" = "popular"), inline = TRUE))
  ),
  
  tabsetPanel(
    tabPanel("Area chart",
             fluidRow(
                 column(6,
                         h3("Filmy",align="center"),
                         showOutput("spellMoviePlotArea", "nvd3")),
                 column(6,
                         h3("Książki",align="center"),
                         showOutput("spellBookPlotArea", "nvd3"))
                 )),
   tabPanel("Bar chart",
          fluidRow(
                 column(6,
                        h3("Filmy",align="center"),
                        showOutput("spellMoviePlot", "nvd3")),
                 column(6,
                        h3("Książki",align="center"),
                        showOutput("spellBookPlot", "nvd3"))
                 )))
  )
)
