library(shiny)
require(rCharts)

spells <- read.csv("spells_frame.csv")[,1]

shinyUI(fluidPage(
  
  h1("Harry Potter", align="center"),
  
  conditionalPanel(
    condition = "input.activeTab != 'Spells\\' description'",
    fluidRow(
      column(12, align="center", radioButtons("filter", NULL,
                                              c("Custom filter" = "custom",
                                                "Unforgivable Curses" = "forbidden",
                                                "Duelling" = "duels",
                                                "Popular" = "popular"), inline = TRUE))
    ),
    
    conditionalPanel(condition = "input.filter == 'custom'",
                     fluidRow(
                       column(12, align="center",
                              selectInput("spells", "Spells", spells, selected=c("Expecto Patronum", "Riddikulus"), multiple = TRUE, width = '100%'))
                     ))
  ),
  
  
  tabsetPanel(
    id ="activeTab",
    tabPanel("Area chart",
             splitLayout(
                   div(h3("Movies",align="center"),
                       showOutput("spellMoviePlotArea", "nvd3")),
                   div(h3("Books",align="center"),
                       showOutput("spellBookPlotArea", "nvd3"))
                 )),
   tabPanel("Bar chart",
            splitLayout(
                 div(h3("Movies",align="center"),
                     showOutput("spellMoviePlot", "nvd3")),
                 div(h3("Books",align="center"),
                     showOutput("spellBookPlot", "nvd3"))
                 )),
   tabPanel("Spells' description",
          dataTableOutput('spellsDescription')))
  )
)
