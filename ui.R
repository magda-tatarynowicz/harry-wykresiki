library(shiny)
require(rCharts)

spells <- read.csv("spells_frame.csv")[,1]

shinyUI(fluidPage(
  
  h1("Spells in the Harry Potter series", align="center"),
  
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
                              selectInput("spells", "Selected spells", spells, selected=c("Expecto Patronum", "Riddikulus"), multiple = TRUE, width = '100%'))
                     ))
  ),
  
  
  tabsetPanel(
   id ="activeTab",
   tabPanel("Spells' visualization",
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
