library(shiny)
library(shinythemes)
require(rCharts)

spells <- read.csv("spells_frame.csv")[,1]

shinyUI(fluidPage(theme = shinytheme("sandstone"),
  tags$head(tags$style(HTML("#DataTables_Table_0_wrapper { margin-top: 16px; }"))),
  tags$head(tags$link(rel="shortcut icon", href="https://png.icons8.com/material/50/000000/harry-potter.png")),
  tags$head(tags$title("Spells in Harry Potter")),
                  
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
   tabPanel("Spells visualization",
            conditionalPanel(
              condition = "input.filter != 'custom' || input.spells != null",
              splitLayout(
                div(h3("Movies",align="center"),
                    showOutput("spellMoviePlot", "nvd3")),
                div(h3("Books",align="center"),
                    showOutput("spellBookPlot", "nvd3"))
              )),
            conditionalPanel(
              condition = "input.filter == 'custom' && input.spells == null",
              h4("Please select spells", align="center"))
            ),
   tabPanel("Spells description",
          dataTableOutput('spellsDescription')))
  )
)
