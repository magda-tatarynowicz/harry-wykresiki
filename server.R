library(shiny)
require(rCharts)

shinyServer(function(input, output) {
  meltSpells <- function(df) {
    result <- NULL
    for(i in 1:(ncol(df) - 1)) {
      result <- rbind(result, data.frame(spell = df[,1], count = df[,i+1], part = rep(i, nrow(df))))
    }
    result
  }
  
  spells <- read.csv("spells_frame.csv")
  
  selectedSpells <- reactive({
    spells[tolower(spells$Incantation) %in% tolower(input$spells),]
  })
   
  output$spellMoviePlot <- renderChart({
    movies <- selectedSpells();
    movies <- meltSpells(movies[,c(1,4:11)])
    
    p = nPlot(count ~ part, group =  "spell", data = movies, type = 'stackedAreaChart', dom="spellMoviePlot")
    p$chart(useInteractiveGuideline=TRUE)
    p$set(width = 900, height = 800)
    return(p)
  })
  
  output$spellBookPlot <- renderChart({
    books <- selectedSpells();
    books <- meltSpells(books[,c(1,12:18)])
    
    p = nPlot(count ~ part, group =  "spell", data = books, type = 'stackedAreaChart', dom="spellBookPlot")
    p$chart(useInteractiveGuideline=TRUE)
    p$set(width = 900, height = 800)
    return(p)
  })
  
})
