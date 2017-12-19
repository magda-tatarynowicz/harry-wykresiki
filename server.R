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
    selected <- spells[tolower(spells$Incantation) %in% tolower(input$spells),]
    if (input$checkboxForbidden) {
       selected <- spells[spells$Incantation %in% c("Avada Kedavra", "Crucio", "Imperio"),]
    }
    if (input$checkboxFight) {
      selected <- spells[spells$Incantation %in% c("Expelliarmus", "Stupefy", "Protego"),]
    }
    if (input$checkboxPopular) {
      selected <- spells[spells$Incantation %in% c("Expecto Patronum", "Accio", "Wingardium Leviosa", "Alohomora"),]
    }
    selected
  })
   
  output$spellMoviePlot <- renderChart({
    movies <- selectedSpells();
    movies[,10] <- movies[,10] + movies[,11]
    movies <- meltSpells(movies[,c(1,4:10)])

    p = nPlot(count ~ part, group =  "spell", data = movies, type = "multiBarChart", dom="spellMoviePlot")
    #p$chart(useInteractiveGuideline=TRUE)
    p$chart(stacked = TRUE)
    p$set(width = 900, height = 800)
    return(p)
  })
  
  output$spellBookPlot <- renderChart({
    books <- selectedSpells();
    books <- meltSpells(books[,c(1,12:18)])
    
    p = nPlot(count ~ part, group =  "spell", data = books, type = 'multiBarChart', dom="spellBookPlot")
    #p$chart(useInteractiveGuideline=TRUE)
    p$chart(stacked = TRUE)
    p$set(width = 900, height = 800)
    return(p)
  })
  
  output$spellMoviePlotArea <- renderChart({
    movies <- selectedSpells();
    movies[,10] <- movies[,10] + movies[,11]
    movies <- meltSpells(movies[,c(1,4:10)])
    
    p = nPlot(count ~ part, group =  "spell", data = movies, type = "stackedAreaChart", dom="spellMoviePlotArea")
    p$chart(useInteractiveGuideline=TRUE)
    #p$chart(stacked = TRUE)
    p$set(width = 900, height = 800)
    return(p)
  })
  
  output$spellBookPlotArea <- renderChart({
    books <- selectedSpells();
    books <- meltSpells(books[,c(1,12:18)])
    
    p = nPlot(count ~ part, group =  "spell", data = books, type = 'stackedAreaChart', dom="spellBookPlotArea")
    p$chart(useInteractiveGuideline=TRUE)
    #p$chart(stacked = TRUE)
    p$set(width = 900, height = 800)
    return(p)
  })
  
})
