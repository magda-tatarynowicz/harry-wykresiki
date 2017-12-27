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
  
  spellsDescription <- spells[,c("Incantation", "Type", "Resulting.Effect")]
  colnames(spellsDescription) <- c("Spell", "Type", "Effect")
  output$spellsDescription <-  renderDataTable(spellsDescription)
  
  selectedSpells <- reactive({
    if(input$filter == "custom"){
      if(length(input$spells) > 0) {
        selected <- spells[tolower(spells$Incantation) %in% tolower(input$spells),]
      }
      else {
        selected <- NULL
      }
    }
    if (input$filter == "forbidden") {
       selected <- spells[spells$Incantation %in% c("Avada Kedavra", "Crucio", "Imperio"),]
    }
    if (input$filter == "duels") {
      selected <- spells[spells$Incantation %in% c("Expelliarmus", "Stupefy", "Protego", "Reducto"),]
    }
    if (input$filter == "popular") {
      selected <- spells[spells$Incantation %in% c("Accio", "Wingardium Leviosa", "Alohomora", "Lumos"),]
    }
    selected
  })
   
  output$spellMoviePlot <- renderChart({
    movies <- selectedSpells();
    if(is.null(movies)) {
      return(NULL);
    }
    movies[,10] <- movies[,10] + movies[,11]
    movies <- meltSpells(movies[,c(1,4:10)])

    p = nPlot(count ~ part, group =  "spell", data = movies, type = "multiBarChart", dom="spellMoviePlot")
    p$chart(stacked = TRUE)
    p$set(width = 600, height = 500)
    p$yAxis(axisLabel ="Number of occurrences", width = 60)
    p$xAxis(axisLabel = "Part")
    p$chart(reduceXTicks = FALSE)
    return(p)
  })
  
  output$spellBookPlot <- renderChart({
    books <- selectedSpells();
    if(is.null(books)) {
      return(NULL);
    }
    books <- meltSpells(books[,c(1,12:18)])
    
    p = nPlot(count ~ part, group =  "spell", data = books, type = 'multiBarChart', dom="spellBookPlot")
    p$chart(stacked = TRUE)
    p$set(width = 600, height = 500)
    p$yAxis(axisLabel ="Number of occurrences", width = 60)
    p$xAxis(axisLabel = "Part")
    p$chart(reduceXTicks = FALSE)
    return(p)
  })
  
})
