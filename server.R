library(DT)
library(shiny)
library(googleVis)

shinyServer(function(input, output){
    # show map using googleVis
    output$map <- renderGvis({
        gvisGeoChart(state_stat, "state.name", input$selected,
                     options=list(region="US", displayMode="regions", 
                                  resolution="provinces",
                                  width="auto", height="auto"))
    })
    
    #show plotly DBPM vs PER
    output$scatPERvsDBPM = renderPlotly({ 
      scat_PERvsDBPM
    })
    
    #show plotly BPM vs PER
    output$scatPERvsBPM = renderPlotly({ 
      scat_PERvsBPM
    })
    

    # show data using DataTable
    output$tblSeasons = renderDataTable({
        season_stats %>% 
          select(-X) %>% 
          datatable(., rownames=FALSE) #%>%
        #    formatStyle(input$selected, background="white")
    })
    
    # show data using DataTable - just +30 PERs
    output$tbl30plusPER = renderDataTable({
      season_stats},
      options = list(
        pageLength = 25,
        initComplete = I("function(settings, json){
                         new $.fn.dataTable.FixedHeader(this, {
                         left:   true,
                         right:  true
                         } );
                         }"))
      #options = list(scrollX = TRUE,
       #              scrolly = TRUE)
        #formatStyle(input$selected, background="skyblue", fontWeight='bold')
    )
    
    # show statistics using infoBox
    output$maxBox <- renderInfoBox({
        max_value <- max(state_stat[,input$selected])
        max_state <- 
            state_stat$state.name[state_stat[,input$selected] == max_value]
        infoBox(max_state, max_value, icon = icon("hand-o-up"))
    })
    output$minBox <- renderInfoBox({
        min_value <- min(state_stat[,input$selected])
        min_state <- 
            state_stat$state.name[state_stat[,input$selected] == min_value]
        infoBox(min_state, min_value, icon = icon("hand-o-down"))
    })
    output$avgBox <- renderInfoBox(
        infoBox(paste("AVG.", input$selected),
                mean(state_stat[,input$selected]), 
                icon = icon("calculator"), fill = TRUE))
})