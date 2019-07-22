library(DT)
library(shiny)
library(googleVis)

shinyServer(function(input, output){
    
  
  output$rndr_scat_PERvs3PA = renderPlotly({ 
    sub_data = stats_all_1978to2017 %>%
      filter(G >= 30,
             MPG >= 24,
             PER >= 20) %>% 
      subset(.,Year >= input$Seasons[1] & Year <= input$Seasons[2])
    
    sub_data %>%  
      plot_ly(
        type = 'scatter',
        mode = 'markers',
        x = ~PER,
        y =~X3PA, 
        #size = ~height_inches,
        marker = list(size = ~WS,sizemode = 'area'),
        color = ~Pos_modern,
        text = ~paste(Player," - ",Year),
        hovertemplate = paste(
          "<b>%{text}</b><br><br>",
          "%{xaxis.title.text}: %{x:.00}<br>",
          "%{yaxis.title.text}: %{y:.00}<br>",
          "3PT%: ",
          .$X3P.*100,
          "<extra></extra>"
        )) %>%
      layout(title = "20+ PER vs 3PA",legend = list(orientation = 'h', y = -0.3))
  })
  
  
  
  #show plotly Seasons w. 20+ PER by Player Height
  output$scatPERvsHeight = renderPlotly({
    sub_data = stats_all_1978to2017 %>%
      filter(G >= 30,
             MPG >= 24,
             PER >= 20) %>% 
      subset(.,Year >= input$Seasons[1] & Year <= input$Seasons[2])
    
    ggplotly(
      sub_data %>%
        ggplot(aes(x = round(height_inches,0), y = PER, color = .$Pos_modern)) +
        geom_point() + theme_tufte() + 
        labs(title = "Players with 20+ PER by Height", x = 'Height (inches)') +
        theme(legend.title = element_blank(), legend.position = "bottom")
    )
  })
  
  
  #show plotly Seasons w. 20+ PER by Player Height
  output$barPERvsHeight = renderPlotly({ 
    sub_data = stats_all_1978to2017 %>%
      filter(G >= 30,
             MPG >= 24,
             PER >= 20) %>% 
      subset(.,Year >= input$Seasons[1] & Year <= input$Seasons[2])
    
    ggplotly(
      sub_data %>%
        ggplot(aes(x = round(height_inches,0), fill = .$Pos_modern)) +
        geom_bar() + theme_tufte() + 
        labs(xaxis = list(title = "Height"), 
             yaxis = list(title = ""),
             title = paste("Players with 20+ PER from",input$Seasons[1]," - ",input$Seasons[2],"by Height")))
  })
  

  output$barTop10PER_byPos = renderPlotly({
    ggplotly(
      top10PER_PerSeason %>%
        ggplot(aes(x = Year, fill = Pos_modern, order = Pos_modern)) + geom_bar() + theme_tufte() + 
        labs(title = "Distribution of Top 10 PER each season by Position") +
        theme(legend.title = element_blank())
    )
  })
  
  output$line_3ptRate = renderPlotly({
    ggplotly(
      seasons_distro_shots %>% 
        ggplot(aes(x = Year)) + 
        geom_line(aes(y = avg_3PAR, color = "TOT 3PA")) +
        geom_line(aes(y = avg_2PAR, color = "TOT 2PA")) +
        theme_tufte() + xlab("") + ylab("") +
        theme(legend.title = element_blank()) +
        labs(title = "The rate of 3 point attempts has been rising since 1980")
      
    )
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
    #options = list(
    #  pageLength = 25,
    #  initComplete = I("function(settings, json){
    #                   new $.fn.dataTable.FixedHeader(this, {
    #                   left:   true,
    #                   right:  true
    #                   } );
    #                   }"))
    options = list(scrollX = TRUE,
                  scrolly = TRUE
                  )
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