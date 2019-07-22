
shinyServer(function(input, output){
    
  #scatter plot of 20+ PER seasons vs players' 3 point attempts
  output$rndr_scat_PERvs3PA = renderPlotly({ 
    sub_data = stats_all_1978to2017 %>%
      filter(G >= 30,
             MPG >= 24,
             PER >= 20) %>% 
      subset(.,Year >= input$anim_Seasons[1] & Year <= input$anim_Seasons[2])
    
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
  
  
  
  #scatter plot of Seasons w. 20+ PER by Player Height
  output$scatPERvsHeight = renderPlotly({
    sub_data = stats_all_1978to2017 %>%
      filter(G >= 30,
             MPG >= 24,
             PER >= 20) %>% 
      subset(.,Year >= input$Seasons[1] & Year <= input$Seasons[2])
    
    ggplotly(
      sub_data %>%
        ggplot(aes(x = round(height_inches,0), 
                   y = PER, 
                   color = .$Pos_modern, 
                   text = paste(Player," - ", Year)
                   )
              ) +
        geom_point() + theme_tufte() + 
        labs(title = "Players with 20+ PER by Height", x = 'Height (inches)') +
        theme(legend.title = element_blank(), legend.position = "bottom")
    )
  })
  
  
  #bar chart of Seasons w. 20+ PER by Player Height
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
  
  #Bar chart showing distribution of top 10 PER every season by Position
  output$barTop10PER_byPos = renderPlotly({
    ggplotly(
      top10PER_PerSeason %>%
        ggplot(aes(x = Year, fill = Pos_modern, order = Pos_modern)) + geom_bar() + theme_tufte() + 
        labs(title = "Distribution of Top 10 PER each season by Position") +
        theme(legend.title = element_blank())
    )
  })
  
  #scatter plot of 20+ PER seasons vs players' 3 point attempts
  output$scatTop10PER_byPos = renderPlotly({ 
    sub_data_top10 = top10PER_PerSeason %>%
      subset(.,Year >= input$Seasons2[1] & Year <= input$Seasons2[2])
    
    sub_data_top10 %>%  
      plot_ly(
        type = 'scatter',
        mode = 'markers',
        x = ~Year,
        y =~PER, 
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
      layout(title = "Top 10 PER each season: player points",legend = list(orientation = 'h', y = -0.3))
  })
  
  
  #line graph showing rate of 3 point shots taken since 1980, the year the 3pt line was instituted in NBA
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
  
 
})