function(input, output) {
    # plot of gold medals by country by year
    output$plot1_1 <- renderGvis({
      gvisGeoChart(plot1_1, locationvar="region", colorvar="Total Medals")
    })
    
    output$plot1_2 <- renderGvis({
      gvisColumnChart(plot1_1 %>% arrange(desc(`Total Medals`)) %>% head(input$topNcountries), xvar = "region", yvar = "Total Medals")
    })
    
    output$plot2_1 <- renderGvis({
      gvisLineChart(plot2_1, xvar = "Year", yvar = c("Female", "Male"))
    })
    
    output$plot2_2 <- renderPlot({
      ggplot(plot2_2_1, aes(x = ratio, y = region)) +
        geom_point(na.rm = FALSE) +
        xlim(0, 1)
    })
    
    output$plot2_3 <- renderPlot({
      ggplot(plot2_3, aes(x = ratio, y = region)) +
        geom_point(na.rm = FALSE, alpha=0.8) +
        xlim(0,1)
    })
    
    output$plot2_4 <- renderPlot({
      ggplot(plot2_4_1, aes(x = region, y = value, label = value)) +
        geom_point(stat = 'identity', aes(col = type), size = 6)  +
        coord_flip()
    })
    
    output$plot3_1 <- renderPlot({
      ggplot(plot3_1, aes(x = as.factor(Year), y = Height, fill = Sex)) +
        geom_boxplot()
    })
    
    output$plot3_2 <- renderPlot({
      ggplot(plot3_2, aes(x = as.factor(Year), y = Weight, fill = Sex)) +
        geom_boxplot()
    })
    
    output$plot3_3 <- renderPlot({
      ggplot(plot3_3 %>% filter(Sport == input$sportSelected), aes(x=Weight, y=Height)) +
        geom_point(aes(col=Year))
    })
    
    output$plot3_4 <- renderPlot({
      ggplot(plot3_4 %>% filter(Sport == input$sportSelected), aes(x=Weight, y=Height)) +
        geom_point(aes(col=Year))
    })
    
    output$plot3_5 <- renderPlot({
      ggplot(plot3_5, aes(x=Year)) + 
        geom_line(aes(y=`Average Age`, col=Sex))
    })
    
    #output$table4_1 <- renderReactable({
    #  reactable(table4_1)
    #})
    
    output$table4_1 <- renderPlot({
      ggplot(table4_1, aes(x=Name, y=Medals, fill=Medal)) +
        geom_col() +
        coord_flip() 
    })
    
    
}
