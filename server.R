function(input, output) {
    # plot of gold medals by country by year
    output$plot1 <- renderGvis({
      gvisGeoChart(plot1, locationvar="region", colorvar="Total Medals")
    })
    
    output$plot2 <- renderGvis({
      gvisColumnChart(plot1 %>% arrange(desc(`Total Medals`)) %>% head(input$topNcountries), xvar = "region", yvar = "Total Medals")
    })
    
    output$plot3 <- renderGvis({
      gvisLineChart(plot3, xvar = "Year", yvar = c("Female", "Male"))
    })
    
    output$plot4 <- renderPlot({
      ggplot(plot4_1, aes(x = ratio, y = region)) +
        geom_point(na.rm = FALSE) +
        xlim(0, 1)
    })
    
    output$plot5 <- renderPlot({
      ggplot(plot5, aes(x = ratio, y = region)) +
        geom_point(na.rm = FALSE, alpha=0.8) +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.y = element_text(size=6)) +
        xlim(0,1)
    })
    
    output$plot6 <- renderPlot({
      ggplot(plot6_1, aes(x = region, y = value, label = value)) +
        geom_point(stat = 'identity', aes(col = type), size = 6)  +
        coord_flip()
    })
}
