function(input, output) {
    # plot of gold medals by country by year
    output$plot1 <- renderGvis({
      gvisGeoChart(plot1, locationvar="region", colorvar="Total Medals")
    })
    
    output$plot2 <- renderGvis({
      gvisColumnChart(plot1 %>% arrange(desc(`Total Medals`)) %>% head(input$topNcountries), xvar = "region", yvar = "Total Medals")
    })
}
