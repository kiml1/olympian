function(input, output) {
  output$plot1_1 <- renderGvis({
    gvisGeoChart(plot1_1, locationvar="region", colorvar="Total Medals")
  })
  
  output$plot2_1 <- renderGvis({
    gvisLineChart(plot2_1, xvar = "Year", yvar = c("Female", "Male"),
                  options=list(legend="{position: 'top', textStyle: {fontSize: 16}}",
                               hAxis="{format: '0000'}"))
  })
}
