function(input, output) {
  output$plot1_1 <- renderGvis({
    gvisGeoChart(plot1_1, locationvar="region", colorvar="Total Medals")
  })
  
  output$plot1_2 <- renderGvis({
    gvisBubbleChart(plot1_2, idvar = "Country.Name", xvar = "Medals", yvar = "GDPbillions", 
                    colorvar="GDPbillions", sizevar = "GDPbillions",
                    options=list(width=800, height=600))
  })
  
  
  
  
  
  output$plot2_1 <- renderGvis({
    gvisLineChart(plot2_1, xvar = "Year", yvar = c("Female", "Male"),
                  options=list(legend="{position: 'top', textStyle: {fontSize: 16}}",
                               hAxis="{format: '0000'}"))
  })
}
