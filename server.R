function(input, output) {
  output$plot1_1 <- renderGvis({
    gvisGeoChart(plot1_1, locationvar="region", colorvar="Total Medals",
                 options=list(width=800, height=600, colorAxis="{colors:['white', '#F4C300']}"))
  })
  
  output$plot1_2 <- renderGvis({
    gvisBubbleChart(plot1_2, idvar = "Country.Name", xvar = "Medals", yvar = "GDPbillions", 
                    colorvar="GDPbillions", sizevar = "GDPbillions",
                    options=list(width=800, height=600, 
                                 title="GDP x Medal Count", 
                                 hAxis="{title:'Total Medal Count'}",
                                 vAxis="{title:'GDP'}",
                                 colorAxis="{colors:['black']}"))
  })
  

  output$plot1_3 <- renderGvis({
    gvisBubbleChart(plot1_3, idvar = "Country.Name", xvar = "Medals", yvar = input$indices, 
                    colorvar=input$indices, sizevar = input$indices,
                    options=list(width=800, height=600,
                                 hAxis="{title:'Total Medal Count'}",
                                 colorAxis="{legend:{position:'none'}, colors:['black']}"))
  })
  
  output$plot2_1 <- renderGvis({
    gvisLineChart(plot2_1, xvar = "Year", yvar = c("Female", "Male"),
                  options=list(legend="{position: 'top', textStyle: {fontSize: 16}}",
                               hAxis="{format: '0000'}"))
  })
  
  output$plot2_2 <- renderGvis({
    gvisGeoChart(plot2_2, locationvar="region", colorvar="GII")
  })

}
