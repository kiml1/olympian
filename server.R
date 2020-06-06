function(input, output) {
    # plot of gold medals by country by year
    output$plot1 <- renderGvis({
        gvisLineChart(plot1, xvar=plot1x, yvar=plot1y)
    })
}
