fluidPage(navbarPage(
    "Olympians",
    tabPanel(
        "Home",
        tags$h1("What does it take to be an Olympian?"),
        tags$h4("by Lucas Kim, June 2020"),
        
        tags$h3(introTitle1),
        tags$div(introParagraph1,tags$br(),introParagraph2,tags$br(),introParagraph3,tags$br(),introParagraph4),
        
        tags$h3(topic1Title),
        tags$div(topic1Paragraph1,tags$br()),
        htmlOutput("plot1"),
        sliderInput("topNcountries", "Top contries:", min=5, max=20, value=5, step=1),
        htmlOutput("plot2"),
        
        tags$h3(topic2Title),
        tags$div(topic2Paragraph1,tags$br()),
        htmlOutput("plot3"),
        plotOutput("plot4"),
        plotOutput("plot5"),
        
        tags$h3(topic3Title),
        tags$div(
            topic3Paragraph1,
            tags$br()
        ),
        tags$div("PLOT4 HERE - height and weight of olympic athletes", style="color:red"),
        tags$div("PLOT5 HERE - height and weight of olympic athletes over the years", style="color:red"),
        tags$div("PLOT6 HERE - height and weight of olympic athletes men vs women", style="color:red"),
        tags$h3(topic4Title),
        tags$div(
            topic4Paragraph1,
            tags$br(),
            topic4Paragraph2
        ),
        tags$div("PLOT7 HERE - outliers, who are them", style="color:red"),
        tags$div("PLOT8 HERE - physical characteristics", style="color:red"),
        tags$div("PLOT9 HERE - countries from outliers", style="color:red"),
        tags$h3(topic5Title),
        tags$div(
            topic5Paragraph1,
            tags$br()
        ),
        tags$div("PLOT10 HERE - champion countries", style="color:red"),
        tags$div("PLOT11 HERE - GDP and medal count", style="color:red"),
        tags$div("PLOT12 HERE - countries that do better in summer or winter", style="color:red"),
        tags$div("PLOT14 HERE - countries that do better in summer or winter weather season over the years", style="color:red"),
        tags$div("PLOT13 HERE - index trend and medal count", style="color:red"),
    ),
    
    tabPanel("Data set"),
    tabPanel("References")
))
