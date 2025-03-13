library(shiny)

ui <- fluidPage(
  titlePanel("Unequal Pay? Exploring Faculty Salaries by Gender and Rank"),
  tabsetPanel(
    tabPanel("Intro",
      h3("Introduction"),
      p("TODO: Introduction paragraphs explaining data, motivation for analysis, explanation for why we chose the methods we did, and anything else needed to introduce the readers.")
    ),
    tabPanel("1",
      h3("Does sex bias exist at the university in the most current year available (1995)?"),
      p("Empty.")
    ),



    tabPanel("2",
      h3("Has sex bias existed in the starting salaries of faculty members (salaries in the year
hired)?"),
      
      fluidRow(
        column(6, 
          img(src = "BoxPlotsStartingYearSalary.png", height = "400px"),
          p("This plot shows the starting salaries of faculty members by gender. The data suggests some differences in salary between male and female faculty in their first year.")
        ),
        column(6, 
          img(src = "HistogramStartingYear.png", height = "400px"),
          p("This plot compares the starting salaries across different departments, highlighting the variations in salary distributions.")
        )
      ),
      
      br(),
      img(src = "Question2MultipleRegression.png", height = "400px"),
      p('Using multiple regressions allows us to ...'),
      br(),

      fluidRow(
        column(6, 
          img(src = "QQPlot.png", height = "400px"),
          p("Looking at the QQ Plot for Model 3, as an example, allows us to confirm that our constant variance assumption and normality assumption are met. ")
        ),
        column(6, 
          img(src = "ResidualsVFitted.png", height = "400px"),
          p("Test")
        )
      ),

    ),



    tabPanel("3",
      h3("Has sex bias existed in granting salary increases between 1990 -1995?"),
      p("Empty.")
    ),
    tabPanel("4",
      h3("Has sex bias existed in granting promotions from Associate to full Professor?"),
      p("Empty.")
    ),
    tabPanel("Conclusion",
      h3("Conclusion"),
      p("TODO: Add summary of our findings in a qualitative manner that finishes telling the story.")
    )
  )
)
