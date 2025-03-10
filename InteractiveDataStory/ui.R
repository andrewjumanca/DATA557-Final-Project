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
      p("Empty.")
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