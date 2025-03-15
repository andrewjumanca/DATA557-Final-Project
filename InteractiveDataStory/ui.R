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



    tabPanel("2", style = "background-color: #f1f1f1;",
      h3(class = 'tab-title', "Has sex bias existed in the starting salaries of faculty members (salaries in the year
hired)?"),
      p('The following page uses a variety of analysis techniques to understand if sex bias existed in the starting salaries of faculty members. 
      First, we understand set out to understand the data. Through the use of visualizations and tables, our team built an understanding of the starting salary data
      that was available to us. We then used statistical tests, such as a two sample t-test, to establish if there was in fact a difference between the starting salaries
      of males and females. The results of this test showed there was a difference in starting salaries between males and females, which led us to conduct multiple linear regressions.
      The results of the above process can be found below.'),

      fluidRow(
        column(6, 
          img(src = "BoxPlotsStartingYearSalary.png", height = "300px", style = "border: 2px solid black; margin: 20px;", align = 'center'),
          p("This plot shows the starting salaries of faculty members by gender. The box plot shows that the mean for each group is quite similar, though males have 
          far more outliers past the 3rd quartile.")
        ),
        column(6, 
          img(src = "HistogramStartingYear.png", height = "300px", style = "border: 2px solid black; margin: 20px;", align = 'center'),
          p("To further understand the underlying data, this bar chart shows the counts associated with multiple salary bands. It is clear again that there are far more outliers in
          the male group. ")
        )
      ),
      
      br(),
      img(src = "Question2MultipleRegression.png", height = "475px", style = "border: 2px solid black; margin: 20px;", align = 'center'),
      p('After conducting a two sample t-test and deteriming there was a difference between male and female starting salary, we fit 4 linear regression models to test multiple 
      hypothesis about the factors which were causing the observed difference. We first fit a model with salary as the outcome and sex as the predictor. Then we added factors which
      our team thought may be contributing to the difference. The following regressions are shown in the table as well as their results. Of note, when accounting for variables such as
      field, rank, and degree, we found that the stastical significance of sex vanishes. That is to say, we cannot reject the null hypothesis that sex has no effect on salary in each
      model where we account for predictors beyond sex.'),
      br(),

      p('Our team wanted to make sure that our assumptions were met for our linear models. In the below plots we use model 3 to check for normality, linearity, and constant variance.'),
      fluidRow(
        column(6, 
          img(src = "QQPlot.png", height = "300px", style = "border: 2px solid black; margin: 20px;", align = 'center'),
          p("Looking at the QQ Plot for Model 3, as an example, allows us to confirm that our normality assumption is met. ")
        ),
        column(6, 
          img(src = "ResidualsVFitted.png", height = "300px", style = "border: 2px solid black; margin: 20px;", align = 'center'),
          p("Looking at the Residuals vs Fitted Values plot for Model 3, allows us to confirm that our linearity and constant variance assumptions are met."),
        )
      ),

      br(),
      p('All of the above analysis is simply to say that we do cannot attribute the difference in salary between male and female faculty members to sex.'),
    
    ),



    tabPanel("3",
      h3("Has sex bias existed in granting salary increases between 1990 -1995?"),
      p("For this analysis, we are interested in assessing if sex bias exists in granting salary increases between 1990-1995. To focus this analysis, we will measure whether men and women see different percent growth. "),
      h4("Data Prep"),
      p("To start, we filter the data to the 6 years of interest. Then, we aggregate the data on the id in order to treat each individual as an observation. To calculate the percent increase, we subtract the last salary observed by the first and divide by the first salary. We finally remove any observations with no increase. In order to capture the over time effects of rank and admin duties, we take the max admin duty and highest rank during the time period."),
      p("To visualize the difference in salary growth between men and women, we plot density curves of percent increases by women and men and observe that there is substantial overlap in the curves. There is a noted difference in the peak of the distribution, with more women seeing increases in salary. "),
      plotOutput("salaryDensityPlot"),
      verbatimTextOutput("regressionSummary")        
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
