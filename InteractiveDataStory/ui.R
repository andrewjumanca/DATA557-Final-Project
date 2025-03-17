library(shiny)
library(DT)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Neutral background for the page */
      body {
        background-color: #f4f4f4;
        font-family: 'Arial', sans-serif;
        padding-left: 200px;
        padding-right: 200px;
        margin: 0;
      }
      
      /* Title Panel Styling */
      .title-panel {
        background-color: #e0e0e0;
        padding: 20px;
        border-radius: 8px;
        margin-bottom: 20px;
        box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1);
      }

      /* Tab Styling - Gradient and Modern Look */
      .nav-tabs {
        background: linear-gradient(90deg, #FF69B4, #325ea8); /* Gradient from pink to blue */
        border-radius: 15px;
        padding: 5px;
        margin-bottom: 20px;
        box-shadow: 0 4px 10px rgba(0, 0, 0, 0.1);
      }

      .nav-tabs > li > a {
        background-color: #ffffff;
        color: #333;
        border: none;
        border-radius: 12px;
        padding: 10px 20px;
        font-weight: bold;
        font-size: 16px;
        transition: all 0.3s ease-in-out;
        text-transform: uppercase;
      }

      /* Hover effect for tabs */
      .nav-tabs > li > a:hover {
        background-color: #6f32a8;
        color: white;
        transform: scale(1.05);
      }

      /* Active Tab */
      .nav-tabs > .active > a {
        background-color: #998ea3;
        color: #6a11cb; /* Purple accent color for active tab */
        border: 2px solid #998ea3;
        font-weight: bold;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.2);
      }

      /* Content Area Styling */
      .content-area {
        background-color: white;
        padding: 20px;
        border-radius: 8px;
        box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1);
      }

      /* Icon in Tabs (optional) */
      .nav-tabs > li > a:before {
        content: '\f007'; /* Use FontAwesome icon for example */
        color: #6a11cb;
        font-size: 20px;
        margin-right: 10px;
      }

      /* Responsive adjustments */
      @media (max-width: 768px) {
        body {
          padding-left: 50px;
          padding-right: 50px;
        }
      }

      @media (max-width: 480px) {
        body {
          padding-left: 10px;
          padding-right: 10px;
        }
      }

      .shiny-table {
      margin: 0 auto;
      display: table;
    "))
  ),
  h1("Unequal Pay? Exploring Faculty Salaries by Gender and Rank"),
  tabsetPanel(
    tabPanel("Intro",
      h3("Introduction"),
      p("The following data story is a record of our investigation into how university faculty salaries differ, if at all, when considering factors such as sex, highest degree attained, rank, and other variables. 
        Differences between the salaries of male and female faculty members at US universities are well documented. There are many differences that may account for differences in salary including experience, time in a role, rank, degree and responsibilities. The data that was provided to help investigate whether the difference in salary is due to sex, contains 19792 records on 1597 faculty members employed at an unknown university in 1995. The variables available for this analysis included sex, highest degree attained, year of highest degree, field, year hired, rank and administrative duties. 
        The data provided will be used to investigate the following four questions in more depth."),
      br(),
      tags$ol(
        tags$li("Does sex bias exist at the university in the most current year available (1995)?"),
        tags$li("Has sex bias existed in the starting salaries of faculty members (salaries in the year hired)?"),
        tags$li("Has sex bias existed in granting salary increases between 1990-1995?"),
        tags$li("Has sex bias existed in granting promotions from Associate to Full Professor?")
      ),
      br(),
      p('Below is a brief overview of the data in table form. The first table is information from the entire dataset. The second is filtered by males and third by females to help give a better idea of the information at our disposal.'),
      img(src = "df_complete_summary.png", style = "display: block; margin: auto; max-width: 100%; height: auto; border: 2px solid black; margin-bottom: 10px;"),
      p(tags$em('Complete Data Set Summary Statistics'), align= 'center'),
      img(src = "df_male_summary.png", style = "display: block; margin: auto; max-width: 100%; height: auto; border: 2px solid black; margin-bottom: 10px;"),
      p(tags$em('Male Filtered Set Summary Statistics'), align= 'center'),
      img(src = "df_female_summary.png", style = "display: block; margin: auto; max-width: 100%; height: auto; border: 2px solid black; margin-bottom: 10px;"),
      p(tags$em('Female Filtered Data Set Summary Statistics'), align= 'center'),
      br(),
      p('Our methods for each question are well documented in each of the following tabs, but as an overview our team used statistical methods such as a two-sample t-test, linear regressions and logistic regressions to answer the questions.
        '),
      br(),
      p('Each tab also gives an overview of our results for each question with final results and takeaways being found in the conclusion tab.'),
    ),
    tabPanel("1",
      h3("Does sex bias exist at the university in the most current year available (1995)?"),
      h3("Introduction"),
      p("For this analysis, we will examine whether a sex-based salary disparity exists in the most recent year of available data. We'll use several various statistical tests and regression modeling to find out whether male and female faculty members receive significantly different salaries based on a sex bias."),

      h3("Analysis"),
      p("To set the stage for a thorough analysis of the proposed question and the data, we
         we'll use some visualizations and tables to better understand how the data is distributed. Below we have box plots showing the interquartile ranges and medians of each sex acrossall ranks."),
      img(src = "q1_ranksalary_boxplot.png", height = "300px", style = "border: 2px solid black; margin: 20px;", align = 'center'),
      h4("Mean Salaries by Sex and Job Status"),
      tableOutput("summary_table"),

      h4("T-Test Analysis"),
      p("We'll start by performing a Welch Two Sample t-test to compare the average salaries of male and female faculty members.
         The t-tests compare the average salaries of male and female faculty members at each rank (Assistant, Associate, and Full Professors) 
         in 1995 to determine if significant salary differences exist."),
      tableOutput("salary_analysis_table"),
      
      h3("Methods"),
      p("Our methods for this question included the use of boxplot visualization containing the IQR and mean."),
      p("Furthemore, the two-sample Welch test was used to understand the statistical significance of sex in relation to salary, and confirm that the disparity varies by rank."),


      h3("Conclusion"),
      
      h4("Key Takeaways:"),
      p("Our findings indicate a statistically significant gender pay gap, with male faculty members earning higher salaries on average. The gap increases at higher ranks, suggesting that salary disparities persist and may widen over time."),
      tags$ul(
        tags$li("A significant gender pay gap exists at all ranks, but it increases as faculty move up the career ladder."),
        tags$li("Assistant Professors have a small salary gap, suggesting more equal starting salaries."),
        tags$li("Associate Professors show a widening gap, indicating that men receive larger salary increases."),
        tags$li("Full Professors have the largest gap, suggesting that over time, the gender disparity in pay becomes substantial.")
      )
    ),

    tabPanel("2", style = "background-color: #f1f1f1;",
      h3(class = 'tab-title', "Has sex bias existed in the starting salaries of faculty members (salaries in the year
        hired)?"),
      h4('Introduction'),
      p('The following page uses a variety of analysis techniques to understand if sex bias existed in the starting salaries of faculty members. 
         First, we understand set out to understand the data. Through the use of visualizations and tables, our team built an understanding of the starting salary data
         that was available to us.'),
      h4('Analysis'),
      p('Before diving into advanced statistical methods, we sought to understand the data by using box plots and bar graphs as well as general statistics.'),
      fluidRow(
        column(6,
          img(src = "BoxPlotsStartingYearSalary.png", style = "display: block; margin: auto; max-width: 100%; height: auto; border: 2px solid black; margin-bottom: 10px;"),
          p("This plot shows the starting salaries of faculty members by gender. The box plot shows that the mean for each group is quite similar, though males have 
          far more outliers past the 3rd quartile.")
        ),
        column(6,
          img(src = "HistogramStartingYear.png", style = "display: block; margin: auto; max-width: 100%; height: auto; border: 2px solid black; margin-bottom: 10px;"),
          p("To further understand the underlying data, this bar chart shows the counts associated with multiple salary bands. It is clear again that there are far more outliers in
          the male group. ")
        )
      ),
      h4('Methods'),
      p('We used statistical tests, such as a two sample t-test, to establish if there was in fact a difference between the starting salaries
         of males and females. The results of this test showed there was a difference in starting salaries between males and females, which led us to conduct multiple linear regressions.
         The results of the above process can be found below.'),
      br(),
      img(src = "Question2MultipleRegression.png", style = "display: block; margin: auto; max-width: 100%; height: auto;border: 2px solid black; margin-bottom: 10px;"),
      p('After conducting a two sample t-test and deteriming there was a difference between male and female starting salary, we fit 4 linear regression models to test multiple 
         hypothesis about the factors which were causing the observed difference. We first fit a model with salary as the outcome and sex as the predictor. Then we added factors which
         our team thought may be contributing to the difference. The following regressions are shown in the table as well as their results. Of note, when accounting for variables such as
         field, rank, and degree, we found that the stastical significance of sex vanishes. That is to say, we cannot reject the null hypothesis that sex has no effect on salary in each
         model where we account for predictors beyond sex.'),
      br(),
      p('Our team wanted to make sure that our assumptions were met for our linear models. In the below plots we use model 3 to check for normality, linearity, and constant variance.'),
      fluidRow(
        column(6, 
          img(src = "QQPlot.png", style = "display: block; margin: auto; max-width: 100%; height: auto; border: 2px solid black; margin-bottom: 10px;"),
          p("Looking at the QQ Plot for Model 3, as an example, allows us to confirm that our normality assumption is met. ")
        ),
        column(6, 
          img(src = "ResidualsVFitted.png", style = "display: block; margin: auto; max-width: 100%; height: auto; border: 2px solid black; margin-bottom: 10px;"),
          p("Looking at the Residuals vs Fitted Values plot for Model 3, allows us to confirm that our linearity and constant variance assumptions are met."),
        )
      ),
      br(),
      h4('Conclusion'),
      p('All of the above analysis is simply to say that we do cannot attribute the difference in salary between male and female faculty members to sex for the starting year salaries.'),
    ),



     tabPanel("3",
             h3("Has sex bias existed in granting salary increases between 1990 -1995?"),
             
             h4("Introduction"),
             p("For this analysis, we are interested in assessing if sex bias exists in granting salary increases between 1990-1995. We look answer two questions to cover this topic. 1. Do men and women receive different percentage raises? 2. Do men and women receive different absolute raises over time?"),
             
             h4("Methods"),
             p("To address the two questions above, We start with exploratory data analysis and then use multiple linear regression for two different models to address each question specifically. First, we fit a multiple linear regression model with percent increase in salary as the outcome and included the following covariates:"),
             tags$ul(
               tags$li("Sex"),
               tags$li("Administrative duties"),
               tags$li("Degree"),
               tags$li("Field"),
               tags$li("Rank"),
               tags$li("Experience"),
               tags$li("Total years observed")
             ),
             p("We then explored whether the results were influenced by men starting at higher salaries by fitting an interaction model between sex and year, while controlling for the same covariates. This helped us explain the results of the first question and allowed us to evaluate whether men and women received different salary increases over time."),
             h4("Analysis"),
             
             h5("Data Preparation"),
             p("To start, we filter the data to the 6 years of interest. Then, we aggregate the data on the id in order to treat each individual as an observation. To calculate the percent increase, we subtract the last salary observed by the first and divide by the first salary. We finally remove any observations with no increase. In order to capture the over-time effects of rank and administrative duties, we take the maximum administrative duty and highest rank during the time period. The following figure for an example of the aggregated data:"),
             h6("Example of aggregated data:"),
             verbatimTextOutput("aggregatedDataHead"),
             
             h5("Data Exploration"),
             p("To visualize the difference in salary growth between men and women, we plot density curves of percent increases by women and men and observe that there is substantial overlap in the curves. There is a noted difference in the peak of the distribution, with more women seeing increases in salary."),
             plotOutput("salaryDensityPlot"),
             
             h5("Multiple Linear Regression with Percent Increase in Salary as Outcome"),
             p("Next, we fit a multiple linear regression with percent increase as the outcome and sex, administrative duties, degree, field, rank, experience, and total years observed. Interestingly, the model suggests that men had lower average increases in salary that were significant at the .10 level when controlling for the other factors. The model showed strong statistical significance in the controlling factors for administrative duties, rank, and total observations. The model's R² score shows that it captured 26.95% of the variability in the data."),
             
             h5("Model Summary"),
             verbatimTextOutput("regressionSummaryModelPercent"),
             h6("Confidence Intervals"),
             verbatimTextOutput("regressionConfModelPercent"),
             h5("Key Findings and Interpretation of the Model"),
             tags$ul(
               tags$li("Male faculty had lower percent increases than women (β = -2.10, p = 0.003). 
             This effect is statistically significant at the 0.01 level, indicating strong evidence that men received smaller percentage increases in salary. 
             The 95% confidence interval (-3.49, -0.71) excludes zero, reinforcing the significance of this result."),
               
               tags$li("Promotions to higher rank (Associate and Full) are strongly associated with higher salary increases."),
               
               tags$li("Professional degrees are associated with smaller raises due to higher starting salaries."),
               
               tags$li("More experienced faculty tend to have smaller percent increases."),
               
               tags$li("Having an administrative role significantly increases percent increases in salary. ")
              ),
             h5("Analyzing Impact of Salary Gap"),
             p("Given the interesting results of the model, we decided to investigate if the results are due to men starting at higher salaries. We also want to follow up from the first model on whether experienced faculty tend to have smaller percent increases, which is consistent with diminishing returns over time.
    First, we plot the average salary by sex over the time period. It is clear that the average salary is much higher for men than women."),
             plotOutput("plotNewDataLineChart"),
             
             h5("Multiple Regression Using Interaction Between Sex and Year"),
             p("For the next model, instead of aggregating by individual, we keep the same granularity as the original data, where each year with a recorded salary for an individual is treated as a separate observation. Now we fit the model with salary as the outcome and look at the interaction between the year of the recorded salary and sex, while controlling for the factors of rank, degree, field, administrative duties, and experience. The model performs very well, with an R² of 54.18%. All of our controlled variables are significant at the .05 level, except for the interaction between sex and year. This means that there is no statistically significant difference in annual raise rates by sex once rank, field, degree, administrative duties, and experience were accounted for."),
             h6("Model Summary"),
             verbatimTextOutput("summaryNewModel"),
             h6("Confidence Intervals"),
             verbatimTextOutput("confNewModel"),
             h5("Key Findings and Interpretation of the Model"),
             tags$ul(
               
               tags$li("Year is positively associated with salary increases. Each additional year is linked to an increase of $123.93 (p < 0.001), with a confidence interval of ($90.56, $157.30), indicating strong evidence of year-over-year salary growth."),
               
               tags$li("The interaction term between sex and year is not statistically significant (β = 28.74, p = 0.14). The confidence interval (-9.39, 66.86) includes zero, suggesting that the rate of salary increase over time does not differ significantly between men and women.")
             ),
             

             
             h4("Conclusion"),
             p("We can conclude from the first model that if men start with higher salaries, they might appear to have smaller percent increases even if they’re receiving larger absolute raises. The interaction model on absolute salaries over time confirms that men and women do not get different absolute raises over time.")
    ),

    tabPanel("4",
      h3("Has sex bias existed in granting promotions from Associate to full Professor?"),
      fluidRow(
        column(12, 
          h4("Introduction"),
          p("Although this is a 'yes or no' question, you might be surprised at some of the underlying complexities you may run into when trying to identify bias.
            Let's start by narrowing down what sex bias means when considering promotion rates in this scenario. Sex bias means that either women or men (or both, in different ways)
            have an advantage over the other based on something related directly to their sex, whether it is sex alone or an additional predictor interacting with sex. 
            However, is it reasonable to consider sex in isoltion while holding any other potential predictors constant? Moreover, if we are specifically focus on sex, 
            we will need to proportion our data to account for difference in male and female entries, remove any entries that were already listed as Full Professors at the start 
            of the data collection range, and lastly, we'll need to identiy an adequate time range for mean promotions from associate to full professor to rule out any edge cases and outliers."),
          p("Caveat: sex data was highly disproportionate with many more male entries than female entries."),

          h3("Analysis"),
          p("Our data preparation for the analysis and investigation of this question included the following operations:"),
          tags$ul(
            tags$li("Removal of entries already starting at full professor job status"),
            tags$li("Removal of entries with Assistant as their last recorded job status."),
            tags$li("Removal of premature samples:"),
              tags$ul(
                tags$li("When Associate status is carried for less than the average number of 
                          years that individuals in the dataset require to obtain a promotion."),
                tags$li("When entries with Associate status are initially recorded at a time
                         less than one standard deviation from the mean time an individual in the dataset
                         requires for a promotion.")
              ),
            tags$li("Centered the startyr (year career was started) and yeardeg (year degree was obtained) by subtracting the mean 
                     to produce a better regression and fitting more stable interaction terms.")
          ),

      fluidRow(
        column(6, 
          div(style = "text-align: center;",
              h4("Promotion Mean Distribution")
          ),
          div(style = "text-align: center;",
              img(src = "q4_promotion_mean.jpeg", width = "70%", height = "30%"),
              p("Distribution of promotion times across men and women."),
          )
        ),
        column(6, 
          div(style = "text-align: center;",
              h4("Promotions Proportions")
          ),
          div(style = "text-align: center;",
              img(src = "q4_promotions_proportions.jpeg", width = "70%", height = "30%"),
              p("Stacked visualization of amount promoted vs. all by sex."),
          )
        ),
      ),

          h4("Logistic Regression"),
          p("Since we've broken up our data by those who have and have not received a promotion within the mean time span, it makes sense to 
             use a logistic regression model here, as the outcome we want to predict is binary. Given the sex of a data entry (and possibly 
             other values), how accurately can we predict whether that person has gotten a promotion within the mean time frame?"),
          p("Our model results are as follows, incorporating sex, field, job start year, degree completion year, and a few interaction terms:"),
          DTOutput("logit_table"),

          p("Our results look promising, with several key predictors turning out to be statistically significant, including sex and all of its interaction terms."),
          p("While we also considered a model with just sex as the predictor, the following ROC and PR curves will show why this was not an ideal choice 
             to use as our final statistical test. A ROC (Receiver Operating Characteristic) and PR (Precision-Recall) curves focus on visualizing the tradeoff between 
             true positive and false positive rate, while PR focuses on the trade-off between precision and recall when predicting."),
          
      ),
      hr(),
      
      h4("Methods"),
      h4("Conclusion"),
      p("Without the predictor of sex included, we essentially get random guesses as results, based on ROC and PR curves.
        Including sex as a predictor makes it the most significant term (p-value of 0) in our multi-logistic regression model, with 
        the next most significant terms are degree type and yeardeg. With all terms and interactions: sex (most significant), sex/startyear (pos), and sex/yrdeg (neg) are equally significant"),
      ),

      fluidRow(
        column(6, 
          div(style = "text-align: center;",
              h4("ROC")
          ),
          div(style = "text-align: center;",
              img(src = "q4_promotion_mean.jpeg", width = "70%", height = "30%"),
              p("Distribution of promotion times across men and women."),
          )
        ),
        column(6, 
          div(style = "text-align: center;",
              h4("PR")
          ),
          div(style = "text-align: center;",
              img(src = "q4_promotions_proportions.jpeg", width = "70%", height = "30%"),
              p("Stacked visualization of amount promoted vs. all by sex."),
          )
        ),
      ),
    ),

    tabPanel("Conclusion",
      h3("Conclusion"),
      p("TODO: Add summary of our findings in a qualitative manner that finishes telling the story.")
    )
  )
)
