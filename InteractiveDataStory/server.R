library(shiny)

#data function, placed outside of server function so that input/output functions can be called without reloading data
load_and_prepare_data_q3_agg <- function(file_path) {
  library(dplyr)
  library(ggplot2)
  library(lmtest)
  library(sandwich)
  library(stargazer)
  library(broom)
  # Import data
  data <- read.table(file_path, header = TRUE)
  #filter on years 90-95
  data_filtered_on_year <- data %>%
    filter(year >= 90 & year <= 95)
  #aggregate constant variables over time by id
  aggregated_data <- data_filtered_on_year %>%
    group_by(id) %>%
    summarise(
      sex = first(sex),       
      deg = first(deg),       
      yrdeg = first(yrdeg),  
      field = first(field),   
      startyr = first(startyr)
    )
  #transform variables that change over time, aggregat by id
  dynamic_data <- data_filtered_on_year %>%
    group_by(id) %>%
    summarise(
      avg_salary = mean(salary, na.rm = TRUE),  
      min_salary = min(salary, na.rm = TRUE),
      max_salary = max(salary, na.rm = TRUE),
      last_salary = salary[which.max(year)], #used in percent_change calc
      first_salary = salary[which.min(year)], #used in percent_change calc
      percent_increase = ((last_salary - first_salary) / first_salary) * 100, 
      admin_any = max(admin),#find any admin role (1 = Yes, 0 = No)
      total_years = n_distinct(year),#number of years observed in dataset (should be 6 for most)
      last_year = year[which.max(year)]
    )
  
  #find highest rank
  rank_data <- data_filtered_on_year %>%
    group_by(id) %>%
    summarise(
      highest_rank = case_when(
        any(rank == "Full") ~ "Full",
        any(rank == "Assoc") ~ "Assoc",
        TRUE ~ "Assist"
      )
    )
  
  #join all dataset
  final_aggregated_data <- aggregated_data %>%
    left_join(dynamic_data, by = "id") %>%
    left_join(rank_data, by = "id")
  
  #filter out na and 0 pct increases
  final_aggregated_data <- final_aggregated_data  %>% 
    filter(!is.na(percent_increase)) %>%
    filter(percent_increase != 0) 
  
  #add experience column
  final_aggregated_data$experience <- final_aggregated_data$last_year - final_aggregated_data$startyr
  final_aggregated_data$experience <- ifelse(final_aggregated_data$experience < 0, 0, final_aggregated_data$experience)
  
  #to imnprove model
  final_aggregated_data$sex <- factor(final_aggregated_data$sex, levels = c("F", "M"))
  final_aggregated_data$highest_rank <- factor(final_aggregated_data$highest_rank, levels = c("Assist", "Assoc", "Full"))
  final_aggregated_data$field <- factor(final_aggregated_data$field, levels = c("Arts", "Prof", "Other"))
  final_aggregated_data$deg <- factor(final_aggregated_data$deg, levels = c("Other", "PhD", "Prof"))
  final_aggregated_data$admin_any <- as.factor(final_aggregated_data$admin_any)
  
  return(final_aggregated_data)
}

server <- function(input, output) {
  #load aggregated data
  final_aggregated_data <- reactive({
    load_and_prepare_data("InteractiveDataStory/salary.txt")
  })
  
  #show head of aggregated data
  output$aggregatedDataHead <- renderPrint({
  print(head(final_aggregated_data))
  })
  
  #function to load density plot of final_aggregated_data
  output$salaryDensityPlot <- renderPlot({
    ggplot(final_aggregated_data, aes(x = percent_increase, fill = sex)) +
      geom_density(alpha = 0.5) +
      labs(
        title = "Distribution of Salary Increase by Sex",
        x = "Percent Increase in Salary",
        y = "Density"
      ) +
      theme_minimal()
  })
  
  #call model summary and confidence intervals for percent increase model for question 3
  output$regressionSummaryModelPercent <- renderPrint({
    model_percent <- lm(
      percent_increase ~ sex + admin_any + deg + field + highest_rank  + experience + total_years,
      data = data
    )
    summary(model_percent)
    confint(model_percent)
  })
}
