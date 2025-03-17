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

load_and_prepare_data_q3_new <- function(file_path){
  data <- read.table(file_path, header = TRUE)
  #filter on years 90-95
  data_filtered_on_year <- data %>%
    filter(year >= 90 & year <= 95)
  
  new_data <- data_filtered_on_year %>%
    group_by(id) %>%
    filter(n() > 1) %>%
    ungroup()
  
  # Create a year-centered variable (base year = 1990)
  new_data$year_c <- new_data$year - 90
  
  # Create an experience variable (years since highest degree)
  new_data$experience <- new_data$year - new_data$yrdeg
  new_data$experience <- ifelse(new_data$experience < 0, 0, new_data$experience)
  
  # Convert categorical variables to factors
  new_data$sex <- factor(new_data$sex, levels = c("F", "M"))
  new_data$rank <- factor(new_data$rank, levels = c("Assist", "Assoc", "Full"))
  new_data$field <- factor(new_data$field, levels = c("Arts", "Prof", "Other"))
  new_data$deg <- factor(new_data$deg, levels = c("Other", "PhD", "Prof"))
  #new_data$admin <- as.factor(new_data$admin)
  return(new_data)
}

server <- function(input, output) {
  #load aggregated data
  data_agg <- reactive({
    load_and_prepare_data_q3_agg("salary.txt")
  })
  
  #show head of aggregated data
  output$aggregatedDataHead <- renderPrint({
  print(head(data_agg()))
  })
  
  #function to load density plot of final_aggregated_data
  output$salaryDensityPlot <- renderPlot({
    ggplot(data_agg(), aes(x = percent_increase, fill = sex)) +
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
      data = data_agg()
    )
    summary(model_percent)
  })
  output$regressionConfModelPercent <- renderPrint({
    model_percent <- lm(
      percent_increase ~ sex + admin_any + deg + field + highest_rank  + experience + total_years,
      data = data_agg()
    )
    confint(model_percent)
  })
  
  new_data <- reactive({
    load_and_prepare_data_q3_new("salary.txt")
  })
  
  output$plotNewDataLineChart <- renderPlot({
    summary_stats <- new_data() %>%
      group_by(year, sex) %>%
      summarize(avg_salary = mean(salary), .groups = "drop")
    
    
    ggplot(summary_stats, aes(x = year, y = avg_salary, color = sex)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(title = "Average Salary by Sex (1990–1995)", x = "Year", y = "Average Salary") +
      theme_minimal()
    
  })
  
  output$summaryNewModel <- renderPrint({
    model <- lm(
      salary ~ year_c * sex + rank + deg + field + admin + experience,
      data = new_data()
    )
    summary(model)
    
    #coeftest(model, vcov = vcovHC(model, type = "HC1"))
    
    #confint(model)
    
    #coefficients <- tidy(model)
  })
  output$confNewModel <- renderPrint({
    model <- lm(
      salary ~ year_c * sex + rank + deg + field + admin + experience,
      data = new_data()
    )
    confint(model)
  })

  output$summary_table <- renderTable({
    data.frame(
      Rank = c("Assistant", "Associate", "Full"),
      Mean_Salary_Female = c(4502.89, 5018.92, 6839.72),
      Mean_Salary_Male = c(4773.80, 5480.49, 7714.85),
      Difference = c(270.91, 461.57, 875.13)
    )
  })

  output$salary_analysis_table <- renderTable({
    data.frame(
      t_value = c("-2.2913", "-4.5374", "-5.9529"),
      Degrees_of_Freedom = c("312.71", "366.55", "214.31"),
      p_value = c("0.0226", "7.73e-06", "1.065e-08"),
      Confidence_Interval_95 = c("[-503.55, -38.28]", "[-661.61, -261.53]", "[-1164.89, -585.35]")
    )
  }, rownames = FALSE)
}
