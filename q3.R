
library(dplyr)
library(ggplot2)
library(tidyr)
library(stats)
library(broom)

# Read the dataset (assuming it is whitespace-delimited)
df <- read.table("salary.txt", header = TRUE)

# Display column names
cat("Columns in dataset:", colnames(df), "\n")

# Subset data for years 1990 and 1995
df_sub <- df %>% filter(year %in% c(90, 95))

# Pivot the data to wide format
df_wide <- df_sub %>% pivot_wider(names_from = year, values_from = salary)

# Merge constant faculty attributes from 1995
df_1995 <- df %>% 
  filter(year == 95) %>% 
  select(id, sex, rank, field, yrdeg, deg, startyr) %>% 
  distinct(id, .keep_all = TRUE)

df_model <- left_join(df_wide, df_1995, by = "id", suffix = c("_original", ""))

# Keep only individuals with salary data for both years
df_model <- df_model %>% drop_na(`90`, `95`) %>% as.data.frame()

# Compute salary increase
df_model <- df_model %>% mutate(salary_increase = `95` - `90`)

# Compute experience
df_model <- df_model %>% mutate(experience = 95 - df_model$startyr)

# Summary statistics by sex
df_model %>% group_by(sex) %>% summarise(across(salary_increase, list(mean = mean, sd = sd)))

# Two-sample t-test
t_test <- t.test(salary_increase ~ sex, data = df_model, var.equal = FALSE)
print(t_test)

# Multiple regression model
model <- lm(salary_increase ~ sex + experience + rank + field + yrdeg + sex:rank + sex:field, data = df_model)
summary(model)

# # Simple regression model
# model_sex <- lm(salary_increase ~ sex, data = df_model)
# summary(model_sex)

# # Boxplot of salary increase by sex
# ggplot(df_model, aes(x = sex, y = salary_increase, fill = sex)) +
#   geom_boxplot() +
#   labs(title = "Salary Increase (1990 to 1995) by Sex", x = "Sex", y = "Salary Increase") +
#   theme_minimal()

# # Density plot of salary increase by sex
# ggplot(df_model, aes(x = salary_increase, fill = sex)) +
#   geom_density(alpha = 0.5) +
#   labs(title = "Density Plot of Salary Increase (1990 to 1995) by Sex", x = "Salary Increase", y = "Density") +
#   theme_minimal()

# Scatterplot: salary in 1990 vs. salary in 1995 with individual trajectories
min_salary <- min(df_model$`90`, df_model$`95`)
max_salary <- max(df_model$`90`, df_model$`95`)

ggplot(df_model, aes(x = `90`, y = `95`, color = sex)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point() +
  labs(title = "Salaries in 1990 vs. 1995", x = "Salary in 1990", y = "Salary in 1995") +
  theme_minimal()

