# Credit: John Tilelli

# Read raw data (may need to set current working directory to get relative path correct).
data <- readLines("SalaryAnalysis/salary.txt")

data_trim <- trimws(data)  # Remove leading/trailing spaces & tabs


# Clean file and ensure consistent separators
data_csv <- gsub("\\s+", ",", data_trim)  # Convert spaces & tabs to commas

# Write new file
writeLines(data_csv, "salary_csv.txt")

data_cleaned <- read.csv("salary_csv.txt", header=TRUE, stringsAsFactors=FALSE)
head(data_cleaned)