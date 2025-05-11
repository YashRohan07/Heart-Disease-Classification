# Install and load necessary libraries
install.packages("readxl")
install.packages("zoo")
install.packages("ROSE")
install.packages("dplyr")

library(readxl)
library(zoo)
library(ROSE)
library(dplyr)

################################################################################

# Load the dataset
dataset <- read_excel("C:/Users/Rohan/Desktop/Lab Task/Heart_Disease_Dataset.xlsx")  # Update the file path accordingly


# or, If your working directory is already set correctly, you can simply use the file name (if the dataset is in that directory)
dataset <- read_excel("Heart_Disease_Dataset.xlsx")

################################################################################

print(names(dataset))  # Verify the column names
print(head(dataset))   # Check the first few rows of the dataset


# Check for missing values
which(is.na(dataset))

# Identify columns with missing values
for (col in colnames(dataset)) {
  missing_rows <- which(is.na(dataset[[col]]))
  if (length(missing_rows) > 0) {
    cat("Missing in column:", col, "→ Rows:", missing_rows, "\n")
  }
}

# Sum of missing values by column
colSums(is.na(dataset))

# Visualizing missing values with a bar plot
missing_counts <- colSums(is.na(dataset))
barplot(missing_counts,
        main = "Missing Values Per Column",
        xlab = "Columns",
        ylab = "Number of Missing Values",
        col = "skyblue")

################################################################################

# Handle Missing Value of Age using Mean 
mean_age <- mean(dataset$age, na.rm = TRUE)
mean_age  # Print the calculated mean value

# Replace missing values (NA) in 'age' with the calculated mean value
dataset$age[is.na(dataset$age)] <- mean_age 

# Round up all the values in 'age' to the nearest integer
dataset$age <- ceiling(dataset$age) 

# Print the dataset with the updated 'age' column, showing all rows
dataset$age 
print(dataset, n = Inf)

################################################################################

# Handle Missing Value of Age using Median 
median_age <- median(dataset$age, na.rm = TRUE)
median_age # Print the calculated median value

# Replace missing values (NA) in 'age' with the calculated median value
dataset$age[is.na(dataset$age)] <- median_age 

# Print the dataset with the updated 'age' column, showing all rows
dataset$age 
print(dataset, n = Inf)

################################################################################

# Handle Missing Value of Age using Mode
mode_age <- as.numeric(names(which.max(table(dataset$age)))) 
mode_age  # Print the calculated mode value

# Replace missing values (NA) in 'age' with the calculated mode value
dataset$age[is.na(dataset$age)] <- mode_age 

# Print the dataset with the updated 'age' column, showing all rows
dataset$age 
print(dataset, n = Inf)

################################################################################

# Handle Missing Value of Age using Top-Down

# Fill missing values (NA) in 'age' by using the last observed value (top-down imputation)
dataset$age <- na.locf(dataset$age) 

# Print the dataset with the updated 'age' column, showing all rows
print(dataset, n = Inf)


################################################################################

# Handle Missing Value of Impluse using Mean
mean_impluse <- mean(dataset$impluse, na.rm = TRUE)
mean_impluse  # Print the calculated mean value for impluse

# Replace missing values (NA) in 'impluse' with the calculated mean value
dataset$impluse[is.na(dataset$impluse)] <- mean_impluse

# Round up all the values in 'impluse' to the nearest integer
dataset$impluse <- ceiling(dataset$impluse)

# Print the dataset with the updated 'impluse' column
dataset$impluse
print(dataset, n = Inf)


################################################################################

# Handle Missing Value of PressureHight using Mean
mean_pressurehight <- mean(dataset$pressurehight, na.rm = TRUE)
mean_pressurehight  # Print the calculated mean value for pressurehight

# Replace missing values (NA) in 'pressurehight' with the calculated mean value
dataset$pressurehight[is.na(dataset$pressurehight)] <- mean_pressurehight

# Round up all the values in 'pressurehight' to the nearest integer
dataset$pressurehight <- ceiling(dataset$pressurehight)

# Print the dataset with the updated 'pressurehight' column
dataset$pressurehight
print(dataset, n = Inf)

################################################################################

# Handle Missing Value of Gender using Mode
mode_gender <- names(which.max(table(dataset$gender)))
mode_gender  # Print the calculated mode value for gender

# Replace missing values (NA) in 'gender' with the calculated mode value
dataset$gender[is.na(dataset$gender)] <- mode_gender

# Print the dataset with the updated 'gender' column
dataset$gender
print(dataset, n = Inf)

################################################################################

# Handle Missing Value of Glucose using Mode
mode_glucose <- names(which.max(table(dataset$glucose)))
mode_glucose  # Print the calculated mode value for glucose

# Replace missing values (NA) in 'glucose' with the calculated mode value
dataset$glucose[is.na(dataset$glucose)] <- mode_glucose

# Print the dataset with the updated 'glucose' column
dataset$glucose
print(dataset, n = Inf)

################################################################################

# Removing rows with any missing data (NA values)
dataset <- na.omit(dataset)

# Check for missing values in each column
colSums(is.na(dataset))

################################################################################


# Outlier detection and removal function
outlier_removal <- function(col_data, col_name, dataset) {
  # Check if the column exists
  if (is.null(col_data)) {
    print(paste("Column", col_name, "does not exist in the dataset."))
    return(dataset)  # Return dataset without changes
  }
  
  # Calculate the first and third quartiles (Q1 and Q3)
  Q1 <- quantile(col_data, 0.25, na.rm = TRUE)
  Q3 <- quantile(col_data, 0.75, na.rm = TRUE)
  
  # Calculate the Interquartile Range (IQR)
  IQR_value <- Q3 - Q1
  
  # Calculate lower and upper bounds for outliers
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  # Identify outliers
  outliers <- col_data[col_data < lower_bound | col_data > upper_bound]
  print(paste("Outliers in", col_name, ":"))
  print(outliers)
  
  # Identify the indices of the outliers
  outlier_indices <- which(col_data < lower_bound | col_data > upper_bound)
  print(paste("Outlier Indices in", col_name, ":"))
  print(outlier_indices)
  
  # Replace outliers with the median of the column
  col_data[col_data < lower_bound | col_data > upper_bound] <- median(col_data, na.rm = TRUE)
  
  # Update the dataset with the modified column
  dataset[[col_name]] <- col_data
  
  # Print the updated dataset after removal of outliers for this column
  print(paste("Updated Dataset after outlier removal for", col_name, ":"))
  print(dataset)
  
  return(dataset)
}

# Apply outlier removal for the age column
dataset <- outlier_removal(dataset$age, "age", dataset)

# Apply outlier removal for the impluse column
dataset <- outlier_removal(dataset$impluse, "impluse", dataset)

# Apply outlier removal for the pressurehight column 
dataset <- outlier_removal(dataset$pressurehight, "pressurehight", dataset)

# Apply outlier removal for the pressurelow column
dataset <- outlier_removal(dataset$pressurelow, "pressurelow", dataset)



################################################################################

# Convert numeric attributes to categorical (age, impluse, pressurehight, etc.)
dataset$age_cat <- cut(dataset$age,
                       breaks = c(-Inf, 30, 60, Inf),
                       labels = c(0, 1, 2))
dataset$age_cat <- factor(dataset$age_cat, levels = c(0, 1, 2), labels = c("Young", "Middle-aged", "Old"))

dataset$impluse_cat <- cut(dataset$impluse,
                           breaks = c(-Inf, 70, 90, Inf),
                           labels = c(0, 1, 2))
dataset$impluse_cat <- factor(dataset$impluse_cat, levels = c(0, 1, 2), labels = c("Low", "Normal", "High"))

dataset$pressurehight_cat <- cut(dataset$pressurehight,
                                 breaks = c(-Inf, 110, 140, Inf),
                                 labels = c(0, 1, 2))
dataset$pressurehight_cat <- factor(dataset$pressurehight_cat, levels = c(0, 1, 2), labels = c("Low", "Normal", "High"))

dataset$pressurelow_cat <- cut(dataset$pressurelow,
                               breaks = c(-Inf, 60, 80, Inf),
                               labels = c(0, 1, 2))
dataset$pressurelow_cat <- factor(dataset$pressurelow_cat, levels = c(0, 1, 2), labels = c("Low", "Normal", "High"))

print(dataset)  # Print the updated dataset

################################################################################

# Convert categorical attributes into numeric (Gender, Class, Glucose)
dataset$gender <- factor(dataset$gender, levels = c("male", "female"), labels = c(1, 2))
dataset$class <- factor(dataset$class, levels = c("negative", "positive"), labels = c(0, 1))
dataset$glucose <- factor(dataset$glucose, levels = c("Normal", "High"), labels = c(0, 1))

print(dataset)  # Print the updated dataset

################################################################################


# Normalize the 'age' column
dataset$age <- (dataset$age - min(dataset$age, na.rm=TRUE)) / (max(dataset$age, na.rm=TRUE) - min(dataset$age, na.rm=TRUE))
print(dataset)  # Print the updated dataset

################################################################################

# Find and print duplicated rows
duplicated_rows <- dataset[duplicated(dataset), ]
print("Duplicated Rows:")
print(duplicated_rows)

# Remove duplicates
dataset <- unique(dataset)

# Check if there are any remaining duplicates
any_duplicates <- any(duplicated(dataset))
print(paste("Any duplicates left:", any_duplicates))

################################################################################


# Subset or filter the dataset for positive class
approved <- subset(dataset, class == "positive")
print(approved)

################################################################################

# Handling invalid data in gender column and correcting misspellings
invalid_gender <- setdiff(unique(dataset$gender), c("male", "female"))
print(invalid_gender)

# Handle invalid data and correct misspellings
dataset$gender <- tolower(dataset$gender)
dataset$gender <- gsub("femalee", "female", dataset$gender)
dataset$gender <- gsub("malee", "male", dataset$gender)

# Recheck unique gender values
unique(dataset$gender)

################################################################################

# Balance the dataset using oversampling (ROSE package)
balanced <- ovun.sample(class ~ ., data = dataset, method = "over", seed = 123)$data
table(balanced$class)

################################################################################

# Splitting the dataset into training and testing sets (80-20 split)
set.seed(123)
index <- sample(1:nrow(dataset), 0.8 * nrow(dataset))
train <- dataset[index, ]
test <- dataset[-index, ]
nrow(train)
nrow(test)

################################################################################

# Compare central tendencies (mean, median, mode) of Age across different genders
result <- aggregate(age ~ gender, dataset, function(x) {
  m <- mean(x)
  med <- median(x)
  mo <- as.numeric(names(which.max(table(x))))  # Mode calculation
  c(mean = m, median = med, mode = mo)
})

result <- data.frame(
  gender = result$gender,
  mean = result$age[, "mean"],
  median = result$age[, "median"],
  mode = result$age[, "mode"]
)
print(result)

################################################################################

# Compare Age's central tendencies across glucose levels
result_glucose <- aggregate(age ~ glucose, dataset, function(x) {
  m <- mean(x)
  med <- median(x)
  mo <- as.numeric(names(which.max(table(x))))  # Mode calculation
  c(mean = m, median = med, mode = mo)
})
result_glucose <- data.frame(
  glucose = result_glucose$glucose,
  mean = result_glucose$age[, "mean"],
  median = result_glucose$age[, "median"],
  mode = result_glucose$age[, "mode"]
)
print(result_glucose)

################################################################################

# Compare Spread (Range, IQR, Variance, Standard Deviation) of Age across different genders
spread_result <- dataset %>%
  group_by(gender) %>%
  summarise(
    Range = max(age, na.rm = TRUE) - min(age, na.rm = TRUE),
    IQR = IQR(age, na.rm = TRUE),
    Variance = var(age, na.rm = TRUE),
    SD = sd(age, na.rm = TRUE)
  )

print(spread_result)
