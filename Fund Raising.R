#### Part A ####

# Reading the CSV
data <- read.csv('Fundraising Raw Data.csv')
head(data) 
df<- data.frame(data)
head(df)
colnames(df)
nrow(df)

# 1) Identifying the missing values 
missing_HV_1 <- sum(is.na(data$HV_1))
missing_Icmed1 <- sum(is.na(data$Icmed1))
missing_Icavg1 <- sum(is.na(data$Icavg1))
print(paste("Missing values in HV_1:", missing_HV_1))
print(paste("Missing values in Icmed1:", missing_Icmed1))
print(paste("Missing values in Icavg1:", missing_Icavg1))


# 2) Replace the rows 
average_HV_1 <- mean(data$HV_1, na.rm = TRUE)
average_Icmed1 <- mean(data$Icmed1, na.rm = TRUE)
median_Icavg1 <- median(data$Icavg1, na.rm = TRUE)
data$HV_1[is.na(data$HV_1)] <- average_HV_1
data$Icmed1[is.na(data$Icmed1)] <- average_Icmed1
data$Icavg1[is.na(data$Icavg1)] <- median_Icavg1


# 3) Identify outliers in the Columns HV_1, Icmed1, and Icavg1
detect_outliers <- function(column) {
  Q1 <- quantile(column, 0.25)
  Q3 <- quantile(column, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  outliers <- column[column < lower_bound | column > upper_bound]
  return(outliers)
}
outliers_HV_1 <- detect_outliers(data$HV_1)
outliers_Icmed1 <- detect_outliers(data$Icmed1)
outliers_Icavg1 <- detect_outliers(data$Icavg1)


# 4) Replace all the outliers with upper limit / lower limit as applicable
replace_outliers <- function(column) {
  Q1 <- quantile(column, 0.25)
  Q3 <- quantile(column, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  column[column < lower_bound] <- lower_bound
  column[column > upper_bound] <- upper_bound
  
  return(column)
}
data$HV_1 <- replace_outliers(data$HV_1)
data$Icmed1 <- replace_outliers(data$Icmed1)
data$Icavg1 <- replace_outliers(data$Icavg1)


# 5)	Find correlation coefficients between each variable (input or output), and interpret the same
correlation_matrix <- cor(data)
print(correlation_matrix)





# 6)	Plot output variable Target_D vs. some of the input variables (some discrete variable by having Histogram with average value of Target_D, and some continuous variable)
## For Discrete Variable
discrete_variable <- c('INCOME','WEALTH','gender.dummy')
plot_bar_by_category <- function(data, category_column) {
  num_categories <- nlevels(as.factor(data[[category_column]]))
  unique_categories <- levels(as.factor(data[[category_column]]))
  categories_mean <- numeric(num_categories)
  for (i in 1:num_categories) {
    subset_data <- subset(data, data[[category_column]] == unique_categories[i])
    mean_target_d <- mean(subset_data$TARGET_D, na.rm = TRUE)
    categories_mean[i] <- mean_target_d
  }
  barplot(categories_mean, names.arg = unique_categories, 
          xlab = 'Categories', ylab = 'Mean TARGET_D', col = 'skyblue',
          main = paste('Average TARGET_D by', category_column),
          xlim = c(0, num_categories + 1),
          ylim = c(0, max(categories_mean, na.rm = TRUE) + 5))
}
par(mfrow=c(1, 3))
for (i in discrete_variable){
  plot_bar_by_category(data, i)
}

## For Continuous Variable
continuous_variable <- c('HV_1','Icmed1','Icavg1')
par(mfrow=c(1, 3))
for (i in continuous_variable){
  plot(data[[i]], data$TARGET_D, type='p', col = "red", xlab = i, ylab = "TARGET_D", main = "Simple Line Chart")
}


#### Part B ####
library(stats)
# 2)	Divide Sample data in two partitions in proportion of 50% Training Data and 50% Test Data
set.seed(7)
n <- nrow(data)
sample_indices <- sample(1:n, n*0.5)
training_data <- data[sample_indices, ]
test_data <- data[-sample_indices, ]


# 3) 3.	Using Multiple Linear Regression, build the model using training data. (TARGET_D is the output variable. First two columns are neither input nor output. All others are input variables.)
training_data <- training_data[, -c(1,2)]
test_data <- test_data[, -c(1,2)]
lm_model <- lm(TARGET_D ~ ., data = training_data)
summary(lm_model)


# 4) 4.	Apply the model on Test Data. Write the Actual and Predicted value for test data in a file
predicted_values <- predict(lm_model, newdata = test_data)
results <- data.frame(Actual = test_data$TARGET_D, Predicted = predicted_values)
write.csv(results, file = "predicted_results.csv", row.names = FALSE)


# 6 and 7 do be done on the above excel

