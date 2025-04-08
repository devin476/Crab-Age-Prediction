library(tidyverse)
library(class)
library(caret)
library(FNN)

#Load
CrabTrain <- read.csv("Data/train-1.csv")

#EDA
#Select numerical variables
features <- CrabTrain %>% 
  select(where(is.numeric), -id) %>%
  colnames()

#correlation
cor_results <- data.frame(
  Feature = character(),
  Correlation = numeric()
)

for (feature in features) {
  cor_val <- cor(CrabTrain[[feature]], CrabTrain$Age)
  cor_results <- rbind(cor_results, data.frame(
    Feature = feature,
    Correlation = cor_val
  ))
}

#Print
print(cor_results)

#Plotting
#Shell Weight
ggplot(CrabTrain, aes(x = Age, y = Shell.Weight)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue")

#Height
ggplot(CrabTrain, aes(x = Age, y = Height)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue")

#Diameter
ggplot(CrabTrain, aes(x = Age, y = Diameter)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue")

#Length
ggplot(CrabTrain, aes(x = Age, y = Length)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue")

#Model Creation
set.seed(7)
#KNN
feature_df <- CrabTrain %>%
  select(Shell.Weight, Height, Diameter, Length, Weight, Age)

#Standardize
feature_df_scaled <- feature_df %>% mutate(across(where(is.numeric), scale))

#Train-Test
trainIndex <- createDataPartition(feature_df_scaled$Age, p = 0.7, list = FALSE)
train_data <- feature_df_scaled[trainIndex, ]
test_data <- feature_df_scaled[-trainIndex, ]

train_x <- train_data %>% select(-Age)
train_y <- train_data$Age
test_x <- test_data %>% select(-Age)
test_y <- test_data$Age

#KNN Regression
final_knn <- knn.reg(train = train_x, test = test_x, y = train_y, k = 34)

#Predicted ages
predicted_ages <- final_knn$pred

#Actual ages
actual_ages <- test_y

#Evaluation
rmse <- sqrt(mean((predicted_ages - actual_ages)^2))
mae <- mean(abs(predicted_ages - actual_ages))
sst <- sum((actual_ages - mean(actual_ages))^2)
sse <- sum((predicted_ages - actual_ages)^2)
r_squared <- 1 - (sse / sst)

#Results
cat("RMSE:",rmse, "\n")
cat("MAE:",mae, "\n")
cat("R-squared:",r_squared, "\n")

#Linear Regression Model
lm_model <- lm(Age ~ Shell.Weight + Height + Diameter + Length + Weight, data = train_data)

#Predict
lm_pred <- predict(lm_model, newdata = test_data)

#Evaluation 
lm_rmse <- sqrt(mean((lm_pred - test_y)^2))
lm_mae <- mean(abs(lm_pred - test_y))
lm_sst <- sum((test_y - mean(test_y))^2)
lm_sse <- sum((lm_pred - test_y)^2)
lm_r_squared <- 1 - (lm_sse / lm_sst)

#Results
cat("RMSE:",lm_rmse, "\n")
cat("MAE:",lm_mae, "\n")
cat("R-squared:",lm_r_squared, "\n")

