library(tidyverse)
library(class)
library(caret)
library(FNN)
library(Metrics)
#Load
CrabTrain <- read.csv("train-1.csv")

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
#Shucked Weight
ggplot(CrabTrain, aes(x = Age, y = Shucked.Weight)) +
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

#Weight
ggplot(CrabTrain, aes(x = Age, y = Weight)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue")


feature_df <- CrabTrain %>%
  select(Shucked.Weight, Height, Diameter, Length, Weight, Age)

#Standardize
feature_df_scaled <- feature_df %>% mutate(across(where(is.numeric), scale))

#Train-Test
trainIndex <- createDataPartition(feature_df_scaled$Age, p = 0.7, list = FALSE)
train_data <- feature_df_scaled[trainIndex, ]
test_data <- feature_df_scaled[-trainIndex, ]

#Linear Regression Model
lm_model <- lm(Age ~ Shucked.Weight + Height + Diameter + Length + Weight, data = train_data)

lm_pred <- predict(lm_model, newdata = test_data)

#Evaluation 
mae_score <-mae(test_data$Age,lm_pred)
print(mae_score)
