library(tidyverse)
library(class)
library(caret)
library(FNN)
library(Metrics)
#Load
CrabTrain <- read.csv("train-1.csv")

#Select relevant features
feature_df <- CrabTrain %>%
  select(Shucked.Weight, Height, Diameter, Length, Weight, Age)

# Separate predictors and target
X <- feature_df[, c("Shucked.Weight", "Height", "Diameter", "Length", "Weight")]
y <- feature_df$Age

# Standardize using scale() to retain attributes
X_scaled <- scale(X)

# Store mean and sds for later use
means <- attr(X_scaled, "scaled:center")
sds   <- attr(X_scaled, "scaled:scale")

# Combine scaled features with target
feature_df_scaled <- as.data.frame(X_scaled)
feature_df_scaled$Age <- y

#Train-Test
trainIndex <- createDataPartition(feature_df_scaled$Age, p = 0.7, list = FALSE)
train_data <- feature_df_scaled[trainIndex, ]
test_data <- feature_df_scaled[-trainIndex, ]

#Linear Regression Model
lm_model <- lm(Age ~ Shucked.Weight + Height + Diameter + Length + Weight, data = train_data)

lm_pred <- predict(lm_model, newdata = test_data)

#Competition set
comp_set = read.csv("competition-1.csv")

# Separate predictors then scale features
X_comp <- comp_set[, c("Shucked.Weight", "Height", "Diameter", "Length", "Weight")]
scaled_features <- as.data.frame(scale(X_comp, center = means, scale = sds))

all_predictions <- predict(lm_model, newdata = scaled_features)
# Save predictions to a CSV file
output <- data.frame(ID = comp_set$id , Age = all_predictions)
write.csv(output, "Crab age prediction.csv", row.names = FALSE)
