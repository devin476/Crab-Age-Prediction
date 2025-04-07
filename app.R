library(tidyverse)
library(ggplot2)
library(class)
library(caret)
library(e1071)

#Load

CrabTest <- read.csv("Data/competition-1.csv")
CrabTrain <- read.csv("Data/train-1.csv")

#EDA
#Select numerical variables
features <- CrabTrain %>% 
  select(where(is.numeric)) %>%
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