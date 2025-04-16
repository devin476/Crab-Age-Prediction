# 🦀 Crab Age Prediction

This project builds and evaluates a predictive model to estimate the age of crabs based on various physical measurements. It uses exploratory data analysis (EDA), feature engineering, and a linear regression model to assess the strength of relationships between features and age.

## Dataset Overview

The dataset includes the following numerical features:

- `Age` (target variable)
- `Shell.Weight`
- `Height`
- `Diameter`
- `Length`
- `Weight`
- `Viscera.Weight`
- `Shucked.Weight`

Each row corresponds to an individual crab specimen with measured attributes.

## EDA Highlights

- Pairwise linear regressions to assess R² and p-values
- Correlation analysis to understand strength and direction of relationships
- Residual plots to inspect model performance visually

## Model

A multiple linear regression model is built to predict `Age` using:

```r
lm_model <- lm(Age ~ Shucked.Weight + Height + Diameter + Length + Weight, data = train_data)
```

Evaluation includes:

- Mean Absolute Error (MAE)
- Residual diagnostics
- Discussion of heteroscedasticity and model assumptions

## Key Findings

- `Shucked.Weight`, `Length`, and `Diameter` show stronger linear correlation with Age.
- Residual analysis reveals heteroscedasticity — suggesting that a linear model may not be the best fit for higher age predictions.
- Standardization is applied prior to model fitting to ensure consistent scaling.

## Future Improvements

- Try nonlinear or tree-based models
- Use cross-validation for better model robustness
- Explore feature transformation

## Requirements

- R 
- R packages: `tidyverse`, `caret`, `Metrics`, `class`, `FNN`

To install all required packages:
```r
install.packages(c("tidyverse", "caret", "Metrics", "class", "FNN"))
```
## Live App

Check out the interactive RShiny application here:  
[Crab Age Prediction RShiny App](https://devin476.shinyapps.io/Crab-Age-Prediction/)

This app allows users to explore the crab dataset visually and make predictions based on selected features.

## Authors

- [Titus Karuri](https://github.com/titusk2)

- [Devin Streeter](https://github.com/devin476)
---
