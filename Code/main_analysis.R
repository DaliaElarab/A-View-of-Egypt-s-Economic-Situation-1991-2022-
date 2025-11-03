#---------------------------------------------------------------------------
# Author: Dalia Elaraby
# Project: Historical Macroeconomic Analysis of Egypt (1991–2022)
# Data Source: World Bank (https://data.worldbank.org/)
# Date: 2024
# Purpose: Data cleaning, analysis, and visualization in R
# Description: The analysis of Egypt's macroeconomic data aims to uncover insights into and relationships among various economic indicators, such as the unemployment rate, inflation trends, and population growth, and their impact on the economy.
#---------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(readr)
library(ggplot2)
library(knitr)
library(kableExtra)

# Load the dataset
Egypt_Economic_Data <- read_csv("Egypt_Economic_Data_1991-2022.csv")

# Inspect the data
glimpse(Egypt_Economic_Data)
summary(Egypt_Economic_Data)

# Check for missing values
colSums(is.na(Egypt_Economic_Data))
Egypt_Economic_Data$Year <- as.integer(Egypt_Economic_Data$Year)

# Plotting unemployment rate over time
ggplot(Egypt_Economic_Data, aes(x = Year, y = Unemployment_Rate)) +
  geom_line(color = "blue") +
  ggtitle("Unemployment Rate in Egypt Over Time") +
  xlab("Year") +
  ylab("Unemployment Rate (%)")

# Plotting inflation trends (CPI and GDP Deflator)
ggplot(Egypt_Economic_Data, aes(x = Year)) +
  geom_line(aes(y = CPI_Inflation_Rate, color = "CPI Inflation Rate")) +
  geom_line(aes(y = GDP_Inflation_Rate, color = "GDP Inflation Rate")) +
  scale_color_manual(values = c("CPI Inflation Rate" = "blue", "GDP Inflation Rate" = "red")) +
  ggtitle("Inflation Trends in Egypt Over Time") +
  xlab("Year") +
  ylab("Inflation Rate (%)") +
  labs(color = "Inflation Type")

# Plotting population growth over time
ggplot(Egypt_Economic_Data, aes(x = Year, y = Population_Growth_Rate)) +
  geom_line(color = "green") +
  ggtitle("Population Growth in Egypt Over Time") +
  xlab("Year") +
  ylab("Population Growth Rate (%)")

# Correlation matrix
correlation_matrix <- round(cor(Egypt_Economic_Data[,-1]), 2)
correlation_matrix

# Simple linear regression (Unemployment vs. Inflation)
model <- lm(Unemployment_Rate ~ CPI_Inflation_Rate + GDP_Inflation_Rate, data = Egypt_Economic_Data)
summary(model)

print(model)

# Plotting residuals of the linear regression model
plot(model, which = 1)  # Residuals vs Fitted

# A Colorful Residual Plot
residuals_df <- data.frame(
  Fitted_Values = fitted(model),
  Residuals = resid(model)
)
ggplot(residuals_df, aes(x = Fitted_Values, y = Residuals)) +
  geom_point(aes(color = Residuals), size = 3) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  ggtitle("Residuals vs Fitted") +
  xlab("Fitted values") +
  ylab("Residuals") +
  theme_minimal()
