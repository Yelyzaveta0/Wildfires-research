library(readxl)
install.packages("dplyr")
install.packages("forecast")
library(dplyr)
library(ggplot2)
library(forecast)
library(tidyr)
#data_file <- read.csv("C:\\Users\\Лиза\\Downloads\\Data.csv", stringsAsFactors = FALSE)
#View(data_file)

data_file <- read_excel("C:\\Users\\Лиза\\Downloads\\Data.xlsx", skip = 1)
colnames(data_file)[1:3] <- c("Jurisdiction", "Cause", "Qualifier")
data_file_long <- data_file %>%
  pivot_longer(
    cols = 4:ncol(.),   
    names_to = "Year",
    values_to = "Count"
  ) %>%
  mutate(Year = as.numeric(Year))
head(data_file_long)

data_file_long %>%
  group_by(Jurisdiction, Cause) %>%
  summarise(min_year = min(Year), max_year = max(Year), .groups = "drop")

#Example 1: Alberta human activity prediction
alberta_human <- data_file_long %>%
  filter(Jurisdiction == "Alberta", Cause == "Human activity") %>%
  arrange(Year)
ts_data <- ts(alberta_human$Count, start = min(alberta_human$Year), frequency = 1)

fit <- auto.arima(ts_data)

fcast <- forecast(fit, h = 10)
autoplot(fcast) + ggtitle("Alberta - Human activity forecast")
