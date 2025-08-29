library(readxl)
install.packages("dplyr")
install.packages("forecast")
library(dplyr)
library(ggplot2)
library(forecast)
library(tidyr)
library(tidyverse)
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
  arrange(Year) %>%
  filter(!is.na(Count))
#Try ETC data model: 
fit_ets <- ets(ts_data)
fcast_ets <- forecast(fit_ets, h = 10)

fit_arima_trend <- auto.arima(ts_data, d = 1, stepwise = FALSE, approximation = FALSE)
fcast_arima <- forecast(fit_arima_trend, h = 10)

autoplot(ts_data, series = "Observed") +
  autolayer(fcast_ets, series = "ETS Forecast", PI = FALSE) +
  autolayer(fcast_arima, series = "ARIMA Forecast", PI = FALSE) +
  ggtitle("Alberta - Human Activity Wildfires: Forecast Comparison") +
  xlab("Year") + ylab("Number of Fires") +
  theme_minimal()


ts_data <- ts(alberta_human$Count, start = min(alberta_human$Year), frequency = 1)

fit <- auto.arima(ts_data)

fcast <- forecast(fit, h = 10)
autoplot(fcast) + autolayer(ts_data, series="Observed") + ggtitle("Alberta - Human activity forecast") + theme_minimal()
print(fit)


diff_data <- diff(ts_data)
plot(diff_data, main = "Year-to-Year Changes")
abline(h = 0, col = "red")

# Check model diagnostics
checkresiduals(fit)

# Look at the fitted vs actual values
plot(ts_data, main = "Actual vs Fitted")
lines(fitted(fit), col = "red")
legend("topleft", legend = c("Actual", "Fitted"), col = c("black", "red"), lty = 1)

