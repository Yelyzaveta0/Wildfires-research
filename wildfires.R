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

most_cause <- data_file_long %>%
  filter(!is.na(Count), Count > 0) %>%
  group_by(Jurisdiction, Year) %>%
  mutate(is_most = Count == max(Count, na.rm = TRUE)) %>%
  filter(is_most) %>%
  select(Jurisdiction, Year, Count, Cause) %>%
  arrange(Jurisdiction, Year)
most_cause_summary <- most_cause %>%
  group_by(Jurisdiction, Cause) %>%
  summarise(
    Years_dom = n(),
    First_Year = min(Year),
    Last_Year = max(Year), 
    avg_most = mean(Count), 
    .groups = "drop"
  ) %>%
  arrange(Jurisdiction, desc(Years_dom))
print(most_cause_summary, n = 50)

ggplot(most_cause, aes(x = Year, y = Jurisdiction, fill = Cause)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Dominant Wildfire Causes by Province and Year",
       subtitle = "Most frequent cause of wildfires each year",
       x = "Year", y = "Province", fill = "Cause") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")


cause_patterns <- most_cause %>%
  group_by(Jurisdiction, Cause) %>%
  summarise(Years_dom = n(), .groups = 'drop') %>%
  complete(Jurisdiction, Cause, fill = list(Years_dom = 0))

ggplot(cause_patterns, aes(x = Cause, y = Jurisdiction, fill = Years_dom)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Years_dom), color = "black", size = 3.5) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Heatmap of Wildfire Cause Dominance by Province",
       subtitle = "Number of years each cause was the most frequent",
       x = "Cause", y = "Province", fill = "Years Dominant") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
