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


