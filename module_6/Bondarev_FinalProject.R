install.packages("sqldf")
library(sqldf)

rawData <- read.csv(file="./data/6000/world_salaries_6000.csv")
# Quick summary
summary(rawData)
class(rawData)
initial_raw_data_size <- nrow(rawData)
initial_raw_data_size

# Show unique currencies
unique(rawData$Currency)

# Other is unidentifiable
# AUD/NZD (Australian Dollar / New Zealand Dollar) have an 8% difference 

rawData <- sqldf('SELECT *
                  FROM rawData
                  WHERE Currency NOT IN ("AUD/NZD", "Other")')
filtered_currency_size <- nrow(rawData)
filtered_currency_size

num_of_filtered_cur <- initial_raw_data_size - filtered_currency_size
num_of_filtered_cur

# as of 14 February
# Records with these values should be removed
# Conversion to USD ratios should be added as a new collumns for records that are left.
# X x conversion_ratio -> USD
# The ratios as of 14 February are the following:
# USD = 1.000
# GBP = 1.350
# CAD = 0.790
# EUR = 1.130
# CHF = 1.080
# ZAR = 0.066
# SEK = 0.110
# HKD = 0.130

