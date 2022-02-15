install.packages("sqldf")
library(sqldf)

#-------------------------------------------------------------------------------
# Load data set
rawData <- read.csv(file="./data/6000/world_salaries_6000.csv")
# Quick summary
summary(rawData)
class(rawData)

#-------------------------------------------------------------------------------
# Different countries had different ranges of salaries. 
# So it's better to concentrate on 1 specific country that has the most records in a data set:
table(rawData$Currency)

# Most of the records (5197) are from USA. Lets work with them:
usa_data <- sqldf('SELECT *
                  FROM rawData
                  WHERE Currency = "USD"')

#-------------------------------------------------------------------------------
head(usa_data)
class(usa_data$Annual.salary)
# Annual salary column is character. It should be converted to numbers

# Step 1 - remove comma symbols
usa_data$Annual.salary <- gsub(",","",usa_data$Annual.salary)
head(usa_data$Annual.salary)

# Step 2 - use regex to find any Annual.salary that needs additional cleaning
# Reference: https://stackoverflow.com/a/49913163/8661297
which(!grepl('^[0-9]', usa_data$Annual.salary))
# integer(0) - means that all values are valid numbers

# Step 3 - convert Annual.salary to numbers
usa_data$Annual.salary <- as.numeric(usa_data$Annual.salary)
class(usa_data$Annual.salary)

#-------------------------------------------------------------------------------
head(usa_data)
head(usa_data$Other.monetary.comp)
class(usa_data$Other.monetary.comp)

# Replace all NA with 0
# Reference: https://stackoverflow.com/a/8166616/8661297
usa_data$Other.monetary.comp[is.na(usa_data$Other.monetary.comp)] <- 0
head(usa_data$Other.monetary.comp)

#-------------------------------------------------------------------------------
head(usa_data)
head(usa_data$Country)
table(usa_data$Country)
# Looks like filtering by currency was not enough. 
# There is a dozen of different ways USA is written down. 
# Also some countries here are not USA. 
# I will use regex to filter out all non USA records that I can be seen from a table(usa_data$Country)
# Similar websites are useful for regex creation: https://regex101.com/
# (?!.*?(located|Bermuda|Canada|Cayman Islands|Colombia|Contracts|Denmark|England|France|Germany|ISA|Japan|Kuwait|Mexico|Sri lanka|Sweden|U.A.|United Kingdom|Worldwide))^.*$
exclude_non_usa_regex <- "(?!.*?(located|Bermuda|Canada|Cayman Islands|Colombia|Contracts|Denmark|England|France|Germany|ISA|Japan|Kuwait|Mexico|Sri lanka|Sweden|U.A.|United Kingdom|Worldwide))^.*$"
usa_country_ids <- which(grepl(exclude_non_usa_regex, usa_data$Country, perl = TRUE))

# perl = TRUE reference: https://stackoverflow.com/a/31547274/8661297

# Now save filtered values to a new data.frame
usa_filtered_data <- usa_data[usa_country_ids,]
head(usa_filtered_data)

# Rename all Country values to USA
usa_filtered_data$Country <- "USA"
head(usa_filtered_data)

#-------------------------------------------------------------------------------
# Time to combine Annual.salary and Other.monetary.comp into a new column
# called annual income
usa_filtered_data$Annual.income <- 
  usa_filtered_data$Annual.salary + usa_filtered_data$Other.monetary.comp
head(usa_filtered_data)

#-------------------------------------------------------------------------------
# Analyze non income columns that might come in handy
table(usa_filtered_data$How.old.are.you.) # This can be used later for demographics chart
table(usa_filtered_data$Industry) # I have no ideas about this one
table(usa_filtered_data$Job.title) # I could check all the jobs that are related to mine
table(usa_filtered_data$Overall.years.of.professional.experience) # Might use for correlation analysis
table(usa_filtered_data$Years.of.experience.in.field) # Might use for correlation analysis
table(usa_filtered_data$Highest.level.of.education.completed) # Might use for correlation analysis

#-------------------------------------------------------------------------------
# What if there was a way to input your Job title and experience into an app
# that would generate salary percentiles for you:
# getMySalaryExpectation(job, experience):
#   Min: xxx
#   25 percentile: xxx
#   Median: xxx
#   Mean: xxx
#   75 percentile: xxx
#   Max: xxx
# Questions to answer: 
# 1. How complicated is it to prepare a model for such app?
# 2. What is the algorithm?
# 3. Additional questions to consider

#-------------------------------------------------------------------------------
summary(usa_filtered_data$Annual.income)

#-------------------------------------------------------------------------------
# Park for later
#-------------------------------------------------------------------------------
# 1 clean currency data

# Show unique currencies
unique(rawData$Currency)

# Other is unidentifiable
# AUD/NZD (Australian Dollar / New Zealand Dollar) have an 8% difference 
initial_raw_data_size <- nrow(rawData)
initial_raw_data_size

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






