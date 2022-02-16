install.packages("sqldf")
install.packages("gapminder")
install.packages("tidyverse")

library(sqldf)
library(gapminder)
library(tidyverse)

#-------------------------------------------------------------------------------
# Load data set
rawData <- read.csv(file="./data/6000/world_salaries_6000.csv")
# Quick summary
summary(rawData)
apply(rawData, 2, function(x) length(unique(x))) # reference: https://stackoverflow.com/a/22196167/8661297

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
# 4. Is there any way to improve questionnaire?

#-------------------------------------------------------------------------------
# Try to filter out all software related jobs.
# SDET stands for Software Developer In Test
# Related jobs are Quality Assurance Engineer (QA)
# Software Developer and similar. Use table() to see the list of all jobs
table(usa_filtered_data$Job.title)

# Too many jobs have only 1 record. Need to try to filter out

# Step 1 - Make them all lower case and trim spaces
usa_filtered_data$Job.title <- trimws(tolower(usa_filtered_data$Job.title))
jobTitleTable <- table(usa_filtered_data$Job.title)
jobTitleDF <- data.frame(jobTitleTable)
names(jobTitleDF)

# Step 2 - Pull all jobs containing key words
softwareJobs <- sqldf('SELECT *
                         FROM jobTitleDF
                        WHERE Var1 LIKE "%sdet%"
                           OR Var1 LIKE "%qa%"
                           OR Var1 LIKE "%software%"
                           OR Var1 LIKE "%developer%"
                           OR Var1 LIKE "%quality%"
                           OR Var1 LIKE "%assurance%"')

# Step 2 - filter out extra pulled jobs, like managers
softwareJobs <- sqldf('SELECT *
                         FROM softwareJobs
                        WHERE Var1 NOT LIKE "%designer%"
                          AND Var1 NOT LIKE "%director%"
                          AND Var1 NOT LIKE "%manager%"
                          AND Var1 NOT LIKE "%curriculum%"
                          AND Var1 NOT LIKE "%team lead%"
                          AND Var1 NOT LIKE "%architect%"
                          AND Var1 NOT LIKE "%consultant%"
                          AND Var1 NOT LIKE "%chemist%"
                          AND Var1 NOT LIKE "%elearning%"
                          AND Var1 NOT LIKE "%content%"
                          AND Var1 NOT LIKE "%quality improvement associate%"
                          AND Var1 NOT LIKE "%coordinator%"
                          AND Var1 NOT LIKE "%engineering lead%"
                          AND Var1 NOT LIKE "%analyst%"
                          AND Var1 NOT LIKE "%support%"
                          AND Var1 NOT LIKE "%coordinator%"
                          AND Var1 NOT LIKE "%process%"
                          AND Var1 NOT LIKE "%product%"
                          AND Var1 NOT LIKE "%parts%"
                          AND Var1 NOT LIKE "%staff%"
                          AND Var1 NOT LIKE "%instruct%"')

usa_software <- usa_filtered_data[usa_filtered_data$Job.title %in% softwareJobs$Var1,]
summary(usa_software$Annual.income)

#-------------------------------------------------------------------------------
# Looks like there are annual income outliers. 
# Visualization might help to filter them out
stripchart(usa_software$Annual.income, 
           method="stack",
           at=c(0.05),
           pch=20,
           frame.plot)
abline(v=mean(usa_software$Annual.income), col="red",)

# mean is far right, due to several outliers.
# need to filter them out, but first make labels more readable
stripchart(usa_software$Annual.income, 
           method="stack",
           at=c(0.05),
           pch=20,
           frame.plot,
           xaxt = "n")
abline(v=mean(usa_software$Annual.income), col="red",)
axis(1, at=usa_software$Annual.income, labels=format(usa_software$Annual.income, scientific=FALSE))
# reference: https://stackoverflow.com/a/5968136/8661297

# Now try filtering out and drawing again
no_outliers <- usa_software[usa_software$Annual.income < 200000 & usa_software$Annual.income > 1000,]
stripchart(no_outliers$Annual.income, 
           method="stack",
           at=c(0.05),
           pch=20,
           frame.plot,
           xaxt = "n")
abline(v=mean(no_outliers$Annual.income), col="red",)
axis(1, at=no_outliers$Annual.income, labels=format(no_outliers$Annual.income, scientific=FALSE))

head(no_outliers)
#-------------------------------------------------------------------------------
# Now need to group data by experience. 
# I will use Overall.years.of.professional.experience for that purpose
table(no_outliers$Years.of.experience.in.field)

# Need to add min/max experience columns, so that "between" filter can be used:
# Follow up (next day): 
for (i in 1:length(no_outliers$Overall.years.of.professional.experience)) {
  if(no_outliers$Overall.years.of.professional.experience[i] == "1 year or less"){
    no_outliers$Experience.min[i] = 0
    no_outliers$Experience.max[i] = 1
    no_outliers$Experience.category[i] = 1
  } else if(no_outliers$Overall.years.of.professional.experience[i] == "2 - 4 years") {
    no_outliers$Experience.min[i] = 2
    no_outliers$Experience.max[i] = 4
    no_outliers$Experience.category[i] = 2
  }else if(no_outliers$Overall.years.of.professional.experience[i] == "5-7 years") {
    no_outliers$Experience.min[i] = 5
    no_outliers$Experience.max[i] = 7
    no_outliers$Experience.category[i] = 3
  }else if(no_outliers$Overall.years.of.professional.experience[i] == "8 - 10 years") {
    no_outliers$Experience.min[i] = 8
    no_outliers$Experience.max[i] = 10
    no_outliers$Experience.category[i] = 4
  }else if(no_outliers$Overall.years.of.professional.experience[i] == "11 - 20 years") {
    no_outliers$Experience.min[i] = 11
    no_outliers$Experience.max[i] = 20
    no_outliers$Experience.category[i] = 5
  }else if(no_outliers$Overall.years.of.professional.experience[i] == "21 - 30 years") {
    no_outliers$Experience.min[i] = 21
    no_outliers$Experience.max[i] = 30
    no_outliers$Experience.category[i] = 6
  }else if(no_outliers$Overall.years.of.professional.experience[i] == "31 - 40 years") {
    no_outliers$Experience.min[i] = 31
    no_outliers$Experience.max[i] = 199
    no_outliers$Experience.category[i] = 7
  }
}
no_outliers

#-------------------------------------------------------------------------------
# Now we can create a getMySalaryExpectation function
# getMySalaryExpectation(job, experience):
#   Min: xxx
#   25 percentile: xxx
#   Median: xxx
#   Mean: xxx
#   75 percentile: xxx
#   Max: xxx
getMySalaryExpectation <- function(job, experience) {
  if(job=="software"){
    
    all_salaries <- no_outliers[
        no_outliers$Experience.min <= experience & 
        no_outliers$Experience.max >= experience,
      ]$Annual.income
    res <- summary(all_salaries)
    
    names(res)[1] <- "Min"
    names(res)[2] <- "25 percentile"
    names(res)[3] <- "Median"
    names(res)[4] <- "Mean"
    names(res)[5] <- "75 percentile"
    names(res)[6] <- "Max"
    
    return(res)
    
  } else {
    print("Only software related jobs are supported in this version")
    return(NA)
  }
}
getMySalaryExpectation("not software", 5)
getMySalaryExpectation("software", 1)
getMySalaryExpectation("software", 15)
getMySalaryExpectation("software", 50)

#-------------------------------------------------------------------------------
# Visualize salary per category
meanAnualIncome <- mean(no_outliers$Annual.income)
medianAnualIncome <- median(no_outliers$Annual.income)

plot(x = no_outliers$Experience.category,
     y = no_outliers$Annual.income)
abline(lm(no_outliers$Annual.income ~ no_outliers$Experience.category), col = "red", lwd = 2, lty = 2)
abline(h=meanAnualIncome, col = "blue", lwd = 2, lty = 2)
abline(h=medianAnualIncome, col = "green", lwd = 2, lty = 3)

#-------------------------------------------------------------------------------
# Make a pretty visual
# Reference: https://towardsdatascience.com/r-for-beginners-learn-how-to-visualize-data-like-a-pro-840d1828c09c
p <- ggplot( data=no_outliers, aes(x = Experience.category, y = Annual.income)) + geom_point()
p
p <- p + aes(color=continent)

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






