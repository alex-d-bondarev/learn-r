install.packages("sqldf")
install.packages("gapminder")
install.packages("tidyverse")

library(sqldf)
library(gapminder)
library(tidyverse)

# 1. Load and analyse data

rawData <- read.csv(file="./data/6000/world_salaries_6000.csv")
summary(rawData)
apply(rawData, 2, function(x) length(unique(x)))

# 2. Extract data

head(rawData)
table(rawData$Currency)

usa_data <- sqldf('SELECT *
                  FROM rawData
                  WHERE Currency = "USD"')

exclude_non_usa_regex <- "(?!.*?(located|Bermuda|Canada|Cayman Islands|Colombia|Contracts|Denmark|England|France|Germany|ISA|Japan|Kuwait|Mexico|Sri lanka|Sweden|U.A.|United Kingdom|Worldwide))^.*$"
usa_country_ids <- which(grepl(exclude_non_usa_regex, usa_data$Country, perl = TRUE))
usa_data <- usa_data[usa_country_ids,]

filtered_out <- (nrow(rawData) - nrow(usa_data))
print(paste("Filtered out ", filtered_out, " records"))  

write.csv(usa_data, file="./data/interim/usa_data.csv")

