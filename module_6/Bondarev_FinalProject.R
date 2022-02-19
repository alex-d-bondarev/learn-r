install.packages("sqldf")
install.packages("gapminder")
install.packages("tidyverse")
install.packages("ggplot2")

library(sqldf)
library(gapminder)
library(tidyverse)
library(ggplot2)

#-------------------------------------------------------------------------------
# 0. Get the first 6000 lines of raw data
rawData <- read.csv(file="./data/raw/world_salaries.csv")
nrow(rawData)
just6000 <- head(rawData, n=6000)
write.csv(just6000, file="./data/6000/world_salaries_6000.csv")

#-------------------------------------------------------------------------------
# 1. Load and analyse data structure

rawData <- read.csv(file="./data/6000/world_salaries_6000.csv")
summary(rawData)
apply(rawData, 2, function(x) length(unique(x)))

#-------------------------------------------------------------------------------
# 2. Extract USA records

table(rawData$Country)
table(rawData$Currency)

usa_data <- sqldf('SELECT *
                  FROM rawData
                  WHERE Currency = "USD"')

exclude_non_usa_regex <- "(?!.*?(located|Bermuda|Canada|Cayman Islands|Colombia|Contracts|Denmark|England|France|Germany|ISA|Japan|Kuwait|Mexico|Sri lanka|Sweden|U.A.|United Kingdom|Worldwide))^.*$"
usa_country_ids <- which(grepl(exclude_non_usa_regex, usa_data$Country, perl = TRUE))
usa_data <- usa_data[usa_country_ids,]

filtered_out <- (nrow(rawData) - nrow(usa_data))
print(paste("Filtered out", filtered_out, "records"))  

write.csv(usa_data, file="./data/interim/usa_data.csv")

#-------------------------------------------------------------------------------
# 3. Add new columns

# 3.1. Add Annual.income column

class(usa_data$Annual.salary)
usa_data$Annual.salary <- as.numeric(gsub(",","",usa_data$Annual.salary))
class(usa_data$Annual.salary)

usa_data$Other.monetary.comp[is.na(usa_data$Other.monetary.comp)] <- 0
head(usa_data$Other.monetary.comp)

usa_data$Annual.income <- usa_data$Annual.salary + usa_data$Other.monetary.comp
head(usa_data$Annual.income)

# 3.2. Add Experience.XYZ columns

for (i in 1:length(usa_data$Overall.years.of.professional.experience)) {
  if(usa_data$Overall.years.of.professional.experience[i] == "1 year or less"){
    usa_data$Experience.min[i] <- 0
    usa_data$Experience.max[i] <- 1
    usa_data$Experience.category[i] <- 1
  } else if(usa_data$Overall.years.of.professional.experience[i] == "2 - 4 years") {
    usa_data$Experience.min[i] <- 2
    usa_data$Experience.max[i] <- 4
    usa_data$Experience.category[i] <- 2
  }else if(usa_data$Overall.years.of.professional.experience[i] == "5-7 years") {
    usa_data$Experience.min[i] <- 5
    usa_data$Experience.max[i] <- 7
    usa_data$Experience.category[i] <- 3
  }else if(usa_data$Overall.years.of.professional.experience[i] == "8 - 10 years") {
    usa_data$Experience.min[i] <- 8
    usa_data$Experience.max[i] <- 10
    usa_data$Experience.category[i] <- 4
  }else if(usa_data$Overall.years.of.professional.experience[i] == "11 - 20 years") {
    usa_data$Experience.min[i] <- 11
    usa_data$Experience.max[i] <- 20
    usa_data$Experience.category[i] <- 5
  }else if(usa_data$Overall.years.of.professional.experience[i] == "21 - 30 years") {
    usa_data$Experience.min[i] <- 21
    usa_data$Experience.max[i] <- 30
    usa_data$Experience.category[i] <- 6
  }else if(usa_data$Overall.years.of.professional.experience[i] == "31 - 40 years") {
    usa_data$Experience.min[i] <- 31
    usa_data$Experience.max[i] <- 199
    usa_data$Experience.category[i] <- 7
  }
}

attach(usa_data)
print(paste(head(Experience.min), head(Experience.max), head(Experience.category)))
detach(usa_data)
write.csv(usa_data, file="./data/interim/prepared_usa_data.csv")

#-------------------------------------------------------------------------------
# 4. Visualize the interim data

# The annual earnings for a full-time minimum-wage worker is $15,080 
# at the current federal minimum wage of $7.25
#  Reference: https://poverty.ucdavis.edu/faq/what-are-annual-earnings-full-time-minimum-wage-worker

usa_no_outliers <- usa_data[usa_data$Annual.income < 300000 &
                                   usa_data$Annual.income > 15080,]


ggplot(usa_no_outliers,
       aes(x = Experience.category, y = Annual.income),
       options(scipen=10000)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") + 
  labs(x = "Experience category",
       y = "Annual income in USD")

write.csv(usa_no_outliers, file="./data/interim/usa_no_outliers.csv")

#-------------------------------------------------------------------------------
# 5. Computation algorithm

# 5.1. Filter software related jobs
# 5.1.1 Make them all lower case and trim spaces

usa_no_outliers$Job.title <- trimws(tolower(usa_no_outliers$Job.title))
jobTitleTable <- table(usa_no_outliers$Job.title)
usa_software <- data.frame(jobTitleTable)

# 5.1.2 Pull all jobs containing key words

softwareJobs <- sqldf(
  'SELECT *
     FROM usa_software
    WHERE Var1 LIKE "%sdet%"     OR Var1 LIKE "%qa%"
       OR Var1 LIKE "%software%" OR Var1 LIKE "%developer%"
       OR Var1 LIKE "%quality%"  OR Var1 LIKE "%assurance%"'
)

# 5.1.3 Filter out extra pulled jobs, like managers

softwareJobs <- sqldf(
  'SELECT *
     FROM softwareJobs
    WHERE Var1 NOT LIKE "%designer%"
      AND Var1 NOT LIKE "%director%"    AND Var1 NOT LIKE "%manager%"
      AND Var1 NOT LIKE "%curriculum%"  AND Var1 NOT LIKE "%team lead%"
      AND Var1 NOT LIKE "%architect%"   AND Var1 NOT LIKE "%consultant%"
      AND Var1 NOT LIKE "%chemist%"     AND Var1 NOT LIKE "%elearning%"
      AND Var1 NOT LIKE "%content%"     AND Var1 NOT LIKE "%quality improvement associate%"
      AND Var1 NOT LIKE "%coordinator%" AND Var1 NOT LIKE "%engineering lead%"
      AND Var1 NOT LIKE "%analyst%"     AND Var1 NOT LIKE "%support%"
      AND Var1 NOT LIKE "%coordinator%" AND Var1 NOT LIKE "%process%"
      AND Var1 NOT LIKE "%product%"     AND Var1 NOT LIKE "%parts%"
      AND Var1 NOT LIKE "%staff%"       AND Var1 NOT LIKE "%instruct%"'
)

# 5.1.4 Use softwareJobs data frame to pull all software records 
# from usa_no_outliers data frame

usa_software <- usa_no_outliers[usa_no_outliers$Job.title %in% softwareJobs$Var1,]
write.csv(usa_software, file="./data/processed/usa_software.csv")
nrow(usa_software)

# 5.2. Algorithm

getSalaryExpectation <- function(job, experience) {
  if (job == "software") {
    processed_data <-
      read.csv(file = "./data/processed/usa_software.csv")
    
    all_salaries <-
      processed_data[processed_data$Experience.min <= experience &
                       processed_data$Experience.max >= experience, ]$Annual.income
    
    return(summary(all_salaries))
    
  } else {
    print("Only software related jobs are supported in this version")
    return(NA)
  }
}

getSalaryExpectation("not software", 5)
getSalaryExpectation("software", 1)
getSalaryExpectation("software", 15)
