rawData <- read.csv(file="./data/raw/world_salaries.csv")
nrow(rawData)
just6000 <- head(rawData, n=6000)
rawData <- write.csv(just6000, file="./data/6000/world_salaries_6000.csv")
