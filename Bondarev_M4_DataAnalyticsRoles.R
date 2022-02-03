library(ggplot2)
library(scales)
library(hrbrthemes)
library(viridis)

#----------------------------------------#
# Data source: https://www.bls.gov/data/ #
#----------------------------------------#

#-------------------------------------------------------------------------------
# Compare ENG slaries
# Employment percent change 2020 - 2030 = 22.2%

occupation <- "Software Developers and Software Quality Assurance Analysts and Testers"
wageStats <- c("10%", "25%", 	"50%", "75%", "90%")


industries <- c(rep("1 General", 5), 
                rep("2 Pharmaceutical and Medicine Manufacturing", 5), 
                rep("3 Telecommunications", 5),
                rep("4 Data Processing, Hosting, and Related Services", 5),
                rep("5 Computer Systems Design and Related Services", 5))
values <- c(65210, 84020, 110140, 140470, 170100, 
            66130, 81380, 107580, 131080, 156590, 
            69230, 85930, 106010, 128790, 153290,
            62060, 82090, 109570, 138730, 168160, 
            61290, 79830, 106160, 137180, 169610)


allData <- data.frame(wageStats, industries, values)

ggplot(allData, 
       aes(fill=wageStats, y=values, x=stringr::str_wrap(industries, 20))) + 
  geom_bar(position="dodge", stat="identity") + 
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
  scale_fill_viridis(discrete = T, option="cividis") + theme_ipsum() +
  coord_flip() + xlab("Industry") + ylab("Wage")

#-------------------------------------------------------------------------------
#===============================================================================
#-------------------------------------------------------------------------------
# Compare PM slaries
# Employment percent change 2020 - 2030 = 5.6%

occupation <- "Project Management Specialist"
wageStats <- c("10%", "25%", 	"50%", "75%", "90%")


industries <- c(rep("1 General", 5), 
                rep("2 Pharmaceutical and Medicine Manufacturing", 5), 
                rep("3 Telecommunications", 5),
                rep("4 Data Processing, Hosting, and Related Services", 5),
                rep("5 Computer Systems Design and Related Services", 5))
values <- c(42180, 56790, 77420, 104410, 135220, 
            50910, 66120, 88860, 114780, 142580, 
            54100, 71110, 93080, 116410, 136010,
            51520, 67690, 91340, 120170, 147960, 
            52340, 69780, 95910, 126140, 159710)


allData <- data.frame(wageStats, industries, values)

ggplot(allData, 
       aes(fill=wageStats, y=values, x=stringr::str_wrap(industries, 20))) + 
  geom_bar(position="dodge", stat="identity") + 
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
  scale_fill_viridis(discrete = T, option="cividis") + theme_ipsum() +
  coord_flip() + xlab("Industry") + ylab("Wage")

#-------------------------------------------------------------------------------
#===============================================================================
#-------------------------------------------------------------------------------
# Compare PM and ENG slaries in Telecommunications and Pharmaceutical

wageStats <- c("50%", "75%", "90%")


industries <- c(rep("1 Pharmaceutical / ENG", 3), 
                rep("2 Pharmaceutical / PM", 3), 
                rep("3 Telecommunications / ENG", 3),
                rep("4 Telecommunications / PM", 3),
                rep("5 Computer Systems / ENG", 3),
                rep("6 Computer Systems / PM", 3))
values <- c(107580, 131080, 156590, 
            88860, 114780, 142580, 
            106010, 128790, 153290,
            93080, 116410, 136010,
            106160, 137180, 169610,
            95910, 126140, 159710)


allData <- data.frame(wageStats, industries, values)

ggplot(allData, 
       aes(fill=wageStats, y=values, x=stringr::str_wrap(industries, 20))) + 
  geom_bar(position="dodge", stat="identity") + 
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
  scale_fill_viridis(discrete = T, option="cividis") + theme_ipsum() +
  coord_flip() + xlab("Industry / Occupation") + ylab("Wage")
