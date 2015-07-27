library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

options(scipen = 9)

censusData <- read_csv('../input/pums/ss13pusa.csv')
dim(censusData)

coData <- filter(censusData, ST == 8)
dim(coData)

# total weight of census participants ≈ total population
totalPopulation <- sum(coData$PWGTP)
print(totalPopulation)

birthPlaces <-  select(coData, POBP, PWGTP) %>% 
  group_by(POBP) %>% 
  summarize(Total=sum(PWGTP)) %>%
  arrange(desc(Total))

# number of CO natives
filter(birthPlaces, POBP == 8)

birthPlaces <- filter(birthPlaces, POBP != 8)

head(birthPlaces, n=10)

#convert birth place code to names


ggplot(birthPlaces, aes(POBP, Total)) +
  geom_bar(stat="identity")




worldBirthArea <- select(coData, WAOB, PWGTP) %>% 
  group_by(WAOB) %>% 
  summarize(Total=sum(PWGTP))

coChildren <- filter(coData, AGEP < 18)

