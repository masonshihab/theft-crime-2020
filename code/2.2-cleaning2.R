## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(readr)
library(readxl)
library(usdata)
library(tm)
library(stringr)


## ---------------------------------------------------------------------------------------------------------------------------------------------
hope = read.csv("https://raw.githubusercontent.com/grammakov/USA-cities-and-states/master/us_cities_states_counties.csv", sep = "|") %>% rename(State = State.full) 
hope = hope %>% select(-City.alias)
countycrime = read_xlsx("../data/raw/countycrime.xlsx") %>% select(State, County, Total_Theft)
citycrime = read_xlsx("../data/raw/citycrime.xlsx") 


## ---------------------------------------------------------------------------------------------------------------------------------------------
citycrime$State = removeNumbers(citycrime$State)
citycrime$City = removeNumbers(citycrime$City)

countycrime$State = removeNumbers(countycrime$State)
countycrime$County = removeNumbers(countycrime$County)


## ---------------------------------------------------------------------------------------------------------------------------------------------
countycrime$county_words = sapply(gregexpr("\\S+", countycrime$County), length)


stopwords = c(" County Police Department", " County Unified Police Department", " Public Safety")

countycrime$County = removeWords(countycrime$County, stopwords)

countycrime = countycrime %>% group_by(County, State) %>% summarise(Total_Theft = sum(Total_Theft)) %>% ungroup()



## ---------------------------------------------------------------------------------------------------------------------------------------------

citycrimecounty = hope %>% inner_join(citycrime, by = c("State", "City")) %>% distinct() 

citycrimecounty$County = tolower(citycrimecounty$County)
citycrimecounty$County = str_to_title(citycrimecounty$County)



## ---------------------------------------------------------------------------------------------------------------------------------------------
citiestocounties = citycrimecounty %>% group_by(County, State) %>% summarise(Total_Thefts = sum(Total_Thefts))
citiestocounties$County_State = paste(citiestocounties$County, citiestocounties$State)
countycrime$County_State = paste(countycrime$County, countycrime$State)



## ---------------------------------------------------------------------------------------------------------------------------------------------
missingcounties = citiestocounties %>% filter(!(County_State %in% countycrime$County_State))  %>% rename(Total_Theft = Total_Thefts)


## ---------------------------------------------------------------------------------------------------------------------------------------------
response = rbind(missingcounties, countycrime) %>% drop_na() %>% rename(county = County, state = State) 


## ---------------------------------------------------------------------------------------------------------------------------------------------
keyfile2 = read_csv("../data/raw/ZIP-COUNTY-FIPS_2017-06.csv") %>% 
  mutate(state_full = abbr2state(STATE),) %>% rename(county = COUNTYNAME, state = state_full, fips = STCOUNTYFP) %>% select(-ZIP) %>% distinct() 

## ---------------------------------------------------------------------------------------------------------------------------------------------
stopwords3 = c(" County")
keyfile2$county = removeWords(keyfile2$county, stopwords3)


## ---------------------------------------------------------------------------------------------------------------------------------------------
keyfile2$County_State = paste(keyfile2$county, keyfile2$state)


## ---------------------------------------------------------------------------------------------------------------------------------------------
responsekey = keyfile2 %>%  inner_join(response, by = "County_State") %>% select(fips, Total_Theft) %>% mutate(fips = as.numeric(fips))


## ---------------------------------------------------------------------------------------------------------------------------------------------
merge12incar = read_csv("../data/clean/merge12incar.csv")



## ---------------------------------------------------------------------------------------------------------------------------------------------
merge_with_response = merge12incar %>% inner_join(responsekey, by = "fips") %>% mutate(theftrate = Total_Theft/totalpop, .keep = "unused") %>% drop_na()

merge_with_response$county = removeWords(merge_with_response$county, stopwords3)



## ---------------------------------------------------------------------------------------------------------------------------------------------
write.csv(merge_with_response, file = '../data/clean/dataclean.csv', row.names = FALSE)

