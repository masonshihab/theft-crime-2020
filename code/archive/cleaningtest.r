---
title: ""
author: "Mason Shihab"
date: "3/13/2021"
output:
  pdf_document:
    latex_engine: xelatex
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(readr)
library(tidycensus)
library(readxl)
library(usdata)
library(tm)
library(stringr)
```


# Introduction

Today we will be expanding on the election data set and partition it between a test and a train data set. We will use the train data set in order to attempt to predict the remainder of the values. Various methods for doing so will be ran and compared against each other for educational and practice purposes.



## Reloading the data.
```{r}
var <- load_variables(2019, "acs1", cache = TRUE)
```


## Selecting the variables

```{r}
census_data <- get_acs(geography = "county", variables = c(med_age = "B01002_001", 
                                                           permale = "B01001_002",
                                                           bachplus = "B16010_041",
                                                           totalpop = "B01003_001",
                                                           unemployed = "B27011_008",
                                                           employed =	"B27011_003",
                                                           foodstamp = "B09010_002",
                                                           ilefnhi= "B27011_007",
                                                           ilufnhi= "B27011_012",
                                                           nilfnhi = "B27011_017",
                                                           gini = "B19083_001",
                                                           med_income = "B19013_001",
                                                           med_2bed = "B25031_004",
                                                           single_mom = "B11012_010",
                                                           lessthan_hs = "B16010_002",
                                                           housing_units = "B25001_001",
                                                           households = "B11001_002",
                                                           inschool = "B14001_002",
                                                           ingradprofesh = "B14001_009",
                                                           inundergrad = "B14001_008",
                                                           Marriedcouplefamily = "B11001_003",
                                                           withkids = "B23009_002",
                                                           singledad = "B19131_039", 
                                                           divorced = "B07008_004",
                                                           nevermarried = "B06008_002",
                                                           foreignborn = "B99051_005",
                                                           fromdifstate = "B07001_065",
                                                           fromabroad = "B07001_081",
                                                           widowed = "B06008_006",
                                                           dis5to17male = "B18101_007",
                                                           dis5to17female = "B18101_026",
                                                           dis18to34male = "B18101_010",
                                                           dis18to34female = "B18101_029",
                                                           dis35to64male = "B18101_013",
                                                           dis35to64female = "B18101_032",

                       year = 2019))
```






```{r}
census_step = census_data %>% 
  pivot_wider(id_cols = c(NAME), names_from = variable, values_from = estimate) %>% 
  separate(NAME, into = c("county", "state"), sep = ", ") %>% arrange(state, county)


census_wider = census_step %>% 
  mutate(permale = permale/totalpop, 
         bachplus = bachplus/totalpop, unemployed_rate = unemployed/(employed+unemployed), employed_rate = employed/(employed+unemployed),
         foodstamp = foodstamp/totalpop, no_health_ins = 
           (ilefnhi+ilufnhi+nilfnhi)/totalpop, single_mom = 
           single_mom/households, lessthan_hs = lessthan_hs/totalpop, incschool = inschool/totalpop, ingradprofesh = ingradprofesh/totalpop, inundergrad = inundergrad/totalpop, Marriedcouplefamily = Marriedcouplefamily/households, withkids = withkids/households, singledad = singledad/households, divorced = divorced/totalpop, widowed = widowed/totalpop, nevermarried = nevermarried/totalpop, foreignborn = foreignborn/totalpop, fromdifstate = fromdifstate/totalpop, fromabroad = fromabroad/totalpop) %>% mutate(
             dis5to17 = (dis5to17male + dis5to17female)/totalpop,
             dis18to34 = (dis18to34male + dis18to34female)/totalpop,             
             dis35to64 = (dis35to64male + dis35to64female)/totalpop,
             ) %>% 
  select(-ilefnhi,-ilufnhi,-nilfnhi, -unemployed, -employed, -households, -dis5to17male, -dis5to17female, -dis18to34male, -dis18to34female, -dis35to64male, -dis35to64female)
ACS_vars = drop_na(census_wider)



```



## Combine with Election Dataset

```{r}
election = read_csv("../data/raw/president_county_candidate.csv", 
    col_types = cols(won = col_skip()))



election2 = election %>% arrange(state, county) %>% 
  group_by(state, county) %>% 
  mutate(pctvote = 100*total_votes/sum(total_votes)) %>% 
  filter(candidate %in% c("Joe Biden", "Donald Trump"))

election_clean = election2 %>% 
  pivot_wider(id_cols = c(state, county), 
              names_from = candidate, values_from = pctvote)

ACS_election = election_clean %>% inner_join(ACS_vars, by = c("state", "county")) %>% select(-`Joe Biden`) %>% rename(pertrump = 'Donald Trump') %>% mutate(pertrump = pertrump/100)

```

```{r}
keyfile = read_csv("../data/raw/ZIP-COUNTY-FIPS_2017-06.csv") %>% 
  mutate(state_full = abbr2state(STATE),) %>% rename(county = COUNTYNAME, state = state_full, fips = STCOUNTYFP) %>% select(-ZIP) %>% distinct() 
```

```{r}
ACS_election_key = ACS_election %>% inner_join(keyfile, by = c("state", "county")) %>%  mutate(fips = as.numeric(fips))
```

## Land and Density

```{r}
land = read_xlsx("../data/raw/land.xlsx") %>% rename(fips = STCOU, area = LND010190D) %>% mutate(fips = as.numeric(fips)) 
```


```{r}
ACS_election_land = ACS_election_key %>% inner_join(land, by = "fips") %>% mutate(pop_density = totalpop/area, housing_density = housing_units/area) %>% select(-area, -housing_units)
```


## % in Poverty

```{r}
poverty = read_xlsx("../data/raw/poverty.xlsx", skip = 1) %>% mutate(pct_all_in_pov = as.numeric(`Poverty Percent, All Ages`)/100, .keep = "unused")
poverty$fips = as.numeric(paste(poverty$`State FIPS Code`, poverty$`County FIPS Code`, sep = ""))
```

```{r}
merge1 = ACS_election_land %>% inner_join(poverty, by = "fips") %>% 
  select(-STATE, -CLASSFP, -Areaname, -`State FIPS Code`, -`County FIPS Code`) 
```


## Police Performance Vars


```{r}
file = "../data/raw/scorecard.csv"
scorecard_raw = read_csv(file)
scorecard_raw = scorecard_raw %>% select("fips_state_code", "fips_county_code",
                                         "calc_police_violence_score","calc_police_accountability_score",
                                         "calc_approach_to_policing_score","calc_police_funding_score")
scorecard_raw = data.frame(sapply(scorecard_raw, function(x) as.numeric(gsub("%", "", x)))) %>%
  rename(police_violence_score = calc_police_violence_score, 
         police_accountability_score=calc_police_accountability_score, 
         approach_to_policing_score = calc_approach_to_policing_score, 
         police_funding_score = calc_police_funding_score) %>% 
  mutate(fips_state_code=as.character(fips_state_code), fips_county_code = as.character(fips_county_code))
```

```{r}

scorecard_raw$fips_county_code = str_pad(scorecard_raw$fips_county_code, 3, pad = "0")

scorecard_raw$fips = str_c(scorecard_raw$fips_state_code, scorecard_raw$fips_county_code)

scorecard_clean = scorecard_raw %>% select(-fips_state_code,-fips_county_code) %>% 
  mutate(fips = as.numeric(fips)) %>% group_by(fips) %>% 
  summarise(police_violence_score = mean(police_violence_score), 
            police_accountability_score = mean(police_accountability_score), 
            approach_to_policing_score = mean(approach_to_policing_score), 
            police_funding_score = mean(police_funding_score))


```

```{r}
merge2 = merge1 %>% inner_join(scorecard_clean, by = "fips")

```

## HHA


```{r}
hha_raw = read_excel('../data/raw/HHS_OCDO_TDWG_COVID-19_Community_Vulnerability_Crosswalk.xlsx') %>% as_tibble()
```

```{r}
mean_data = hha_raw %>% select(`County FIPS`,`HHA Score`) %>%
  rename(fips = `County FIPS`, hha_score = `HHA Score`) %>%
  mutate(fips=as.numeric(fips))%>%
  group_by(fips) %>%
  summarise(mean_hha_score = mean(hha_score))%>%
  as_tibble()
```

```{r}
merge3 = merge2 %>% inner_join(mean_data, by = "fips")

```

## Soc Vulnerability


```{r}
file = "../data/raw/Social_Vulnerability_Index_2018.csv"
vulner_raw = read_csv(file)
vulner_raw = vulner_raw %>% select("FIPS","RPL_THEMES")%>% filter(RPL_THEMES>=0)%>%
  rename(fips = FIPS, svi_overall=RPL_THEMES)
```

```{r}
vulner_raw$fips = as.numeric(vulner_raw$fips)
```

```{r}
merge4 = merge3 %>% inner_join(vulner_raw, by = "fips")

```

## County Health Rank


```{r}
file = "../data/raw/analytic_data2020_0.csv"
healthrank_raw = read_csv(file, skip=1)
healthrank_raw= healthrank_raw %>% select(fipscode,v142_rawvalue, 
                          v024_rawvalue, v154_rawvalue, v136_rawvalue, v002_rawvalue) %>%
  rename(res_seg_nonwhite_white = v142_rawvalue, pct_child_in_pov = v024_rawvalue, sev_hou_cost_burden = v154_rawvalue, sev_hou_prob = v136_rawvalue, poor_fair_health = v002_rawvalue , fips =fipscode)
```

```{r}
healthrank_raw$fips = as.numeric(healthrank_raw$fips)
```

```{r}
merge5 = merge4 %>% inner_join(healthrank_raw, by = "fips")
```


```{}

stopwords2 = c(" County")

merge5$county = removeWords(merge5$county, stopwords2)
```



## state expenses

```{r}
state_expenses = read_xlsx("../data/raw/expenditures.xlsx") %>% rename(state = State, spend_per_capita = `State and Local Direct General Expenditures, Per Capita`)
```

```{r}
merge6 = merge5 %>% inner_join(state_expenses, by = "state")
```


## state unemployment benefits

```{r}
unemp_bens = read_xlsx("../data/raw/unemployment bens.xlsx")

stopwords3 = c("Individual", "w/dependents", "up", "to")

unemp_bens$amount = removeWords(unemp_bens$amount, stopwords3)

unemp_bens = unemp_bens %>% separate(amount, into = c("individual", "w_dependents"), sep = "  ") %>% mutate(individual = extract_numeric(individual), w_dependents = extract_numeric(w_dependents))

unemp_bens$w_dependents = ifelse(is.na(unemp_bens$w_dependents), unemp_bens$individual, unemp_bens$w_dependents) 

unemp_bens = unemp_bens %>% mutate(average_bens = (as.numeric(individual)+as.numeric(w_dependents))/2) %>% mutate(unemp_bens_possible = average_bens*weeks) %>% select(state, unemp_bens_possible)
```

```{r}
merge7 = merge6 %>% inner_join(unemp_bens, by = "state") 
```


#IRS-retirement saving

```{r}
census_data_zip <- get_acs(geography = "zcta", variables = c(totalpop = "B01003_001"), 
                       year = 2018)



census_step_zip = census_data_zip %>% 
  pivot_wider(id_cols = c(NAME), names_from = variable, values_from = estimate) %>% 
  separate(NAME, into = c("zcta", "zip"), sep = " ") %>% select(-zcta) %>% mutate(zip = as.numeric(zip))

```

```{r}
file = "../data/raw/irstiny.csv"
irs_raw = read_csv(file)

irs_raw = irs_raw%>%select(zipcode, N07240)%>% mutate(zip = as.numeric(zipcode),saverscredit = N07240)%>%select(-zipcode,-N07240) %>% group_by(zip) %>% summarise(saverscredit = sum(saverscredit))%>%ungroup()
irs_clean = irs_raw%>% inner_join(census_step_zip, by="zip") %>% mutate(saversperpop=saverscredit/totalpop)

```

```{r}
file = "../data/raw/ZIP-COUNTY-FIPS_2017-06.csv"
key_zip_county = read_csv(file)

key_zip_county = key_zip_county %>% rename(zip=ZIP, county=COUNTYNAME, fips=STCOUNTYFP)%>% mutate(zip=as.numeric(zip)) %>% select(zip, county,fips)

irs_joined = key_zip_county%>%inner_join(irs_clean, by="zip")

irs_final = irs_joined%>%group_by(fips)%>% summarise(saversperpop = mean(saversperpop)) %>% ungroup() %>% mutate(fips=as.numeric(fips))
```

```{r}
merge8=merge7 %>%inner_join(irs_final,by="fips") %>% drop_na()
```



```{r}
file = "../data/raw/People.csv"
atlas_people_raw = read_csv(file)
atlas_people_raw = atlas_people_raw %>% select(FIPS, ForeignBornEuropePct,ForeignBornMexPct,ForeignBornCaribPct, ForeignBornCentralSouthAmPct, ForeignBornAsiaPct, ForeignBornAfricaPct, AvgHHSize, PopChangeRate1819, NonEnglishHHNum) %>% rename(fips = FIPS) %>% mutate(fips = as.numeric(fips))

```




```{r}
merge9=merge8 %>%inner_join(atlas_people_raw,by="fips") %>% drop_na()
```




```{r}
file = "../data/raw/Jobs.csv"
atlas_jobs_raw = read_csv(file)

atlas_jobs_raw = atlas_jobs_raw %>% select(FIPS, PctEmpChange1920, PctEmpConstruction, PctEmpMining, PctEmpTrade, PctEmpTrade, PctEmpInformation, PctEmpFIRE) %>% rename(fips = FIPS)  %>% mutate(fips = as.numeric(fips))
```


```{r}
merge10=merge9 %>%inner_join(atlas_jobs_raw,by="fips") %>% drop_na()
```


```{r}
file = "../data/raw/Income.csv"
atlas_income_raw = read_csv(file)
atlas_income_raw = atlas_income_raw %>% select(FIPS, Deep_Pov_Children, PerCapitaInc, Deep_Pov_All)  %>% rename(fips = FIPS)  %>% mutate(fips = as.numeric(fips))

```


```{r}
merge11=merge10 %>%inner_join(atlas_income_raw,by="fips") %>% drop_na()
```



```{r}
write.csv(merge11, file = '../data/clean/merge11.csv', row.names = FALSE)
```


