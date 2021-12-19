## ----setup, include=FALSE------------------------------------------------------------------------------------------------------------------
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
library(kableExtra)
library(corrplot)
library(cowplot)  


set.seed(471)
## ------------------------------------------------------------------------------------------------------------------------------------------
theft_alldata=read_csv("../data/clean/dataclean.csv")


## ------------------------------------------------------------------------------------------------------------------------------------------
theft_train=read_csv("../data/clean/theft_train.csv")


## ------------------------------------------------------------------------------------------------------------------------------------------
h = theft_train %>% ggplot(aes(x = theftrate)) +
  geom_histogram()+
  labs(y = "Count", 
       x = "Theft Rate",
       title = "Histogram of Theft Rate")+
  geom_vline(aes(xintercept = mean(theftrate),colour = "mean"),
             linetype ="longdash", size = .8)+
  geom_vline(aes(xintercept = median(theftrate),colour = "median"),
             linetype ="longdash", size = .8)+
  theme_bw()+
  scale_y_log10()+
  scale_color_manual(name = "statistics", values = c(median = "blue", mean = "red"))+
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "../results/histogram_of_response.png", 
       plot = h, 
       device = "png", 
       width = 5, 
       height = 3)

## ------------------------------------------------------------------------------------------------------------------------------------------
mean(theft_train$theftrate)
median(theft_train$theftrate)


## ------------------------------------------------------------------------------------------------------------------------------------------
theft_train %>% select(state,county,theftrate) %>% arrange(desc(theftrate)) %>% head(10) %>% 
  write_tsv("../results/top-10-counties-data.tsv")


## ------------------------------------------------------------------------------------------------------------------------------------------
map = map_data("county") %>%
  as_tibble() %>% 
  left_join(theft_train %>% 
              rename(region = state, 
                     subregion = county,
                     `Theft Rate` = theftrate) %>% 
              mutate(region = str_to_lower(region), 
                     subregion = str_to_lower(subregion)), 
            by = c("region", "subregion")) %>%
  ggplot() + 
  geom_polygon(data=map_data("state"), 
               aes(x=long, y=lat, group=group),
               color="black", fill=NA,  size = 1, alpha = .3) + 
  geom_polygon(aes(x=long, y=lat, group=group, fill = `Theft Rate`),
               color="darkblue", size = .1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_void()

ggsave(filename = "../results/response-map.png", 
       plot = map, 
       device = "png", 
       width = 7, 
       height = 4)

## ------------------------------------------------------------------------------------------------------------------------------------------


theft_train_corrAll = theft_train%>% select(-fips, -state, -county,-theftrate)
M = cor(theft_train_corrAll)

pdf(file = "../results/all_correlations.pdf")
allcorrs = corrplot(M, type = 'lower', order = 'hclust', tl.col = 'black',
         cl.ratio = 0.2, tl.srt = 45, col = COL2('PuOr', 10), tl.cex = 0.35)
dev.off()

## ------------------------------------------------------------------------------------------------------------------------------------------
cluster_safetynet = theft_train%>% select(-fips, -state, -county) %>% select(unemp_bens_possible, spend_per_capita, saversperhouses, no_health_ins, foodstamp)

cluster_criminaljustice = theft_train%>% select(-fips, -state, -county) %>% select(incar_rate, police_violence_score, police_accountability_score, approach_to_policing_score, police_funding_score)

cluster_health = theft_train%>% select(-fips, -state, -county) %>% select(mean_hha_score, poor_fair_health, dis5to17, dis18to34, dis35to64)

cluster_ses = theft_train%>% select(-fips, -state, -county) %>% select(lessthan_hs, bachplus, unemployed_rate, employed_rate, med_2bed, gini, svi_overall, pct_all_in_pov, pct_child_in_pov, med_income, sev_hou_cost_burden, sev_hou_prob, PctEmpChange1920, PctEmpConstruction, PctEmpMining, PctEmpTrade, PctEmpTrans, PctEmpInformation, PctEmpFIRE, PerCapitaInc, Deep_Pov_All, Deep_Pov_Children, inschool, ingradprofesh, inundergrad)

cluster_demo= theft_train%>% select(-fips, -state, -county) %>% select(med_age,permale,divorced,widowed,nevermarried,pertrump,pop_density,housing_density,res_seg_nonwhite_white,Marriedcouplefamily,ForeignBornEuropePct,ForeignBornMexPct,ForeignBornCaribPct,ForeignBornCentralSouthAmPct,ForeignBornAsiaPct,ForeignBornAfricaPct,NonEnglishHHNum,AvgHHSize,PopChangeRate1819,withkids,single_mom,singledad,foreignborn,fromdifstate,fromabroad)


## ------------------------------------------------------------------------------------------------------------------------------------------
M_safetynet = cor(cluster_safetynet)
M_criminaljustice = cor(cluster_criminaljustice)
M_health = cor(cluster_health)
M_ses = cor(cluster_ses)
M_demo = cor(cluster_demo)



## ------------------------------------------------------------------------------------------------------------------------------------------
corrplot(M_safetynet, type = 'lower', order = 'hclust', tl.col = 'black',
         cl.ratio = 0.2, tl.srt = 45, col = COL2('PuOr', 10), tl.cex = 1)


## ------------------------------------------------------------------------------------------------------------------------------------------
corrplot(M_criminaljustice, type = 'lower', order = 'hclust', tl.col = 'black',
         cl.ratio = 0.2, tl.srt = 45, col = COL2('PuOr', 10), tl.cex = 0.8)


## ------------------------------------------------------------------------------------------------------------------------------------------
corrplot(M_health, type = 'lower', order = 'hclust', tl.col = 'black',
         cl.ratio = 0.2, tl.srt = 45, col = COL2('PuOr', 10), tl.cex = 0.8)


## ------------------------------------------------------------------------------------------------------------------------------------------
corrplot(M_ses, type = 'lower', order = 'hclust', tl.col = 'black',
         cl.ratio = 0.2, tl.srt = 45, col = COL2('PuOr', 10), tl.cex = 0.6)


## ------------------------------------------------------------------------------------------------------------------------------------------
corrplot(M_demo, type = 'lower', order = 'hclust', tl.col = 'black',
         cl.ratio = 0.2, tl.srt = 45, col = COL2('PuOr', 10), tl.cex = 0.5)


## ------------------------------------------------------------------------------------------------------------------------------------------
# plot theftrate against housing_density
p1 = theft_train %>% select(-fips, -state, -county)%>%
  ggplot(aes(x = housing_density, y = theftrate)) +
  geom_point(alpha = 0.6) + 
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm", formula = "y~x", se = FALSE) +
  labs(x = "Housing Density", 
       y = "Theft Rate",
       title = "Housing Density vs Theft Rate") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5,size=10,face="bold"))+ 
  theme(axis.title = element_text(size = 10))

p1


## ------------------------------------------------------------------------------------------------------------------------------------------
# plot theftrate against no_health_ins
p2 = theft_train %>% select(-fips, -state, -county) %>%
  ggplot(aes(x = no_health_ins, y = theftrate)) +
  geom_point(alpha = 0.6) + 
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm", formula = "y~x", se = FALSE) +
  labs(x = "No Health Insurance", 
       y = "Theft Rate",
       title = "No Health Insurance vs Theft Rate") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5,size=10,face="bold"))+ 
  theme(axis.title = element_text(size = 10))+
  theme(axis.title.y = element_blank())

p2


## ------------------------------------------------------------------------------------------------------------------------------------------
# plot theftrate against pertrump
p3 = theft_train %>% select(-fips, -state, -county)%>%
  ggplot(aes(x = pertrump, y = theftrate)) +
  geom_point(alpha = 0.6) + 
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm", formula = "y~x", se = FALSE) +
  labs(x = "Percent of Trump Supporters", 
       y = "Theft Rate",
       title = "Percent of Trump Supporters\n (in 2020) vs Theft Rate") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5,size=10,face="bold"))+ 
  theme(axis.title = element_text(size = 10))

p3


## ------------------------------------------------------------------------------------------------------------------------------------------
# plot theftrate against PctEmpFIRE
p4 = theft_train %>% select(-fips, -state, -county)%>%
  ggplot(aes(x = PctEmpFIRE, y = theftrate)) +
  geom_point(alpha = 0.6) + 
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm", formula = "y~x", se = FALSE) +
  labs(x = "Percent of People Employed in FIRE", 
       y = "Theft Rate",
       title = "Percent of People Employed in\n Finance/Insurance/Real Estate(FIRE)\n vs Theft Rate") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5,size=10,face="bold"))+ 
  theme(axis.title = element_text(size = 10))+
  theme(axis.title.y = element_blank())

p4


## ------------------------------------------------------------------------------------------------------------------------------------------
# plot theftrate against unemp_bens_possible
p5 = theft_train %>% select(-fips, -state, -county)%>%
  ggplot(aes(x = unemp_bens_possible, y = theftrate)) +
  geom_point(alpha = 0.6) + 
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm", formula = "y~x", se = FALSE) +
  labs(x = "State unemployment insurance", 
       y = "Theft Rate",
       title = "State Unemployment Insurance\n vs Theft Rate") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5,size=10,face="bold"))+ 
  theme(axis.title = element_text(size = 10))

p5


## ------------------------------------------------------------------------------------------------------------------------------------------
# plot theftrate against police_funding_score
p6= theft_train %>% select(-fips, -state, -county)%>%
  ggplot(aes(x = police_funding_score, y = theftrate)) +
  geom_point(alpha = 0.6) + 
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm", formula = "y~x", se = FALSE) +
  labs(x = "Police Funding Score", 
       y = "Theft Rate",
       title = "Police Funding Score vs Theft Rate") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5,size=10,face="bold"))+ 
  theme(axis.title = element_text(size = 10))+
  theme(axis.title.y = element_blank())

p6


## ------------------------------------------------------------------------------------------------------------------------------------------
imp_vars_vs_theft = plot_grid(p1, p2,p3, p4,p5, p6, nrow = 3)

ggsave(filename = "../results/imp_vars_vs_theft.png", 
       plot = imp_vars_vs_theft, 
       device = "png", 
       width = 7, 
       height = 4)