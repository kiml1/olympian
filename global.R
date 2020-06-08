#### libraries ####
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(googleVis)
library(data.table)
library(tidyverse)
library(ggplot2)
library(reactable)

#### data set ####
athlete_events.df <- read.csv("./dataset/athlete_events.csv")
noc_regions.df <- read.csv("./dataset/noc_regions.csv") %>% 
  mutate(region = ifelse(region == "Boliva", "Bolivia", region)) #fix typos
athlete_regions.df <- inner_join(athlete_events.df, noc_regions.df, by = "NOC")
gii.df <- read.csv("./dataset/GII.csv", skip=1) %>% .[, unlist(lapply(., function(x) !all(is.na(x))))] %>% 
  dplyr::rename(region = Country)
gdp.df <- read.csv("./dataset/GDP.csv", skip=3)
area.df <- read.csv("./dataset/country_area.csv", skip=3)
population.df <- read.csv("./dataset/population.csv", skip=3)
gdpCapita.df <- read.csv("./dataset/gdp_capita.csv", skip=3)
educationGDP.df <- read.csv("./dataset/educationGDP.csv", skip=3)
mortalityRate.df <- read.csv("./dataset/mortalityRate.csv", skip=3)
gini.df <- read.csv("./dataset/gini.csv", skip=3)

#### treated data set ####
#(plot1_1) medals by country world map
plot1_1 <- athlete_regions.df %>% dplyr::group_by(region, Year, Event, Medal) %>% summarise("Medal Count" = n()) %>% 
  mutate(MedalCount = ifelse(Medal %in% c("Bronze", "Silver", "Gold"), 1, 0)) %>% select(-Medal) %>% 
  ungroup() %>% group_by(region) %>% summarise("Total Medals" = sum(MedalCount)) %>% filter(!is.na(region)) %>% 
  mutate(region = ifelse(region == "USA", "United States", region))

#(plot1_2) number of medals vs GDP
medalsCountries <- athlete_events.df %>% filter(!is.na(Medal)) %>% group_by(NOC, Year) %>% 
  distinct(., Event, .keep_all=TRUE) %>% summarise(Medals=n())

gdpCountries <- gdp.df %>% select(Country.Name, NOC=Country.Code, X2016)

plot1_2 <- inner_join(medalsCountries, gdpCountries, by ="NOC") %>% filter(Year == 2016) %>% ungroup() %>% 
  mutate(GDPbillions = X2016/1e9) %>% select(Country.Name, Medals, GDPbillions, GDP=X2016)

#(plot1_3) number of medals vs indices
areaCountries <- area.df %>% select(Country.Name, NOC=Country.Code, X2016)
populationCountries <- population.df  %>% select(NOC=Country.Code, X2016)
gdpCapita <- gdpCapita.df  %>% select(NOC=Country.Code, X2016)
educationGDP <- educationGDP.df  %>% select(NOC=Country.Code, X2016)
mortalityRate <- mortalityRate.df  %>% select(NOC=Country.Code, X2016)
gini <- gini.df  %>% select(NOC=Country.Code, X2016)

plot1_3 <- inner_join(medalsCountries, areaCountries, by ="NOC") %>% 
  select(1,2,3,4,Area=X2016) %>% inner_join(., populationCountries, by ="NOC") %>% 
  select(1,2,3,Country.Name=4,5,Population=6) %>% inner_join(., gdpCapita, by ="NOC") %>% 
  select(1,2,3,Country.Name=4,5,6,"GDP per capita"=7) %>% inner_join(., educationGDP, by ="NOC") %>%
  select(1,2,3,Country.Name=4,5,6,7,"%GDP invested Education"=8) %>% inner_join(., mortalityRate, by ="NOC") %>%
  select(1,2,3,Country.Name=4,5,6,7,8,"Mortality Rate"=9) %>% inner_join(., gini, by ="NOC") %>%
  select(1,2,3,Country.Name=4,5,6,7,8,9,"GINI"=10) %>% 
  filter(Year == 2016)



#(plot2_1) rate of women athletes in the olympics every year
# match the year of winter games with summer games to have a better graph
plot2_1 <- athlete_regions.df %>% mutate(Year = ifelse(Year == 1994, 1996, 
                                                ifelse(Year == 1998, 2000, 
                                                ifelse(Year == 2002, 2004,
                                                ifelse(Year == 2006, 2008,
                                                ifelse(Year == 2010, 2012,
                                                ifelse(Year == 2014, 2016, Year))))))) %>% 
  group_by(Sex, Year) %>% summarise("Number of athletes" = length(unique(ID))) %>% 
  mutate(Female = ifelse(Sex == "F", `Number of athletes`, NA)) %>% 
  mutate(Male = ifelse(Sex == "M", `Number of athletes`, NA))





