#### libraries ####
library(shiny)
library(googleVis)
library(data.table)
library(tidyverse)
library(ggplot2)

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
gini.df <- read.csv("./dataset/GINI.csv", skip=3)
lifeExpectancy.df <- read.csv("./dataset/life_expectancy.csv", skip=3)

#### treat data set ####
#(plot1_1) medals by country world map
plot1_1 <- athlete_regions.df %>% dplyr::group_by(region, Year, Event, Medal) %>% summarise("Medal Count" = n()) %>% 
  mutate(MedalCount = ifelse(Medal %in% c("Bronze", "Silver", "Gold"), 1, 0)) %>% select(-Medal) %>% 
  ungroup() %>% group_by(region) %>% summarise("Total Medals" = sum(MedalCount)) %>% filter(!is.na(region)) %>% 
  mutate(region = ifelse(region == "USA", "United States", region))

#---------------------------------------------------------------------------------------------------------#

#(plot1_2) number of medals vs GDP
medalsCountries <- athlete_events.df %>% filter(!is.na(Medal)) %>% group_by(NOC, Year) %>% 
  distinct(., Event, .keep_all=TRUE) %>% summarise(Medals=n())

gdpCountries <- gdp.df %>% select(Country.Name, NOC=Country.Code, X2016)

plot1_2 <- inner_join(medalsCountries, gdpCountries, by ="NOC") %>% filter(Year == 2016) %>% ungroup() %>% 
  mutate(GDPbillions = X2016/1e9) %>% select(Country.Name, Medals, GDPbillions, GDP=X2016)

#---------------------------------------------------------------------------------------------------------#

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

#---------------------------------------------------------------------------------------------------------#

#(plot2_1) number of women athletes in the olympics every year
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
#plot and save graph
ggplot(plot2_1 , aes(x=Year, y=`Number of athletes`)) + 
  geom_line(aes(col=Sex)) +
  theme(panel.background = element_rect(fill = "#f8f9fa"),
        plot.background = element_rect(fil = "#f8f9fa"))
ggsave("./www/plots/plot2_1.png")

#---------------------------------------------------------------------------------------------------------#

#(plot2_2) gii world graph
plot2_2 <- gii.df %>% select(2, GII=X2018)
#fix a lot of name typos
plot2_2$region[plot2_2$region == "Korea (Republic of)"] <- "South Korea"
plot2_2$region[plot2_2$region == "Bolivia (Plurinational State of)"] <- "Bolivia"
plot2_2$region[plot2_2$region == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
plot2_2$region[plot2_2$region == "Russian Federation"] <- "Russia"
plot2_2$region[plot2_2$region == "Viet nam"] <- "Vietnam"
#filter for values that are not numbers
plot2_2 <- plot2_2 %>% filter(GII != "..", GII != "")

#---------------------------------------------------------------------------------------------------------#

#(plot2_3) compare gender equality index with ratio of women athletes
plot2_3 <- athlete_regions.df %>% mutate(Year = ifelse(Year == 1994, 1996, 
                                                ifelse(Year == 1998, 2000, 
                                                ifelse(Year == 2002, 2004,
                                                ifelse(Year == 2006, 2008,
                                                ifelse(Year == 2010, 2012,
                                                ifelse(Year == 2014, 2016, Year))))))) %>% 
  group_by(Year, region, Sex) %>% summarise("Number Athletes" = length(unique(ID)))
#calculate the ratio and set-up factor levels for ggplot
plot2_3 <-
  dcast(setDT(plot2_3),
        Year + region ~ Sex,
        fun.aggregate = sum,
        value.var = "Number Athletes") %>%
  mutate(ratio = F / (F + M)) %>% filter(Year == 2016) %>% select(region, ratio)
level2_3 <- plot2_3 %>% arrange(ratio) %>% select(region)
plot2_3$region <-
  factor(plot2_3$region, levels = c(level2_3$region))
#filter for unwanted values in gii column
gii2016 <-
  gii.df %>% filter(X2016 != "..", X2016 != "") %>% select(region, X2016)
gii2016$X2016 <- as.numeric(gii2016$X2016)
#join ratio and gii tables
plot2_3 <-
  inner_join(plot2_3, gii2016 %>% select(region, GII = X2016), by = "region")
plot2_3 <- plot2_3 %>% select(ratio, GII)
#plot and save graph
ggplot(plot2_3, aes(x = ratio, y = GII)) +
  geom_point(
    shape = 23,
    fill = "#DF0024",
    color = "#DF0024",
    size = 3
  ) +
  theme(
    panel.background = element_rect(fill = "#f8f9fa"),
    plot.background = element_rect(fil = "#f8f9fa")
  )
ggsave("./www/plots/plot2_3.png")

#---------------------------------------------------------------------------------------------------------#

#(plot3_1) change in height of olympic athletes across the years
#filter NA values for the plot
plot3_1 <- athlete_events.df %>% mutate(Year = ifelse(Year == 1994, 1996, 
                                               ifelse(Year == 1998, 2000, 
                                               ifelse(Year == 2002, 2004,
                                               ifelse(Year == 2006, 2008,
                                               ifelse(Year == 2010, 2012,
                                               ifelse(Year == 2014, 2016, Year))))))) %>% filter(!is.na(Height), Year >= 1936)
#plot and save graph
ggplot(plot3_1, aes(x = as.factor(Year), y = Height, fill = Sex)) +
  geom_boxplot() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fil = "white")
  ) +
  labs(x = "Year", y = "Height (meters)", title = "Median height each Olympic event")
ggsave("./www/plots/plot3_1.png")

#---------------------------------------------------------------------------------------------------------#

#(plot3_2) change in weight of olympic athletes across the years
#filter NA values for the plot
plot3_2 <- athlete_events.df %>% mutate(Year = ifelse(Year == 1994, 1996, 
                                               ifelse(Year == 1998, 2000, 
                                               ifelse(Year == 2002, 2004,
                                               ifelse(Year == 2006, 2008,
                                               ifelse(Year == 2010, 2012,
                                               ifelse(Year == 2014, 2016, Year))))))) %>% filter(!is.na(Weight), Year >= 1936)
#plot and save graph
ggplot(plot3_2, aes(x = as.factor(Year), y = Weight, fill = Sex)) +
  geom_boxplot() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fil = "white")
  ) +
  labs(x = "Year", y = "Weight (kg)", title = "Median weight each Olympic event")
ggsave("./www/plots/plot3_2.png")

#---------------------------------------------------------------------------------------------------------#

#(plot3_3) scatterplot of heightxweight for men in athletics
scatterHxW <-
  athlete_events.df %>% filter(!is.na(Weight)) %>% filter(!is.na(Height)) %>% filter(Year %in% c(1968, 2016))
#get rid of duplicates athletes using distinct
scatterHxW <- distinct(scatterHxW, ID, .keep_all = TRUE)
#to get a better plot visualization, change the column Year to class character
scatterHxW$Year <- as.character(scatterHxW$Year)
#finally, filter to men only
plot3_3 <- scatterHxW %>% filter(Sex == "M")
#plot and save graph
ggplot(plot3_3 %>% filter(Sport == "Athletics"),
       aes(x = Weight, y = Height)) +
  geom_point(aes(col = Year)) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fil = "white")
  ) +
  labs(x = "Weight (kg)", y = "Height (m)", title = "Men Weight x Height in Athletics")
ggsave("./www/plots/plot3_3.png")

#---------------------------------------------------------------------------------------------------------#

#(plot3_4) scatterplot of heightxweight for women in athletics
#we can use most of the work done for plat3_3 and just filter for women
plot3_4 <- scatterHxW %>% filter(Sex == "F")
ggplot(plot3_4 %>% filter(Sport == "Athletics"),
       aes(x = Weight, y = Height)) +
  geom_point(aes(col = Year)) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fil = "white")
  ) +
  labs(x = "Weight (kg)", y = "Height (m)", title = "Women Weight x Height in Athletics")
ggsave("./www/plots/plot3_4.png")

#---------------------------------------------------------------------------------------------------------#

#(plot3_5) age of athletes
plot3_5 <- athlete_regions.df %>% mutate(Year = ifelse(Year == 1994, 1996, 
                                                ifelse(Year == 1998, 2000, 
                                                ifelse(Year == 2002, 2004,
                                                ifelse(Year == 2006, 2008,
                                                ifelse(Year == 2010, 2012,
                                                ifelse(Year == 2014, 2016, Year))))))) %>% 
  filter(!is.na(Age)) %>% group_by(Sex, Year) %>% summarise("Average Age" = mean(Age)) %>%
  rename(., Category = Sex)
#get average life expectancy from each year
lifeExpectancyWorldAvg <-
  data.frame(as.list(colMeans(lifeExpectancy.df[, 5:65], na.rm = TRUE))) %>%
  tidyr::gather(., "Year", "lifeExpAvg") %>% mutate(Year = as.numeric(gsub('X', '', Year))) %>%
  mutate(Category = c("World")) %>% rename(., "Average Age" = lifeExpAvg) %>% filter(Year <= 2018)
#plot and save graph
ggplot(plot3_5, aes(x = Year)) +
  geom_line(aes(y = `Average Age`, col = Category)) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fil = "white")
  )
ggsave("./www/plots/plot3_5.png")

#---------------------------------------------------------------------------------------------------------#

#(plot4_1) how many times each country has hosted
summerOlympics <- athlete_events.df %>% distinct(., Games, .keep_all=TRUE) %>% select(Year, City, Season) %>% 
  filter(Season == "Summer") %>% group_by(Year) %>% mutate(Count = 1)

#had to create a relational table city-country because the dataset did not provide
citiesSummer <- (summerOlympics %>% arrange(Year))$City
countriesSummer <- c("Greece","France","United States","Greece","United Kingdom","Sweden","Belgium","France",
               "Netherlands","United States","Germany","United Kingdom","Finland","Australia","Italy",
               "Japan","Mexico","Germany","Canada","Russia","United States","South Korea","Spain",
               "United States","Australia","Greece","China","United Kingdom","Brazil")

summer <- data.frame(citiesSummer, countriesSummer) %>% group_by(countriesSummer) %>% 
  summarise(Count=n()) %>% rename(., Country = countriesSummer)


winterOlympics <- athlete_events.df %>% distinct(., Games, .keep_all=TRUE) %>% select(Year, City, Season) %>% 
  filter(Season == "Winter") %>% group_by(Year) %>% mutate(Count = 1)

#had to create a relational table city-country because the dataset did not provide
citiesWinter <- (winterOlympics %>% arrange(Year))$City
countriesWinter <- c("France","Switzerland","United States","Germany","Switzerland","Norway","Italy",
                     "United States","Austria","France","Japan","Austria","United States","Yugoslavia",
                     "Canada","France","Norway","Japan","United States","Italy","Canada","Russia")

winter <- data.frame(citiesWinter, countriesWinter) %>% group_by(countriesWinter) %>% 
  summarise(Count=n()) %>% rename(., Country = countriesWinter)

#---------------------------------------------------------------------------------------------------------#

#(table4_2) medals host country summer olympics
medalsCountries <-
  medalsCountries %>% inner_join(., noc_regions.df, by = "NOC") %>% select(Year, Medals, Country = region) %>%
  mutate(Country = ifelse(Country == "USA", "United States", Country)) %>%
  mutate(Country = ifelse(Country == "UK", "United Kingdom", Country))
medalsCountries <-
  medalsCountries %>% mutate("code" = paste0(Year, Country))

summerHost <-
  strsplit(
    "Greece,France,United States,United Kingdom,Sweden,Cancelled,Belgium,France,Netherlands,United States,Germany,Cancelled,Cancelled,United Kingdom,Finland,Australia,Italy,Japan,Mexico,Germany,Canada,Russia,United States,South Korea,Spain,United States,Australia,Greece,China,United Kingdom,Brazil",
    split = ','
  )
summerOlympicsHosts <-
  data.frame(Year = seq(1896, 2016, 4), summerHost)
summerOlympicsHosts <-
  rename(summerOlympicsHosts, "Country" = colnames(summerOlympicsHosts)[2])
summerOlympicsHosts <-
  summerOlympicsHosts %>% mutate("code" = paste0(Year, Country))
#get medals for each hosting country
table4_2 <-
  right_join(medalsCountries, summerOlympicsHosts, by = "code") %>% ungroup() %>% select(Year =
                                                                                           Year.x, Country = Country.x, Medals)
#now get medals from the previous year for comparison
table4_2 <-
  table4_2 %>% mutate("code" = paste0(Year - 4, Country)) %>% filter(!is.na(Country))
table4_2 <-
  inner_join(table4_2, medalsCountries, by = "code") %>% ungroup() %>% mutate("Change" =
                                                                                Medals.x - Medals.y) %>%
  select(
    Country = Country.x,
    Year = Year.x,
    "Previous Games" = Medals.y,
    "Host Year" = Medals.x,
    Change
  )

