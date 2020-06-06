#### libraries ####
library(shiny)
library(shinythemes)
library(dplyr)
library(googleVis)
library(data.table)
library(tidyverse)

#### data set ####
athlete_events.df <- read.csv("./dataset/athlete_events.csv")
noc_regions.df <- read.csv("./dataset/noc_regions.csv") %>% 
  mutate(region = ifelse(region == "Boliva", "Bolivia", region)) #fix typos
athlete_regions.df <- inner_join(athlete_events.df, noc_regions.df, by = "NOC")

#### treated data set ####
#(plot1) medals by country world map
plot1 <- athlete_regions.df %>% group_by(region, Year, Event, Medal) %>% summarise("Medal Count" = n()) %>% 
  mutate(MedalCount = ifelse(Medal %in% c("Bronze", "Silver", "Gold"), 1, 0)) %>% select(-Medal) %>% 
  ungroup() %>% group_by(region) %>% summarise("Total Medals" = sum(MedalCount)) %>% filter(!is.na(region)) %>% 
  mutate(region = ifelse(region == "USA", "United States", region))

#(plot2) ranking of countries by medal count with user input
#done in server.R

#(plot3) rate of women athletes in the olympics every year
# match the year of winter games with summer games to have a better graph
plot3 <- athlete_regions.df %>% mutate(Year = ifelse(Year == 1994, 1996, 
                                              ifelse(Year == 1998, 2000, 
                                              ifelse(Year == 2002, 2004,
                                              ifelse(Year == 2006, 2008,
                                              ifelse(Year == 2010, 2012,
                                              ifelse(Year == 2014, 2016, Year))))))) %>% 
  group_by(Sex, Year) %>% summarise("Number of athletes" = length(unique(ID))) %>% 
  mutate(Female = ifelse(Sex == "F", `Number of athletes`, NA)) %>% 
  mutate(Male = ifelse(Sex == "M", `Number of athletes`, NA))

#(plot4) ratio of women for each country in 1900
plot4 <- athlete_regions.df %>% mutate(Year = ifelse(Year == 1994, 1996, 
                                              ifelse(Year == 1998, 2000, 
                                              ifelse(Year == 2002, 2004,
                                              ifelse(Year == 2006, 2008,
                                              ifelse(Year == 2010, 2012,
                                              ifelse(Year == 2014, 2016, Year))))))) %>% 
 group_by(Year, region, Sex) %>% summarise("No Athletes" = length(unique(ID)))
  
plot4_1 <- dcast(setDT(plot4), Year + region ~ Sex, fun.aggregate = sum, value.var = "No Athletes") %>% 
  mutate(ratio = F/(F+M)) %>% filter(Year == 1900)

level4_1 <- plot4_1 %>% arrange(ratio) %>% select(region)

plot4_1$region <- factor(plot4_1$region, levels = c(level4_1$region))


#(plot5) ratio of women for each country in 2016
plot5 <- dcast(setDT(plot4), Year + region ~ Sex, fun.aggregate = sum, value.var = "No Athletes") %>% 
  mutate(ratio = F/(F+M)) %>% filter(Year == 2016)

level5_1 <- plot5 %>% arrange(ratio) %>% select(region)

plot5$region <- factor(plot5$region, levels = c(level5_1$region))


#### texts ####
introTitle1 = "The Olympics"
introParagraph1 = "The Olympics games is an international sporting event featuring summer and winter sports 
competitions in which thousands of athletes around the world compete in various competitions.
The Olympic Games are considered the world's foremost sports competition with more than 200 
nations participating. The Olympic Games are held every four years, alternating between the Summer and Winter
Games every two years in the four-year period."
introParagraph2 = "Their creation was inspired by the ancient Olympic Games 
(Ancient Greek: Ὀλυμπιακοί Ἀγῶνες), which were held in Olympia, Greece, from the 8th century BC to
the 4th century AD. Baron Pierre de Coubertin founded the International Olympic Committee (IOC) in 1894, 
leading to the first modern Games in Athens in 1896."
introParagraph3 = "In midst of the global pandemic of Coronavirus, for the first time in its history, the 
Olympic Games Tokyo 2020 are posponed to the next year. Many and most of the athletes who secured a spot
in Tokyo 2020 will now have to wait with an uncertainty of the games next year. The impacts of the postponement 
of the Olympic Games are really hard to measure especially
for the athletes who train for most of their lives to this event."
introParagraph4 = "Besides hard work and talent, how much does it really take to become an Olympian?
What comes into factor that separates an Olympian athlete to a non-Olympic level athlete?
Does the background have a direct impact on a person aiming to compete at the Olympics?
Let's try to find out."

topic1Title = "Performance of each country in the Olympics"
topic1Paragraph1 = "Ever since the start of the Olympics, it was possible to note a dominance in the number
of medals in a handful number of countries."

topic2Title = "Women in the Olympics"
topic2Paragraph1 = "The first participation of women in the Olympics was in 1900. Women participated in the 
Games in Paris, France. Twenty-two women (2.2 per cent) out of a total of 997 athletes competed in five sports:
tennis, sailing, croquet, equestrian and golf.
The number of female athletes has been increasing since then."

topic3Title = "Physical characteristics of Olympic athletes"
topic3Paragraph1 = "How tall or fit is an Olympian? Of course it may depend on the type of sport but let's 
try to find a trend"

topic4Title = "The Outliers"
topic4Paragraph1 = "Securing a spot to compete in the Olympics is a big deal itself. Winning an Olympic medal
is another huge deal itself. Now imagine being a multiple times Olympic medalist? Not people in the history 
of mankind were able to do that."
topic4Paragraph2 = "What we want to analize here is what differs outliers from other athletes. The physical 
characteristics take a role in this? From which country these people come from? Is it more appearant in a 
specific sport?"

topic5Title = "Influence of a country's economy in Olympics"
topic5Paragraph1 = "Now that we looked into the athletes themselves, we could examine why and how some countries 
produce more champions and others. Do more developed countries have an advantage over undeveloped countries? 
Do some countries do better in Winter (or Summer) Olympics than others? What index is a good measure of an 
Olympian country? Could we predict the next underdog champion?"



#### references ####
"https://en.wikipedia.org/wiki/Participation_of_women_in_the_Olympics"
"https://www.olympic.org/women-in-sport/background/key-dates"
"https://www.nytimes.com/2018/02/28/well/move/do-you-have-what-it-takes-to-be-an-olympian.html#:~:text=Becoming%20the%20most%20decorated%20Winter,surprisingly%20light%20intensity%20%E2%80%94%20and%20a"


