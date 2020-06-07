#### libraries ####
library(shiny)
library(shinythemes)
#library(dplyr)
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


#### data set cleaner ####
#regionName <- function(df) {
#  df$region <- as.character(df$region)
#  df$region[df$region == ""] <- ""
#}



#### treated data set ####
#(plot1_1) medals by country world map
plot1_1 <- athlete_regions.df %>% dplyr::group_by(region, Year, Event, Medal) %>% summarise("Medal Count" = n()) %>% 
  mutate(MedalCount = ifelse(Medal %in% c("Bronze", "Silver", "Gold"), 1, 0)) %>% select(-Medal) %>% 
  ungroup() %>% group_by(region) %>% summarise("Total Medals" = sum(MedalCount)) %>% filter(!is.na(region)) %>% 
  mutate(region = ifelse(region == "USA", "United States", region))

#(plot1_2) ranking of countries by medal count with user input
#done in server.R

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

#(plot2_2) ratio of women for each country in 1900
plot2_2 <- athlete_regions.df %>% mutate(Year = ifelse(Year == 1994, 1996, 
                                              ifelse(Year == 1998, 2000, 
                                              ifelse(Year == 2002, 2004,
                                              ifelse(Year == 2006, 2008,
                                              ifelse(Year == 2010, 2012,
                                              ifelse(Year == 2014, 2016, Year))))))) %>% 
 group_by(Year, region, Sex) %>% summarise("No Athletes" = length(unique(ID)))
  
plot2_2_1 <- dcast(setDT(plot2_2), Year + region ~ Sex, fun.aggregate = sum, value.var = "No Athletes") %>% 
  mutate(ratio = F/(F+M)) %>% filter(Year == 1900)

level2_2_1 <- plot2_2_1 %>% arrange(ratio) %>% select(region)

plot2_2_1$region <- factor(plot2_2_1$region, levels = c(level2_2_1$region))

#(plot2_3) ratio of women for each country in 2016
plot2_3 <- dcast(setDT(plot2_2), Year + region ~ Sex, fun.aggregate = sum, value.var = "No Athletes") %>% 
  mutate(ratio = F/(F+M)) %>% filter(Year == 2016)

level2_3_1 <- plot2_3 %>% arrange(ratio) %>% select(region)

plot2_3$region <- factor(plot2_3$region, levels = c(level2_3_1$region))

#(plot2_4) GII index and women athletes comparison
# fix region names
plot2_3$region <- as.character(plot2_3$region)
plot2_3$region[plot2_3$region == "USA"] <- "United States"
plot2_3$region[plot2_3$region == "UK"] <- "United Kingdom"
gii.df$region <- as.character(gii.df$region)
gii.df$region[gii.df$region =="Korea (Republic of)"] <- "South Korea"
gii.df$region[gii.df$region =="Russian Federation"] <- "Russia"
gii.df$region[gii.df$region =="Viet Nam"] <- "Vietnam"
gii.df$region[gii.df$region =="Tanzania"] <- "Tanzania"
gii.df$region[gii.df$region =="Venezuela (Bolivarian Republic of)"] <- "Venezuela"
# join women athletes ratio and GII tables on region
plot2_4 <- full_join(
  plot2_3 %>% select(region, ratio) %>% filter(!is.na(region)), 
  gii.df %>% filter(X2016 != "..") %>% select(region, X2016), 
  by ="region")
# fix column so that googleVis can plot
plot2_4$X2016 <- as.numeric(plot2_4$X2016)
# final df to be plotted
plot2_4_1 <- plot2_4 %>% filter(!is.na(ratio)) %>% filter(!is.na(X2016))

plot2_4_1 <- plot2_4_1 %>% gather(ratio, X2016, key="type", value="value") %>% arrange(region)

#(plot3_1) change in height of olympic athletes across the years
#filter NA values for the plot
plot3_1 <- athlete_events.df %>% filter(!is.na(Height))

#(plot3_2) change in weight of olympic athletes across the years
#filter NA values for the plot
plot3_2 <- athlete_events.df %>% filter(!is.na(Weight))

#(plot3_3) scatterplot of heightxweight for men in athletics
scatterHxW <- athlete_events.df %>% filter(!is.na(Weight)) %>% filter(!is.na(Height)) %>% filter(Year %in% c(1968,2016))
#get rid of duplicates athletes using distinct
scatterHxW <- distinct(scatterHxW, ID, .keep_all = TRUE)
#to get a better plot visualization, change the column Year to class character
scatterHxW$Year <- as.character(scatterHxW$Year)
#finally, filter to men only
plot3_3 <- scatterHxW %>% filter(Sex == "M")

#(plot3_4) scatterplot of heightxweight for women in athletics
#we can use most of the work done for plat3_3 and just filter for women
plot3_4 <- scatterHxW %>% filter(Sex == "F")

#(plot3_3 & plot3_4) create a list of sports to be selected by the user
sportsFemale1968 <- scatterHxW %>% select(Sex, Year, Sport) %>% filter(Sex=="F") %>% filter(Year=="1968") %>% distinct(., Sport, .keep_all=TRUE)
sportsFemale2016 <- scatterHxW %>% select(Sex, Year, Sport) %>% filter(Sex=="F") %>% filter(Year=="2016") %>% distinct(., Sport, .keep_all=TRUE)
sportsMale1968 <- scatterHxW %>% select(Sex, Year, Sport) %>% filter(Sex=="M") %>% filter(Year=="1968") %>% distinct(., Sport, .keep_all=TRUE)
sportsMale2016 <- scatterHxW %>% select(Sex, Year, Sport) %>% filter(Sex=="M") %>% filter(Year=="2016") %>% distinct(., Sport, .keep_all=TRUE)

sportsFemale <- inner_join(sportsFemale1968, sportsFemale2016, by="Sport")
sportsMale <- inner_join(sportsMale1968, sportsMale2016, by="Sport")

listOfSports <- inner_join(sportsFemale, sportsMale, by = "Sport")$Sport

#(plot3_5) age of athletes
plot3_5 <- athlete_regions.df %>% mutate(Year = ifelse(Year == 1994, 1996, 
                                                ifelse(Year == 1998, 2000, 
                                                ifelse(Year == 2002, 2004,
                                                ifelse(Year == 2006, 2008,
                                                ifelse(Year == 2010, 2012,
                                                ifelse(Year == 2014, 2016, Year))))))) %>% 
  filter(!is.na(Age)) %>% group_by(Sex, Year) %>% summarise("Average Age" = mean(Age))

#(table4_1) list of athletes with most medals
table4_1 <- athlete_events.df %>% filter(!is.na(Medal)) %>% group_by(Name, Medal) %>%
  summarize(Medals=n()) %>% ungroup() %>% group_by(Name) %>% mutate(Total = sum(Medals)) 
#this is a hack I had to do to make an ordered list to level the factor later
table4_1Namelevels <- table4_1 %>% distinct(., Name, .keep_all=TRUE) %>% 
  arrange(Total) %>% select(Name) %>% tail(10)
table4_1Medallevels <- table4_1 %>% ungroup() %>% distinct(., Medal)
table4_1Medallevels[1,1] = "Bronze"
table4_1Medallevels[2,1] = "Silver"
table4_1Medallevels[3,1] = "Gold"
table4_1 <- inner_join(table4_1, table4_1Namelevels, by="Name") 
# factor columns so it shows in an ordered manner for the graph
table4_1$Name <- factor(table4_1$Name, levels=table4_1Namelevels$Name)
table4_1$Medal <- factor(table4_1$Medal, levels=table4_1Medallevels$Medal)




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
topics2Paragraph2 = "What are the characteristics of countries with the highest ratio of women representing 
the country? Can we relate them to any indexes? The GII is an inequality index. It measures gender 
inequalities in three important aspects of human development—reproductive health, measured by maternal 
mortality ratio and adolescent birth rates; empowerment, measured by proportion of parliamentary seats 
occupied by females and proportion of adult females and males aged 25 years and older with at least some 
secondary education; and economic status, expressed as labour market participation and measured by labour 
force participation rate of female and male populations aged 15 years and older."

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
topic4Paragraph3 = "So we see that the athletes with the most Olympic medals tend  to come from developed 
countries. Does it have a correlation? Let's find out."

topic5Title = "Influence of a country's economy in Olympics"
topic5Paragraph1 = "Now that we looked into the athletes themselves, we could examine why and how some countries 
produce more champions and others. Do more developed countries have an advantage over undeveloped countries? 
Do some countries do better in Winter (or Summer) Olympics than others? What index is a good measure of an 
Olympian country? Could we predict the next underdog champion?"



#### references ####
"https://en.wikipedia.org/wiki/Participation_of_women_in_the_Olympics"
"https://www.olympic.org/women-in-sport/background/key-dates"
"https://www.nytimes.com/2018/02/28/well/move/do-you-have-what-it-takes-to-be-an-olympian.html#:~:text=Becoming%20the%20most%20decorated%20Winter,surprisingly%20light%20intensity%20%E2%80%94%20and%20a"
"http://hdr.undp.org/en/data#"
"http://hdr.undp.org/sites/default/files/hdr2019_technical_notes.pdf"


