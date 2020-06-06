#### libraries ####
library(shiny)
library(shinythemes)
library(dplyr)
library(googleVis)

#### data set ####
athlete_events.df <- read.csv("./dataset/athlete_events.csv")
noc_regions.df <- read.csv("./dataset/noc_regions.csv")

#### treated data set ####
#(plot1) medals count by country by year
plot1 <-
  inner_join(athlete_events.df, noc_regions.df, by = "NOC") %>% select(region, Event, Medal, Year) %>% 
  filter(!is.na(Medal)) %>% group_by(region, Event, Year) %>% summarise() %>% ungroup() %>% 
  group_by(region, Year) %>% summarise(medalCount = n())

#(plot2)






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
of medals in a handful number of countries. They are: Russia, USA, Great Britan, Germany and China"

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


