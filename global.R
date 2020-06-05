#### libraries ####
library(shiny)
library(shinythemes)

#### data set ####
athlete_events.df <- read.csv("./dataset/athlete_events.csv")
noc_regions.df <- read.csv("./dataset/noc_regions.csv")


#### texts ####
introParagraph1 = "The Olympics games is an international sporting event featuring summer and winter sports 
competitions in which thousands of athletes around the world compete in various competitions.
The Olympic Games are considered the world's foremost sports competition with more than 200 
nations participating. The Olympic Games are held every four years, alternating between the Summer and Winter
Games every two years in the four-year period."

introParagraph2 = "Their creation was inspired by the ancient Olympic Games 
(Ancient Greek: Ὀλυμπιακοί Ἀγῶνες), which were held in Olympia, Greece, from the 8th century BC to
the 4th century AD. Baron Pierre de Coubertin founded the International Olympic Committee (IOC) in 1894, 
leading to the first modern Games in Athens in 1896."

