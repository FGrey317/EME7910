#Loading TidyVerse
library(tidyverse)
library(tidyr)
library(ggplot2)
install.packages("GGally")
library(GGally)
install.packages("plotly")
library(plotly)

#Checking Directory
getwd()

#Importing the Data Set
MagnetData <- read.csv("C:/Users/Frank/Downloads/Magnet Game/flattened_MAGNET_data2024-02-27 15_13_51.372302wUniqueSessionIDs2.csv")

#Mutation of data to combine the Southern Distance Guess Score
#and the Northern Distance Guess Score

MagnetData <- mutate(MagnetData,
       totalscore = guessScore.northDist + guessScore.southDist)

#Fill all columns of Magnet Location with information for that session id
#This will allow me to create a distance formula from the objects and the
#North and South Poles

#This didnt work. Had to do so by hand in the excel file and save as new.


#Mutation attempt to compare the distance of the x/y coordinates of all objects
#in a game session to the x/y coordinates of the magnet north

MagnetData <- mutate(MagnetData,
          ObjectsDistanceFromNorthPole 
          = sqrt((magnetLocation.xNorth - location.x)^2 
          + (magnetLocation.yNorth - location.y)^2))


#Mutation attempt to compare the distance of the x/y coordinates of all objects
#in a game session to the x/y coordinates of the magnet south

MagnetData <- mutate(MagnetData,
           ObjectsDistanceFromSouthPole 
           = sqrt((magnetLocation.xSouth - location.x)^2 
           + (magnetLocation.ySouth - location.y)^2))


#I also needed to fill in the Total Score column by session id which I got
#to work by adding the direction up portion that i did not have before

MagnetData <- MagnetData %>%
  group_by(persistent_session_id) %>%
  fill(totalscore, .direction = "up")

#Now time to visualize the comparison of distance between objects coordinates
#To coordinates of north and south pole to the players score

#First I filtered out the Magnetic Film Data so I could first look at that

MagneticFilmData <- MagnetData %>%
  filter(toolType == "MAGNETIC_FILM")

#Lets start with a comparison of the coordinates of the magnetic film coordinates
#to the coordinates of the north poles

ggplot(data = MagneticFilmData, mapping = aes(x = ObjectsDistanceFromNorthPole, y = totalscore)) + 
  geom_point() + 
  geom_smooth()

#Now compare to the south pole

ggplot(data = MagneticFilmData, mapping = aes(x = ObjectsDistanceFromSouthPole, y = totalscore)) + 
  geom_point() + 
  geom_smooth()


#Now I want to do the same with Iron Filings as Magnetic Film

IronFilingData <- MagnetData %>%
  filter(toolType == "IRON_FILINGS")

#Lets start with a comparison of the coordinates of the iron filings coordinates
#to the coordinates of the north poles

ggplot(data = IronFilingData, mapping = aes(x = ObjectsDistanceFromNorthPole, y = totalscore)) + 
  geom_point() + 
  geom_smooth()

#Now compare to the south poles

ggplot(data = IronFilingData, mapping = aes(x = ObjectsDistanceFromSouthPole, y = totalscore)) + 
  geom_point() + 
  geom_smooth()

#Now one more time with the Compass tool

CompassData <- MagnetData %>%
  filter(toolType == "COMPASS")

#Lets start with a comparison of the coordinates of the Compass coordinates
#to the coordinates of the north poles

ggplot(data = CompassData, mapping = aes(x = ObjectsDistanceFromNorthPole, y = totalscore)) + 
  geom_point() + 
  geom_smooth()

#Now compare to the south poles

ggplot(data = CompassData, mapping = aes(x = ObjectsDistanceFromSouthPole, y = totalscore)) + 
  geom_point() + 
  geom_smooth()


#Maybe lets simply look at the overall tool distance from the south and north poles

ggplot(data = MagnetData, mapping = aes(x = ObjectsDistanceFromNorthPole, y = totalscore)) + 
  geom_point() + 
  geom_smooth()

#Now compare to the south poles

ggplot(data = MagnetData, mapping = aes(x = ObjectsDistanceFromSouthPole, y = totalscore)) + 
  geom_point() + 
  geom_smooth()

#How about drag time to score

ggplot(data = MagnetData, mapping = aes(x = dragTime, y = totalscore)) + 
  geom_point() + 
  geom_smooth()

#Now compare to the south poles

ggplot(data = MagnetData, mapping = aes(x = dragTime, y = totalscore)) + 
  geom_point() + 
  geom_smooth()



#Looking at the distance from north and distance from south of magnetic film
#repeated with iron filings and compasses
ggplot(data = MagneticFilmData, mapping = aes(x = ObjectsDistanceFromNorthPole, y = ObjectsDistanceFromSouthPole, color = totalscore)) + 
  geom_point() + 
  geom_smooth()

ggplot(data = IronFilingData, mapping = aes(x = ObjectsDistanceFromNorthPole, y = ObjectsDistanceFromSouthPole, color = totalscore)) + 
  geom_point() + 
  geom_smooth()

ggplot(data = CompassData, mapping = aes(x = ObjectsDistanceFromNorthPole, y = ObjectsDistanceFromSouthPole, color = totalscore)) + 
  geom_point() + 
  geom_smooth()

#This does not visualize well








#Now lets try the last plot one more time to see if its more legible and usable
ggplot(data = MagneticFilmData) + 
  geom_point(mapping = aes(x = ObjectsDistanceFromNorthPole, y = ObjectsDistanceFromSouthPole)) + 
  facet_wrap(~ totalscore, nrow = 4)



#Finally lets try out a trusty bar chart to see if this gives a better view of the data
ggplot(data = MagneticFilmData) +
  geom_bar(mapping = aes(x = totalscore, y = ObjectsDistanceFromNorthPole), stat = "identity")


ggpairs(MagneticFilmData, columns = c("ObjectsDistanceFromNorthPole", "ObjectsDistanceFromSouthPole", "totalscore"))


plot_ly(data = MagneticFilmData, x = ~ObjectsDistanceFromNorthPole, y = ~ObjectsDistanceFromSouthPole, z = ~totalscore, 
        type = "scatter3d", mode = "markers", marker = list(size = 5)) %>%
  layout(scene = list(xaxis = list(title = "Distance from North Pole"),
                      yaxis = list(title = "Distance from South Pole"),
                      zaxis = list(title = "Total Score")))
