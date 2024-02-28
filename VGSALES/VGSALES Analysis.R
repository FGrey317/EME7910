#Loading TidyVerse
library(tidyverse)

#Checking Directory
getwd()

#Importing the Data Set
data <- read.csv("cleaned_vgsales.csv")

#Tests of Line and Scatter combined
ggplot(data = data, mapping = aes(x = Genre, y = NA_Sales)) + 
  geom_point(mapping = aes(color = Publisher)) + 
  geom_smooth()

#Testing filtering for better data visualization. Filtering NA_Sales out that are lower than 1.
filter(data, NA_Sales > 1)

#Saving this filter into a new dataset
(NA_SalesFiltered <- filter(data, NA_Sales > 1))

#Tests of Line and Scatter combined for new dataset with filter applied
ggplot(data = NA_SalesFiltered, mapping = aes(x = Genre, y = NA_Sales)) + 
  geom_point(mapping = aes(color = Publisher)) + 
  geom_smooth()

#Changed to Platform as 3rd aesthetic because there are simply too many different publishers
ggplot(data = NA_SalesFiltered, mapping = aes(x = Genre, y = NA_Sales)) + 
  geom_point(mapping = aes(color = Platform)) + 
  geom_smooth()

#Switched Genre and Platform to see if the data wasmore understandable
ggplot(data = NA_SalesFiltered, mapping = aes(x = Platform, y = NA_Sales)) + 
  geom_point(mapping = aes(color = Genre)) + 
  geom_smooth()

#Not sure why I am not getting a line plot. Is it because there are only categorical variables?

# Lets try one more filter to see if I can remove all that are lower than one for NA_Sales and EU_Sales
# and then map those by genre and platform
(NA_SalesAndEU_SalesFiltered <- filter(NA_SalesFiltered, EU_Sales > 1))

#Now lets see what this looks like with both columns narrowed down
ggplot(data = NA_SalesAndEU_SalesFiltered, mapping = aes(x = EU_Sales, y = NA_Sales)) + 
  geom_point(mapping = aes(color = Genre)) + 
  geom_smooth()

# I still think its too much data to get as much as I would like. Lets try factoring
ggplot(data = NA_SalesAndEU_SalesFiltered) + 
  geom_point(mapping = aes(x = EU_Sales, y = NA_Sales)) + 
  facet_wrap(~ Genre, nrow = 4)

# Not great, lets try faceting by genre and then sticking to NA_Sales on Y with Platform as X. Seeing which
# Platform sold the most shares of the NA market for each genre
#Lets also test with our first data set again to see if its manageable first
ggplot(data = data) + 
  geom_point(mapping = aes(x = Platform, y = NA_Sales)) + 
  facet_wrap(~ Genre, nrow = 4)

# Lets try filtering out a certain year so that it reduces the platforms
filter(NA_SalesAndEU_SalesFiltered, Year > 2010)
(data2 <- filter(NA_SalesAndEU_SalesFiltered, Year > 2010))

#Now lets try the last plot one more time to see if its more legible and usable
ggplot(data = data2) + 
  geom_point(mapping = aes(x = Platform, y = NA_Sales)) + 
  facet_wrap(~ Genre, nrow = 4)

#better so lets try a scatter plot
ggplot(data = data2, mapping = aes(x = Platform, y = NA_Sales)) + 
  geom_point(mapping = aes(color = Genre)) 

#Finally lets try out a trust bar chart to see if this gives a better view of the data
ggplot(data = data2) + 
  geom_bar(mapping = aes(x = Platform))

# I want to map to the NA Sales so I need to use the following:
ggplot(data = data2) +
  geom_bar(mapping = aes(x = Platform, y = NA_Sales, fill = Platform), stat = "identity")

#writing out excels
write.csv(data2, file="filtered_vgsales.csv")
write.csv(NA_SalesAndEU_SalesFiltered, file="NA_SalesAndEU_SalesFiltered.csv")
write.csv(NA_SalesFiltered, file="NA_SalesFiltered.csv")
