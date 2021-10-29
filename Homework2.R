library(here) #Load all the necessary packages
library(dplyr)
library(janitor)
library(stringr)
library(sf)
library(tidyverse)
library(tmap)
library(tmaptools)
library(countrycode)

#Read in the shapefile containing country names and geographical data
#"Here" removes the need for me to specify the full path, as it will start from the project folder

CountryData <- st_read("World_Countries_(Generalized)") %>%
  clean_names() #Neatens the column names, removing the capitalization

#Repeat this process for the data with country names and inequality indices 
#'skip = 5' removes the first five rows, which are all header material
#'na = ".." removes na values which are stored as ".." in this data set
#This is necessary as we need the values in the index column to be recognized as numeric
#remove_empty() removes the blank columns in this data set, making it neater
#'quiet = True' stops the program declaring the columns it's removed
#'#clean_names also adds an "x" to columns with numeric titles, making later calculations possible

InequalityData <- read_csv(here("Gender Inequality Index (GII).csv"),
                           skip = 5, na = "..",
                           locale = locale(encoding = "latin1")) %>%
  remove_empty(which = "cols", quiet = TRUE) %>%
  clean_names() %>%
  slice(1:189) %>%
  #Creates a new column containing an iso code to match columns in CountryData
  mutate(iso_code=countrycode(country, origin = 'country.name', destination = 'iso2c'))

#The country names from the two dataframes cannot be merged as it stands
#This is because InequaliyData has a blank space " " before the country name
#This block of code fixes this problem
#Because of countrycode, this section now isn't strictly necessary

CountryList <- dplyr::select(InequalityData, country) #Extracts countries as a list
CountryListTrimmed <- as.list(trimws(CountryList$country, "l")) #Removes the blank spaces
#Adds this fixed list back into the dataframe
CleanInequalityData <- mutate(InequalityData, CountryListTrimmed)

#This block merges the two dataframes together using their common country names columns
#**Some data is lost because some country names do not match perfectly
#The code also produces a new column with the change of index from columns "x2010" and "x2019"

JoinedDataFrame <- merge(CountryData, CleanInequalityData,
                         #The titles of the columns with the common value names
                         by.x = "iso", by.y = "iso_code") %>%
  #Creates the new comparison column
  mutate(., InequalityDifference2010s = x2010 - x2019) %>%
  #Reduces the number of columns to just the important ones we want
  select(country.x, iso, geometry, InequalityDifference2010s)

#This calculates and prints a mean value for the change in inequality index across the world
#"na.rm = TRUE" removes the na values, which prevents "na" from being returned as our output
MeanChange <- mean(JoinedDataFrame$InequalityDifference2010s, na.rm = TRUE)
print(MeanChange)

#Plot the map with the values colourising the countries
tm_shape(JoinedDataFrame) +
  tm_polygons(
    col = "InequalityDifference2010s",
    palette="RdYlGn", #Red, Yellow, Green Pallette
    style="pretty", #Pretty is one of the colouring styles
    n=8, #Sets eight colour categories
    midpoint = 0.1) #The value for the bland colour between yellow and green
