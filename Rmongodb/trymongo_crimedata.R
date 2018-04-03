## TEST Script NoSQL MongoDB ##
## Data used is Crimes data from data.gov ##
## Data source: https://catalog.data.gov/dataset/crimes-2001-to-present-398a4 ##
## TUtorial source: https://datascienceplus.com/using-mongodb-with-r/ ##

library(ggplot2)
library(dplyr)
library(maps)
library(ggmap)
library(mongolite)
library(lubridate)
library(gridExtra)

setwd('C:/NotBackedUp/')
crimes=data.table::fread("datafiles/Crimes_-_2001_to_present.csv")
names(crimes)

#Remove spaces in the columns names
names(crimes) = gsub(" ", "", names(crimes))
names(crimes)

# Using insert function from mongolite to insert rows
## create connection, database and collection
my_collection = mongo(collection = "crimes", db = "Chicago")
my_collection$insert(crimes)

## Check count
my_collection$count()

# Retrieving data

## get 1 rows
my_collection$iterate()$one()

## How many distinct 'Primary Type'?
length(my_collection$distinct('PrimaryType'))

## How many crime type is Assault?
my_collection$count('{"PrimaryType": "ASSAULT"}')

## How many crime type is Assault and domestic in nature?
my_collection$count('{"PrimaryType": "ASSAULT", "Domestic":"true"}')

# Get filtered data and only retrieve columns of interest
query1= my_collection$find('{"PrimaryType" : "ASSAULT", "Domestic" : "true" }')
query2= my_collection$find('{"PrimaryType" : "ASSAULT", "Domestic" : "true" }',
                           fields = '{"_id":0, "PrimaryType":1, "Domestic":1}')
# query1 is dataframe
# query2 is dataframe with less columns
ncol(query1) # with all the columns
ncol(query2) # only the selected columns

# Where do most crimes take place?
my_collection$aggregate('[{"$group":{"_id":"$LocationDescription", "Count": {"$sum":1}}}]')%>%na.omit()%>%
  arrange(desc(Count))%>%head(10)%>%
  ggplot(aes(x=reorder(`_id`,Count),y=Count))+
  geom_bar(stat="identity",color='skyblue',fill='#b35900')+geom_text(aes(label = Count), color = "blue") +coord_flip()+xlab("Location Description")

query3= my_collection$find('{}', fields = '{"_id":0, "Latitude":1, "Longitude":1,"Year":1}')
