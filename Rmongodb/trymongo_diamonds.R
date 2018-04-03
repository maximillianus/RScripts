## Test R mongodb mongolite package ##
## Tutorial source: https://jeroen.github.io/mongolite/ ##
library(mongolite)
library(ggplot2)

# Init mongo collection & insert data
m <- mongo(collection = 'diamonds')
m$insert(diamonds)

# check data count integrity
m$count() == nrow(diamonds) # TRUE

m$count('{"cut":"Fair"}')

# Read all data back into R
dmd_data <- m$find('{}')

# Get all rows where cut == Premium and price < 1000
query1 <- m$find('{"cut":"Premium", "price":{"$lt":1000}}')

# Get all rows and filter field
query2 <- m$find(
  query = '{"cut": "Premium", "price": {"$lt":1000} }',
  fields = '{"cut":true, "clarity":true}',
  limit = 5
)

# Sort and limit output
## Find all premium cut
query3 <- m$find('{"cut" : "Premium"}')
## Sort based on highest price first
query3 <- m$find('{"cut" : "Premium"}', sort = '{"price" : -1}')
## Sort based on  highest price and limit results
query3 <- m$find('{"cut" : "Premium"}', sort = '{"price" : -1}', limit = 10)
