# R MongoDB
This is a sample script to demonstrate how to connect R to mongodb. Dataset is 
obtained from data.gov's Crime dataset and it is a 6.5M rows 1.5GB dataset thus
it cannot be stored here and have to be downloaded from the source itself.
There is also another sample script using in-built diamonds dataset.

## R package dependency
- ggplot2
- dplyr2
- maps
- ggmap
- mongolite
- lubridate
- gridExtra

## Dataset
Script: trymongo_crimedata.R
Rows: ~6.5M
Cols: 22
Size: 1.5GB (.csv)
Main Page: https://catalog.data.gov/dataset/crimes-2001-to-present-398a4
CSV: https://data.cityofchicago.org/api/views/ijzp-q8t2/rows.csv?accessType=DOWNLOAD


## Trivia
MongoDB is an example of **document-store** NoSQL database
Key-value: Redis, Riak, Oracle NoSQL, Amazon Dynamo
Document-store: CouchDB, MongoDB
Columnar: Apache Cassandra, Apache HBase
GraphL Amazon Neptune, Neo4j, OrientDB
