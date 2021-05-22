#merge 24 csv files into one master data file
library(plyr)
rm(dataset)
rm(titles)
rm(masterDf)

setwd("/Users/n2n/RWorkingDirectory/WQD7004/merged_files")
dataset <- ldply(list.files(), read.csv, header=TRUE)
titles <- c("name", "propertyName", "rental","pricePerSizeUnitBuiltUp", "address", 
            "lat", "lng", "city", "district", "bedrooms", "bathrooms", "builtUp", "carParks", 
            "furnishing", "postedDate", "buildingId", "unitType",  "propertyType")
write.table(dataset, file = "iprop_master_dataframe.csv", sep = ",", col.names = titles, row.names = FALSE)

#A peek at data
masterDf <- read.csv('iprop_master_dataframe.csv', header = T)
head(masterDf)
summary(masterDf$rental)
count(masterDf)  #4759 records

#--------------
#data cleaning
#--------------
#1. remove the properties without a name, total 20
sprintf("Number of listings without a name: %s", count(masterDf[masterDf$propertyName == 'NULL',])) #20 records
masterDf <- masterDf %>% filter(propertyName != 'NULL')
count(masterDf) #4739 records

#2. remove outliers, rental above 100k, total 9
# we will remove the records with monthly rental price above 100k, those units seems like sales units accidentally appearing under rental
summary(masterDf$rental)
sprintf("Number of listings with monthly rental >100k: %s", count(masterDf[masterDf$rental > 100000,])) #9 records
masterDf <- masterDf %>% filter(rental < 100000)
sprintf("Listings after removing outliers: %s", count(masterDf)) #4730 records
summary(masterDf$rental)

#3. data type conversion
