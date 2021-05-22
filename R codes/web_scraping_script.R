#web scraping from iproperty website
#data scrapped on 8th May 2021

setwd("/users/n2n/RWorkingDirectory/WQD7004")

packageDescription("rvest")
packageDescription("curl")

#install.packages("selectr")
#install.packages("xml2")
#install.packages("rvest")
#install.packages("curl")
#library(rvest)
library(dplyr)
library(selectr)
library(xml2)

#url = "https://www.iproperty.com.my/rent/bukit-bintang/all-residential/"
#iprop <- read_html(url)

#Error in open.connection(x, "rb") : HTTP error 429.

#So we need to change strategy, after closely examining the HTML files, we found the JSON object embedded in the header
#- download html files
#- extract JSON part
#- process JSON

getwd()
pagesNum <- c(1:10)
inputFileNamePrefix <- "iprop_wangsa_maju"
outputCsvFileName <- paste0(inputFileNamePrefix, "_df.csv")

#create a function to extract json, and write to file
extractJsonFromPage <- function(inputFileName, outputFileName){
  iprop_html <- read_html(inputFileName)
  script_nodes <- tail(iprop_html %>%
                         html_nodes("script"))
  pop <- html_text(script_nodes[[5]])
  startIndex <- gregexpr('asyncData', pop)
  endIndex <- gregexpr('false}}', pop)
  jsonString <- substr(pop, startIndex[[1]] - 2, endIndex[[1]] + 6)

  writeLines(jsonString, outputFileName)
}

for(i in pagesNum){
  inputFileName <- paste0(inputFileNamePrefix, i, ".html")
  outputFileName <- paste0(inputFileNamePrefix, i, ".json")
  extractJsonFromPage(inputFileName, outputFileName)
}

#------------------------------
#read JSON file and convert to csv
#------------------------------
#create an empty dataframe with 14 column and add column names
df <- data.frame(matrix(ncol = 14, nrow = 0), stringsAsFactors = FALSE)

#read json file
library(jsonlite)

convertJsonToList <- function(inputFileName){
  
  propJson <- read_json(inputFileName)
  listings <- (propJson['listings'])[[1]]
  listingItems <- (listings['items'])[[1]]
  
  mylist <- list() #create an empty list
  
  for(i in 1:length(listingItems)){
    listingItem1 <- listingItems[[i]]
    fullName <- listingItem1$title
    rental <- listingItem1$prices[[1]]$min
    address <- listingItem1$address$formattedAddress
    lat <- listingItem1$address$lat
    lng <- listingItem1$address$lng
    city <- listingItem1$multilanguagePlace$`en-GB`$level1
    district <- listingItem1$multilanguagePlace$`en-GB`$level2
    propertyName <- listingItem1$multilanguagePlace$`en-GB`$level3
    bedrooms <- listingItem1$attributes$bedroom
    bathrooms <- listingItem1$attributes$bathroom
    builtUp <- listingItem1$attributes$builtUp
    carParks <- listingItem1$attributes$carPark
    furnishing <- listingItem1$attributes$furnishing
    pricePerSizeUnitBuiltUp <- listingItem1$attributes$pricePerSizeUnitBuiltUp
    buildingId <- listingItem1$attributes$buildingId
    unitType <- listingItem1$attributes$unitType
    postedDate <- listingItem1$postedAt
    propertyType <- listingItem1$propertyType
    
    mylist[[i]] <- list(fullName, propertyName, rental, pricePerSizeUnitBuiltUp, address, 
                        lat, lng, city, district, bedrooms, bathrooms, builtUp, carParks, 
                            furnishing, postedDate, buildingId, unitType, propertyType)
  }
  
  return(mylist)
}

df <- do.call("rbind", convertJsonToList(inputFileName)) 

myDfList <- list()
for(i in 1:10){
  inputFileName <- paste0(inputFileNamePrefix, i , ".json")
  myDfList[[i]] <- convertJsonToList(inputFileName) 
}

mainDf <- data.frame(matrix(ncol = 14, nrow = 0), stringsAsFactors = FALSE)
for(i in 1:10){
  temp_df <- do.call("rbind", myDfList[[i]])
  mainDf <- rbind(mainDf, temp_df)
}

titles <- c("name", "propertyName", "rental","pricePerSizeUnitBuiltUp", "address", 
            "lat", "lng", "city", "district", "bedrooms", "bathrooms", "builtUp", "carParks", 
            "furnishing", "postedDate", "buildingId", "unitType",  "propertyType")

# First coerce the data.frame to all-character
mainDf <- data.frame(lapply(mainDf, as.character), stringsAsFactors=FALSE)
write.table(mainDf, file = outputCsvFileName, sep = ",", col.names = titles, row.names = FALSE)
