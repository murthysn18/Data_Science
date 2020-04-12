install.packages("plyr")
install.packages("stringr") 
library("stringr")  

library(dplyr)
library(readr)

#Setting the working directory

workingdic <- getwd()
workingdic <- trimws(workingdic)  # Trimming and spaces at either ends

setwd(workingdic)
print(workingdic)     # Printing the working directory
workingdic

####################################################################### Question 1 #######################################################

#       Reading the data from all the excel sheets and storing them in the dataframe.

####################################################################### ##################################################################

file_paths = list.files(path="CrimeAndVillageData/NI Crime Data", pattern="*.csv", full.names=TRUE, recursive= TRUE)

AllNICrimeData <- sapply(file_paths, read_csv, simplify=FALSE) %>%
bind_rows()

nrow(AllNICrimeData)

crime_column_name <- c("Crime_ID", "Month", "Reported_by", "Falls within", "Longitude", "Latitude", "Location", "LSOA code", "LSOA_name", "Crime_type", "Last_outcome", "Context")
colnames(AllNICrimeData) <- crime_column_name

head(AllNICrimeData)   # Displaying the first couple of data in the dataframe
str(AllNICrimeData)    # Displaying the structure of the dataframe

write.csv(AllNICrimeData, file = "CrimeAndVillageData/ALLNICrimeData.csv", row.names = FALSE)

####################################################################### Question 2 #######################################################

# Modifying the data amd removing the unwanted columns

##########################################################################################################################################

drop <- c("Crime_ID","Reported_by", "Falls_within", "LSOA_code", "LSOA_name", "Last_outcome","Context")
AllNICrimeData <- AllNICrimeData[,!(names(AllNICrimeData) %in% drop)]

str(AllNICrimeData)


####################################################################### Question 3 #######################################################

# Shortening all the names of the crime_types

##########################################################################################################################################

AllNICrimeData$Crime_type <- as.character(AllNICrimeData$Crime_type)
str(AllNICrimeData)
AllNICrimeData$Crime_type[AllNICrimeData$Crime_type == "Anti-social behaviour"] <- "ASBO" 
AllNICrimeData$Crime_type[AllNICrimeData$Crime_type == "Bicycle theft"] <- "BITH" 
AllNICrimeData$Crime_type[AllNICrimeData$Crime_type == "Burglary"] <- "BURG"
AllNICrimeData$Crime_type[AllNICrimeData$Crime_type == "Criminal damage and arson"] <- "ASBO"
AllNICrimeData$Crime_type[AllNICrimeData$Crime_type == "DRUGS"] <- "DRUG"
AllNICrimeData$Crime_type[AllNICrimeData$Crime_type == "Other theft"] <- "OTTH"
AllNICrimeData$Crime_type[AllNICrimeData$Crime_type == "Public order"] <- "PUBO"
AllNICrimeData$Crime_type[AllNICrimeData$Crime_type == "Robbery"] <- "ROBY"
AllNICrimeData$Crime_type[AllNICrimeData$Crime_type == "Shoplifting"] <- "SHOP"
AllNICrimeData$Crime_type[AllNICrimeData$Crime_type == "Theft from the person"] <- "THPR"
AllNICrimeData$Crime_type[AllNICrimeData$Crime_type == "Vehicle crime"] <- "VECR"
AllNICrimeData$Crime_type[AllNICrimeData$Crime_type == "Violence and sexual offences"] <- "VISO"
AllNICrimeData$Crime_type[AllNICrimeData$Crime_type == "Other crime"] <- "OTCR"
AllNICrimeData$Crime_type[AllNICrimeData$Crime_type == "Possession of weapons"] <- "POWN"

AllNICrimeData


####################################################################### Question 4 #######################################################

# Plotting the frequency of crime_type

##########################################################################################################################################
library(plyr)

CrimeFreq <- prop.table(table(AllNICrimeData$Crime_type))   # converting the entries into frequency table
CrimeFreq

barplot(CrimeFreq, ylab = "Frequency", xlab = "Crime Type", main = "Different crime and frequency in Northern Ireland", col = rainbow(14))

####################################################################### Question 5 #######################################################

# Refactoring the location column to remove enpty data and substring.
  
###########################################################################################################################################

AllNICrimeData$Location <- str_remove(AllNICrimeData$Location, "On or near ") # Removing the sbubstring
AllNICrimeData$Location[AllNICrimeData$Location == ""] <- NA  # removing the empty spaces and replacing it with NA

head(AllNICrimeData)

####################################################################### Question 6 #######################################################

# Choosing 5000 Random samples from the crime data excluding those which have location data as NA

##########################################################################################################################################

CleanNIPostcodeData <- read.csv("NIPostCodesData/CleanNIPostcodeData.csv", header = TRUE)

CleanNIPostcodeData

find_a_town <- function()    # function to get the town names from the post code data from previous section CA
{
  set.seed(100)
  
  random_crime_sample_data <- crimedata_new[ sample( which( crimedata_new$Location != "NA" ) , 5000) , ]

  CleanNIPostcodeData <- read.csv("NIPostCodesData/CleanNIPostcodeData.csv", header = TRUE)
  
  random_crime_sample_data$Town <- CleanNIPostcodeData$Town[match(toupper(random_crime_sample_data$Location), toupper(CleanNIPostcodeData$Primary_Thorfare))]
  
  return(random_crime_sample_data) # Ramdom crime data with town data added.
}

random_crime_sample <- find_a_town()
  
head(random_crime_sample)

####################################################################### Question 7 #######################################################

# Create a function to compare town details and add population to sample crime data dataframe

##########################################################################################################################################

add_town_data <- function(random_crime_sample_dat)  # function for Ramdom crime data with population data added. 
{
  
  villageList <- read.csv("CrimeAndVillageData/villagedata/VillageList.csv", header = TRUE)
  villageList
  
  random_crime_sample_dat$POPULATION <- villageList$POPULATION[match(toupper(random_crime_sample_dat$Town), toupper(villageList$ï..CITY.TOWN.VILLAGE))]
  return(random_crime_sample_dat)
  
}
random_crime_sample <- add_town_data(random_crime_sample) # Ramdom crime data with population data added.

head(random_crime_sample)

####################################################################### Question 8 #######################################################

# Removing unwanted columns from random_crime_sample dataframe

##########################################################################################################################################

drop <- c("Falls within","LSOA code")
random_crime_sample <- random_crime_sample[,!(names(random_crime_sample) %in% drop)]

random_crime_sample

####################################################################### Question 9 #######################################################

# Getting the crime data for two cities of Belfast and LondonDerry and plotting the same

##########################################################################################################################################

belfast_Crime_data <- filter(random_crime_sample, Town == 'BELFAST')   # filtering the crime data for belfast
belfast_Crime_data

londonderry_Crime_data <- filter(random_crime_sample, Town == 'LONDONDERRY') # filtering the crime data for LondonDerry
londonderry_Crime_data

count_belfast <- count(belfast_Crime_data$Crime_type) # Getting the count of each crime for Belfast
sort(count_belfast$freq)

count_derry <- count(londonderry_Crime_data$Crime_type)   # Getting the count of each crime for Londonderry
sort(count_derry$freq)


par(mfrow = c(2,2))
plot(count_belfast$x, count_belfast$freq, xlab = "Crime TYpe", ylab = "Crime numbers", main = "All crimes and their numbers in Belfast")  
plot(count_derry$x, count_derry$freq, xlab = "Crime TYpe", ylab = "Crime numbers", main = "All crimes and their numbers in Londonderry")  


