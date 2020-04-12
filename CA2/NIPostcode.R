install.packages("stringr")  # Install stringr package
library(stringr)
library(dplyr)


#########################################################################################################################################
#solution for question 1
#########################################################################################################################################

#setting the working directory
workingdic <- getwd()
setwd(workingdic)


print(workingdic)
#Read the data from the CSV file.
niPostCodes <- read.csv("NIPostCodesData/NIPostcodes.csv", header = FALSE)

#Displaying first 10 rows of the csv file records.
head(niPostCodes, 10)

#showing the structure of data from csv file
str(niPostCodes)

#getting number of rows and columns

cat("number of columns: ", length(niPostCodes))
cat("number of rows: ", nrow(niPostCodes))


#########################################################################################################################################
#solution for question 2
#########################################################################################################################################

# creating column names

niPostCodes_column_name <- c("Building_Name", "Bulding_No", "Block_Name", "Block_No", "Location", "Location_Galic", "Road_Name", "Village", "TownLand", "Town", "County", "PostCode", "X-Cordinates", "Y-Cordinates", "Index" )
colnames(niPostCodes) <- niPostCodes_column_name

head(niPostCodes)


#########################################################################################################################################
#solution for question 3
#########################################################################################################################################

#Replacing all the blank values with NA

niPostCodes[niPostCodes == ""] <- NA

head(niPostCodes)


#########################################################################################################################################
#solution for question 4
#########################################################################################################################################

nrow(niPostCodes[niPostCodes$Building_Name == "NA",])

nrow(niPostCodes[niPostCodes$Bulding_No == "NA",])

nrow(niPostCodes[niPostCodes$Block_Name == "NA",])

nrow(niPostCodes[niPostCodes$Block_No == "NA",])

nrow(niPostCodes[niPostCodes$Location == "NA",])

nrow(niPostCodes[niPostCodes$Location_Galic == "NA",])

nrow(niPostCodes[niPostCodes$Road_Name == "NA",])

nrow(niPostCodes[niPostCodes$Village == "NA",])

nrow(niPostCodes[niPostCodes$TownLand == "NA",])

nrow(niPostCodes[niPostCodes$Town == "NA",])

nrow(niPostCodes[niPostCodes$County == "NA",])

nrow(niPostCodes[niPostCodes$PostCode == "NA",])

nrow(niPostCodes[niPostCodes$`X-Cordinates` == "NA",])
nrow(niPostCodes[niPostCodes$`Y-Cordinates` == "NA",])

#########################################################################################################################################
#solution for question 5
#########################################################################################################################################

# moving the column named index to the beginning of the dataframe

niPostCodes <- niPostCodes[,c(which(colnames(niPostCodes)=="Index"),which(colnames(niPostCodes)!="Index"))]

head(niPostCodes)


#########################################################################################################################################
#solution for question 6
#########################################################################################################################################
#create a new dataset called Limavady_data

Limavady_data <- niPostCodes %>% filter(str_detect(niPostCodes$Location, "LIMAVADY") | str_detect(niPostCodes$TownLand, "LIMAVADY") | str_detect(niPostCodes$Town, "LIMAVADY"))

Limavady_data

cat("number of rows: ", nrow(Limavady_data))

write.csv(Limavady_data, file = "NIPostCodesData/Limavady_data.csv", row.names = FALSE)


#########################################################################################################################################
#solution for question 7
#########################################################################################################################################

write.csv(niPostCodes, file = "NIPostCodesData/CleanNIPostcodeData.csv", row.names = FALSE)

