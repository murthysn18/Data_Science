install.packages("stringr")
install.packages("VIM")# Install stringr package
library(stringr)
library(dplyr)
library(VIM)


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

niPostCodes_column_name <- c("Organisation Name", "Sub-building Name", "Building Name",
                             "Number", "Primary_Thorfare", "Alt Thorfare",
                             "Secondary Thorfare", "Locality", "Townland", "Town", "County",
                             "PostCode", "X-Cordinates", "Y-Cordinates", "Index" )
colnames(niPostCodes) <- niPostCodes_column_name

head(niPostCodes)
str(niPostCodes)

#########################################################################################################################################
#solution for question 3
#########################################################################################################################################

#Counting the missing values before replacing or deleting the rows

niPostCodes[niPostCodes == ""] <- NA

missing_data_count <- sapply(niPostCodes, function(x) sum(length(which(is.na(x)))))   # Taking the missing fields cound before changes 
missing_data_count

niPostCodes <- with(niPostCodes, niPostCodes[!(is.na(niPostCodes$Primary_Thorfare)| is.na(niPostCodes$Town) 
                                               | is.na(niPostCodes$PostCode) | is.na(niPostCodes$PostCode)),])

niPostCodes
head(niPostCodes)


#########################################################################################################################################
#solution for question 4
#########################################################################################################################################

missing_data_count <- sapply(niPostCodes, function(x) sum(length(which(is.na(x)))))
missing_data_count

head(niPostCodes)

#########################################################################################################################################
#solution for question 5
#########################################################################################################################################

# moving the column named index to the beginning of the dataframe

niPostCodes <- niPostCodes[,c(which(colnames(niPostCodes)=="Index"),which(colnames(niPostCodes)!="Index"))]

str(niPostCodes)


#########################################################################################################################################
#solution for question 6
#########################################################################################################################################
#create a new dataset called Limavady_data

Limavady_data <- niPostCodes %>% filter(str_detect(niPostCodes$Primary_Thorfare, "LIMAVADY") | str_detect(niPostCodes$Townland, "LIMAVADY") | str_detect(niPostCodes$Town, "LIMAVADY"))

Limavady_data

cat("number of rows: ", nrow(Limavady_data))

write.csv(Limavady_data, file = "NIPostCodesData/Limavady_data.csv", row.names = FALSE)


#########################################################################################################################################
#solution for question 7
#########################################################################################################################################

write.csv(niPostCodes, file = "NIPostCodesData/CleanNIPostcodeData.csv", row.names = FALSE)

