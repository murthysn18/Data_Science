install.packages("readxl")
install.packages("matrixStats")
library(matrixStats)
library("readxl")

#Setting the working directory

workingdic <- getwd()
workingdic <- trimws(workingdic)  # Trimming and spaces at either ends
print(workingdic)
setwd(workingdic)

#reading the data from excel

Sales <- read_xlsx("InputFiles/Irish Industries in Ireland.xlsx", sheet = "Total sales of goods")

#Adding column names
Sales_column_name <- c("Indutry", "Industry_type", "Value_in",  "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
                       "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")

colnames(Sales) <- Sales_column_name
head(Sales)

# Printing the number of column and rows

print(paste(length(Sales), " are the number of columns"))
print(paste(nrow(Sales), " are the number of rows"))
##############################################  Total sales all industries put together, each year and plot the same.  ###############################################################

#Add All the rows column vise using function colSums()

Total_sales_each_year <- colSums(Filter(is.numeric, Sales))
Total_sales_each_year

# plot graph
barplot(Total_sales_each_year, xlab = "Years", ylab = "Total sales in millions of Euros", main = "Over all sales in Millions over the years", col = rainbow(length(Sales)))

######################################### Trend of Total sales each year by Industry Type  #################################################################

#sum the columns and group by with respect to indsutry type

Sales_by_industry_type <- aggregate(Filter(is.numeric, Sales), by=list(Sales$Industry_type), FUN=sum)
Sales_by_industry_type

Energy <- Sales_by_industry_type[c("1"),]
Information <- Sales_by_industry_type[c("2"),]
Manufacturing <- Sales_by_industry_type[c("3"),]

#Picking up only the numeric values for each dataframe

Energy <- colSums(Filter(is.numeric, Energy))
Information <- colSums(Filter(is.numeric, Information))
Manufacturing <- colSums(Filter(is.numeric, Manufacturing))

max_value <- max(max(Energy), max(Information), max(Manufacturing))

# Plotting the graph

plot(Manufacturing, type = "o", ylim=c(0,(max_value+12000)), col="red", xlab = "Years", ylab = "Sales in Millions of Euros", main = "Total Sales in Ireland", xaxt = 'n')
axis(side=1,at=c(1,5,10,15,20),labels=c("2000","2005","2010","2015", "2020"))
lines(Information, type = "o", col = "blue")
lines(Energy, type = "o", col = "black")
legend("top", legend=c("Manufacturing", "Information", "Energy"), col=c("red", "blue", "black"), lty=1:2, cex=0.8)


############################################## Which industry has given better business over the years #############################################

# Taking the median of the each industry

Sales_median <- rowMedians(as.matrix(Sales[,c(-1:-3)]))
Sales$median <- Sales_median
str(Sales)

# Assigning row numbers

Sales$Index <- 1:nrow(Sales)

# Plotting the graph to show the median of each industry over the years

barplot(Sales$median,names.arg =  Sales$Index, col = (rainbow(28)), xlab = "Industries", ylab = "Sales in millions", ylim = c(0, max(Sales_median+2000) ))
legend("top", c(paste(Sales$Index," ", Sales$Indutry )), cex=0.5)



