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
##############################################  Total sales all industries put together.  ###############################################################

Total_sales_each_year <- colSums(Filter(is.numeric, Sales))
Total_sales_each_year

########################### Creating a dataframe showing total sales and

sales_data <- data.frame(2000:2017)
sales_data_columns <- c("Years")
colnames(sales_data) <- sales_data_columns

sales_data$totalsales <- c(Total_sales_each_year)

head(sales_data)

######################### Applying Linear regression ############################

simple_linear_model <- lm(sales_data$totalsales ~ sales_data$Years)

summary(simple_linear_model)
confint(simple_linear_model)

plot(x = sales_data$Years, y = sales_data$totalsales, main = "Total_Sales of all industries over the years", 
               xlab = "Years", ylab = "Total Sales in millions")
abline(simple_linear_model)

######################### Applyting Polynomial regression #######################


poly_with_2 <- lm(sales_data$totalsales ~ poly(sales_data$Years, degree = 2, raw = T))
summary(poly_with_2)
confint(poly_with_2)


poly_with_5 <- lm(sales_data$totalsales ~ poly(sales_data$Years, degree = 5, raw = T))
summary(poly_with_5)
confint(poly_with_5)


lines(smooth.spline(sales_data$Years, fitted(poly_with_5)), col = 'red')


########### Choosing the degree 5 for further as it gives the best R-squared value. 

set.seed(1)
no_of_records <- sample(1:nrow(sales_data), 0.8 * nrow(sales_data))

training_data <- sales_data[no_of_records,]
training_data
# test data
testing_data <- sales_data[-no_of_records,]
testing_data

# Build the model on training data
# lm(formula, data) where
# formula describes the model to be fit


poly_with_5 <- lm(testing_data$totalsales ~ poly(testing_data$Years, degree = 5, raw = T))
summary(poly_with_5)
confint(poly_with_5)
options(warn=-1)

# predict sales from testing data
sales_predicted <- predict(poly_with_5, testing_data)  

#Making actual vs prediction dataframe

actuals_preds <- data.frame(cbind(actuals = testing_data$totalsales, predicted = sales_predicted))
head(actuals_preds)

correlation_accuracy <- cor(actuals_preds)
correlation_accuracy

# Min - max accuracy
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy

# MAPE
mape <- mean(abs((actuals_preds$predicted - actuals_preds$actuals)) / actuals_preds$actuals)
mape

################## predicting future values.

df <- data.frame(year = c(2018, 2019, 2020, 2021))
predicted_sales <- predict(poly_with_5, df)
pred_values <- data.frame(cbind(Years = df$year, predicted = predicted_sales))
preds_values

