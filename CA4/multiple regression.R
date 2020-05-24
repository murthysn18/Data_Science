install.packages("gvlma")
install.packages("MASS")
install.packages("leaps")

############################################## Read the data from excel from each sheet ############################################

library("readxl")
xl_data <- "InputFiles/Irish Industries in Ireland.xlsx"
sheets <- excel_sheets(xl_data)
no_of_sheets <- length( excel_sheets( xl_data ) )
economy_CBIC_ind <- exceldata[,1:2]

for(i in 1:no_of_sheets)
{
  exceldata <- read_xlsx(xl_data, sheet = sheets[i])
  sheetname <- sheets[i]
  economy_CBIC_ind[, ncol(economy_CBIC_ind) + 1] <- rowSums(Filter(is.numeric, exceldata))
  names(economy_CBIC_ind)[ncol(economy_CBIC_ind)] <- paste0(sheetname)
  
}

Econony_industry_basis <- economy_CBIC_ind
head(Econony_industry_basis)

colnames(Econony_industry_basis)

############################################## Removing the Unwanted colu

Econony_industry_basis <- subset(Econony_industry_basis, select = -c(`Industry Type`, 
                                                                     `Sales per employment`, 
                                                                     Employment, 
                                                                     `Export per percentage sales`, 
                                                                     `Payroll Costs per person employ`, 
                                                                     `Material cost`,
                                                                     `Materials cost in Ireland`, 
                                                                     `Irish sourced to total material`, 
                                                                     `Irish source service percentage`,
                                                                     `Total value added per person`,
                                                                     `payroll percetage to value add`,
                                                                     `formal structural training cost`,
                                                                     `Service cost Ireland source`, 
                                                                     `formal structural training cost`
))



################## Dividing the data into Training and Testing dataset.

set.seed(1)
no_rows_data <- nrow(Econony_industry_basis)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)

training_data <- Econony_industry_basis[sample, ]
testing_data <- Econony_industry_basis[-sample, ]


################## Applying Linear regression on the data

fit <- lm(training_data$`Expenditure into Irish Economy` ~ training_data$`Total sales of goods` + training_data$Exports + 
            training_data$`Payroll costs` + training_data$`Cost of services` + training_data$`Total Value added services`,  data=training_data)
summary(fit)
confint(fit)

library(car)

qqPlot(fit, labels=row.names(Econony_industry_basis$Industry), id.method="identify", simulate=TRUE, main="Q-Q Plot")


#####################################Studentized residuals

  student_fit <- rstudent(fit)

hist(student_fit, 
     breaks=10, 
     freq=FALSE, 
     xlab="Studentized Residual", 
     main="Distribution of Errors")

rug(jitter(student_fit), col="brown")

curve(dnorm(x, mean=mean(student_fit), sd=sd(student_fit)), add=TRUE, col="blue", lwd=2)

lines(density(student_fit)$x, density(student_fit)$y, col="red", lwd=2, lty=2)

legend("topright", legend = c( "Normal Curve", "Kernel Density Curve"), lty=1:2, col=c("blue","red"), cex=.7)

outlierTest(fit)

########################## plot the partial resudials of each factor.

crPlots(fit)

influencePlot(fit, main="Factors Influence Plot")

########################## AIC values comparison of normal and sqrt values.

summary(powerTransform(training_data$`Expenditure into Irish Economy`))

sqrt_transform_expenditure <- sqrt(training_data$`Expenditure into Irish Economy`)
training_data$expenditure_sqrt <- sqrt_transform_expenditure

fit_model1 <- lm(training_data$`Expenditure into Irish Economy` ~ training_data$`Total sales of goods` + training_data$Exports + 
                   training_data$`Payroll costs` + training_data$`Cost of services` + training_data$`Total Value added services`, data=training_data)
fit_model2 <- lm(expenditure_sqrt ~ training_data$`Total sales of goods` + training_data$Exports + 
                   training_data$`Payroll costs` + training_data$`Cost of services` + training_data$`Total Value added services`, data=training_data)

AIC(fit_model1,fit_model2)

spreadLevelPlot(fit_model2)


########################### Stepwise Regression for normal value

library(MASS)
library(leaps)
fit_test <- lm(training_data$`Expenditure into Irish Economy` ~ training_data$`Total sales of goods` + training_data$Exports + 
                 training_data$`Payroll costs` + training_data$`Cost of services` + training_data$`Total Value added services`, data=training_data)
stepAIC(fit_test, direction="backward")

leaps <-regsubsets(training_data$`Expenditure into Irish Economy` ~ training_data$`Total sales of goods` + training_data$Exports + 
                     training_data$`Payroll costs` + training_data$`Cost of services` + training_data$`Total Value added services`, data=training_data, nbest=4)
plot(leaps, scale="adjr2")




library(MASS)
fit_test <- lm(expenditure_sqrt ~ training_data$`Total sales of goods` + training_data$Exports + 
                 training_data$`Payroll costs` + training_data$`Cost of services` + training_data$`Total Value added services`, data=training_data)
stepAIC(fit_test, direction="backward")

leaps <-regsubsets(expenditure_sqrt ~ training_data$`Total sales of goods` + training_data$Exports + 
                     training_data$`Payroll costs` + training_data$`Cost of services` + training_data$`Total Value added services`, data=training_data, nbest=4)
plot(leaps, scale="adjr2")


############################################ Predicting the Testing data values using predict function.

predicted_economy <- predict(fit_model1, testing_data)
predicted_economy_sqrt <- predict(fit_model2, testing_data)
converted_economy_sqrt <- predicted_economy_sqrt ^2

actuals_predictions <- data.frame(cbind(actuals = testing_data$`Expenditure into Irish Economy`, predicted = predicted_economy))
head(actuals_predictions)

actuals_predictions_sqrt <-  data.frame(cbind(actuals = testing_data$`Expenditure into Irish Economy`, predicted = converted_economy_sqrt))
head(actuals_predictions_sqrt)


################################### Correlation accuracy

correlation_accuracy <- cor(actuals_predictions)
correlation_accuracy

correlation_accuracy <- cor(actuals_predictions_sqrt)
correlation_accuracy

################################### Min - max accuracy

min_max_accuracy <- mean(apply(actuals_predictions, 1, min) / apply(actuals_predictions, 1, max))
min_max_accuracy

min_max_accuracy <- mean(apply(actuals_predictions_sqrt, 1, min) / apply(actuals_predictions_sqrt, 1, max))
min_max_accuracy

################################### Getting residual standard Error


sigma(fit_model1)/ mean(testing_data$`Expenditure into Irish Economy`)

sigma(fit_model2)/ mean(testing_data$`Expenditure into Irish Economy`)

summary(Econony_industry_basis)

