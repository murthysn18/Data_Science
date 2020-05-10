install.packages("readxl")
install.packages("matrixStats")
install.packages("factoextra")

library(matrixStats)
library(reshape2)

workingdic <- getwd()
workingdic <- trimws(workingdic)  # Trimming and spaces at either ends
print(workingdic)
setwd(workingdic)


###################################################### Total of the data of for perticular industry over the years to give yearly numbers for all tabs and 
###################################################### copied to the same dataframe

library("readxl")
xl_data <- "InputFiles/Irish Industries in Ireland.xlsx"
sheets <- excel_sheets(xl_data)
no_of_sheets <- length( excel_sheets( xl_data ) )
economy_CBIC_ind <- data.frame(economy_data)
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


###################################################### Adding the data of all industries to give yearly numbers for all tabs and 
###################################################### copied to the same dataframe
economy_CBIC_year <- data.frame(c(2000:2017))
economy_column_name <- c("Years")
colnames(economy_CBIC_year) <- economy_column_name


for(i in 1:no_of_sheets)
{
  library("readxl")
  sheetname <- sheets[i]
  exceldata <- read_xlsx(xl_data, sheet = sheetname)
  
  economy_CBIC_year[, ncol(economy_CBIC_year) + 1] <- colSums(Filter(is.numeric, exceldata))
  names(economy_CBIC_year)[ncol(economy_CBIC_year)] <- paste0(sheetname)
  
}

Econony_yearly_basis <- economy_CBIC_year
head(Econony_yearly_basis)


########################## Just the numerical data needs to be taken for PCA hence doing the same in further steps
########################## Further test here is carried out for Yearly numbers of all industries

data_numeric_variables <- sapply(Econony_industry_basis, is.numeric)
data_numeric_variables

######################### Removing the non numeric values

data_file_adjusted <- Econony_industry_basis[, data_numeric_variables]

###################### Numeric data passed to the into the prcomp() function by keeping the center and scale to TRUE 
# Peek at the PCA object with is attained by summary().

pca <- prcomp(data_file_adjusted, center = TRUE, scale. = TRUE)
summary(pca)

# 18 principal components, are identified and are named from PC1-18.

str(pca)

######################################## Eigenvalues / Variances###################################################

library("factoextra")
eig_values <- get_eigenvalue(pca)
eig_values



# FactoMineR package is used here to display this information


fviz_eig(pca, addlabels = TRUE, ylim = c(0, 75))


# From the graph it could be seen that majority of the infomarion is availabe from one to fifth component so 
#stopping it at that point should be enough, we might want to stop at the fifth principal component. 

pca_for_variables <- get_pca_var(pca)
pca_for_variables



# -----------------------------------------------------------------------
# Using `Correlation` plot
# -----------------------------------------------------------------------

library("corrplot")
corrplot(pca_for_variables$cos2, is.corr = FALSE)


fviz_pca_var(pca, col.var = "blue")

# -----------------------------------------------------------------------
# Cos2 - quality of representation
# -----------------------------------------------------------------------

head(pca_for_variables$cos2, 10)

##################################################Plotting the PCA as a bargraph using fviz_cos2

fviz_cos2(pca, choice = "var", axes = 1:2)

# -----------------------------------------------------------------------
# Biplot
# -----------------------------------------------------------------------

# A biplot is a type of plot that will allow you to visualise how 
# Colour by cos2 values: quality on the factor map

fviz_pca_var(pca, col.var = "cos2", gradient.cols = c(rainbow(10)), repel = TRUE)  

# Contribution of variables to each PC
# The larger the value of the contribution, the more the variable contributes to the component. 

head(pca_for_variables$contrib, 20)

# The most important (or, contributing) variables can be highlighted on the correlation plot as follows

fviz_pca_var(pca, col.var = "contrib", gradient.cols = c("red", "Blue", "Green"),)


# Now to plot the graph using fviz_contrib() in the form of bar graph from the factoextra package
# to draw a bar plot of variable contributions. 
# Here only top 10 of them are made to be displayed
library(factoextra)


fviz_contrib(pca, choice = "var", axes = 1, top = 15)

# Contributions of variables to PC2
fviz_contrib(pca, choice = "var", axes = 2, top = 15)

# Contributions of variables to PC2
fviz_contrib(pca, choice = "var", axes = 3, top = 15)

# Contribution to PC3 - PC5
fviz_contrib(pca, choice = "var", axes = 3:5, top = 15)

# The red dashed line on the graphs indicate the expected 
# average contribution. 

# We can see that the following  "Total Percentage of sales", "Irish Source service", "Payrol cost per employmeny" and "Irish source to Total Material"
# contribute most to Dim1 - 
# "Total Percentage of sales", "Irish Source service", "Sales per employment", "Payrol cost per employmeny" and "Irish source to Total Material" contribute most to PC 2.
# "Total Percentage of sales", "Irish Source service", "Payrol cost per employmeny" contribute most and "Irish source to Total Material" just reaching the averagecontribute most to PC 2.

fviz_pca_ind(pca,
             axes = c(1, 2),
             geom.ind = "point", # show points only (but not "text values")
             col.ind = economy_CBIC_ind$`Industry Type`, # colour by groups
             palette = c(rainbow(3)),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Industry_types"
)

# Graphical parameters
# We can change the graphical parameters using the function ggpar() from the ggpubr package.

biplot <- fviz_pca_ind(pca, geom = "point", col.ind = economy_CBIC_ind$`Industry Type`)
ggpubr::ggpar(biplot,
              title = "Principal Component Analysis",
              subtitle = "Business sector in Ireland",
              xlab = "PC 1", ylab = "PC 2",
              legend.title = "Industry_types", legend.position = "top",
              ggtheme = theme_gray(), palette = "jco")

# Lets see how PC 3 and PC 4 represent Business data.
biplot <- fviz_pca_ind(pca, 
                       axes = c(3, 4),
                       geom = "point", 
                       col.ind = economy_CBIC_ind$`Industry Type`)
ggpubr::ggpar(biplot,
              title = "Principal Component Analysis",
              subtitle = "Business sector in Ireland",,
              xlab = "PC 3", ylab = "PC 4",
              legend.title = "Industry_types", legend.position = "top",
              ggtheme = theme_gray(), palette = "jco")

