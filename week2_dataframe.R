date_vec <- c("2018-15-10", "2018-01-11", "2018-21-10", "2018-28-10", "2018-01-05")
country_vec <- c("US", "US", "IRL", "IRL", "IRL")
gender_vec <- c("M", "F", "F", "M", "F")
age_vec <- c(32, 45, 25, 39, 99)
q1_vec <- c(5, 3, 3, 3, 2)
q2_vec <- c(4, 5, 5, 3, 2)
q3_vec <- c(5, 2, 5, 4, 1)
q4_vec <- c(5, 5, 5, NA, 2)
q5_vec <- c(5, 5, 2, NA, 1)

column_names <- c("Date", "Country", "Gender", "Age", "Q1", "Q2", "Q3", "Q4", "Q5")

managers <- data.frame(date_vec, country_vec, gender_vec, age_vec, q1_vec, q2_vec, q3_vec, q4_vec, q5_vec)
colnames(managers) <- column_names

str(managers)

managers$Age[managers$Age == 99] <-NA

managers$AgeCat[managers$Age >= 45] <- "Elder"
managers$AgeCat[managers$Age >= 26 & managers$Age <= 44] <- "Middle Age"
managers$AgeCat[managers$Age <= 25] <- "Young"
managers$AgeCat[is.na(managers$Age)] <- "Elder"

managers

modified_Agecat <- factor(managers$Agecat, ordered = TRUE, levels = c("Young", "Middle Age", "Elder"))
managers$Agecat <- modified_Agecat
managers
