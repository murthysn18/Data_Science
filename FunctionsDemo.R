
# Creating a function ------------------------------------------------
#
# Follow notes on Creating a function in Blackboard
#
# --------------------------------------------------------------------

# This function accepts a vector
# and calculates the percentage value for 
# each element

add_percent <- function(x)
{
  # Multiply the number by 100, and only show 1 digit
  percent <- round(x * 100, digits = 1)
  result <- paste(percent, "%", sep = "")
  # Return the calculation. Note this is optional 
  #since R returns the value of the last line of code
  return(result)
}

# Saved R script containing the function
# above into "percent.R"
# This line calls it. Ensure you are using
# the current working directory

getwd() # Check working directory
source("percent.R")
sample_vector <- c(0.458, 1.6653, 0.83112)
updated_vector <- add_percent(sample_vector)
updated_vector

# Add extra argument in the function

add_percent <- function(value, multiplier, no_digits)
{
  # Multiply the number by inputted multiplier, and only show 1 digit
  percent <- round(value * multiplier, digits = no_digits)
  result <- paste(percent, "%", sep = "")
  # Return the calculation. Note this is optional 
  #since R returns the value of the last line of code
  return(result)
}

# Test this new function out
add_percent(sample_vector, multiplier = 1, no_digits = 5)


# Adding default values

add_percent <- function(value, multiplier = 100, no_digits = 1)
{
  # Multiply the number by inputted multiplier, and only show 1 digit
  percent <- round(value * multiplier, digits = no_digits)
  result <- paste(percent, "%", sep = "")
  # Return the calculation. Note this is optional 
  #since R returns the value of the last line of code
  return(result)
}

# decision making --------------------------------------------------------


price_calculator <- function(hours, pph = 40){
  net_price <- hours * pph
  round(net_price)
  return(net.price)
}

price_calculator(hours = 10)

# Adding a simple IF statement

price_calculator <- function(hours, pph = 40){
  net_price <- hours * pph
  if(hours > 100) {
    net_price <- net_price * 0.9
  }
  round(net_price)
  return(net_price)
}

price_calculator(hours = 110)
price_calculator(hours = 90)

price_calculator <- function(hours, pph = 40, public = TRUE){
  net_price <- hours * pph
  if(hours > 100){
    net_price <- net_price * 0.9
  } 
  if(public) {
    total_price <- net_price * 1.06
  } 
  else {
    total_price <- net_price * 1.12
  }
  round(total_price)
  return(total_price)
}

price_calculator(45, public = TRUE)
price_calculator(25, public = FALSE)


# Complex function example
my_stats <- function(x, parametric=TRUE, print=FALSE) {
  # Use an IF statement to check if value
  # of the "parametric variable is TRUE
  if (parametric) 
  {
    # If true, set CT to use mean and SD
    central_tendency <- mean(x)
    spread <- sd(x)
  } 
  else 
  {
    # otherwise it is false. Use median and
    # median absolute deviation
    central_tendency <- median(x)
    spread <- mad(x)
  }
  if (print & parametric) # if both are true
  {
    # Concatenate is the same as print function
    cat("Mean=", central_tendency, "\n", "SD=", spread, "\n")
  } 
  else if (print & !parametric) 
  {
    cat("Median=", central_tendency, "\n", "MAD=", spread, "\n")
  }
  result <- list(central_tendency, spread)
  return(result)
}

# Set the environment to test this function
set.seed(1234)

# rnorm generates a random value from the normal distribution
random_numbers <- rnorm(500)
random_numbers
plot(random_numbers)
summary(random_numbers)

my_stat_result <- my_stats(random_numbers, parametric = FALSE, print = TRUE)
my_stat_result <- my_stats(random_numbers) # doesnt show anything - print is set to FALSE by default

# Switch function
mydate <- function(type="long") {
  switch(type,
         long = format(Sys.time(), "%A %B %d %Y"),
         short = format(Sys.time(), "%m-%d-%y"),
         # cat used to output objects
         cat(type, "is not a recognised type")
  )
}

mydate("long")
mydate("short")
mydate()
mydate("medium")