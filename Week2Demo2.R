x <- 2
y <- 5

is.numeric(x)

date1 <- as.Date("2020-03-05")
class(date1)

vec1 <- c(3, 5, 6, 8)
vec2 <- c(3, 3, 3)

vec3 <- vec1 + vec2
vec3

#vector arithmetic

a <- c(10.4, 5.6, 3.1, 6.4, 21.7)
b <- c(x, 0, x)
c <- 2*x + y + 1

min(c)
max(c)
range(c)
prod(c)

mean_of_c <- mean(c)
sort_mean_of_c <- sort(mean_of_c)
class_of_mean_of_c <- class((mean_of_c))

my_array <-c(1:3, NA)
result <- is.na(my_array)
result

seq1 <- seq(-100, 100, by = 0.6)
seq1

mean(seq1)
sort(seq1, decreasing = TRUE)