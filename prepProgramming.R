##### Conditional statement #####
a <- 0
if(a!=0){
  print(1/a)
} else{
  print("No reciprocal for 0.")
}
# [1] "No reciprocal for 0."

a <- c(0, 1, 2, -4, 5)
ifelse(a > 0, 1/a, NA)
# [1]  NA 1.0 0.5  NA 0.2

##### Functions #####
avg <- function(x){
  s <- sum(x)
  n <- length(x)
  s/n
}
avg(a)
# [1] 0.8

##### Iteration (for-loop) #####
m <- 5
s_n <- vector(length = m) # create an empty vector
for(n in 1:m){
  s_n[n] <- avg(seq(1:n))
}
s_n
# [1] 1.0 1.5 2.0 2.5 3.0

##### Vectorization #####
x <- 1:5
y <- 1:5
x*y
# [1]   1   4   9  16  25

sapply(x, sqrt)
# [1] 1.000000 1.414214 1.732051 2.000000 2.236068
