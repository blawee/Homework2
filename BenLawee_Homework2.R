## Ben Lawee - Homework 2

# 1) The new function is_pythagorean has the arguments a, b, and c, which are required inputs to run the function.
## The function returns whether the numbers fit in the pythagorean equation (TRUE if yes, FALSE if no).
is_pythagorean = function(a, b, c) {
  return(a*a + b*b == c*c)
}

## This is a test to show that the function works for both cases.
is_pythagorean(3, 4, 5) ## True
is_pythagorean(3, 4, 6) ## False


# 2) The loop works through a vector of integers starting at 1000 and ending at 100.
## The if condition checks for a prime number by seeing if it can be divided cleanly by any number between one-below itself and 2 (all prime numbers can be divided by themselves and 1).
## If the number is not prime, the sum of other factors is >0, so nothing is printed.
## If the number is determined prime, then it is printed.
for(i in 1000:100) { 
  if(sum(i %% 2:(i-1) == 0) == 0) {
    print(i)                   
  }
}

## This is the extra credit function that checks if a number is prime. It is just the condition from the if statement in the for loop above, except with n instead of i.
is_prime = function(n) {
    return(sum(n %% 2:(n-1) == 0) == 0)
}


#3)
#  a) First, the amounts for the three blends are saved as seperate vectors (A, B, and C).
A = c(20, 30, 50)
B = c(30, 20, 60)
C = c(30, 30, 32)

## Then, the blend vectors are saved together as one matrix with 3 rows, filled by row. This is saved as blends.
## costs is created, a 3x1 matrix that has the price-per-unit for each of the ingredients.
blends = matrix(c(A, B, C), nrow = 3, byrow = TRUE)
costs  = matrix(c(5, 45, 10), nrow = 3, byrow = TRUE)

## To calculate the price-per-blend, the matrix blends is multiplied by costs using the %*% operator, and saved as prices.
## Source for the %*% operator (because I did not have it in my notes): https://stat.ethz.ch/R-manual/R-devel/library/base/html/matmult.html
## I set the row and column names so the output is easier to interpret (using the rownames() and colnames() functions).
## Then, prices is shown.
prices = blends %*% costs
rownames(prices) = c("A", "B", "C")
colnames(prices) = "Prices"
prices

#  b) 10 blends of Type A, 4 blends of Type B and 5 blends of Type C are calculated by summing the prices matrix with the amounts of each blend (in vector form).
sum(prices * c(10, 4, 5))