## Homework 1 Part 1
# Author: Alia Ajina

## Question 1
# Creating a vector X that is logical with a length of 10
x <- vector(mode = "logical", length=10)
# Filling in x with number values from 5:14
x <- (5:14) 
# Printing the new vector, x
x

## Question 2
# Identifying Y as a logical vector with a length of 100
Y <- vector(mode = "logical", length = 100) 
# Filling in the numbers in vector Y as a sequence from 2 to 200 every 2 numbers
Y <- seq(2,200, by=2)
# Writing a logical for Y values that are greater than or equal to 134, if so return as NA
logic.index1 <- Y >= 134
Y[logic.index1] <- NA
# Writing a logical for Y values that are less than or equal to 20; if so return as 0
logic.index2 <- Y <= 20
Y[logic.index2] <- 0
# Printing Y
Y

## Question 3
#' @return a function that sums the numbers from 1:100

sum100 <- function(){
  
  answer <- sum(1:100)
  
  return(answer)
}

# Printing function
sum100()

## Question 4
#' @param N random number chosen by user
#' @return sum of 1:N, N being chosen by function user

sumN <- function(N){
  
  answer <- sum(1:N)
  
  return(answer)
}

# Printing function with N as 100
sumN(N=100)
# Sum = 5,050; which is the same as sum(100) function, as expected

## Question 5
# Matrix function
#' @param tot.row total number of rows
#' @param tot.col total number of col
#' @return answer, a matrix by column

increasing.mat <- function(tot.row, tot.col) {

  N <- tot.row*tot.col # Local variable to use for the matrix
  answer <- matrix(data = (1:N), nrow = tot.row, ncol = tot.col, byrow = FALSE)
  
  return(answer)
}

increasing.mat(tot.row=4, tot.col=5)
