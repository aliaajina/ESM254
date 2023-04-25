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

# hint: ?matrix

increasing.mat <- function(tot.row, tot.col) {
  
  N <- tot.row*tot.col # use this local variable for the data argument of matrix()
  answer <- # use the matrix() function here
    
  return(answer)
}

increasing.mat(tot.row=4, tot.col=5)

#### Part 2

install.packages("MQMF")
library(MQMF)

## Section 1: growth curve function

growth.curve <- function(p, age.data) {
  
  Sim.Length.Data <- p[1] * (1 - exp(-p[2] * (age.data - p[3])))
  
  return(Sim.Length.Data)
}

# create an object that defines the p input to growth.curve()
pars <- c(75,0.1,-10.0,3.5) # these will be given to the p argument later
plabels <- c("Linf","K","t0","sigma")


# Section 2:
# let's load in a practice data set

data(minnow)
week <- minnow$week
length <- minnow$length

# plot the experimental data
plot(x=week,
     y=length,
     ylab="Body Length (mm)",
     xlab="Age (Weeks)",
     pch=19,
     main="Minnow")


# Let's look at an example for simulated data using some stand-in parameter values
example.sim.Length <- growth.curve(p=pars[1:3], age.data=week)
lines(x=week, y=example.sim.Length, lwd=2, col="darkgray")


# Section 3: 
# Let's use a statistical model to estimate parameter values for growth.curve() to use

best.growth.est <- nlm(f=negNLL,
                       p=pars,
                       funk=growth.curve,
                       age.data=week,
                       observed=length,  
                       typsize=magnitude(pars))  


# The estimated parameter values:
best.growth.est$estimate[1:3]

# The parameter values we used to draw a line earlier
pars[1:3]


# Section 4 - how well do the estimated parameter values perform?
predicted.Length <- growth.curve(best.growth.est$estimate,0:160)  
lines(x=0:160, y=predicted.Length, lwd=2, col="red")

