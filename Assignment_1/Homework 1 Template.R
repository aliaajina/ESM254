
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

