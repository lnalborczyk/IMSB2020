## ----setup, include = FALSE-----------------------------------------------------------------------------------------
library(tidyverse)
library(plotly)
library(knitr)
library(BEST)
library(brms)

# setting up knitr options
opts_chunk$set(
  cache = TRUE, echo = TRUE, warning = FALSE, message = FALSE,
  fig.align = "center", dev = "svg"
  )

prior.color <- "steelBlue"
likelihood.color <- "orangered"
posterior.color <- "magenta4"


## ---- eval = TRUE, echo = FALSE-------------------------------------------------------------------------------------
# from https://plotly.com/r/3d-surface-plots/

z <- c(
  c(8.83,8.89,8.81,8.87,8.9,8.87),
  c(8.89,8.94,8.85,8.94,8.96,8.92),
  c(8.84,8.9,8.82,8.92,8.93,8.91),
  c(8.79,8.85,8.79,8.9,8.94,8.92),
  c(8.79,8.88,8.81,8.9,8.95,8.92),
  c(8.8,8.82,8.78,8.91,8.94,8.92),
  c(8.75,8.78,8.77,8.91,8.95,8.92),
  c(8.8,8.8,8.77,8.91,8.95,8.94),
  c(8.74,8.81,8.76,8.93,8.98,8.99),
  c(8.89,8.99,8.92,9.1,9.13,9.11),
  c(8.97,8.97,8.91,9.09,9.11,9.11),
  c(9.04,9.08,9.05,9.25,9.28,9.27),
  c(9,9.01,9,9.2,9.23,9.2),
  c(8.99,8.99,8.98,9.18,9.2,9.19),
  c(8.93,8.97,8.97,9.18,9.2,9.18)
  )

dim(z) <- c(15, 6)
# z2 <- z + 1
# z3 <- z - 1

fig <- plot_ly(showscale = FALSE)
fig <- fig %>% add_surface(z = ~z)
# fig <- fig %>% add_surface(z = ~z2, opacity = 0.98)
# fig <- fig %>% add_surface(z = ~z3, opacity = 0.98)

# exporting it to an html object
# orca(fig, file = "figures/plotly.png")
htmlwidgets::saveWidget(fig, file = "plotly1.html")


## ----metropolis_picture, echo = FALSE, out.width = "20%"------------------------------------------------------------
knitr::include_graphics("figures/Nicholas_Metropolis_cropped.png")


## ----pi_gif, echo = FALSE, out.width = "25%"------------------------------------------------------------------------
knitr::include_graphics("figures/Pi_30K.gif")


## ----pi1, eval = TRUE, echo = TRUE, out.width = "25%"---------------------------------------------------------------
trials <- 1e5
radius <- 1
x <- runif(n = trials, min = 0, max = radius)
y <- runif(n = trials, min = 0, max = radius)
distance <- sqrt(x^2 + y^2)
inside <- distance < radius
pi_estimate <- 4 * sum(inside) / trials


## ----pi2, eval = TRUE, echo = FALSE, out.width = "33%"--------------------------------------------------------------
ggplot(data.frame(x, y, inside), aes(x, y, color = inside) ) +
  theme_bw(base_size = 20) +
  ggtitle(paste(round(trials), "Trials,", "Estimate =", pi_estimate) ) +
  guides(color = FALSE) +
  geom_point(size = 1 / trials)


## ----simulated_annealing, echo = FALSE, out.width = "50%"-----------------------------------------------------------
knitr::include_graphics("figures/Hill_Climbing_with_Simulated_Annealing.gif")


## ---- eval = TRUE, echo = FALSE-------------------------------------------------------------------------------------
# from https://plotly.com/r/3d-surface-plots/

z <- c(
  c(8.83,8.89,8.81,8.87,8.9,8.87),
  c(8.89,8.94,8.85,8.94,8.96,8.92),
  c(8.84,8.9,8.82,8.92,8.93,8.91),
  c(8.79,8.85,8.79,8.9,8.94,8.92),
  c(8.79,8.88,8.81,8.9,8.95,8.92),
  c(8.8,8.82,8.78,8.91,8.94,8.92),
  c(8.75,8.78,8.77,8.91,8.95,8.92),
  c(8.8,8.8,8.77,8.91,8.95,8.94),
  c(8.74,8.81,8.76,8.93,8.98,8.99),
  c(8.89,8.99,8.92,9.1,9.13,9.11),
  c(8.97,8.97,8.91,9.09,9.11,9.11),
  c(9.04,9.08,9.05,9.25,9.28,9.27),
  c(9,9.01,9,9.2,9.23,9.2),
  c(8.99,8.99,8.98,9.18,9.2,9.19),
  c(8.93,8.97,8.97,9.18,9.2,9.18)
  )

dim(z) <- c(15, 6)
z2 <- z * 3 - 15

fig <- plot_ly(showscale = FALSE)
fig <- fig %>% add_surface(z = ~z)
fig <- fig %>% add_surface(z = ~z2, opacity = 0.98)

# exporting it to an html object
htmlwidgets::saveWidget(fig, file = "plotly2.html")


## ----distribution_theta1, echo = FALSE, fig.width = 12, fig.height = 6, out.width = "75%"---------------------------
# knitr::include_graphics("figures/distributionTheta1-7.png")

theta <- c(1, 2, 3, 4, 5, 6, 7)

theta %>%
  data.frame %>%
  ggplot(aes(x = theta, y = theta) ) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_bw(base_size = 20) +
  labs(x = expression(theta), y = expression(paste(p, "(", theta, ")") ) ) +
  scale_x_continuous(breaks = 1:7)


## ----distribution_theta2, echo = FALSE, out.width = "50%"-----------------------------------------------------------
knitr::include_graphics("figures/DistribCarré1-7.png")


## ---- eval = TRUE, echo = FALSE, fig.width = 25---------------------------------------------------------------------
source("code/IMSB_binomial.R")

set.seed(1789)

trajLength <- 100
theta <- 1:7
ptheta <- theta
trajectory <- sample(theta, prob = ptheta, size = trajLength, replace = TRUE)

layout(matrix(1:2, ncol = 2), widths = c(.75, .25) )

plot(
  trajectory,
  main = "Distribution postérieure basée sur 100 tirages",
  ylab = bquote(theta), xlim = c(0, trajLength), xlab = "Nombre d'itérations",
  type = "o", pch = 20, col = posterior.color, cex.lab = 2, cex.main = 3, cex.axis = 2
  )

barplot(table(trajectory), col = posterior.color, horiz = TRUE, axes = FALSE, axisnames = FALSE)


## ---- echo = FALSE, out.width = "75%"-------------------------------------------------------------------------------
knitr::include_graphics("figures/DiscretMCMC_1.png")


## ---- echo = FALSE, out.width = "75%"-------------------------------------------------------------------------------
knitr::include_graphics("figures/DiscretMCMC_2.png")


## ---- echo = FALSE, out.width = "75%"-------------------------------------------------------------------------------
knitr::include_graphics("figures/DiscretMCMC_3.png")


## ---- echo = FALSE, out.width = "75%"-------------------------------------------------------------------------------
knitr::include_graphics("figures/DiscretMCMC_4.png")


## ----metropolis1, eval = TRUE, echo = FALSE, fig.width = 25, fig.height = 6, fig.align = "center"-------------------
source("code/IMSB_binomial.R")
set.seed(1789)

trajLength <- 250
theta <- 1:7
ptheta <- theta
trajectory <- sample(theta, prob = ptheta, size = trajLength, replace = TRUE)

layout(matrix(1:2, ncol = 2), widths = c(.75, .25) )

plot(
  trajectory,
  main = "Méthode Monte Carlo",
  ylab = bquote(theta), xlim = c(0, trajLength), xlab = "Nombre d'itérations",
  type = "o", pch = 20, col = prior.color, cex.lab = 2, cex.main = 3, cex.axis = 2
  )

barplot(table(trajectory), col = prior.color, horiz = TRUE, axes = FALSE, axisnames = FALSE)


## ----metropolis2, eval = TRUE, echo = FALSE, fig.width = 25, fig.height = 6, fig.align = "center"-------------------
source("code/DBDA2E-utilities.R")
set.seed(1789)

nextPosition = function(currentPosition) {
  
  # flip coin to generate proposal
  proposal <- currentPosition + sample( c(-1, 1), size = 1)
  
  # now make sure he loops around the archipelago
  if ( proposal < 1 ) proposal = 1
  if ( proposal > 7 ) proposal = 7
  
  # move?
  prob_move <- min(1.0, proposal / currentPosition)
  result <- ifelse(runif(1) < prob_move, proposal, currentPosition)
  
  return(result)
  
}

positionLength <- 250
trajectory <- rep(0, positionLength)
trajectory[1] <- 1

for (ii in 1:(positionLength - 1) ) {
  
        trajectory[ii  +1] = nextPosition(trajectory[ii])
        
}

idxToPlot <- 1:positionLength
layout(matrix(1:2, ncol = 2), widths = c(.75, .25) )

# plot histogram
plot(
  idxToPlot, trajectory[idxToPlot],
  main = "Algorithme Metropolis",
  ylab = bquote(theta), xlim = c(0, positionLength), xlab = "Nombre d'itérations",
  type = "o", pch = 20, col = prior.color, cex.lab = 2, cex.main = 3, cex.axis = 2
  )

# barplot
barplot(table(trajectory[idxToPlot]), col = prior.color, horiz = TRUE, axes = FALSE, axisnames = FALSE)


## ---- echo = FALSE, out.width = "75%"-------------------------------------------------------------------------------
knitr::include_graphics("figures/MetroAlgoAcceptProposal.png")


## ----eval = TRUE, echo = FALSE, fig.width = 8, fig.height = 8-------------------------------------------------------
source("code/DBDA2E-utilities.R")

# specifies the data to be used in the likelihood function
myData <- c(rep(0, 6), rep(1, 14) )

# defines the Bernoulli likelihood function p(D|theta)
# the argument theta could be a vector, not just a scalar

likelihood <- function(theta, data) {
  
  z <- sum(data)
  N <- length(data)
  pDataGivenTheta <- theta^z * (1 - theta)^(N - z)
  
  # the theta values passed into this function are generated at random,
  # and therefore might be inadvertently greater than 1 or less than 0.
  # the likelihood for theta > 1 or for theta < 0 is zero:
  
  pDataGivenTheta[theta > 1 | theta < 0] = 0
  
  return(pDataGivenTheta)
  
}

# defines the prior density function

prior <- function(theta) {
  
  pTheta <- dbeta(theta, 1, 1)
  
  # the theta values passed into this function are generated at random,
  # and therefore might be inadvertently greater than 1 or less than 0.
  # the prior for theta > 1 or for theta < 0 is zero:
  
  pTheta[theta > 1 | theta < 0] = 0
  
  return(pTheta)
  
}

# defines the relative probability of the target distribution, 
# as a function of vector theta. For our application, this
# target distribution is the unnormalized posterior distribution.

targetRelProb <- function(theta, data) {
  
  targetRelProb <- likelihood(theta, data) * prior(theta)
  
  return(targetRelProb)
  
}

# specifies the length of the trajectory, that is, the number of jumps to try
trajLength <- 50000 # arbitrary large number

# initialises the vector that will store the results:
trajectory <- rep(0 , trajLength)

# specifies where to start the trajectory
trajectory[1] <- 0.01 # arbitrary value

# specifies the burn-in period
burnIn <- ceiling(0.0 * trajLength) # arbitrary number, less than trajLength

# initialises accepted, rejected counters, just to monitor performance:
nAccepted <- 0
nRejected <- 0

# now generate the random walk. The 't' index is time or trial in the walk.
# specifies seed to reproduce same random walk:
set.seed(47405)

# specifies standard deviation of proposal distribution
proposalSD <- c(0.02, 0.2, 2.0)[2]

for (t in 1:(trajLength - 1) ) {
  
	currentPosition <- trajectory[t]
	
	# uses the proposal distribution to generate a proposed jump
	
	proposedJump <- rnorm(1, mean = 0, sd = proposalSD)
	
	# computes the probability of accepting the proposed jump
	
	probAccept <- min(
	  1,
		targetRelProb(currentPosition + proposedJump, myData) / targetRelProb(currentPosition, myData)
		)
	
	# generates a random uniform value from the interval [0,1] to
	# decide whether or not to accept the proposed jump
	
	if (runif(1) < probAccept) {
	  
		# accept the proposed jump
		trajectory[t + 1] <- currentPosition + proposedJump
		
		# increment the accepted counter, just to monitor performance
		if (t > burnIn) {nAccepted = nAccepted + 1}
		
	} else {
	  
		# rejects the proposed jump, stay at current position
		trajectory[t + 1] = currentPosition
		
		# increments the rejected counter, just to monitor performance
		if (t > burnIn) {nRejected = nRejected + 1}
	
	}
	
}

# extracts the post-burnIn portion of the trajectory
acceptedTraj <- trajectory[ (burnIn+1) : length(trajectory) ]

##########################################
# Display the chain
###################################

# layout(matrix(1:3, nrow = 3) )
# par(mar = c(3, 4, 2, 1), mgp = c(2, 0.7, 0) )

# layout(matrix(c(1,3,2,3), 2, 2, byrow = TRUE) )
layout(matrix(c(1, 2, 3, 3), 2, 2, byrow = TRUE) )

# trajectory, a.k.a. trace plot, beginning of chain
idxToPlot <- 1:100

plot(
  trajectory[idxToPlot], idxToPlot, main = "Beginning of Chain",
  xlab = bquote(theta), xlim = c (0, 1), ylab = "Step in Chain",
  type = "o", pch = 20, col = posterior.color, cex.lab = 1.5
  )

# indicates burn in limit (might not be visible if not in range)
if (burnIn > 0) {
  
  abline(h = burnIn, lty = "dotted")
  text(0.5, burnIn + 1, "Burn In", adj = c(0.5, 1.1) )
  
}

# trajectory, a.k.a. trace plot, end of chain
idxToPlot <- (trajLength - 100):trajLength

plot(
  trajectory[idxToPlot], idxToPlot, main = "End of Chain",
  xlab = bquote(theta), xlim = c(0, 1), ylab = "Step in Chain",
  type = "o", pch = 20, col = posterior.color, cex.lab = 1.5
  )

# displays proposal SD and acceptance ratio in the plot
text(
  0.0, trajLength, adj = c(0.0, 1.1), cex = 1.5,
  labels = bquote(
    frac(N[acc], N[pro]) == .(signif(nAccepted / length(acceptedTraj), 3) )
    )
  )

# posterior histogram
paramInfo <- plotPost(
  acceptedTraj, xlim = c(0, 1), xlab = bquote(theta), 
  cex = 2, cex.main = 1.5, col = posterior.color,
  main = bquote(list(
    "Proposal SD" == .(proposalSD),
    "ESS" == .(round(effectiveSize(acceptedTraj), 1) )
    ) )
  )


## ----eval = TRUE, echo = FALSE, fig.width = 8, fig.height = 8-------------------------------------------------------
source("code/DBDA2E-utilities.R")

# specifies the data to be used in the likelihood function
myData <- c(rep(0, 6), rep(1, 14) )

# defines the Bernoulli likelihood function p(D|theta)
# the argument theta could be a vector, not just a scalar

likelihood <- function(theta, data) {
  
  z <- sum(data)
  N <- length(data)
  pDataGivenTheta <- theta^z * (1 - theta)^(N - z)
  
  # the theta values passed into this function are generated at random,
  # and therefore might be inadvertently greater than 1 or less than 0.
  # the likelihood for theta > 1 or for theta < 0 is zero:
  
  pDataGivenTheta[theta > 1 | theta < 0] = 0
  
  return(pDataGivenTheta)
  
}

# defines the prior density function

prior <- function(theta) {
  
  pTheta <- dbeta(theta, 1, 1)
  
  # the theta values passed into this function are generated at random,
  # and therefore might be inadvertently greater than 1 or less than 0.
  # the prior for theta > 1 or for theta < 0 is zero:
  
  pTheta[theta > 1 | theta < 0] = 0
  
  return(pTheta)
  
}

# defines the relative probability of the target distribution, 
# as a function of vector theta. For our application, this
# target distribution is the unnormalized posterior distribution.

targetRelProb <- function(theta, data) {
  
  targetRelProb <- likelihood(theta, data) * prior(theta)
  
  return(targetRelProb)
  
}

# specifies the length of the trajectory, that is, the number of jumps to try
trajLength <- 50000 # arbitrary large number

# initialises the vector that will store the results:
trajectory <- rep(0 , trajLength)

# specifies where to start the trajectory
trajectory[1] <- 0.01 # arbitrary value

# specifies the burn-in period
burnIn <- ceiling(0.0 * trajLength) # arbitrary number, less than trajLength

# initialises accepted, rejected counters, just to monitor performance:
nAccepted <- 0
nRejected <- 0

# now generate the random walk. The 't' index is time or trial in the walk.
# specifies seed to reproduce same random walk:
set.seed(47405)

# specifies standard deviation of proposal distribution
proposalSD <- c(0.02, 0.2, 2.0)[1]

for (t in 1:(trajLength - 1) ) {
  
	currentPosition <- trajectory[t]
	
	# uses the proposal distribution to generate a proposed jump
	
	proposedJump <- rnorm(1, mean = 0, sd = proposalSD)
	
	# computes the probability of accepting the proposed jump
	
	probAccept <- min(
	  1,
		targetRelProb(currentPosition + proposedJump, myData) / targetRelProb(currentPosition, myData)
		)
	
	# generates a random uniform value from the interval [0,1] to
	# decide whether or not to accept the proposed jump
	
	if (runif(1) < probAccept) {
	  
		# accept the proposed jump
		trajectory[t + 1] <- currentPosition + proposedJump
		
		# increment the accepted counter, just to monitor performance
		if (t > burnIn) {nAccepted = nAccepted + 1}
		
	} else {
	  
		# rejects the proposed jump, stay at current position
		trajectory[t + 1] = currentPosition
		
		# increments the rejected counter, just to monitor performance
		if (t > burnIn) {nRejected = nRejected + 1}
	
	}
	
}

# extracts the post-burnIn portion of the trajectory
acceptedTraj <- trajectory[ (burnIn+1) : length(trajectory) ]

##########################################
# Display the chain
###################################

# layout(matrix(1:3, nrow = 3) )
# par(mar = c(3, 4, 2, 1), mgp = c(2, 0.7, 0) )

# layout(matrix(c(1,3,2,3), 2, 2, byrow = TRUE) )
layout(matrix(c(1, 2, 3, 3), 2, 2, byrow = TRUE) )

# trajectory, a.k.a. trace plot, beginning of chain
idxToPlot <- 1:100

plot(
  trajectory[idxToPlot], idxToPlot, main = "Beginning of Chain",
  xlab = bquote(theta), xlim = c (0, 1), ylab = "Step in Chain",
  type = "o", pch = 20, col = posterior.color, cex.lab = 1.5
  )

# indicates burn in limit (might not be visible if not in range)
if (burnIn > 0) {
  
  abline(h = burnIn, lty = "dotted")
  text(0.5, burnIn + 1, "Burn In", adj = c(0.5, 1.1) )
  
}

# trajectory, a.k.a. trace plot, end of chain
idxToPlot <- (trajLength - 100):trajLength

plot(
  trajectory[idxToPlot], idxToPlot, main = "End of Chain",
  xlab = bquote(theta), xlim = c(0, 1), ylab = "Step in Chain",
  type = "o", pch = 20, col = posterior.color, cex.lab = 1.5
  )

# displays proposal SD and acceptance ratio in the plot
text(
  0.0, trajLength, adj = c(0.0, 1.1), cex = 1.5,
  labels = bquote(
    frac(N[acc], N[pro]) == .(signif(nAccepted / length(acceptedTraj), 3) )
    )
  )

# posterior histogram
paramInfo <- plotPost(
  acceptedTraj, xlim = c(0, 1), xlab = bquote(theta), 
  cex = 2, cex.main = 1.5, col = posterior.color,
  main = bquote(list(
    "Proposal SD" == .(proposalSD),
    "ESS" == .(round(effectiveSize(acceptedTraj), 1) )
    ) )
  )


## ----eval = TRUE, echo = FALSE, fig.width = 8, fig.height = 8-------------------------------------------------------
source("code/DBDA2E-utilities.R")

# specifies the data to be used in the likelihood function
myData <- c(rep(0, 6), rep(1, 14) )

# defines the Bernoulli likelihood function p(D|theta)
# the argument theta could be a vector, not just a scalar

likelihood <- function(theta, data) {
  
  z <- sum(data)
  N <- length(data)
  pDataGivenTheta <- theta^z * (1 - theta)^(N - z)
  
  # the theta values passed into this function are generated at random,
  # and therefore might be inadvertently greater than 1 or less than 0.
  # the likelihood for theta > 1 or for theta < 0 is zero:
  
  pDataGivenTheta[theta > 1 | theta < 0] = 0
  
  return(pDataGivenTheta)
  
}

# defines the prior density function

prior <- function(theta) {
  
  pTheta <- dbeta(theta, 1, 1)
  
  # the theta values passed into this function are generated at random,
  # and therefore might be inadvertently greater than 1 or less than 0.
  # the prior for theta > 1 or for theta < 0 is zero:
  
  pTheta[theta > 1 | theta < 0] = 0
  
  return(pTheta)
  
}

# defines the relative probability of the target distribution, 
# as a function of vector theta. For our application, this
# target distribution is the unnormalized posterior distribution.

targetRelProb <- function(theta, data) {
  
  targetRelProb <- likelihood(theta, data) * prior(theta)
  
  return(targetRelProb)
  
}

# specifies the length of the trajectory, that is, the number of jumps to try
trajLength <- 50000 # arbitrary large number

# initialises the vector that will store the results:
trajectory <- rep(0 , trajLength)

# specifies where to start the trajectory
trajectory[1] <- 0.01 # arbitrary value

# specifies the burn-in period
burnIn <- ceiling(0.0 * trajLength) # arbitrary number, less than trajLength

# initialises accepted, rejected counters, just to monitor performance:
nAccepted <- 0
nRejected <- 0

# now generate the random walk. The 't' index is time or trial in the walk.
# specifies seed to reproduce same random walk:
set.seed(47405)

# specifies standard deviation of proposal distribution
proposalSD <- c(0.02, 0.2, 2.0)[3]

for (t in 1:(trajLength - 1) ) {
  
	currentPosition <- trajectory[t]
	
	# uses the proposal distribution to generate a proposed jump
	
	proposedJump <- rnorm(1, mean = 0, sd = proposalSD)
	
	# computes the probability of accepting the proposed jump
	
	probAccept <- min(
	  1,
		targetRelProb(currentPosition + proposedJump, myData) / targetRelProb(currentPosition, myData)
		)
	
	# generates a random uniform value from the interval [0,1] to
	# decide whether or not to accept the proposed jump
	
	if (runif(1) < probAccept) {
	  
		# accept the proposed jump
		trajectory[t + 1] <- currentPosition + proposedJump
		
		# increment the accepted counter, just to monitor performance
		if (t > burnIn) {nAccepted = nAccepted + 1}
		
	} else {
	  
		# rejects the proposed jump, stay at current position
		trajectory[t + 1] = currentPosition
		
		# increments the rejected counter, just to monitor performance
		if (t > burnIn) {nRejected = nRejected + 1}
	
	}
	
}

# extracts the post-burnIn portion of the trajectory
acceptedTraj <- trajectory[ (burnIn+1) : length(trajectory) ]

##########################################
# Display the chain
###################################

# layout(matrix(1:3, nrow = 3) )
# par(mar = c(3, 4, 2, 1), mgp = c(2, 0.7, 0) )

# layout(matrix(c(1,3,2,3), 2, 2, byrow = TRUE) )
layout(matrix(c(1, 2, 3, 3), 2, 2, byrow = TRUE) )

# trajectory, a.k.a. trace plot, beginning of chain
idxToPlot <- 1:100

plot(
  trajectory[idxToPlot], idxToPlot, main = "Beginning of Chain",
  xlab = bquote(theta), xlim = c (0, 1), ylab = "Step in Chain",
  type = "o", pch = 20, col = posterior.color, cex.lab = 1.5
  )

# indicates burn in limit (might not be visible if not in range)
if (burnIn > 0) {
  
  abline(h = burnIn, lty = "dotted")
  text(0.5, burnIn + 1, "Burn In", adj = c(0.5, 1.1) )
  
}

# trajectory, a.k.a. trace plot, end of chain
idxToPlot <- (trajLength - 100):trajLength

plot(
  trajectory[idxToPlot], idxToPlot, main = "End of Chain",
  xlab = bquote(theta), xlim = c(0, 1), ylab = "Step in Chain",
  type = "o", pch = 20, col = posterior.color, cex.lab = 1.5
  )

# displays proposal SD and acceptance ratio in the plot
text(
  0.0, trajLength, adj = c(0.0, 1.1), cex = 1.5,
  labels = bquote(
    frac(N[acc], N[pro]) == .(signif(nAccepted / length(acceptedTraj), 3) )
    )
  )

# posterior histogram
paramInfo <- plotPost(
  acceptedTraj, xlim = c(0, 1), xlab = bquote(theta), 
  cex = 2, cex.main = 1.5, col = posterior.color,
  main = bquote(list(
    "Proposal SD" == .(proposalSD),
    "ESS" == .(round(effectiveSize(acceptedTraj), 1) )
    ) )
  )


## ----metropolis-hastings1, eval = TRUE, echo = TRUE-----------------------------------------------------------------
target <- function(x) {
  # target distribution is Exponential(1)
  # implementation from https://stephens999.github.io/fiveMinuteStats/MH-examples1.html
  
  if (x < 0) {
    return(0)
    }
  else {
    return(exp(-x) )
    }
}

metropolis_hastings <- function(niter, startval, proposalsd) {
  
  x <- rep(0, niter) # initialises the chain vector
  x[1] <- startval # defines the starting value
  
  for(i in 2:niter) {
    currentx <- x[i - 1] # retrieves current value of the parameter
    proposedx <- rnorm(1, mean = currentx, sd = proposalsd) # proposed move
    A <- target(proposedx) / target(currentx) # probability ratio
    if (runif(1) < A) {
      x[i] = proposedx # accept move with probability min(1, A)
      } else {
        x[i] = currentx # otherwise "reject" move, and stay where we are
        }
  }
  return(x)
}


## ----metropolis-hastings2, eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 5---------------------------------
z1 <- metropolis_hastings(niter = 1000, startval = 3, proposalsd = 1)
z2 <- metropolis_hastings(niter = 1000, startval = 3, proposalsd = 1)

data.frame(z1 = z1, z2 = z2) %>%
  pivot_longer(cols = z1:z2) %>%
  rownames_to_column() %>%
  mutate(rowname = as.numeric(rowname) ) %>%
  ggplot(aes(x = rowname, y = value, colour = name) ) +
  geom_line(show.legend = FALSE) +
  theme_bw(base_size = 20) +
  labs(x = "Nombre d'itérations", y = expression(theta) )


## ----metropolis-hastings3, eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 5---------------------------------
data.frame(z1 = z1, z2 = z2) %>%
  pivot_longer(cols = z1:z2) %>%
  rownames_to_column() %>%
  mutate(rowname = as.numeric(rowname) ) %>%
  ggplot(aes(x = value) ) +
  geom_histogram() +
  facet_wrap(~name) +
  theme_bw(base_size = 20) +
  labs(x = expression(theta), y = "Nombre d'échantillons")


## ----bivariate_prior, echo = FALSE, out.width = "50%"---------------------------------------------------------------
knitr::include_graphics("figures/BayesianInferenceBivariate_PRIOR.png")


## ----bivariate, echo = FALSE, out.width = "33%"---------------------------------------------------------------------
knitr::include_graphics("figures/BayesianInferenceBivariate.png")


## ----metropolis-2d, echo = FALSE, out.width = "75%"-----------------------------------------------------------------
knitr::include_graphics("figures/MetroAlgo2Var.png")


## ----gibbs1, echo = FALSE, out.width = "25%"------------------------------------------------------------------------
knitr::include_graphics("figures/MetroAlgoVarByVar.png")


## ----gibbs2, echo = FALSE, out.width = "75%"------------------------------------------------------------------------
knitr::include_graphics("figures/MetroAlgo2VarResults.png")


## ----gibbs2_code1, echo = TRUE--------------------------------------------------------------------------------------
# code from https://stats.stackexchange.com/questions/266665/gibbs-sampler-examples-in-r
n <- 30 # sample size
ybar <- 15 # sample mean
s2 <- 3 # sample variance

mu <- rep(NA, 11000) # initialises mu vector
tau <- rep(NA, 11000) # initialises tau vector

burn <- 1000 # burnin period
tau[1] <- 1 # initialisation value for tau

# samples from the joint posterior (mu, tau | data)
for(i in 2:11000) {
  
  mu[i]  <- rnorm(n = 1, mean = ybar, sd = sqrt(1 / (n * tau[i - 1]) ) )    
  tau[i] <- rgamma(n = 1, shape = n / 2, scale = 2 / ((n - 1) * s2 + n * (mu[i] - ybar)^2) )
  
}

mu  <- mu[-(1:burn)] # removes burnin
tau <- tau[-(1:burn)] # removes burnin


## ----gibbs2_code2, echo = TRUE, fig.width = 14, out.width = "66%"---------------------------------------------------
data.frame(mu = mu, tau = tau) %>%
  pivot_longer(cols = mu:tau) %>%
  ggplot(aes(x = value) ) +
  geom_histogram() +
  facet_wrap(~name, scales = "free") +
  theme_bw(base_size = 20) +
  labs(x = "Valeur du paramètre", y = "Nombre d'échantillons")


## ----gibbs_error, echo = FALSE, out.width = "50%"-------------------------------------------------------------------
knitr::include_graphics("figures/Gibbs Error.png")


## ----hmc1, echo = FALSE, out.width = "50%"--------------------------------------------------------------------------
knitr::include_graphics("figures/HMC alorithme.png")


## ----hmc_erreur, echo = FALSE, out.width = "50%"--------------------------------------------------------------------
knitr::include_graphics("figures/HMC alorithme ERREUR1.png")


## ----hmc_erreur2, echo = FALSE, out.width = "50%"-------------------------------------------------------------------
knitr::include_graphics("figures/HMC alorithme ERREUR2.png")


## ----repres1, echo = FALSE, out.width = "50%"-----------------------------------------------------------------------
knitr::include_graphics("figures/Verif_representativité1.png")


## ----repres2, echo = FALSE, out.width = "50%"-----------------------------------------------------------------------
knitr::include_graphics("figures/Verif_representativité2.png")


## ----repres3, echo = FALSE, out.width = "50%"-----------------------------------------------------------------------
knitr::include_graphics("figures/Verif_representativité3.png")


## ----autocorrelation, echo = FALSE, out.width = "40%"---------------------------------------------------------------
knitr::include_graphics("figures/Verif_autocorrelation.png")


## ----repres4, echo = FALSE, out.width = "50%"-----------------------------------------------------------------------
knitr::include_graphics("figures/Verif_representativité4.png")


## ----repres5, echo = FALSE, out.width = "50%"-----------------------------------------------------------------------
knitr::include_graphics("figures/Verif_representativité5.png")


## ----diagnostics1, eval = TRUE, echo = TRUE, results = "hide"-------------------------------------------------------
library(rethinking)
library(tidyverse)
library(brms)

data(Howell1)
d <- Howell1
d2 <- d %>% filter(age >= 18)

priors <- c(
  set_prior("normal(150, 20)", class = "Intercept"),
  set_prior("normal(0, 10)", class = "b"),
  set_prior("exponential(0.01)", class = "sigma")
  )

mod1 <- brm(
  height ~ 1 + weight,
  prior = priors,
  family = gaussian(),
  data = d2
  )


## ----diagnostics2, eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6-----------------------------------------
# combo can be hist, dens, dens_overlay, trace, trace_highlight...
# cf. https://mc-stan.org/bayesplot/reference/MCMC-overview.html

plot(
  x = mod1, combo = c("dens_overlay", "trace"),
  theme = theme_bw(base_size = 20)
  )


## ----diagnostics3, eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6-----------------------------------------
library(bayesplot)
post <- posterior_samples(mod1, add_chain = TRUE)

post %>%
  mcmc_acf(pars = vars(b_Intercept:sigma), lags = 10) +
  theme_bw(base_size = 20)


## ----diagnostics4, eval = TRUE, echo = TRUE-------------------------------------------------------------------------
summary(mod1)


## ----diagnostics5, eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6-----------------------------------------
post %>% # rank plots
  mcmc_rank_overlay(pars = vars(b_Intercept:sigma) ) +
  labs(x = "Rang", y = "Fréquence") +
  theme_bw(base_size = 20) +
  coord_cartesian(ylim = c(25, NA) )


## ----rugged, eval = TRUE, echo = TRUE-------------------------------------------------------------------------------
library(rethinking)
library(tidyverse)

data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
df1 <- d[complete.cases(d$rgdppc_2000), ]
str(df1[, 1:5])


## ----mod2, eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------------------
priors2 <- c(
  set_prior("normal(0, 100)", class = "Intercept"),
  set_prior("normal(0, 10)", class = "b"),
  set_prior("exponential(0.01)", class = "sigma")
  )

mod2 <- brm(
  log_gdp ~ 1 + rugged * cont_africa,
  prior = priors2,
  family = gaussian(),
  data = df1
  )


## ----mod2-summary, eval = TRUE, echo = TRUE-------------------------------------------------------------------------
summary(mod2)


## ----mod2-diagnostics, eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6-------------------------------------
plot(
  x = mod2, combo = c("dens_overlay", "trace"), pars = "^b_",
  theme = theme_bw(base_size = 16)
  )


## ----mod2-pairs, eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 7.5-----------------------------------------
pairs(mod2)

