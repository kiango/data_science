### Random variables ###

## Compute the upper and lower emperical quantiles
comp.ci <- function(vec, quantile = 0.05){
  lower <- quantile/2.0
  upper <- 1.0 - lower
  c(quantile(vec, probs = lower, na.rm = TRUE),
    quantile(vec, probs = upper, na.rm = TRUE))
}

## function to plot and print a summary of the distribution
dist.summary <- function(dist, name, num.bins = 120){
  maxm <- max(dist)
  minm <- min(dist)
  bw <- (maxm - minm)/num.bins
  breaks <- seq(minm - bw/2, maxm + bw/2, by = bw)
  hist(dist, col = 'blue', breaks = breaks, xlab = name,
       main = paste('Distribution of ', name))
  
  std <- round(sd(dist), digits = 2)
  print(paste('Summary of', name, '; with std = ', std))
  print(summary(dist))
}

## Simulate Customers Using a Normal Distribution
sim.normal <- function(num, mean = 600, sd = 30){
  dist = rnorm(num, mean, sd)
  titl = paste('Normal: ', as.character(num), ' values')
  dist.summary(dist, titl)     
  print('Empirical 95% CIs')
  print(comp.ci(dist))
  NULL
}

## call the **sim.normal** function for 100, 1,000, 10,000, and 100,000 values.
nums <- c(100, 1000, 10000, 100000)
lapply(nums, sim.normal)

## Simulate Customers Using a Poisson Distribution
sim.poisson <- function(num, mean = 600){
  ## Simulate from a Poisson distribution
  dist = rpois(num, mean)
  titl = paste('Poisson: ', as.character(num), ' values')
  dist.summary(dist, titl)    
  print('Empirical 95% CIs')
  print(comp.ci(dist))
  NULL
}

## call it with 100000 valus:
sim.poisson(100000)







### Computing Specialized Random Variables ###

## Generates the profit from the uniform distribution
profits <- function(num){
  unif <- runif(num)
  ifelse(unif < 0.3, 5,
         ifelse(unif < 0.6, 3.5, 4))
}
## run it with 100000 values
prfts <- profits(100000)
dist.summary(prfts, 'profits')


## Compute a Distribution for Tips
## Generates the tips from the uniform distribution
tips <- function(num){
  unif <- runif(num)
  ifelse(unif < 0.5, 0, #50% give no tips
         ifelse(unif < 0.7, 0.25, #30% give .25$
                ifelse(unif < 0.9, 1, 2))) # 10% give 1$ or 2$
}
## call it:
tps <- tips(100000)
dist.summary(tps, 'tips')



### Create a Function to Simulate Lemonade Stand Income
## Simulate the profits and tips for a lemonade stand.
sim.lemonade <- function(num, mean = 600, sd = 30, pois = FALSE){
  ## pois: poisson distribution
  
  ## number of customer arrivals
  if(pois){
    arrivals <- rpois(num, mean)
  } else {
    arrivals <- rnorm(num, mean, sd) 
  }
  dist.summary(arrivals, 'customer arrivals per day')
  
  ## Compute distibution of average profit per arrival
  proft <- profits(num) 
  dist.summary(proft, 'profit per arrival')
  
  ## Total profits are profit per arrival times number of arrivals.
  total.profit <- arrivals * proft 
  dist.summary(total.profit, 'total profit per day')
  
  ## Compute distribution of average tips per arrival
  tps <- tips(num) 
  dist.summary(tps, 'tips per arrival')
  
  ## Compute average tips per day
  total.tips <- arrivals * tps
  dist.summary(total.tips, 'total tips per day')
  
  ## Compute total profits plus total tips and normalize.
  total.take <- total.profit + total.tips
  dist.summary(total.take, 'total net per day')
}

sim.lemonade(100000)