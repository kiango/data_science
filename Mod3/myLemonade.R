# normal distribution simulation function
sim_normal <- function(num){
  # num is number of normal distribution realization we simulate
  mean = 600
  std = 30 # standard deviation
  
  dist = rnorm(num, mean, std);
  title = paste('Normal distribution with ', as.character(num), ' values')
  dist_summary(dist,title)
  
  print('Emperical 95% CIs')
  print(compute_ci(dist))
  NULL
}

sim_poisson <- function(num, mean = 600){
  ## Simulate from a Poisson distribution
  dist = rpois(num, mean)
  titl = paste('Poisson distribution with ', as.character(num), ' values')
  dist_summary(dist, titl)    
  print('Empirical 95% CIs')
  print(compute_ci(dist))
  NULL
}


# compute lower and upper emperial quantiles
compute_ci <- function(vec, quantile = 0.05){
  lower = quantile/2.0
  upper = 1.0 - lower
  c(quantile(vec, probs = lower, na.rm = TRUE), quantile(vec, probs = upper, na.rm = TRUE))
}

# plot and print summary of the distribution
dist_summary <- function(dist, name, num_bins = 120){
  maxm = max(dist)
  minm = min(dist)
  bw = (maxm - minm) / num_bins
  breaks <- seq(minm-bw/2, maxm+bw/2, by=bw)
  
  hist(dist, col='blue', breaks = breaks, xlab = name, main = paste('Distribution of ', name))
  std = round(sd(dist), digits=2)
  print(paste('Summary of', name, '; with std = ', std))
  print(summary(dist))
}

# generates profits form uniform distribution
profits <- function(num){
  unif = runif(num)
  ifelse( unif<0.3, 5,
          ifelse( unif<0.6, 3.5, 4 ))
}

## Generates the tips from the uniform distribution
tips <- function(num){
  unif <- runif(num)
  ifelse(unif < 0.5, 0,
         ifelse(unif < 0.7, 0.25, 
                ifelse(unif < 0.9, 1, 2)))
}


## Simulate the profits and tips for a lemonade stand.
sim_lemonade <- function(num, mean = 600, sd = 30, pois = FALSE){
  
  ## number of customer arrivals
  if(pois){
    arrivals <- rpois(num, mean)
  } else {
    arrivals <- rnorm(num, mean, sd) 
  }
  dist_summary(arrivals, 'customer arrivals per day')
  
  ## Compute distibution of average profit per arrival
  proft <- profits(num) 
  dist_summary(proft, 'profit per arrival')
  
  ## Total profits are profit per arrival times number of arrivals.
  total.profit <- arrivals * proft 
  dist_summary(total.profit, 'total profit per day')
  
  ## Compute distribution of average tips per arrival
  tps <- tips(num) 
  dist_summary(tps, 'tips per arrival')
  
  ## Compute average tips per day
  total.tips <- arrivals * tps
  dist_summary(total.tips, 'total tips per day')
  
  ## Compute total profits plus total tips and normalize.
  total.take <- total.profit + total.tips
  dist_summary(total.take, 'total net per day')
}

