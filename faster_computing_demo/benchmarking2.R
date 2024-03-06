
rm(list = ls())
graphics.off()


set.seed(1) # for reproducibility
geom_growth_base <- function(N0 = 2,
                             T = 999,
                             lambda = 1.01,
                             sigma = 0.2){
  Nvals <- vector('numeric') # initiate a place to put the values
  Nvals[1] <- N0
  for (t in 1:T){
    Nvals[t+1] <- Nvals[t]*(lambda*exp(rnorm(1,0,sigma)))
  }
  return(Nvals)
}

# Benchmark with microbenchamark
library(microbenchmark)

comp <- microbenchmark(TS_009 = {geom_growth_base(T = 9)},
                       TS_099 = {geom_growth_base(T = 99)},
                       TS_999 = {geom_growth_base(T = 999)})
comp
comp %>% data.frame()

# Plot
library(ggplot2)
autoplot(comp)


set.seed(1) # for reproducibility
geom_growth_preallocated <- function(N0 = 2,
                                     T = 999,
                                     lambda = 1.01,
                                     sigma = 0.2){
  Nvals <- vector('numeric', length = T+1) # here's the only change
  Nvals[1] <- N0
  for (i in 1:T){
    Nvals[i+1] <- Nvals[i]*(lambda*exp(rnorm(1,0,sigma)))
  }
  return(Nvals)
}
# Compare the old and new simulation functions
comp <- microbenchmark(Old = {geom_growth_base(T = 9999)},
                       New = {geom_growth_preallocated(T = 9999)})
comp
autoplot(comp)
















