# Chapter 2

# loading data, number of seeds (j \in {0,1,2,...}) for different 50 species (i=1,2,...,50), [50x1]
load("kubobook_2012/distribution/data.RData")

# 2.1 get summary stat
summary(data); table(data); hist(data)
mean(data); var(data)
## * Data contains non-zero integers
## * starting from zero but no upper limit
## * mean and variance take close values
## --> Poission distribution

# 2.2, 2.3 Compare with poisson distribution 
hist<-hist(data)
prob <-dpois(0:9, 3.56) # browseURL('https://en.wikipedia.org/wiki/Poisson_distribution')
lines(0:9-0.5,prob*length(data)) # add pmf onto hist, -0.5 to adjust x axis (data point comes centre)

# 2.4 Maximum likelihood
logL <- function(lambda){
  p<- dpois(data, lambda) # you can include `log=T` in arg instead of `log(p)`
  sum(log(p)) # sum of likelihood (p) in log scale
}

lambda<- seq(2,5,0.1)  # possible values for lambda
plot(lambda, sapply(X=lambda, FUN=logL), type='l') # SAPPLY: applies FUN to X and returns vector (or mat)

max_ind <-which.max(sapply(lambda, logL)) # find where max is
lambda[max_ind]

## MC simulation (Fig 2.8, p.28)
k<-10000 # nr of sim
lambda_0<-3.5 # true lambda value
lambda_range=seq(2,5,0.05) # prediction range 
n<-50 # sample size
lambda_pred=numeric(k) # placeholder, stores pred value from ML

  
for (i in 1:k){
  # generate samples
  sample<- rpois(n, lambda_0)
  # get prediction for lambda
  # ML-value lambda can be computed by taking samole mean (see p.27)
  lambda_pred[i]<- mean(sample)
}

h <- hist(lambda_pred)
h$counts = h$counts/sum(h$counts)
plot(h)
curve()
