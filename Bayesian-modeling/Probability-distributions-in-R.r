
#######################################
# Binomial distribution
#######################################


## Probability density/value of obtaining x successes out of n bernoulli trials 

size <- 50
#xsim represents total number of correct responses
#Xsim is Binomially distributed
# f(x;p) 
  
xsim <- rbinom(n=10000,size=50,prob=0.9)
hist(xsim)

x <- 45

probs <- seq(from=0,to=1,length=10000)
lkl <- dbinom(x=450,size=500,prob=probs)
plot(probs,lkl)
## Likelihood function
x <- 45
n <- 500
p <- seq(from=0,to=1,length=1000)
likelihood <- dbinom(x=x,size=n,prob = p)
plot(p,likelihood)

## Probability of an event that the outcome occurs between 0 and q

pbinom(q=40,size=50,prob=0.9)

## Generate n random values for the random variable X 
## which has a Binomial distribution with underlying prob of success p

x <- rbinom(10000,size=100,prob=0.95)
hist(x,breaks=80:100)



#######################################
# Poisson distribution
#######################################


## Probability density/value of obtaining x occurrences of something given the underlying rate is lambda 

lambda <- 0.1
xsim <- rpois(n=10000,lambda=lambda)
hist(xsim,breaks=100)

x <- 50
lambda <- seq(from=0,to=100,length=10000)
likelihood <- dpois(x=50,lambda=lambda)
plot(lambda,likelihood)


dpois(x=74,lambda=50)
dpois(x=55,lambda=50)
dpois(x=200,lambda=50)

## Likelihood function
x <- 55
lambda <- seq(from=1, to=200,length=10000)
likelihood <- dpois(x=45,lambda=lambda)
plot(lambda,likelihood)


## Probability of an event that the outcome occurs between 0 and q
ppois(q=74,lambda=50)

## Generate n random values for the random variable X 
## which has a Poisson distribution with underlying rate lambda

rpois(n=1,lambda=50)
rpois(n=10,lambda=0.3)

x <- rpois(n=10000,lambda=0.7)
hist(x,breaks=0:10)

mean(x)

x <- rpois(n=10000,lambda=1.7)
hist(x,breaks=0:10)

x <- rpois(n=10000,lambda=3.3)
hist(x,breaks=0:20)


#######################################
# Normal distribution
#######################################
#X ~ N(100,10)

mu <- 100
sigma <- 10
xsim <- rnorm(n=10000,mean = mu, sd=sigma)
hist(xsim)

#Y ~ N(50,10)
# Z = X-Y
ysim <- rnorm(10000,50,10)

zsim <- xsim-ysim
hist(zsim)
var(zsim)
## Probability density of obtaining a value x when the mean is 'mean' and standard deviation is 'sd'

dnorm(x=74,mean=50,sd=10)

## Likelihood of obtaining a value 74
x <- 74
sd <- 10
mu <- seq(from=10,to=100,length=10000)
likelihood <- dnorm(x=x,mean=mu,sd=sd)
plot(mu,likelihood)

## Joint-likelihood of obtaining the values 74, 78, 79, 84, 65 in 5 trials
x <- c(74, 78, 79, 84, 65)
sd <- 10

## Joint-likelihood graph
mu <- seq(from=10,to=100,length=10000)
likelihood <- rep(NA,10000)
for(i in 1:10000){
  likelihood[i] <- prod(dnorm(x=x,mean=mu[i],sd=sd))
}

plot(mu,likelihood)
mean(x)


## Probability of an event that the outcome occurs between 0 and q
## Area under the probability density function curve between 0 and q

pnorm(q=74,mean=50,sd=10)

## the above value is also called a cumulative density.

## Generate n random values for the random variable X 
## which has a normal distribution

rnorm(n=5,mean=50,sd=10)
rnorm(n=5,mean=50,sd=20)
rnorm(n=5,mean=100,sd=10)

x <- rnorm(n=10000,mean=50,sd=10)
hist(x)


x <- rnorm(n=10000,mean=50,sd=20)
hist(x)

x <- rnorm(n=10000,mean=100,sd=10)
hist(x)


#######################################
# Beta distribution
#######################################
alpha <- 100
beta <- 1
xsim <- rbeta(n=10000,shape1=alpha,shape2=beta)
hist(xsim)

## Probability density of obtaining a value x when the shape1 parameter is alpha and shape2 parameter is 'beta'

dbeta(x=0.3,shape1 = 1,shape2 = 1)
dbeta(x=0.3,shape1 = 1,shape2 = 2)
dbeta(x=0.3,shape1 = 2,shape2 = 2)
dbeta(x=0.3,shape1 = 10,shape2 = 10)




## Probability of an event that the outcome occurs between 0 and q

pbeta(q=0.3,shape1 = 10,shape2 = 10)

## Generate n random values for the random variable X 
## which has a beta distribution

rbeta(n=5,shape1 = 2,shape2 = 2)
rbeta(n=5,shape1 = 20,shape2 = 20)
rbeta(n=5,shape1 = 200,shape2 = 200)

# MEAN: a/(a+b), MODE: (a-1)/(a+b-2)

x <- rbeta(n=1000,shape1=1,shape2=1)
hist(x)

x <- rbeta(n=10000,shape1=2,shape2=2)
hist(x)

x <- rbeta(n=10000,shape1=20,shape2=20)
hist(x)

x <- rbeta(n=10000,shape1=200,shape2=200)
hist(x)

x <- rbeta(n=10000,shape1=200,shape2=400)
hist(x)

x <- rbeta(n=10000,shape1=8,shape2=2)
hist(x)

p <- rbeta(n=10000,shape1=800,shape2=200)
hist(p)


#########################################

xsim <- rnorm(10000,mean = 5, sd = 0)
hist(xsim)

# data: sample of 5 heights from the population
xobs <- rnorm(1000,5.5,0.3)

mu <- sample(seq(from=4.5,to=6.5,length=10000))
sigma <- 0.3

nsamp <- 10000
lkl <- rep(NA,nsamp)
for(i in 1:nsamp){
  lkl[i] <- prod(dnorm(xobs,mean=mu[i],sd=sigma))
}

df.lkl <- data.frame(mu=mu,sigma=sigma,lkl=lkl)

library(ggplot2)

ggplot(df.lkl,aes(x=mu,y=lkl))+
  geom_point()
########################################

xsim <- rbeta(10000,6,2)
hist(xsim)

xsim <- rbeta(10000,200,200)
hist(xsim)

p <- rbeta(10000,160,40)
hist(p)


  
X <- rlnorm(n=10000,meanlog = 7, sdlog=0.5)
hist(X)

Y <- log(X)
hist(Y)

300, 315, 400
log(300)
Lognormal()


#################

# Word recognition expt
# X: no of correct responses
# X ~ Binomial(theta)
X <- 0:50
pm <- dbinom(x=X,size=50,prob=0.9)
plot(X,pm)
library(ggplot2)
ggplot(data.frame(X=X,Probability=pm),
       aes(x=X,y=Probability))+
  geom_point(size=2)

#data: x = 45

# Likelihood function
# L(theta|x) = f(x;theta)

theta <- seq(from=0,to=1,length=10000)
likelihood <- dbinom(x=900,size=1000,prob = theta)

ggplot(data.frame(theta=theta,likelihood=likelihood),
       aes(x=theta,y=likelihood))+
  geom_point(size=2)












