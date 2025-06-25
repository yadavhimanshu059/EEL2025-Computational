# Likelihood assumption
# y_i ~ Normal(mu,sigma)
# Prior assumptions
# sigma = 20
# mu ~ Normal(100,20)

# p(mu|y,sigma) = L(mu|y,sigma) p(mu) / ML
y <- 100  # data
mu <- seq(from=0,to=500,length=10000)  
sigma <- 20

likelihood = dnorm(x=y,mean=mu,sd=sigma) 
prior = dnorm(x=mu,mean=100,sd=20)
plot(mu,likelihood)
ML <- sum(likelihood*prior)

posterior = (likelihood*prior)/ML
plot(mu,posterior)

# Draw likelihood, prior and posterior above each other
dat <- data.frame(mu=mu,likelihood=likelihood,
                  prior=prior,posterior=posterior)
library(reshape2)
dat.m <- melt(dat,id=("mu"))
library(ggplot2)
ggplot(dat.m,aes(x=mu,y=value,group=variable,color=variable))+
  geom_point()+facet_wrap(~variable,scales="free",nrow=3)



# Data
x <- 45
n <- 50

# Assumptions
# The likelihood assumption: 
# x ~ Binomial(theta) 
# where theta is probability of success
# Hence, L(theta|x) = nCx theta^(x) (1-theta)^(n-x) = dbinom(x,n,theta)
# The prior assumption: 
# p(theta) = 1 if theta > 0.5 else p(theta) = 0

# Choosing 10000 values of theta between 0 and 1
theta <- seq(from=0,to=1,length=10000)

# Computing likelihood for each value
likelihood <- dbinom(x=45,size=50,prob = theta)
plot(theta,likelihood)

# Computing prior for each value
prior <- ifelse(theta<0.5,0,2)
plot(theta,prior)

# Computing approx. marginal likelihood as SUM(prior*likelihood)
ML <- mean(likelihood*prior)

# Calculate posterior density corresponding to each value of theta
posterior <- (likelihood*prior)/ML
plot(theta,posterior)

# Draw likelihood, prior and posterior above each other
dat <- data.frame(theta=theta,likelihood=likelihood,
                  prior=prior,posterior=posterior)
library(reshape2)
dat.m <- melt(dat,id=("theta"))
library(ggplot2)
ggplot(dat.m,aes(x=theta,y=value,group=variable,color=variable))+
  geom_point()+facet_wrap(~variable,scales="free",nrow=3)

##################################
# Highly informative prior

# Data
x <- 45
n <- 50

# Assumptions
# The likelihood assumption: x ~ Binomial(theta) where theta is probability of success
# Hence, L(theta|x) = nCx theta^(x) (1-theta)^(n-x) = dbinom(x,n,theta)
# The prior assumption: theta ~ Beta(80,20)

# Choosing 10000 values of theta between 0 and 1
theta <- seq(from=0,to=1,length=10000)

# Computing likelihood for each value
likelihood <- dbinom(x=450,size=500,prob = theta)
plot(theta,likelihood)

# Computing prior for each value
prior <- dbeta(theta,80,20)
plot(theta,prior)

# Computing approx. marginal likelihood as SUM(prior*likelihood)
ML <- sum(likelihood*prior)

# Calculate posterior density corresponding to each value of theta
posterior <- (likelihood*prior)/ML
plot(theta,posterior)

# Draw likelihood, prior and posterior above each other
dat <- data.frame(theta=theta,likelihood=likelihood,
                  prior=prior,posterior=posterior)
library(reshape2)
dat.m <- melt(dat,id=("theta"))
library(ggplot2)
ggplot(dat.m,aes(x=theta,y=value,group=variable,color=variable))+
  geom_point()+facet_wrap(~variable,scales="free",nrow=3)

##################################





###########################
# Data: 350, 300, 200
# Likelihood assumption:
# x_ì ~ Normal(mu,sigma)
# Prior assumptions:
# sigma=40
# mu ~ Normal(350,25)

# Choose a lot of equidistant values of mu
mu <- seq(from=0,to=500,length=10000)
sigma <- 40               
x <- rnorm(100,220,40)    # data
x
hist(x)
# We need to calculate likelihood corresponding
# to each value of mu

# Creating an empty likelihood vector
Likelihood <- rep(NA,10000)

# Calculate the joint likelihood of observing 
# c(350,300,200) from a particular value of mu
x <- c(350,300,200)

for(i in 1:length(mu)){
  Likelihood[i] <- prod(dnorm(x=x,mean=mu[i],sd=sigma)*2e5)
}
Likelihood 

plot(mu,Likelihood)

# Calculate prior densities correspoding to each mu
prior <- dnorm(mu,mean=350,sd=25)
plot(mu,prior)


unormalized_posterior <- Likelihood*prior
plot(mu,unormalized_posterior)

ML <- sum(Likelihood*prior)
posterior <- unormalized_posterior/ML
plot(mu,posterior)



# Plot all of them together
df.post <- data.frame(mu=mu,Prior=prior,
                      Likelihood=Likelihood,
                      Posterior=posterior)

head(df.post)

library(reshape2)
df.post.m <- melt(df.post,id=c("mu"))
head(df.post.m)

library(ggplot2)
ggplot(df.post.m,aes(x=mu,y=value,group=variable))+
  geom_point()+geom_line()+
  theme_bw()+
  facet_wrap(~variable,scales = "free_y",nrow = 3)

ggplot(df.post.m,aes(x=mu,y=value,group=variable))+
  geom_point()+geom_line()+
  theme_bw()+
  facet_wrap(~variable,scales = "free_y",nrow = 3)+
  scale_x_continuous(limits = c(100,500))


#########################

# No. of covid infections on each day in Kanpur

# Likelihood assumption
# X_i ~ Poisson(lambda)

xsim <- rpois(10000,lambda = 10)
hist(xsim,breaks = 50)

X <- 0:50
probability <- dpois(x=X,lambda = 10)
plot(X,probability)

library(ggplot2)
ggplot(data.frame(X=X,probability=probability),
       aes(x=X,y=probability))+
  geom_bar(stat = "identity",width=0.5)

# Prior assumption
# lambda ~ normal+(10,5)
library(truncnorm)
lambda_sim <- rtruncnorm(10000,a=0,b=Inf,mean=10,sd=5)
hist(lambda_sim)

# Likelihood and prior assumptions
# x_i <- Poisson(lambda)
# lambda ~ Normal+(10,5)

# Prior predictions of the model
lambda_samples <- rtruncnorm(100,a=0,b=Inf,
                             mean=10,sd=5)


df.pred <- data.frame(matrix(nrow=0,ncol=3))
colnames(df.pred) <- c("sample_id","lambda","infections")

i <- 0
for(lambdax in lambda_samples){
  i <- i+1
  xsim <- rpois(n=30,lambda = lambdax)
  pred <- data.frame(sample_id=rep(i,30),
                     lambda=rep(lambdax,30),
                     infections=xsim)
  df.pred <- rbind(df.pred,pred)
  
}

ggplot(subset(df.pred,sample_id<11),
       aes(x=infections))+geom_histogram()+
  facet_wrap(~sample_id)

##################
# Data: 24, 21, 43, 27, 18

# x_i ~ Poisson(lambda)
data <- c(24,21,43,27,18)
lambda <- seq(from=0.5,to=100,length=10000)
likelihood <- rep(NA,10000)
for(i in 1:10000){
  likelihood[i] <- 
    prod(dpois(x=data,lambda = lambda[i]))
}

plot(lambda,likelihood)

prior <- dtruncnorm(x=lambda,a=0,b=Inf,
                    mean=10,sd=5) 
plot(lambda,prior)

unnormalized_posterior <- likelihood*prior
plot(lambda,unnormalized_posterior)  


df.post <- data.frame(lambda=lambda,Prior=prior,
                      Likelihood=likelihood,
                      Posterior=unnormalized_posterior)

head(df.post)

library(reshape2)
df.post.m <- melt(df.post,id=c("lambda"))
head(df.post.m)

library(ggplot2)
ggplot(df.post.m,aes(x=lambda,y=value,group=variable))+
  geom_point()+geom_line()+
  theme_bw()+
  facet_wrap(~variable,scales = "free_y",nrow = 3)

ggplot(df.post.m,aes(x=lambda,y=value,group=variable))+
  geom_point()+geom_line()+
  theme_bw()+
  facet_wrap(~variable,scales = "free_y",nrow = 3)+
  scale_x_continuous(limits = c(0,60))










