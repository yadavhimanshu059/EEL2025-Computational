# Reading times per word
# Generate fake data
# True generative process
# rt_i ~ Lognormal(mu,sigma)
# True parameter values
# mu = 5.5, sigma = 0.25

# Observed data
rt <- rlnorm(500,5.5,0.25)
hist(rt)

# Assumptions
# 1. Likelihood assumption
# rt_i ~ Normal(mu,sigma)

# 2. Prior assumptions
#mu ~ Normal(250,50)
#sigma = 20

# Prior predictions
mu <- rnorm(10000,mean=250,sd=50)
sigma <- rep(20,10000)

rt_sim <- data.frame(matrix(nrow=0,ncol=4))
colnames(rt_sim) <- c("sample_id","mu","sigma","rt_pred")
for(i in 1:10000){
  sample_i <- rnorm(500,mu[i],sigma[i])
  sample_df <- data.frame(rt_pred=sample_i)
  sample_df$sample_id <- i
  sample_df$mu <- mu[i]
  sample_df$sigma <- sigma[i]
  rt_sim <- rbind(rt_sim,sample_df)
}

max(rt_sim$sample_id)

library(ggplot2)
ggplot(subset(rt_sim,sample_id<10),aes(x=rt_pred))+
  geom_histogram()+facet_wrap(~sample_id)

# parameter estimation

mu <- runif(100000,100,400)
sigma <- 20
lkl <- rep(NA,100000)
for(i in 1:100000){
  lkl[i] <- prod(dnorm(rt,mean=mu[i],sd=sigma)*exp(10.2))
}

prior <- dnorm(mu,mean=250,sd=50)

ML <- mean(lkl*prior)
posterior <- lkl*prior/ML

plot(mu,posterior)

# Posterior predictions
posterior_samples <- 
  sample(mu,size=2000,prob=posterior)
hist(posterior_samples)

rt_sim <- data.frame(matrix(nrow=0,ncol=4))
colnames(rt_sim) <- c("sample_id","mu","sigma","rt_pred")
for(i in 1:500){
  sample_i <- rnorm(500,posterior_samples[i],20)
  sample_df <- data.frame(rt_pred=sample_i)
  sample_df$sample_id <- i
  sample_df$mu <- mu[i]
  sample_df$sigma <- sigma[i]
  rt_sim <- rbind(rt_sim,sample_df)
}

ggplot(subset(rt_sim,sample_id<10),aes(x=rt_pred))+
  geom_histogram()+facet_wrap(~sample_id)

ggplot(rt_sim,aes(x=rt_pred,group=sample_id))+
  geom_density(color="gray")

  
obs <- data.frame(rt_pred=rt)
obs$sample_id <- 0
obs$mu <- NA
obs$sigma <- NA
rt_sim <- rbind(rt_sim,obs)

rt_sim$type <- ifelse(rt_sim$sample_id>0,"Predicted","Observed")
ggplot(subset(rt_sim),aes(x=rt_pred,group=sample_id,color=type))+
  geom_density()

#######################

#rt_i <- lognormal(mu,sigma)

rt <- rlnorm(10000,5.5,0.25)
hist(rt)
hist(log(rt))
log(250)
