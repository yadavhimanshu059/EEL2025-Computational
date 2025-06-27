# Data
dat <- read.table("recognition.csv",sep=",",header = T)
head(dat)

# Null hypothesis model
# Likelihood assumption
# Tw_i ~ N(mu,sigma)
# Tnw_i ~ N(mu+delta,sigma)
# Prior assumptions
# mu ~ N(300,50)
# delta = 0
# sigma = 60

mu <- seq(from=100,to=800,length=10000)
likelihood <- rep(NA,10000)
for(i in 1:10000){
  likelihood[i] <- 
    prod(dnorm(x=dat$Tw,mean=mu[i],sd=60))*
    prod(dnorm(x=dat$Tnw,mean=mu[i]+0,sd=60))
}
plot(mu,likelihood)

prior <- dnorm(x=mu,mean=300,sd=50)
unnorm_posterior <- likelihood*prior
df.post <- data.frame(mu=mu,
                      prior=prior,
                      likelihood=likelihood,
                      posterior=unnorm_posterior)

library(ggplot2)
library(reshape2)
ggplot(melt(df.post,id="mu"),aes(x=mu,y=value))+
  geom_line()+facet_wrap(~variable,scale="free_y",nrow=3)

##################

# Lexical-access model
# Likelihood assumption
# Tw_i ~ N(mu,sigma)
# Tnw_i ~ N(mu+delta,sigma)
# Prior assumptions
# mu ~ N(300,50)
# delta ~ N+(0,50)
# sigma = 60

mu <- sample(seq(from=200,to=500,length=100000))
delta <- sample(seq(from=0,to=150,length=100000))


library(truncnorm)

prior <- dnorm(x=mu,mean=300,sd=60)*
  dtruncnorm(x=delta,a=0,b=Inf,mean=0,sd=50)

posterior <- likelihood*prior
plot(delta,posterior)

melt(dat,id="X")


library(reshape2)
dat <- melt(dat,id="X")
head(dat)
colnames(dat) <- c("trial","cond","rt")
library(brms)




