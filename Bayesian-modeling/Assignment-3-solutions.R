# graph of PDF, graph of assumptions

library(ggplot2)
mu <- 6
X <- seq(from=10, to=2000, length=10000)
pd <- dlnorm(X,meanlog=mu,sdlog=1)

ggplot(data.frame(X=X,density=pd),aes(x=X,y=density))+
  geom_line()+theme_bw()

# graph of the likelihood function
x <- c(303.25,443,220,560,880)

x <- 220
mu <- seq(from=1,to=10,length=100000)
likelihood <- dlnorm(x=x,meanlog=mu,sdlog=1)

ggplot(data.frame(mu=mu,likelihood=likelihood),
       aes(x=mu,y=likelihood))+
  geom_line()+theme_bw()+xlab(expression(mu))+
  geom_vline(xintercept=mean(log(x)),color="red")

MLE <- mean(log(x))


x <- c(303.25,443,220,560,880)
mu <- seq(from=1,to=10,length=100000)

likelihood <- rep(NA, 100000)
for(i in 1:100000){
  likelihood[i] <- prod(dlnorm(x=x,meanlog=mu[i],sdlog=1))
}

ggplot(data.frame(mu=mu,likelihood=likelihood),
       aes(x=mu,y=likelihood))+
  geom_line()+theme_bw()+xlab(expression(mu))+
  geom_vline(xintercept=mean(log(x)),color="red")
