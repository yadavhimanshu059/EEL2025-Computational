freq <- rep(c(0,1),50)
alpha <- 6.2
beta <- -0.005
sigma <- 0.25

tau_alpha <- 0.3
tau_beta <- 0.1
corr <- 0.7
Sigma <- matrix(c(tau_alpha^2,corr*tau_alpha*tau_beta,
                  corr*tau_alpha*tau_beta,tau_beta^2),nrow=2,ncol=2)

Sigma
library(MASS)
subj_ab <- mvrnorm(n=50,mu=c(alpha,beta),Sigma)
subj_ab
plot(subj_ab[,1],subj_ab[,2])

fake_data <- data.frame(subj=rep(1:50,each=100),
                        word=rep(1:100,50),
                        frequency=rep(freq,50),
                        subj_alpha = rep(subj_ab[,1],each=100),
                        subj_beta = rep(subj_ab[,2],each=100))

fake_data$rt <- NA

for(i in 1:nrow(fake_data)){
  mu = fake_data$subj_alpha[i] + 
    fake_data$subj_beta[i] * fake_data$frequency[i]
  rt_i <- rlnorm(1,meanlog = mu,sdlog = sigma)
  fake_data$rt[i] <- rt_i
}

hist(fake_data$rt)

library(ggplot2)
ggplot(fake_data,aes(x=rt,group = subj,color=subj))+
  geom_density()

ggplot(fake_data,aes(x=rt,group = subj))+
  geom_histogram()+facet_wrap(~subj)

save(fake_data,file="Data/hierarchical-fake-data.Rda")

library(brms)
m1 <- brm(rt~1+frequency,data=fake_data,
          family = lognormal(),
          prior = c(prior(normal(6,1),class=Intercept),
                    prior(normal(0,0.01),class=b),
                    prior(normal(0,0.2),class=sigma))
          )

save(m1,file="FittedModels/Frequency_model_average.Rda")
load("FittedModels/Frequency_model_average.Rda")

load("Data/hierarchical-fake-data.Rda")

m2 <- brm(rt~1+frequency + (1+frequency|subj),
          data=fake_data,
          family = lognormal(),
          prior = c(prior(normal(6,1),class=Intercept),
                    prior(normal(0,0.01),class=b),
                    prior(normal(0,0.2),class=sigma),
                    prior(normal(0,0.2),class=sd),
                    prior(lkj(2),class=cor)
                    )
)

null <- brm(rt~1 + (1+frequency|subj),
          data=fake_data,
          family = lognormal(),
          prior = c(prior(normal(6,1),class=Intercept),
                    prior(normal(0,0.2),class=sigma),
                    prior(normal(0,0.2),class=sd),
                    prior(lkj(2),class=cor)
          )
)

summary(m2)
summary(m1)
save(m2,file="FittedModels/Frequency_model_hierarchical.Rda")


subj_slopes <- data.frame(matrix(nrow=0,ncol=2))
colnames(subj_slopes) <- c("subj","slope_estimate")

for(i in 1:50){
  subj_samples <- post_samples[,57+i]
  df_samples <- data.frame(subj=rep(i,4000),
                           slope_estimate=subj_samples)
  subj_slopes <- rbind(subj_slopes,df_samples)
}

head(subj_slopes)

library(ggplot2)
library(dplyr)

subj_slopes.m <- subj_slopes %>% group_by(subj) %>% 
  summarise(Estimate=mean(slope_estimate),
            q.lower=unname(quantile(slope_estimate,probs=c(0.025,0.975)))[1],
            q.upper=unname(quantile(slope_estimate,probs=c(0.025,0.975)))[2])

subj_levels <- subj_slopes.m$subj[order(subj_slopes.m$Estimate)]
subj_slopes.m$subj <- factor(subj_slopes.m$subj,levels=subj_levels)

ggplot(subj_slopes.m,aes(x=Estimate,y=subj))+
  geom_point()+
  geom_errorbar(aes(xmin=q.lower,xmax=q.upper))

subj_slopes$subj <- factor(subj_slopes$subj,levels = subj_levels)
ggplot(subj_slopes,aes(x=slope_estimate,y=subj,group=subj))+
  geom_violin()
