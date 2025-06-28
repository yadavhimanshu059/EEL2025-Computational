library(reshape2)
library(brms)

dat <- read.table("recognition.csv",sep=",",header = T)
head(dat)

dat <- melt(dat,id="X")
head(dat)
colnames(dat) <- c("trial","cond","rt")
dat$cond_c <- ifelse(dat$cond=="Tw",0,1)
head(dat)

# Null model
m0 <-
  brm(rt~1,
      data=dat,
      family=gaussian(),
      prior = c(prior(normal(500,50),class="Intercept"),
                prior(normal(0,30),class="sigma")),
      chains = 4)

m1 <-
  brm(rt~1+cond_c,
      data=dat,
      family=gaussian(),
      prior = c(prior(normal(500,50),class="Intercept"),
                prior(normal(0,40),class="b"),
                prior(normal(0,30),class="sigma")),
      chains = 4)
