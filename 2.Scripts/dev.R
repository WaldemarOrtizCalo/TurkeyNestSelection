### number of pairs
n <- 100
### create data
ai <- c(rep(0,n/2), rep(1,n/2))
bi <- 1-ai
ci <- c(rep(0,42), rep(1,8), rep(0,18), rep(1,32))
di <- 1-ci
### change data to long format 
event <- c(rbind(ai,ci))
group <- rep(c(1,0), times=n)
id    <- rep(1:n, each=2)

library(survival)
summary(clogit(event ~ group + strata(id)))
