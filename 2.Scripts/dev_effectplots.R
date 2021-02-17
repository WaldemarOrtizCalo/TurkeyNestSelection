library("car") # also loads the carData package
Prestige$type <- factor(Prestige$type, levels=c("bc", "wc", "prof"))
lm1 <- lm(prestige ~ education + poly(women, 2) +
               + log(income)*type, data=Prestige)

summary(lm1)
library("effects")
e1.lm1 <- predictorEffects(lm1)
plot(e1.lm1)





ForestEdge100m_NestSelect <- clogit(response ~ ForestEdge_100m + 
                                      strata(strata),data = Data_NestSelection)

m1<-predictorEffects(ForestEdge100m_NestSelect)
library("effects")
m1 <- predictorEffect("ForestEdge_100m", ForestEdge100m_NestSelect)
plot(e1.lm1)

summary(ForestEdge100m_NestSelect)