##################################################################################################################
########       SORU 1                 ############################################################################
set.seed(10)
Y=sample(10:30, 30, replace=TRUE)
X=c(rep("A",10), rep("B",10), rep("C",10))
VS=data.frame(X,Y)




#Uç değer
library(ggplot2)
ggplot(VS,aes(x=X, y=Y, fill=X)) +
      geom_boxplot() +
      geom_jitter(width=0.25, alpha=0.1)
      
      
#Normallik    
#install.packages("sm")
library(sm)
sm.density.compare(VS$Y, VS$X)

#Artık hatalar testi

hata <- residuals(lm(Y ~ X,
           data=VS))
plot(hata)
abline(h=0, col="red")
plot(density(hata))

sonuc=aov(Y~X, data=VS)
summary(sonuc)


TukeyHSD(sonuc, conf.level=0.95)

library(apaTables)
apa.aov.table(lm(Y ~ X, data = VS), filename = "tekYonluVA.doc")



##################################################################################################################
########       SORU 2                 ############################################################################

library(dplyr)
VS2 <- VS %>% filter(X == "A" | X == "B")

ggplot(VS2,aes(x=X, y=Y, fill=X)) +
      geom_boxplot() +
      geom_jitter(width=0.25, alpha=0.1)

sm.density.compare(VS2$Y, VS2$X)
shapiro.test(VS2$Y)


A  <- filter(VS2, X == "A")[,2]
B  <- filter(VS2, X == "B")[,2]


library(lsr)
independentSamplesTTest( Y ~ X, VS2, var.equal=TRUE )

library(pwr)
pwr.2p2n.test(h=0.362,n1=11,n2=11,sig.level=0.05,alternative="two.sided")

pwr.t.test(d=0.362,n=30,sig.level=0.05,type="two.sample",alternative="two.sided")
pwr.t.test(d=0.362,power=0.8,sig.level=0.05,type="two.sample",alternative="two.sided")


##################################################################################################################
########       SORU 3                 ############################################################################


summary(VS)

library(psych)
psych::describe(VS)

library(summarytools)
summarytools::descr(VS)
view(dfSummary(VS))


##################################################################################################################
########       SORU 4                 ############################################################################


library(dplyr)
A  <- filter(VS, X == "A")[,2]
B  <- filter(VS, X == "B")[,2]
C  <- filter(VS, X == "C")[,2]

VS3 <- data.frame(A,B,C)
korlsyn <- round(cor(VS3, method = "spearman"),2)


library(psych)
pairs.panels(korlsyn, scale=TRUE, hist.col="gray")



##################################################################################################################
##################################################################################################################
