#####Boolum 10 Alistirmalar

Veri<-read.csv( file= 'Bolum10_Alistirma.csv')

#(1) Varsayilmar
#Uc degerler
zpuan<-apply(Veri,2,scale)
head(zpuan)
any(zpuan>3.29,na.rm = TRUE)
which(zpuan>3.29)

library(psych)

#coklu baglanti
smc(Veri)
Veri1<-Veri[,-12]

#KMO ve barlet
KMO(Veri)
bartlett.test(Veri)

KMO(Veri1)
bartlett.test(Veri1)

#(2)Paralel analiz
#paralel analiz
library("nFactors")

Veri1.Cor<-polychoric(Veri1)$rho
Ozdeger <- eigen(Veri1.Cor)
Ozdeger$values
PA<-nScree(eig=Ozdeger$values, x=Veri1.Cor, aparallel=NULL, cor=TRUE, model="factors",
           criteria=NULL) 

PA$Components

plotnScree(PA, legend = TRUE,
           ylab   = "Ozdegerler",
           xlab   = "Faktor",
           main   = "Faktor Cozumu")


#faktor analizi
AFA11<- fa(Veri1,nfactors=3,rotate="none", scores="regression",  fm="minres")
AFA1$loadings

AFA2<- fa(Veri1,nfactors=3,rotate="oblimin", scores="regression",  fm="minres")
AFA2$loadings

AFA3<- fa(Veri1,nfactors=3,rotate="varimax", scores="regression",  fm="minres")
AFA3$loadings

fa.diagram(AFA1)
fa.diagram(AFA2)
fa.diagram(AFA3)
