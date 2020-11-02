#Alistirma 1

Key<-read.fwf(file = 'Bolum13Alistirma1Key.txt',header = F,widths = rep(1,27))
Data<-read.csv(file = 'Bolum13Alistirma1.csv')
MaddeCevap<-Data[,-1]
ID<-Data[,1]
library("CTT")
KTK.data <- score(MaddeCevap, Key, ID=ID, 
                  output.scored = TRUE)

KTK.guvenirlik <- reliability(KTK.data$scored, itemal=TRUE)
KTK.guvenirlik$alphaIfDeleted

str(reliability(KTK.data$scored, itemal=TRUE))

distractor.analysis(MaddeCevap, Key, p.table = FALSE, 
                    write.csv="celdirici.analizi.csv")

#Alistirma 2
data<-read.csv(file = 'Bolum13Alistirma2.csv')
colnames(data)<-paste('V',1:10,sep = '-')

library("mirt")
modelAT <- mirt(data, 1, itemtype='graded', SE=TRUE)
madde.kestirim <- coef(modelAT, IRTpars=TRUE, printSE=TRUE, 
                       as.data.frame=TRUE)

plot(modelAT, type='trace')
plot(modelAT, type='trace', which.items = 8)
plot(modelAT, type='infotrace')
plot(modelAT, type='infotrace', which.items = 8:9)
plot(modelAT, type='infoSE')
plot(modelAT, type='rxx')
