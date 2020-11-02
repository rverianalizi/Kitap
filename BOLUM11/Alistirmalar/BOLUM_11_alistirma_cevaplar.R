#Bolum 11 Alistirmalar

#1


Data<-read.csv(file='Bolum11_Alistirma1.csv')


library(lavaan)
#DFA
Model1<- 'OOzellik=~Degerlendirme+Yonlendirme+Ogretim+Destek'                       
Fit1<-cfa(model = Model1, data = Data)
summary(Fit1, fit.measures=TRUE) 
modindices(Fit1,sort = TRUE,standardized = FALSE)
Model11<- 'OOzellik=~Degerlendirme+Yonlendirme+Ogretim+Destek
          Degerlendirme~~Yonlendirme'
Fit2<-cfa(model = Model11, data = Data)
summary(Fit2,fit.measures=TRUE)

#2Cok gruplu
Bicimsel<-cfa(model = Model1, data = Data,group = "cinsiyet" )
Metrik<-cfa(model = Model1, data = Data,group = "cinsiyet" ,group.equal="loadings")
Skalar<-cfa(model = Model1, data = Data,group = "cinsiyet", 
            group.equal=c("loadings","intercepts"))
Kati<-cfa(model = Model1, data = Data,group = "cinsiyet", 
          group.equal=c("loadings","intercepts","residuals"))

anova(Bicimsel,Metrik,Skalar,Kati)

FBicimsel<-fitmeasures(Bicimsel,c("rmsea","cfi","tli","srmr","gfi"))
FMetrik<-fitmeasures(Metrik,c("rmsea","cfi","tli","srmr","gfi"))
FSkalar<-fitmeasures(Skalar,c("rmsea","cfi","tli","srmr","gfi"))
FKati<-fitmeasures(Kati,c("rmsea","cfi","tli","srmr","gfi"))
sonuc<-rbind(FBicimsel,FMetrik,FSkalar,FKati)
apply(sonuc, 2, round,3)

#3 yapisal model
Model2<- 'OOzellik=~Degerlendirme+Yonlendirme+Ogretim+Destek
        Motivasyon ~ OOzellik 
        SinifIklm ~ OOzellik + cinsiyet'  
YolModel1<-sem(Model2,Data)
summary(YolModel1,fit.measures=TRUE)


library(semPlot)
semPaths(YolModel1, whatLabels="est",layout = "tree",style="lisrel", edge.color = "black", reorder = FALSE, nCharNodes=17,sizeMan=8)
