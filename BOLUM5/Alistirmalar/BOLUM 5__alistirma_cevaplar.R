
alistirma_veri<-read.csv("alistirma_veri.csv", header=FALSE, sep = ";")
alistirma_veri

###  1a  ###
dim(alistirma_veri)

###  1b  ###
names(alistirma_veri) <- c("duzey", "sube", "cinsiyet", paste("madde", 1:20, sep = "_"))
head(alistirma_veri)

###  1c  ###
head(alistirma_veri, 10); tail(alistirma_veri,5) 

###  1d  ###
alistirma_veri<- cbind(id=paste("id", 1:200, sep="_"), alistirma_veri)


###  1e  ###
alistirma_veri$toplam <- rowSums(alistirma_veri[,5:24])

###  1f  ###
library(dplyr)
alistirma_veri <- alistirma_veri %>% mutate(toplam2 = rowSums(select(.,starts_with("madde"))))
###  1g  ###
attach(alistirma_veri)
summary(toplam)

###  1h  ###
alistirma_veri %>% group_by(duzey) %>% summarise(mean(toplam))
library(data.table)
alistirma_veri<-as.data.table(alistirma_veri)
alistirma_veri[, mean(toplam), by=duzey] 

###  1i  ###
alistirma_veri %>% group_by(cinsiyet) %>% summarise(mean(toplam))
# ya da
alistirma_veri[, mean(toplam), by=cinsiyet] 

###  1j  ###
alistirma_veri %>% group_by(sube,cinsiyet) %>% summarise(mean(toplam))
# ya da
alistirma_veri[, mean(toplam),by = .(sube,cinsiyet)] 


###  1k  ###
arrange(alistirma_veri, -toplam)

###  1l  ###
sinav_order<-arrange(alistirma_veri, -toplam)

###  1m  ###

grup <- factor(cut(sinav_order$toplam, breaks = c(0, 6.9 , 12.9, 20), 
                   include.lowest=TRUE, right = FALSE), 
               labels = c("alt grup","orta grup","ust grup"))
sinav_order<- cbind(sinav_order, grup)
# ya da
setDT(sinav_order)[ , grup := cut(toplam, 
                                  breaks = c(0, 6.9 , 12.9, 20),
                                  labels = c("alt grup","orta grup","ust grup"))]
###  1n  ###
sınav_order[, .N, by=grup] 
# ya da
table(sınav_order$grup)

  
###  1o  ###  
sinav_1A <- sinav_order %>% filter(duzey==1 & sube=="A")
# ya da
sinav_1A<-sinav_order[duzey=="1",][sube=="A",]
write.csv2(sinav_1A, file = "sinav_1A.csv",quote=FALSE)

###  2a  ###  
alistirma_veri[,lapply(.SD,mean),.SDcols = 5:24] 
###  2b  ### 
alistirma_veri[,lapply(.SD,mean),by=cinsiyet,.SDcols = 5:24]  



