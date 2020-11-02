setwd("")
dat1<- read.csv2("bolum_7_alistirma_veri.csv")  
# eger düzgün okumada ise aşağıdaki komut satırını deneyiniz. 
dat1<- read.csv("bolum_7_alistirma_veri.csv")  

attach(dat1)

#1a
plot(SAYISAL~SOZEL)  

pchlistesi <- c(15,16,17)
pchlistesi[IL]

#1b
plot(SAYISAL~SOZEL, pch=pchlistesi[IL], col=IL)
abline(h=by(SAYISAL,IL,mean),lty=2:5, col=c("black","red","green"))
abline(v=by(SOZEL,IL,mean),lty=2:5, col=c("black","red","green"))

#1c
plot(SAYISAL~SOZEL, xlab="Sayısal Yetenek Puanları", ylab="Sözel Yetenek Puanları", main="İllere Göre Başarı Grafiği", pch=pchlistesi[IL], col=IL)
abline(h=by(SAYISAL,IL,mean),lty=2:5, col=c("black","red","green"))
abline(v=by(SOZEL,IL,mean),lty=2:5, col=c("black","red","green"))

#1d

pdf("GrafikPDF.pdf") 
plot(SAYISAL~SOZEL, xlab="Sayısal Yetenek Puanları", ylab="Sözel Yetenek Puanları", main="İllere Göre Başarı Grafiği", pch=pchlistesi[IL], col=IL)
abline(h=by(SAYISAL,IL,mean),lty=2:5, col=c("black","red","green"))
abline(v=by(SOZEL,IL,mean),lty=2:5, col=c("black","red","green"))
dev.off() 

#2

x <- dat1[order(dat1$SOZEL),] 

x$color[x$IL=="Istanbul"] <- "blue"   
x$color[x$IL=="Ankara"] <- "gray"
x$color[x$IL=="Izmir"] <- "orange"

dotchart(x$SOZEL,cex=.7,groups = x$IL,color = x$color,pch=c(2,17,25),
main = "İllere Göre Başarı Puanları",
xlab = "SOZEL Yetenek Puanları")
abline(v=by(SOZEL,IL,mean),lty=2:5, col=c("gray","blue","orange"))

legend(680,130, c("Ankara", "Istanbul", "Izmir"), pch=c(2,17,25), 
col=c("gray", "blue","orange"), cex = 0.75)



#3a


dat2 <- dat1[,c(4:9)]

opar <- par(no.readonly=TRUE)

par(mfrow=c(2,3))


for (i in 1:ncol(dat2) ){

x <- dat2[,i]
hist(x,
       main = paste(colnames(dat2))[i],
       xlab = "Puan", ylab="Frekans")

}




#3b
opar <- par(no.readonly=TRUE)

par(mfrow=c(2,3))

for (i in 1:ncol(dat2) ){

x <- dat2[,i]
plot(density(x),  main = paste(colnames(dat2[i])), xlab = "Puan", ylab="Yoğunluk")
polygon(density(x), col="red", border="black")

}

#4
a1  <- seq(-3, 3, 0.001)

a2 <- dnorm(a1)
par(mfrow=c(1,1))
plot(a1, cumsum(a2)/sum(a2), type = "l", col = "blue",
    xlab = expression(theta), ylab = "Olasılık", main = "3PLM Göre Madde Karakteristik Eğrisi")
text(-1.5, 0.7, expression(P(x) == paste(c+frac((1-c), 1+e^-a(theta-b)),
    " ", )), cex = 0.7)
	



