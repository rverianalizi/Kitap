#1
library(difR)
#dmfdata veri setini olusturmak amacıyla puan2PL ve  dmf.data fonksiyonları çalıştırılmıştır. 
puan2PL <- function(madde, birey)
{
  a <- madde[, 1]
  b <- madde[, 2]
  k <- length(b)
  n <- length(birey)
  theta <- rep(birey, k)
  aa <- rep(a, rep(n, k))
  bb <- rep(b, rep(n, k))
  p <- 1/(1+exp(-((aa)*(theta-bb))))
  rr <- runif(n*k, 0, 1)
  puan <- ceiling(p-rr)
  puan <- matrix(puan, ncol=k)
  return(puan)
}


dmf.data <- function(madde.ref, madde.odak, n.ref, n.odak, gr.ref, gr.odak)
{
  birey <- rnorm(n.ref) 
  puan.ref <- puan2PL(madde=madde.ref, birey=birey)
  group.ref <- matrix(gr.ref, nrow=n.ref, ncol=1)
  data.ref <- cbind(group.ref, puan.ref)
  birey <- rnorm(n.odak) 
  puan.odak <- puan2PL(madde=madde.odak, birey=birey)
  group.odak <- matrix(gr.odak, nrow=n.odak, ncol=1)
  data.odak <- cbind(group.odak, puan.odak)
  data <- rbind(data.ref, data.odak)
}
set.seed(121)
madde_r<- matrix(scan("madder.dat"), byrow=TRUE, ncol=3)
madde_o<- matrix(scan("maddeo.dat"), byrow=TRUE, ncol=3)

dmfdata <- dmf.data(madde.ref=madde_r, madde.odak=madde_o, n.ref=400, n.odak=400, gr.ref=1, gr.odak=0)

### fonksiyon ilk verisyonu olarak ifelse kosul cümlesi ile yazılmıştır
library(difR)
sibtest_Etki_v1<- function(veri,grup,focal.name,type){
dmf.SIBTEST <- difSIBTEST(veri, group = 1, focal.name = 0,type = "udif")
Beta <- dmf.SIBTEST$Beta
p <- dmf.SIBTEST$p.value
EtkiBuy <- character()

EtkiBuy <- ifelse(p<=0.05 & abs(Beta) <0.059,"A",
       ifelse(p<=0.05 & (abs(Beta)>=0.059 & abs(Beta)<0.088 ),"B",
                         ifelse(p<=0.05 | abs(Beta)>=0.088, "C","-" )))

sonuc <- data.frame(round(Beta,3), round(p,3),EtkiBuy)
colnames(sonuc)<- c(" Beta ","p"," EtkiBuY")

row.names(sonuc) <- paste("Madde",1:(ncol(veri)-1),sep="")
sonuc
}

sibtest_Etki_v1(veri=dmfdata,grup=1,focal.name=0,type="unif")



### fonksiyon ikinci versiyonu if- else kosul cümlesi ile yazılmıştır


sibtest_Etki_v2<- function(veri,grup,focal.name,type){
  dmf.SIBTEST <- difSIBTEST(veri, group = 1, focal.name = 0,type = "udif")
  Beta <- dmf.SIBTEST$Beta
  p <- dmf.SIBTEST$p.value
  EtkiBuy <- character()
  
  for(i in  1:(ncol(dmfdata)-1)){
    if(p[i]<=0.05){
      if( abs(Beta[i]) <0.059){
        EtkiBuy[i] <- "A"
      }else if(abs(Beta[i])>=0.059 & abs(Beta[i])<0.088){
        EtkiBuy[i] <- "B"
      } else {EtkiBuy[i] <- "C"}
    }else{ EtkiBuy[i] <- "-"}
  }
  
  
  sonuc <- data.frame(round(Beta,3),round(p,3),EtkiBuy)
  colnames(sonuc)<- c(" Beta ","p"," EtkiBuY")
  row.names(sonuc) <- paste("Madde",1:(ncol(veri)-1),sep="")
  sonuc
}



sibtest_Etki_v2(veri=dmfdata,grup=1,focal.name=0,type="unif")



#2
library(dplyr)
library(equate)
esityuzdelik <- list() # equate fonksiyonundan elde edilen sonuclar icin
ESITYUZDELIK <- list()  # kisisel tanımlı fonksiyonun sonuclar icin
for(i in 1:10){
  puan.X<- read.fwf(paste("FORMX_",i,".dat",sep=""),widths=c(3,7,rep(1,20)))[,-c(1:2)]
  puan.Y <- read.fwf(paste("FORMY_",i,".dat",sep=""),widths=c(3,7,rep(1,20)))[,-c(1:2)]
  x <- rowSums(puan.X)
  y <- rowSums(puan.Y)
X <- data.frame(puan.X,x)
XTABLO <- X %>% count(x)
XTABLO <- left_join(tibble(x=0:ncol(puan.X)),XTABLO)
XTABLO$n[is.na(XTABLO$n)] <- 0
fx <- XTABLO$n/nrow(puan.X)
Fx <- cumsum(fx)
XTABLO  <- data.frame(XTABLO,fx,Fx)


Px <- c()
Px[1] <- 100*(XTABLO$fx[1]/2)
for(j in 2:nrow(XTABLO)){
  Px[j] <- 100*(XTABLO$Fx[j-1]+(XTABLO$fx[j]/2))
}
XTABLO <- data.frame(XTABLO, Px)
head(XTABLO)
Y <- data.frame(puan.Y,y)
YTABLO <- Y %>% count(y)


YTABLO  <- left_join(tibble(y=0:ncol(puan.Y)),YTABLO )
YTABLO$n[is.na(YTABLO$n)] <- 0
gy <- YTABLO$n/nrow(puan.Y)
Gy <- cumsum(gy)
YTABLO  <- data.frame(YTABLO,gy,Gy)
YTABLO


Qy <- c()
Qy[1] <- 100*(YTABLO$gy[1]/2)
for(z in 2:nrow(YTABLO)){
  Qy[z] <- 100*(YTABLO$Gy[z-1]+(YTABLO$gy[z]/2))
}
YTABLO <- cbind(YTABLO, Qy)
YTABLO
head(YTABLO)


X.ESITYUZDELIK <- c()
Px_100 <- Px/100
for(k in 1:length(Px)){
  YTABLO.yeni <-  YTABLO[(YTABLO["Gy"]-Px_100[k])>=0,]
  yu <- as.numeric(YTABLO.yeni$y[1])
  Gyu <- YTABLO[YTABLO["y"]==yu,"Gy"]
  Gyu_1 <- YTABLO[YTABLO["y"]==yu-1,"Gy"]
  if(yu==0){
    if(Gyu!=0){
      X.ESITYUZDELIK[k] <- ((Px_100[k])/(Gyu))+(yu-0.5)
    }else(X.ESITYUZDELIK[k] <- -0.5)
  }else if(yu!=0){
    if(Px[k]<100){
      X.ESITYUZDELIK[k] <- ((Px_100[k]-Gyu_1)/(Gyu-Gyu_1))+(yu-0.5)
    }else(X.ESITYUZDELIK[k] <-ncol(puan.Y)+0.5) 
  }
}

X.ESITYUZDELIK <- round(X.ESITYUZDELIK,2)

data.X <- freqtab(puan.X, scale=0:20, items=1:20)
head(data.X)
data.Y <- freqtab(puan.Y, scale=0:20, items=1:20)
head(data.Y)
es.esityuzdelik <- equate(data.X, data.Y, type = "equipercentile")
es.esityuzdelik
X.esityuzdelik <- as.vector(round(es.esityuzdelik$concordance$yx, 2))
X.esityuzdelik


esityuzdelik [[i ]] <- X.esityuzdelik
ESITYUZDELIK [[i]] <- X.ESITYUZDELIK

}


esityuzdelik <- data.frame(esityuzdelik)
colnames(esityuzdelik) <- paste("grup",1:10,sep="")
ESITYUZDELIK <- round(data.frame(ESITYUZDELIK),2)
colnames(ESITYUZDELIK) <- paste("grup",1:10,sep="")


