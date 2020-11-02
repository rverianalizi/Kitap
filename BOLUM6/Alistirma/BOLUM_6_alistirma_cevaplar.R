#1
x <- rnorm(1)
if(x>1){
  print("1'den buyuk")
}else if(x>=-1){
  print("-1 ile 1 arasında")
}else{
  print("-1'den kucuk")
}
x

#2
# vektorlerde tum elemanlara da islem icin all() fonksiyonu kullanılır. 
x <- rnorm(5,1,0)
if(all(x>0)){
  print("vektörün tüm elemanları 0'dan büyüktür")
} else{
  print("vektörün tüm elemanları 0'dan büyük değildir")
}
x

x <- rnorm(500)
if(all(x>0)){
  print("tum sayilar 0'dan buyuktur")
} else{
  print("tum sayilar 0'dan buyuk degildir")
}
x
#3
# vize ve final notları
vize <- c(60,70,80,90,55)
final <- c(45,65,70,50,80)
devam <- c(14,10,13,12,11)


gecme_not <- ifelse(final>50 & devam>10,final*0.6 + vize*0.4,0)
       ifelse(gecme_not>=50,"geçti","kaldi")


#4
       
x <- c(NA, NA, NA,NA)
y <- c( 1, NA, NA, NA)
       
ortak_na <- function(x, y){
# koşulun yazımı
if (length(x) != length(y)) {
# hata mesajı
  stop("x ve  y aynı uzunlukta olamalıdır", call. = FALSE)
         }  
         sum(is.na(x) & is.na(y))
       }

ortak_na(x, y)
       
       
x <- c(NA, NA, NA)
y <- c( 1, NA, NA, NA)
ortak_na(x, y)
       
#5
orneklem1 <- function(N, ort, stds){
  ortalama <- mean(rnorm(N,ort,stds))
  return(ortalama)
}

set.seed(10)
orneklem1(100,5,1)



#6

orneklem2 <- function(N, ort, stds, iteras){
  orn.dagilimi <- array()
   for(i in 1:iteras){
    orn.dagilimi[i] <- mean(rnorm(N,ort,stds))
  }
list(ort= mean(orn.dagilimi), std_sapma = sd(orn.dagilimi))
}

set.seed(100)
orneklem2(100,5,1,20)
orneklem2(100,5,1,300)

#7

set.seed(10)
notlar <- sample(1:100, 60)

puan_donusumu <- function(x){
  z_puan <- (x-mean(x))/sd(x)
  t_puan <- 50+(10*z_puan)
list(z_puan= z_puan, t_puan = t_puan, z_ort=mean(z_puan), t_ort=mean(t_puan))
}

puan_donusumu(notlar)

#8

mtk <- function( theta = 0 ,a = 1 ,b=0 , c=0){
           olasılık <- c+(1-c)/(1+exp(-a*(theta-b)))
    return(olasılık)
}

mtk(2,1,1,0.25)



