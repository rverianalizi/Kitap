#1a
8+12
#1b
8^2 #8*8
#1c
(5 + 5) / 2 
#1d
36^(1/2) # sqrt(36)
#2
8+12 #toplama islemi
8^2 #kare alma 
(5 + 5) / 2  # toplami bölme islemi
36^(1/2) # sqrt(36) # karekok alma islemi
#3a
r <- 5
#3b
pi 
#3c
pi*r*r   #pi*r*r
#3d
r <- 5
alan1 <- pi*r*r
r <- 10
alan2 <- pi*r*r
r <- 15
alan3 <- pi*r*r
#3e
alan1 < alan2
alan2 > alan1
alan3 == alan1
alan2 != alan3
#3f
ls()
#3g
rm(list=ls())
#4
x <- 56
z <- (x-mu)/sigma
#4a
mu <- 40
sigma <- 4
z <- (x-mu)/sigma;z

#4b
mu <- 40
sigma <- 8
z <- (x-mu)/sigma;z

#4c
mu <- 40
sigma <- 16
z <- (x-mu)/sigma;z

#4d
mu <- 40
sigma <- 32
z <- (x-mu)/sigma;z

#5
install.packages("data.table")
library(data.table) # require(data.table)
#6
install.packages("foreign") # islemleri sag alt pencereden yapiniz
library(data.table) # require(data.table)

#7
getwd()
#8
setwd("C:/Users/<kullanıcı adı>/Desktop/R _Giris") # kendi bilgisayarınız icin komut degisecektir.
# Session menu-set workind directory- choose directory islemleri ile de yapabilirsiniz.
#9 
rnorm
set.seed(1000)
dagilim <- rnorm(1000,5,10)
mean(dagilim)
sd(dagilim) 
hist(dagilim)
