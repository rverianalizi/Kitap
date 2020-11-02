
#1 

puan2PL <- function(madde, birey)
{
  a <- madde2PL[, 1]
  b <- madde2PL[, 2]
  k <- length(b)
  n <- length(birey)
  theta <- rep(birey, k)
  aa <- rep(a, rep(n, k))
  bb <- rep(b, rep(n, k))
  p <- 1/(1+exp(-((aa)*(theta-bb))))
  rr <- runif(n*k, 0, 1)
  puan <- ifelse(p > rr,1,0)
  puan <- matrix(puan, ncol=k)
  return(puan)
}

yetenek_2PL <- function(madde, birey,r)
{
  n <- length(birey)
  yetenek_r <- matrix(0,n,nrow=r)
for (i in 1:r){
  puan <- puan2PL(madde=madde2PL, birey=birey)
  id <- matrix(1:n, ncol=1)
  data <- cbind(id, puan)
  write.fwf(data, file="data2PL.dat", 
            colnames=F, sep="", justify="right")
  system("blg2PL30.bat")
  sco_degisken <- list(WEIGHT = "", TEST = "", NO.ATTEMPT = "", NO.CORRECT = "", PERCENT.CORRECT = "",
                       SCALESCORE = "", SCALESCORE.SE = "",GROUPFIT.PROB="", MARGINAL.PROB="")
  sco <-  data.frame(scan("2PL30.SCO", skip = 2, sep="", 
                          multi.line = FALSE, fill = TRUE, 
                          what = sco_degisken))
  sco_theta <- as.matrix(sco$SCALESCORE)
  yetenek = matrix(0, 1, nrow=n)
      for(j in 1:length(birey)){
        yetenek[j,]=as.numeric(sco_theta[2*i,])
      }
  yetenek
  yetenek_r[i,] <-matrix(c(yetenek), ncol = 1)
  
}

gercek <-matrix(rep(birey,r), byrow=TRUE, nrow=r)
gercek

rmse <- (colSums((yetenek_r - gercek)^2)/r)^0.5
rmse

bias <- (colSums(yetenek_r)/r)- birey
bias


ortalama <- matrix(rep(colMeans(yetenek_r), r), byrow=TRUE, nrow=r)
ortalama

se <- (colSums((yetenek_r-ortalama)^2)/r)^0.5
se
sonuclar<-t(round((matrix(c(rmse, bias, se), byrow=T, nrow=3)),3))
sonuclar_par<-round(matrix(c(mean(rmse), mean(bias), mean(se)), byrow=T, nrow=1),3)
dimnames(sonuclar_par) <- list(c("yetenek"),c("RMSE","BIAS","SE"))
sonuclar_par
}

round(rmse^2,3)==round(bias^2+se^2,3)


set.seed(41)
birey <- rnorm(400)
madde2PL <- matrix(scan("madde2PL.dat"), byrow=TRUE, ncol=3)

a1 <- yetenek_2PL(madde=madde2PL, birey=birey,r=3)
a2 <- yetenek_2PL(madde=madde2PL, birey=birey,r=10)
a3 <- yetenek_2PL(madde=madde2PL, birey=birey,r=20)

