
par(font=1, las=1) 
plot(0:15, 0:15, type="n", xaxt="n", yaxt="n", ylim=c(15,0), xlab="", ylab="",
     main="Font 1 Sembolleri
           Satır Numarası 16 ile Çarpılır, 
           Çarpıma Sütun Numarası Eklenerek pch Değeri Bulunur") 
axis(BOTTOM<-1, at=0:15, 1:16) 
axis(LEFT<-2, at=0:15) 
abline(v=0.5+0:14, h=0.5+0:14, col="grey", lty="dotted") 
 
# Her bir hücredeki değeri 16*(satirno)+(sütunno)
for(i in 0:255){ 
  x <- i%%16; 
  y <- i%/%16; 
  points(x, y, pch=i+1) 
} 

par(font=5, las=1) 
plot(0:15, 0:15, type="n", xaxt="n", yaxt="n", ylim=c(15,0), xlab="", ylab="",
     main="Font 1 Sembolleri
          Satır Numarası 16 ile Çarpılır, 
          Çarpıma Sütun Numarası Eklenerek pch Değeri Bulunur") 
axis(BOTTOM<-1, at=0:15, 1:16) 
axis(LEFT<-2, at=0:15) 
abline(v=0.5+0:14, h=0.5+0:14, col="grey", lty="dotted") 
 
# pch index of any cell is 16*row+column 
for(i in 0:255){ 
  x <- i %%16; 
  y <- i %/% 16; 
  points(x, y, pch=i+1) 
} 
