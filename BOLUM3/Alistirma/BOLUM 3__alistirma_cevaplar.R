isim<-c("Ali","Defne","Meltem","Semih","Sevda", "Gizem","Emre","Zeynep", "Utku", "Beril")
vize<-c(60,70,50,80,65,70,85,70,92,80)
final<-c(70,65,50,45,67,75,80,85,95,85)
bilgi <- list(isim,vize,final)
rm(isim,vize,final)
#1a
str(bilgi)
#1b
names(bilgi) <- c("öğrenci","vize_not","final_not")
#1c
bilgi$not <- bilgi$final_not*0.6 + bilgi$vize_not*0.40
#1d
bilgi$vize_not
#1e
bilgi$vize_not[3]
#1f
which(bilgi$not==(max(bilgi$not)))
bilgi$not[9]<-100



#2
isim<-c("Ali","Defne","Meltem","Semih","Sevda", "Gizem","Emre","Zeynep", "Utku", "Beril")
vize<-c(60,70,50,80,65,70,85,70,92,80)
final<-c(70,65,50,45,67,75,80,85,95,85)
test <- data.frame(isim,vize,final)
rm(isim,vize,final)
#2a
View(test)
#2b
test$not <- test$vize*0.6 + test$final*0.40
#2c
test_gecti <- subset(test,not>70)
#2d
dim(test_gecti)
#2e
summary(test_gecti[,-1])
#2f
test_gecti[order(test_gecti$not),]
