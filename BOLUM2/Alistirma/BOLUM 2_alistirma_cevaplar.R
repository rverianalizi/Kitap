#1a
isim<-c("Ali","Defne","Meltem","Semih","Sevda", "Gizem","Emre","Zeynep", "Utku", "Beril")
isim
#1b
vize<-c(60,70,50,80,65,70,85,70,92,80)
vize
#10c
final<-c(70,65,50,45,67,75,80,85,95,85)
final
#10d
names(vize)<-isim
names(final)<-isim
#10e
vize["Ali"]
final["Beril"]
#10f
mean(vize)
mean(final)
sd(vize)
sd(final)
#10g
summary(vize)
summary(final)
#10h
vize>90 & final>90
#10i
vize<50 | final<50
#10j
yeni <- final>vize;yeni
#10k
vize_y <- vize[final>50];vize_y
final_y <- final[final>50];final_y
#10l
length(vize_y)
length(final_y)
#10m
which.max(vize)
which.max(final)
#10n
vize_toplam<-sum(vize)
vize_toplam
final_toplam<-sum(final)
final_toplam
#10o
not<-(0.6*final_y)+(0.4*vize_y)
not
summary(not)
#10p
not_s<-sort(not,decreasing = TRUE)
not_s
#10q
ls.str()
#10r
not_s[not_s>mean(not_s)]
#10s
which(not_s==66)
which(not_s==72)
which(not_s==88)

#2
round(seq(1,20,length.out = 40),2)
#3a
paste("ogrenci",1:10,sep="_")
#3b
rep(c(10,15,25,50,100),c(3,5,1,4,3))
#3c
rep(1:3,c(10,10,10))
#4
cinsiyet<-c("erkek","kiz","erkek","kiz","kiz","kiz","kiz","erkek","kiz")
cinsiyet<- factor(cinsiyet)
cinsiyet

cinsiyet2 <- as.numeric(cinsiyet)
factor(cinsiyet2)
levels(cinsiyet2) <- c("e","k")

#5
not
harf_not <- cut(not,breaks = c(0,49.9,59.9,69.9,79.9,89.9,100),labels=c("F","E","D",
                                                            "C","B","A"), 
    ordered_result=TRUE)


#6a
m1 <- cbind(vize_y,final_y,not);m1
#6b
m1 <- rbind(m1,colMeans(m1))
#6c
cbind(m1,harf_not)

