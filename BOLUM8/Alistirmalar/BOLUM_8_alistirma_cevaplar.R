library(lattice)
library(ggplot2)
library(gridExtra)

dat1<- read.csv("bolum_8_alistirma_veri.csv")  

#Soru1

pdf("Histogram Grafikleri.pdf") 

histogram(~ PUAN | IL, data = dat1, 
          xlab = "My y variable", col = "gray")

histogram(~ PUAN | CINSIYET, data = dat1, 
          xlab = "My y variable", col = "gray")
		  


ggplot(aes(x = PUAN), data = dat1) + geom_histogram() + 
    xlab("PUAN") + ylab("N") + facet_wrap(~IL)
	
	
ggplot(aes(x = PUAN), data = dat1) + geom_histogram() + 
    xlab("PUAN") + ylab("N") + facet_wrap(~CINSIYET)
	
dev.off() 
	
	
	
#Soru2
	
	
	
pdf("Histogram ve Yoğunluk Grafikleri.pdf") 
	
histogram(~ PUAN | IL, data = dat1, xlab = "PUAN", 
          col = "gray", type = "density",
          panel = function(...){
              panel.histogram(...);
              panel.densityplot(..., 
                                col.line = "black")
          }
)	

histogram(~ PUAN | CINSIYET, data = dat1, xlab = "PUAN", 
          col = "gray", type = "density",
          panel = function(...){
              panel.histogram(...);
              panel.densityplot(..., 
                                col.line = "black")
          }
)	
	
	
	
ggplot(aes(x = PUAN), data = dat1) + 
    geom_histogram(aes(y = ..density..)) + 
    geom_density(colour = "grey", size = 1.2) + 
    xlab("PUAN") + ylab("N") + 
    facet_wrap(~IL)

	
ggplot(aes(x = PUAN), data = dat1) + 
    geom_histogram(aes(y = ..density..)) + 
    geom_density(colour = "grey", size = 1.2) + 
    xlab("PUAN") + ylab("N") + 
    facet_wrap(~CINSIYET)

dev.off() 

		
#Soru3
	
pdf("Kutu Grafikleri.pdf") 
	
	
bwplot(PUAN ~ CINSIYET | IL, data = dat1)

ggplot(dat1, aes(x = CINSIYET, y = PUAN, fill = IL)) +
geom_boxplot() +
facet_wrap(~ IL)

dev.off() 




#Soru4

RMSE <- read.csv("RMSE.csv", sep = ",", header = T)

RMSE$Madde.Sayisi= as.factor(RMSE$Madde.Sayisi)
RMSE$Boyutlararasi_Kor= as.factor(RMSE$Boyutlararasi_Kor)
RMSE$Kalibrasyon= as.factor(RMSE$Kalibrasyon)

names(RMSE)
par.settings <- list(superpose.symbol = list(col = c("red", "green",
                                                     "blue", "black"),
                                             fill = c("red", "green", "blue", "black")), 
          superpose.line = list(col = c("red", "green", "blue", "black")), pch=18)

xyplot(Yontem_1 + Yontem_2 + Yontem_3 + Yontem_4 ~ Kalibrasyon|Madde.Sayisi + Boyutlararasi_Kor, data=RMSE, 
       panel = function( x,y,...) {
         panel.abline( h=c(0.005, 0.01,0.015, 0.02,0.025,0.03), lty = "dotted", col = "gray")
         panel.xyplot( x,y,...)
       },
       main="RMSE Degerleri",
       xlab="Kalibrasyon Yontemi",
       ylab="RMSE",
       layout=c(2,3), par.settings = par.settings,auto.key=list(columns= 1, space="right",lines=TRUE, 
                                                                cex= 1.1, points=F, size=1),
       type="b", ylim=c(0,0.03),
 
             distribute.type = TRUE, as.table=TRUE) 



#Soru5
df<-read.csv("mirt.csv") 
df$ScoreCategory <- as.factor(df$ScoreCategory)
head(df)
str(df)


scatterPlot <- ggplot(df,aes(Theta1, Theta2, color=ScoreCategory)) + 
  geom_point() + 
  scale_color_manual(values = c('#FF0000','#008000')) + 
  theme(legend.position=c(0,0), legend.justification=c(0,0)) +
  geom_hline(yintercept=0) + geom_vline(xintercept=0)+ xlim(-5, 5)+ ylim(-5, 5)+
   labs(x = "Theta 1", 
                y = "Theta 2", 
                title = "Some Plots")
  
scatterPlot



xdensity <- ggplot(df, aes(Theta1, fill=ScoreCategory)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c('#FF0000','#008000')) + 
   theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
     
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.position="none")
xdensity


ydensity <- ggplot(df, aes(Theta2, fill=ScoreCategory)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c('#FF0000','#008000')) + 
  theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.position="none") + coord_flip()
ydensity


blankPlot <- ggplot()+geom_blank(aes(1,1))+
  theme(plot.background = element_blank(), 
   panel.grid.major = element_blank(),
   panel.grid.minor = element_blank(), 
   panel.border = element_blank(),
   panel.background = element_blank(),
   axis.title.x = element_blank(),
   axis.title.y = element_blank(),
   axis.text.x = element_blank(), 
   axis.text.y = element_blank(),
   axis.ticks = element_blank()
     )
	 
	
grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, 
        ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))















