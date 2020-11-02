#1
ornek1 <- read.table("ornek1.txt",skip=2,header=TRUE)
ornek1
str(ornek1)
ornek1$id <- as.character(ornek1$id)
#2
ability <- read.csv("ability.csv")
str(ability)

#3
arasinav <-read.table("arasinav.txt",skip=3,header=TRUE)
#4
arasinav2.not <-read.table("arasinav2.txt",skip=4,sep="\\")[,2]


