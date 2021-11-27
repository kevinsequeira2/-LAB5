install.packages("corrplot")
install.packages("PerformanceAnalytics")
install.packages("psych")
install.packages("caret")
library(RODBC)
library(ggplot2)
library(dplyr)
library(corrplot)
library(PerformanceAnalytics)
library(psych)
library(caret)

# 2 ----------------------------------------------------------
airquality<-airquality
airquality
View(airquality)
# 3 ----------------------------------------------------------
print(apply(is.na(airquality),2, mean))
print(apply(is.na(airquality),2, sum))

# 4 ------------------------------------------------------------
airquality <- na.omit(airquality)

# 5 ------------------------------------------------------------

cor_quality_mat<-cor(airquality,use="pairwise.complete.obs") 
cor_quality_mat
cor_quality <- cor(airquality)
pairs.panels(cor_quality,smooth = TRUE,21,lm = FALSE,cor = TRUE,jiggle = FALSE,     
             factor = 2,hist.col = 4) 

corrplot(cor_quality,method = "number", type = "upper", order = "original",
         tl.col = "black", tl.srt = 45)

# 6  ------------------------------------------------------------


c1 <-"blue"
c2 <-"red"
c3 <-rainbow(4, v=0.7)

par(mfrow=c(3,5))


for (i in 1:6){
  boxplot(airquality[,i],main=names(airquality)[i],axes=FALSE,col=c1,
          medcol = c2,
          staplecol=c3,boxcol=c3,outcol=c3,pch=4, cex=1)
}
# 7  ------------------------------------------------------------

outliersReplace <- function(data, lowLimit, highLimit){
  data[data < lowLimit] <- mean(data)
  data[data > highLimit] <- median(data)
  data           
}

correcion <- outliersReplace(airquality$Ozone, 100,1000)
# 8  ------------------------------------------------------------
boxplot(airquality$Ozone)
# 9  ------------------------------------------------------------
train <- createDataPartition(airquality$Ozone,p=0.7,list=F)
train
# 10  ------------------------------------------------------------

# Yo aplicaria el modelo de regresion lineal logistica ya que
# poseemos varables de resultado en las que se puede ganar o perder
# y variables predictivas continuas que se relacionan con la probabilidad o variable
# de resultado.También se puede utilizar con predictores categóricos y con múltiples predictores

m1 <- glm(formula = as.factor(Ozone)  ~ . ,
          data= airquality[train,], family = binomial)
summary(m1)

