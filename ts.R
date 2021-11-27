install.packages("timeSeries")
install.packages("forecast")
library(RODBC)
library(ggplot2)
library(dplyr)
library(readxl)
library(timeSeries)
library(forecast)

# 11 ----------------------------------------------------------


ventas <- read_excel("./ventas.xlsx")
View(ventas)
# 12 ----------------------------------------------------------
ventas.ts<-ts(ventas)
class(ventas.ts)
# 13 ----------------------------------------------------------
ventas.ts.a <- ts(ventas,start = c(1986,1),frequency = 12)
ventas.ts.a
plot(ventas.ts.a,type="o")

# 14 ----------------------------------------------------------

flour.l<- log(ventas.ts.a[,1])

flour.stl<-stl(flour.l,s.window = "period")

plot(flour.stl)

# 15 ----------------------------------------------------------

inf.hw<-HoltWinters(ventas.ts.a)
plot(inf.hw,col="blue",col.predicted="red")

# 16 ----------------------------------------------------------

infy.fore <- forecast(inf.hw,h=12)
plot(infy.fore)
