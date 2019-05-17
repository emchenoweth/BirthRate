devtools::install_github("seananderson/ggsidekick")
library(ggsidekick)
library(ggplot2)

CrudeBirthRate <- read.csv("C:/Users/emchenoweth/Desktop/BirthRate/CrudeBirthRate/CrudeBirthRate.csv")
summary(CrudeBirthRate)
e<-2.71828
X<-2000:2020
Y<-5*log(X+6)+18
mod2<-nls(Crude.Birth.Rate~ CONST+log(Number)*A, start = list(CONST = 2, A = 2), data = CrudeBirthRate)

-0.032*log(CrudeBirthRate$Year,e)+0.1595

Crude.Birth.Rate ~ CONST+log(Crude.Birth.Rate)*A
x<-15:29
mod<-lm(Crude.Birth.Rate~Number, data = CrudeBirthRate[x,])
summary(step(mod))
mod<-nls(CrudeBirthRate$Crude.Birth.Rate~ Const+ A*log(CrudeBirthRate$Year),start = list(Const = 0.1595, A = -0.032), data = CrudeBirthRate)
CrudeBirthRate$fitted<-fitted(mod)
ggplot()+
  geom_ribbon(aes(Year,ymax=0.065333+0.00533, ymin = 0.065333-0.00533),data = CrudeBirthRate[x,], fill = "lightskyblue")+
  geom_ribbon(aes(Year,ymax=0.0677+0.002, ymin = 0.0677-0.002),data = CrudeBirthRate, fill = "lightcoral")+
  geom_point(aes(Year,Crude.Birth.Rate), size = 2, data = CrudeBirthRate)+
  geom_line(aes(Year,Crude.Birth.Rate), size = 1, data = CrudeBirthRate)+
  geom_line(aes(Year, -0.032*log(Number)+0.1595), size = 1,CrudeBirthRate, lty =2)+
  ylab("Crude Birth Rate")+theme_sleek()+
  #geom_line(aes(2000:2014, -0.0004*x+0.0681, color = 2))
  #geom_line(aes(),data = CrudeBirthRate[])+
  geom_line(aes(Year,0.065333),data = CrudeBirthRate[x,], color = 4)+
  geom_point(aes(Year,Crude.Birth.Rate), color = 4,data = CrudeBirthRate[x,])+
  geom_line(aes(Year,0.0677),data = CrudeBirthRate, size =1,color = 2)
  

ggplot(CrudeBirthRate, aes(Year,Crude.Birth.Rate))+geom_point()+stat_smooth(method = loess, se =FALSE)
geom_line(aes(Year, -0.032*log(Year)+0.1595),CrudeBirthRate)+
  geom_smooth(aes(Year,Crude.Birth.Rate), method = "nls", formula = ,start = c(CONST = 17.4, A = 5.5),data = CrudeBirthRate)+
  ylab("Crude Birth Rate")+theme_sleek()
plot(mod)
