
devtools::install_github("seananderson/ggsidekick")
library(ggsidekick)
library(ggplot2)

CrudeBirthRate <- read.csv("C:/Users/emchenoweth/Desktop/BirthRate/CrudeBirthRate.csv")
summary(CrudeBirthRate)
e<-2.71828
X<-2000:2020
Y<-5*log(X+6)+18
mod2<-nls(Crude.Birth.Rate~ CONST+log(Year)*A, start = list(CONST = 2, A = 2), data = CrudeBirthRate)

-0.032*log(CrudeBirthRate$Year,e)+0.1595

Crude.Birth.Rate ~ CONST+log(Crude.Birth.Rate)*A

mod<-nls(CrudeBirthRate$Crude.Birth.Rate~ Const+ A*log(CrudeBirthRate$Year),start = list(Const = 0.1595, A = -0.032), data = CrudeBirthRate)
CrudeBirthRate$fitted<-fitted(mod)
ggplot()+geom_point(aes(Year,Crude.Birth.Rate), data = CrudeBirthRate)+
  geom_line(aes(Year, -0.032*log(Year)+0.1595),CrudeBirthRate)+
  geom_smooth(aes(Year,Crude.Birth.Rate), method = "nls", formula ='Crude.Birth.Rate~ CONST+log(YEAR)*A' ,start = c(CONST = 17.4, A = 5.5),data = CrudeBirthRate)+
  ylab("Crude Birth Rate")+theme_sleek()

ggplot(CrudeBirthRate, aes(Year,Crude.Birth.Rate))+geom_point()+stat_smooth(method = loess, se =FALSE)
  geom_line(aes(Year, -0.032*log(Year)+0.1595),CrudeBirthRate)+
  geom_smooth(aes(Year,Crude.Birth.Rate), method = "nls", formula = ,start = c(CONST = 17.4, A = 5.5),data = CrudeBirthRate)+
  ylab("Crude Birth Rate")+theme_sleek()
plot(mod)
