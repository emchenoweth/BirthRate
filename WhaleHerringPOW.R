WhalesHerring <- read.csv("C:/Users/emchenoweth/Desktop/BirthRate/BirthRate/WhaleHerringData.csv")
summary(WhalesHerring)
WhalesHerring<-WhalesHerring[c(1,2,5,6,7,8,),]
devtools::install_github("seananderson/ggsidekick")
library(ggsidekick)
library(ggplot2)

mod<-lm(Whales~Herring.biomass, data = WhalesHerring)
summary(mod)

ggplot()+  
  geom_smooth (aes(Herring.biomass,Whales), data = WhalesHerring,lty = 2,method = "lm", size = 1.2, color = "gray", fill = "light gray")+
  geom_point(aes(Herring.biomass,Whales), data = WhalesHerring)+
  geom_path(aes(Herring.biomass,Whales), data = WhalesHerring, size = 1.2)+
  xlab("Herring Biomass (metric tons)")+
  theme_sleek()+ geom_text(aes(Herring.biomass,Whales,label = Year), data = WhalesHerring,hjust = 0, nudge_x = 0.05)


ggplot()+
  geom_point(aes(Year,Whales), data = WhalesHerring)+
  geom_smooth(aes(Year,Whales),method = "lm", data = WhalesHerring)+
  theme_sleek()

ggplot()+
  geom_point(aes(Year,Herring.biomass), data = WhalesHerring)+
  geom_smooth(aes(Year,Herring.biomass), data = WhalesHerring)+
  theme_sleek()+ylab("Herring biomass (metric tons)")


ggplot()+  
  geom_smooth (aes(Herring.mile.days.of.spawn,whales.effort), data = WhalesHerring,lty = 2,method = "lm", size = 1.2, color = "gray", fill = "light gray")+
  geom_point(aes(Herring.mile.days.of.spawn,whales.effort), data = WhalesHerring)+
  geom_path(aes(Herring.mile.days.of.spawn,whales.effort), data = WhalesHerring, size = 1.2)+
  xlab("Herring Biomass (metric tons)")+
  theme_sleek()+ geom_text(aes(Herring.mile.days.of.spawn,whales.effort,label = Year), data = WhalesHerring,hjust = 0, nudge_x = 0.05)

ggplot()+
  geom_point(aes(Whales,Effort.Days),data = WhalesHerring)+
  geom_smooth(aes(Whales,Effort.Days),method = "lm",data = WhalesHerring)

mod2<-lm(Effort.Days~Whales, data = WhalesHerring)
summary(mod2)

ggplot()+
  geom_point(aes(Whales,whales.effort.day),data = WhalesHerring)+
  theme_sleek()+
  geom_smooth(aes(Whales,whales.effort.day),method = "lm",data = WhalesHerring)

ggplot()+
  geom_point(aes(Herring.biomass,Herring.mile.days.of.spawn),data = WhalesHerring)+
  theme_sleek()+
  geom_smooth(aes(Herring.biomass,Herring.mile.days.of.spawn),method = "lm",data = WhalesHerring)

mod3<-lm(Herring.mile.days.of.spawn~Herring.biomass, data = WhalesHerring)
summary(mod3)

mod4<-lm(cbind(WhalesHerring[!is.na(WhalesHerring$Whales),]$Whales,WhalesHerring[!is.na(WhalesHerring$Whales),]$whales.effort.day)~Herring.biomass+Herring.mile.days.of.spawn, data = WhalesHerring[!is.na(WhalesHerring$Whales),])
summary(mod4)

plot(mod4)

ggplot()+
  geom_point(aes(predict(mod4), WhalesHerring$Whales), data = WhalesHerring[!is.na(WhalesHerring$Whales),])
