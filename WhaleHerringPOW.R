WhalesHerring <- read.csv("C:/Users/emchenoweth/Desktop/BirthRate/BirthRate/WhaleHerringData.csv")
summary(WhalesHerring)
WhalesHerring<-WhalesHerring[c(1,2,5,6,7,8,),]
devtools::install_github("seananderson/ggsidekick")
library(ggsidekick)
library(ggplot2)
library("mgcv", lib.loc="~/R/win-library/3.5")
library("AICcmodavg", lib.loc="~/R/win-library/3.5")
library("gridExtra", lib.loc="~/R/win-library/3.5")

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


mod<-lm(Whales~Herring.biomass, data = WhalesHerring)
summary(mod)

mod1<-lm(Whales~poly(Year,0),data = WhalesHerring)
AICc(mod1)
summary(step(mod1))

mod2<-lm(Herring.mile.days.of.spawn~poly(Year,2),data = WhalesHerring)
mod2<-gam(Herring.mile.days.of.spawn~s(Year),data = WhalesHerring)
AICc(mod2)
summary(mod2)  
plot(gammod)

ggplot()+  
  geom_smooth (aes(Herring.biomass,Whales), data = WhalesHerring,lty = 2,method = "lm", size = 1.2, color = "gray", fill = "light gray")+
  geom_point(aes(Herring.biomass,Whales), data = WhalesHerring)+
  geom_path(aes(Herring.biomass,Whales), data = WhalesHerring, size = 1.2)+
  xlab("Herring Biomass (metric tons)")+
  theme_sleek()+ geom_text(aes(Herring.biomass,Whales,label = Year), data = WhalesHerring,hjust = 0, nudge_x = 0.05)


p1<-ggplot()+
  geom_point(aes(Year,Whales), data = WhalesHerring)+
  geom_smooth(aes(Year,Whales), method = "gam", color = "black", size = 0.5, data = WhalesHerring)+
  theme_sleek()+ylab("Unique Whales")+xlab("")

p2<-ggplot()+  
  geom_ribbon(aes(x = Year, ymax = predict(mod2)+predict(mod2,se = TRUE)$se.fit*1.96, ymin = predict(mod2)-predict(mod2,se = TRUE)$se.fit*1.96),fill = "light gray", data = WhalesHerring)+
  geom_point(aes(Year,Herring.mile.days.of.spawn), data = WhalesHerring)+
  geom_line(aes(Year,predict(mod2)), data = WhalesHerring)+
  theme_sleek()+
  theme_sleek()+ylab("Miles of Herring Spawn")

gA <- ggplotGrob(p1)
gB <- ggplotGrob(p2)
maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)
grid.arrange(gA, gB, ncol=1)

ggplot()+
  geom_point(aes(Year,Herring.mile.days.of.spawn), data = WhalesHerring)+
  geom_line(aes(Year,predict(mod2)), data = WhalesHerring)+
  theme_sleek()+ylab("Miles of Herring Spawn")

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

mod4<-lm(WhalesHerring[!is.na(WhalesHerring$Whales),]$Whales~Herring.biomass+Herring.mile.days.of.spawn, data = WhalesHerring[!is.na(WhalesHerring$Whales),])
mod5<-lm(WhalesHerring[!is.na(WhalesHerring$Whales),]$Whales~Herring.mile.days.of.spawn, data = WhalesHerring[!is.na(WhalesHerring$Whales),])
mod6<-lm(WhalesHerring[!is.na(WhalesHerring$Whales),]$Whales~Herring.biomass, data = WhalesHerring[!is.na(WhalesHerring$Whales),])

summary(step(mod4))
summary(step(mod5))
summary(step(mod6))

plot(mod4)

ggplot()+
  theme_sleek()+
  ylab("Observed Whales")+
  xlab("Predicted Whales")+
  geom_point(aes(predict(mod4), WhalesHerring[!is.na(WhalesHerring$Whales),]$Whales), data = WhalesHerring[!is.na(WhalesHerring$Whales),])+
  geom_smooth(aes(predict(mod4), WhalesHerring[!is.na(WhalesHerring$Whales),]$Whales), method = "lm",data = WhalesHerring[!is.na(WhalesHerring$Whales),])
