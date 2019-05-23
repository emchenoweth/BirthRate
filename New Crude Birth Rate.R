

#######libraries#####

library("data.table", lib.loc="~/R/win-library/3.5")
library("ggplot2", lib.loc="~/R/win-library/3.5")
devtools::install_github("seananderson/ggsidekick")
library(ggsidekick)
library(ggplot2)
library("PerformanceAnalytics")



#### Define you parameters!!####

tp.mean<-11.8 #mean female reproductive rate Gabriele 2007 in Zerbini 2010
tp.sd<-11.8
S.max.mean<-0.999  # non-calf survival
S.max.sd<-0.01
Sc.mean<-0.705#calf mortality assumes calf morality in 2nd 6 months = 0.5 mortality in first half of 1st year
Sc.sd<-0.084
ma.mean<- 90 # max age based on whale "Max" with high uncertainty
ma.sd<-2
OYO<-2000
nmodels<-500#number of model iterations you want to run
Year.max<-200
#p<-0.36 #birth probability, will be definied by the model



### Original Functions ####


#Function to calculate the Age-specific non-calf survival
Scalc<-function(Age, .ma = ma, .S = S.max, .Sc = Sc, .tp = tp){
  
  Sprop<-ifelse(Age< .tp, -0.1898*(Age/.tp)^2 +0.3857*(Age/.tp)+0.7938,
            ifelse((Age/.ma)<0.5,1, 
                ifelse((Age/.ma) <(15/18), 
                       1-2.199e-05*exp((Age/.ma)*1.038e+01),-1.07183*(Age/.ma)+1.77302)))
  Sprop*.S
}

#Function to calculate Age-specific birth rate (p)
Pcalc<-function(Age,.ma = ma, .Pmax=Pmax, .tp = tp){
  Pmature<-pnorm(Age, mean = .tp, sd = 2.22)
  Age<-Age/.ma
  Fprop<- -1.7826*Age^2+1.9553*Age+0.4389
  Fprop*.Pmax*Pmature
}

#function to be minizized:output is birth rate (p)
simulation.ns<-function(Pmax,.S=S.max, .Sc=Sc, .ma = ma, .tp = round(tp), .OYO = OYO){
  
  #set up initial matrix
  age<-as.character(0:(.ma-1))
  Sim <- matrix(ncol = .ma, nrow = 201)
  colnames(Sim)<-as.character(age)
  rownames(Sim)<-paste("Year",0:Year.max,sep = " ")
  x<-1:(.ma-1)
  Sim[1,2:ncol(Sim)]<- (.OYO/.S)*exp(-1*(1-.S)*x)
  Sim[1,1]<- .OYO/.Sc
 
  
  #calculate annual age distributions
  for (Year in 2:201){
    for (Age in 3:ncol(Sim)){
      Sim[Year,Age]<-Sim[Year-1,Age-1]*Scalc(Age)
    }
    
    Sim[Year, 2]<-Sim[Year-1,1]*.Sc
    Sim[Year, 1]<- sum(Sim[Year-1,1:ncol(Sim)]*(Pcalc(1:ncol(Sim),.Pmax = Pmax)/2))
  }
  
  assign("Sim", Sim, envir = globalenv())
  
  #value to be minimized
  print(abs(Sim[(Year.max-50),1]-Sim[Year.max,1]))
}

###Begin Model###

Pmax_vector<-c()
cbr.S_vector<-c()
n<-1
parameters<-c()
while (n < nmodels){
 
  tp<-rnorm(1,tp.mean,tp.sd) 
  S.max<- rnorm(1,S.max.mean,S.max.sd)
  if(S.max>1) {S.max<-1}
  Sc<-rnorm(1,Sc.mean,Sc.sd)
  ma<- round(rnorm(1,ma.mean,ma.sd)) 
  
  z<-0
  Pmax.sim<-0.001
  while (Pmax.sim < (0.0011+z)){
    z<-z+0.001
    #find the max birth rate (p.max) that results in a stable population for a set of given other life history parameters
  Pmax.sim<-optim(0.04,simulation.ns, lower = 0.001+z, upper = 0.8, method = "Brent")$par
  }
  
  #apply that birth rate then extract corresponding variables form the resulting age distribution
  simulation.ns(Pmax.sim)
  cbr.S<-mean(Sim[(Year.max-50):Year.max,1]/apply(Sim[(Year.max-50):Year.max,],FUN = sum, 1))
  S<-sum((Scalc(Age = 1:(ma-1))*Sim[(Year.max-50),-1])/sum(Sim[(Year.max-50),-1]))
  Pmean<-sum((Pcalc(Age = 1:(ma-1), .Pmax= Pmax.sim)*Sim[(Year.max-50),-1])/sum(Sim[(Year.max-50),-1]))
  Pmax_vector<-c( Pmax_vector,Pmax.sim)
  cbr.S_vector<-c(cbr.S_vector,cbr.S)
  
  n<-n+1
  parameters<- rbind(parameters, c(tp,S.max,S,Pmax.sim,Pmean,Sc,ma,OYO,cbr.S))
  #plot(apply(Sim,FUN=sum,1 ))# Shows how population changes over time (and hopefully stabilizes)
}
parameters<-as.data.frame(parameters)
names(parameters)<-c("tp", "S.max","S","Pmax.sim","Pmean","Sc","ma","OYO","cbr.S")



### PLOT ##

CrudeBirthRate <- read.csv("C:/Users/emchenoweth/Desktop/BirthRate/BirthRate/CrudeBirthRate.csv")
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
  geom_ribbon(aes(Year,ymax=quantile(cbr.S_vector,0.95), ymin = quantile(cbr.S_vector,0.05)),data = CrudeBirthRate, fill = "lightcoral")+
  geom_point(aes(Year,Crude.Birth.Rate), size = 2, data = CrudeBirthRate)+
  geom_line(aes(Year,Crude.Birth.Rate), size = 1, data = CrudeBirthRate)+
  geom_line(aes(Year, -0.032*log(Number)+0.1595), size = 1,CrudeBirthRate, lty =2)+
  ylab("Crude Birth Rate")+theme_sleek()+
  geom_point(aes(Year,Crude.Birth.Rate), color = "lightskyblue",data = CrudeBirthRate[x,])+
  geom_line(aes(Year,quantile(cbr.S_vector)[3]),data = CrudeBirthRate, size =1,color = 2)+
  geom_line(aes(Year,mean(cbr.S_vector)),data = CrudeBirthRate, size =1,color = 2, lty =2)

boxplot(cbr.S_vector)
plot(cbr.S_vector, parameters[,3])
hist(parameters[1:4999,3])

chart.Correlation(parameters[-8], histogram=TRUE, pch=19)
####
parameters0.9999<-parameters
barplot(Sim[Year.max,])


mod<-lm(parameters$cbr.S~parameters$tp+parameters$S.max+parameters$Pmax.sim+parameters$Sc+parameters$ma)

summary(step(mod))




