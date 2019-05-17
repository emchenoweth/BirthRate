#Crude birth rate:

library("data.table", lib.loc="~/R/win-library/3.5")
library("ggplot2", lib.loc="~/R/win-library/3.5")


#### THese will be redefined as random variables by the while loop code!!####
r<-8 
tp<-11.8 #mean female reproductive rate
S.max<-0.999  # non-calf survival
p<-0.36 #birth probability
Sc<-0.7#calf mortality assumes calf morality in 2nd 6 months = 0.5 mortality in first half of 1st year
ma<- 80 # max age
OYO<-2000
nmodels<-5000 # how many model simluations do you want to run?

age<-0:ma
Year1<-rep(10,ma+1)
Year1<-round(Sim[100,]*5)

survival<- function(x,.S=S){
  sum(rbinom(x,1,.S))}

.Scalc<-function(Age, .ma = ma, .S = S.max, .Sc = Sc){
  Age<-Age/.ma
  Sprop<-ifelse(Age<0.5,1, ifelse(Age <(15/18), 1-4.397e-05*exp(Age*1.038e+01),-2.1437*Age+2.5460))
  Sprop*.S
}
.Fcalc<-function(Age,.ma = ma, .F.max=F.max, .tp = tp){
  Pmature<-pnorm(Age, mean = .tp, sd = 2.22)
  Age<-Age/.ma
  Fprop<- -1.7826*Age^2+1.9553*Age+0.4389
  Fprop*.F.max*Pmature
}

simulation.ns<-function(F.max,.S=S.max, .Sc=Sc, .ma = ma, .tp = round(tp), .OYO = OYO){
  
  #set up initial matrix
  age<-as.character(0:(.ma-1))
  Sim <- matrix(ncol = .ma, nrow = 201)
  colnames(Sim)<-as.character(age)
  rownames(Sim)<-paste("Year",0:200,sep = " ")
  #Sim[1,]<-rep(10,.ma)
  x<-1:(.ma-1)
  Sim[1,2:ncol(Sim)]<- (.OYO/.S)*exp(-1*(1-.S)*x)
  Sim[1,1]<- .OYO/.Sc
  #Sim[1,]<-Gen1
  #barplot(Sim[150,])
  
  #calculate annual age distributions
  for (Year in 2:201){
    for (Age in 3:ncol(Sim)){
      Sim[Year,Age]<-Sim[Year-1,Age-1]*.Scalc(Age)
    }
    
    Sim[Year, 2]<-Sim[Year-1,1]*.Sc
    
    Sim[Year, 1]<- sum(Sim[Year-1,1:ncol(Sim)]*(.Fcalc(1:ncol(Sim),.F.max = F.max)/2))
  }
  
  #Results<-as.data.frame(cbind(apply(Sim,FUN = sum, 1),1:101))
  #mod<-lm(V1~V2,data = Results[1:101,])  
  assign("Sim", Sim, envir = globalenv())
  #lm<-abs(mod$coefficients[2])
  #print(lm)
  #if(sum(Sim[100,])<75) {print(NA)} else { 
    #print(abs(sum(Sim[100,])-sum(Sim[200,])))
  print(abs(Sim[100,1]-Sim[200,1]))
}

F.max_vector<-c()
cbr.S_vector<-c()
n<-1

#with parameters as random variables
parameters<-c()
while (n < nmodels){
  n
  tp<-rnorm(1,11.8,0.76) #mean female reproductive rate Gabriele 2007 in Zerbini 2010
  S.max<- rnorm(1,0.996,0.02)
  if(S.max>0.999) {S.max<-0.999}
  # non-calf survival Hendrix
  p<-rnorm(1,0.36,0.035) #birth probability Clapham 1992
  Sc<-rnorm(1,0.705,0.084)#calf mortality assumes calf morality in 2nd 6 months = 0.5 mortality in first half of 1st year
  ma<- round(rnorm(1,90,2)) # max age based on whale "Max" with high uncertainty
  OYO<-2000
  
  z<-0
  Fmax.sim<-0.001
  while (Fmax.sim < (0.0011+z)){
    z<-z+0.001
  Fmax.sim<-optim(0.04,simulation.ns, lower = 0.001+z, upper = 0.8, method = "Brent")$par
  }
  
  simulation.ns(Fmax.sim)
  cbr.S<-mean(Sim[100:200,1]/apply(Sim[100:200,],FUN = sum, 1))
  S<-sum((.Scalc(Age = 1:(ma-1))*Sim[150,-1])/sum(Sim[150,-1]))
  Fmean<-sum((.Fcalc(Age = 1:(ma-1), .F.max= Fmax.sim)*Sim[150,-1])/sum(Sim[150,-1]))
  F.max_vector<-c( F.max_vector,Fmax.sim)
  cbr.S_vector<-c(cbr.S_vector,cbr.S)
  
  n<-n+1
  parameters<- rbind(parameters, c(tp,S.max,S,Fmax.sim,Fmean,Sc,ma,OYO,cbr.S))
  #plot(apply(Sim,FUN=sum,1 ))
}
parameters<-as.data.frame(parameters)
names(parameters)<-c("tp", "S.max","S","Fmax.sim","Fmean","Sc","ma","OYO","cbr.S")

###compare###

summary(lm(cbr.S_vector~parameters))

#p should not be a predictor as it was redefined by the model prior to reporting CBR

### PLOT ##

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
  geom_ribbon(aes(Year,ymax=quantile(cbr.S_vector,0.95), ymin = quantile(cbr.S_vector,0.05)),data = CrudeBirthRate, fill = "lightcoral")+
  #geom_ribbon(aes(Year,ymax=0.065333+0.00533, ymin = 0.065333-0.00533),data = CrudeBirthRate[x,], fill = "lightskyblue")+
  geom_point(aes(Year,Crude.Birth.Rate), size = 2, data = CrudeBirthRate)+
  geom_line(aes(Year,Crude.Birth.Rate), size = 1, data = CrudeBirthRate)+
  geom_line(aes(Year, -0.032*log(Number)+0.1595), size = 1,CrudeBirthRate, lty =2)+
  ylab("Crude Birth Rate")+theme_sleek()+
  #geom_line(aes(2000:2014, -0.0004*x+0.0681, color = 2))
  #geom_line(aes(),data = CrudeBirthRate[])+
  #geom_line(aes(Year,0.065333),data = CrudeBirthRate[x,], color = 4)+
  geom_point(aes(Year,Crude.Birth.Rate), color = "lightskyblue",data = CrudeBirthRate[x,])+
  geom_line(aes(Year,quantile(cbr.S_vector)[3]),data = CrudeBirthRate, size =1,color = 2)+
  geom_line(aes(Year,mean(cbr.S_vector)),data = CrudeBirthRate, size =1,color = 2, lty =2)

boxplot(cbr.S_vector)
plot(cbr.S_vector, parameters[,3])
hist(parameters[1:4999,3])
####


mod<-lm(cbr.S_vector~parameters[1:52,c(2,4)])

summary(step(mod))






x<-1

my_vector<-c()
while (x<5){
  my_vector<-c(my_vector, optim(0.2,simulation.ns, lower = 0.0001, upper = 0.5, method = "Brent")$par)
  x<- x+1
}

simulation.ns(mean(my_vector))
barplot(Sim[10, ])
hist(my_vector)
mean(Sim[,1]/apply(Sim,FUN = sum, 1))

plot(apply(Sim,FUN = sum, 1))

Rep<-NA
Calves<-NA
y<-1
while (y < 50){
  simulation.ns(median(my_vector))
  Rep<-cbind(apply(Sim,FUN = sum, 1),Rep)
  Calves<-cbind(Sim[,1],Calves)
  y<-y+1
}

Rep<-as.data.frame(Rep)
names(Rep)<-paste("Rep",1:50,sep = " ")
Gen<-1:101
Rep<-cbind(Rep, Gen)
summary(as.data.frame(Rep))
Rep.melt<-melt(Rep, id = c("Gen")) 

cbr.S<-mean(Sim[81:100,1]/apply(Sim[81:100,],FUN = sum, 1))
cbr.S.se<-sd(Sim[81:100,1]/apply(Sim[81:100,],FUN = sum, 1))

ggplot()+ geom_line(aes(Gen, value, color = variable),data = Rep.melt)+ylab("Whale Population")+ xlab("Years")+theme_classic()
#+ylim(0,11000)


Are fluctuations dependant or stochastic??

  Take original data and apply indepedance test. 



Are fluctuations a natural results of fluctuations?
for (Year in 1:200)
  
  whales<-c()
  new.whale<-function(Year = Year){
    BY<-rnorm(1,11.8,2.2)
    whales<-rbind(c(dim(whales)[1]+1,rbinom(1),.Year,BY,0,NA,NA))
  }
  names(whales)<-c("Whale", "Sex", "BirthYear","Mature","NCalves", "DeathYear", "RecentCalf")


Are fluctuations a results of enivronmental variability?
  