#question:  What would be considered a replacement reproductive rate for humpback Whales
#question:  can we fit a lotka voltera to discribe humpback whales and herring
#question:  can we fit carrying capacity to a funciton of herring density?

library("data.table", lib.loc="~/R/win-library/3.5")

r<-8
tp<-11.8
S<-0.98
p<-0.36
Sc<-0.7#assumes calf morality in 2nd 6 months = 0.5 mortality in first half of 1st year

age<-0:80
Gen1<-rep(10,81)
Gen1<-round(Sim[100,]*5)
#Sim<-as.data.frame(rbind(age,Gen1))


survival<- function(x,.S=S){
  sum(rbinom(x,1,.S))}
  
simulation<-function(p,.S=S, .Sc=Sc){
  .S<-0.98
  .Sc<-0.7#assumes calf morality in 2nd 6 months = 0.5 mortality in first half of 1st year
  age<-as.character(0:79)
  Sim <- matrix(ncol = 80, nrow = 101)
  colnames(Sim)<-as.character(age)
  rownames(Sim)<-paste("Gen",0:100,sep = " ")
  Sim[1,]<-rep(10,80)
  x<-1:79
  Sim[1,2:ncol(Sim)]<- 526.32*exp(-0.051*x)
    Sim[1,1]<- 500/.Sc
  #Sim[1,]<-Gen1
    
    
  for (Gen in 2:101){
    for (Age in 3:ncol(Sim)){
          Sim[Gen,Age]<-sum(rbinom(Sim[Gen-1,Age-1],1,.S))
    }
    
    Sim[Gen, 2]<-sum(rbinom(Sim[Gen-1,1],1,.Sc))
      
    Sim[Gen, 1]<- sum(rbinom(sum(Sim[Gen-1,12:ncol(Sim)]),1,p/2))
  }
    
  Results<-as.data.frame(cbind(apply(Sim,FUN = sum, 1),1:101))
  mod<-lm(V1~V2,data = Results[1:101,])  
  assign("Sim", Sim, envir = globalenv())
  lm<-abs(mod$coefficients[2])
       if(Results[101,1]<5000) {print(NA)} else { print(lm)}
  }
  
  
plot(apply(Sim,FUN = sum, 1))

Rep<-NA
Calves<-NA
y<-1
while (y < 100){
#simulation(mean(my_vector))
  simulation(median(my_vector))
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

cbr.S<-mean(Sim[,1]/apply(Sim,FUN = sum, 1))
cbr.S.se<-sd(Sim[,1]/apply(Sim,FUN = sum, 1))

ggplot()+ geom_line(aes(Gen, value, color = variable),data = Rep.melt)+ylab("Whale Population")+ xlab("Years")+theme_classic()
#+ylim(0,11000)
