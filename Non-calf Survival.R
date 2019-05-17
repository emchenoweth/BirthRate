.S_vec<- c(rep(.S,(8/18)*.ma))
           
 vecS<-c(1-0.9671,1-0.959,1-0.9467,1-0.9267,1-0.897,1-0.802,1-0.729,1-0.633,1-0.517)
 vecS<-c(1,0.99484,0.98658,0.97317,0.95253,0.9226,0.82456,0.74923,0.65119,0.53148,0.39216)
 vecS1<-vecS[c(1:6,8)]
  vecY<-c((8/18),(9/18), (10/18),(11/18),(12/18),(13/18),
        (14/18),(15/18),(16/18),(17/18), 18/18)
  vecY1<-vecY[c(1:6,8)]
  
plot(vecY,vecS) 

Smod<-nls(vecS1~1-(a*exp((vecY1*b))),start = list(a = 0.0012, b = 6.35))
summary(Smod)
vecS2<-vecS[8:11]
vecY2<-vecY[8:11]
Smod2<-lm(vecS2~vecY2)
summary(Smod2)
plot(Smod2)

ggplot()+
  geom_point(aes(vecY,vecS))+
  geom_line(aes(vecY1,1-4.397e-05*exp(1.038e+01*vecY1)))+
  geom_line(aes(vecY2, -2.1437*vecY2+2.5460 ))
  

.Scalc<-function(Age, .ma = ma, .S = S, .Sc = Sc){
  Age<-Age/.ma
Sprop<-ifelse(Age<0.5,1, ifelse(Age <(15/18), 1-4.397e-05*exp(Age*1.038e+01),-2.1437*Age+2.5460))
Sprop*.S
}

sum(.Scalc(Age = 1:74, .ma = 80, .S = 1, .Sc = 0.705)*Sim[150,2:75])/sum(Sim[150,2:75])

plot(.Scalc(1:74, .ma = 80, .S = 1, .Sc = 0.705))
