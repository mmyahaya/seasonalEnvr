library(deSolve)
library(bipartite)
library(ggplot2)

lotka<-function(t,y,parameters){
  with(as.list(c(y,parameters)),{
    Xx<-y
    XP<-Xx[1:M]
    XA<-Xx[(M+1):(M+N)]
    Fi<-Xx[(M+N+1):(2*M+N)]
    bet<-matrix(Xx[(2*M+N+1):(M*N+2*M+N)],M,N)
    dXP<-rP*sin(t/58.0916+sP)+uP-CP*XP+(((sigma_P*bet)%*%XA)*(Fi/XP))
    dXA<-rA*sin(t/58.0916+sA)+uA-CA*XA+(t(sigma_A*bet)%*%Fi)
    dX<-Xx[1:(M+N)]*rbind(dXP,dXA)
    dFi<-a*XP-w*Fi-rowSums(bet*(Fi%*%t(XA)))
    dbet<-(bet%*%diag(c(G)))*sweep(sigma_A*Fi,2,colSums(bet*sigma_A*Fi),"-")
    return(list(c(dX,dFi,dbet))) 
  })
}


Urand<-function(mu_y,var_y,m,n){
  a<-mu_y
  b<-sqrt((3*var_y)/(mu_y**2))
  
  Urand<-a*(1+b*(2*matrix(runif(m*n,0,1),m,n)-1))
  return(Urand)
}
#58.09155
#B=100
M=7
N=13
{RMatA<-matrix(1/M,M,N)

uP<-Urand(0.1,0.01,M,1)
uA<-Urand(0.1,0.01,N,1)
    
sP<-Urand(0.1,0.1,M,1)
sA<-Urand(0.1,0.1,N,1)
# Intrinsic growth
rP<-Urand(2.5,0.1,M,1) 
rA<-Urand(0.5,0.01,N,1) 
# Density dependent
CP<-Urand(0.5,0.01,M,1)
CA<-Urand(0.5,0.01,N,1) 
# floral resource production rate
a<-Urand(2.5,0.01,M,1) 
#matrix(rbeta(M,2,5),M,1)
#decay rate
w=Urand(1,0.01,M,1) 
#conversion rate
sigma_P=matrix(runif(M*N,0,.1), nr=M)
sigma_A=matrix(runif(M*N,0,0.1), nr=M)
#Consumption rate
bet0<-RMatA
# Initial food 
Fi0<-matrix(runif(M,0,1), nr=M)
# initial population
XP0<-matrix(runif(M,0,1), nr=M)
XA0<-matrix(runif(N,0,1), nr=N)
#Adaptation rate
G=matrix(2,N,1)
X0=rbind(XP0,XA0)}





yini=c(c(X0),c(Fi0),c(bet0))
times=seq(0,20000,.1)
parameters=list(rP=rP,rA=rA,CP=CP,CA=CA,a=a,w=w,sigma_A=sigma_A,
                sigma_P=sigma_P, G=G,sP=sP,sA=sA,uP=uP,uA=uA)
system.time(solution<-ode(y=yini, times=times, func=lotka, parms=parameters))
{#solution[solution<0]<-0
X<-as.matrix(solution[,2:(M+N+1)])
Fi<-as.matrix(solution[,(M+N+2):(2*M+N+1)])
ForEffMatA<-as.matrix(solution[,(2*M+N+2):(M*N+2*M+N+1)])
XPF<-as.matrix(X[length(times),1:M],nr=M)
XAF<-as.matrix(X[length(times),(M+1):(M+N)],nr=N)
FiF<-as.matrix(Fi[length(times),],nr=M)
bet<-matrix(ForEffMatA[length(times),],M,N)
V<-bet*(FiF%*%t(XAF))}
#bipartite::H2fun(V, H2_integer = FALSE)[1]
#par(mfrow=c(3,1))

#xn<-ceiling(100001-B*2*pi*41)
#par(mfrow=c(2,2))
matplot(times,X[,1:M], type = "l",lty = "solid" , pch=1,col = 1:12, main=paste(" Plant : P=",M,"A=", N),
        ylab = "Density", xlab = "Time" )
#abline(v=54*365,lwd=3,col="red")
#dev.off() ,xlim = c(365*51,20001)
#png("test.png", width = 1200, height = 800, res = 600)
matplot(times,X[,(M+1):(M+N)], type = "l",lty = "solid" , pch = 1, col=1:12,
        main=paste(" Animal : P=",M,"A=", N), ylab = "Density", xlab = "Time")
#dev.copy(jpeg,paste("Modified Tomas Recource-service model - Animal P=",M, "A=",N,".jpeg"))
#dev.off()

#png("myresultFE.png", width = 300, height = 225,units = "mm", res = 600)
matplot(times,ForEffMatA, type = "l", lwd=2,lty ="solid" ,pch = 1, col = rep(1:N, each=M), main=paste(" Animal foraging effort : P=",M,"A=", N) ,ylab = "Foraging Efforts ",
        xlab = "Time")
  #dev.off()

#png("myresultFR.png", width = 300, height = 225,units = "mm", res = 600)
matplot(times,Fi, type = "l",lty = "solid" , pch = 1, col=1:12,
        main=paste("Floral resource: P=",M, "A=",N), 
        ylab = "Floral resource", xlab = "Time")




layout(matrix(1:4, ncol = 2), widths = 1, heights = c(1,1), respect = FALSE)

matplot(times[(51*365*10+1):200001],X[(51*365*10+1):200001,1:M], type = "l",lwd=2,lty = "solid" , pch = 1, col=1:M,
        main=NA, 
        ylab = "Plant density", xlab = NA,xaxt="n",cex.lab=2.0,cex.axis=2.0)

matplot(times[(51*365*10+1):200001],Fi[(51*365*10+1):200001,],lwd=2, type = "l",lty = "solid" , pch = 1, col=1:M, 
        main=NA, ylab = "Floral resource", xlab ="Time", cex.lab=2.0,cex.axis=2.0)


matplot(times[(51*365*10+1):200001],X[(51*365*10+1):200001,(M+1):(M+N)], type = "l",lwd=2,lty = "solid" , pch=1,col = 1:N, 
        main=NA, ylab = "Animal density", xlab = "Time",xaxt="n",cex.lab=2.0,cex.axis=2.0)


matplot(times[(51*365*10+1):200001],ForEffMatA[(51*365*10+1):200001,], type = "l", lwd=2,lty ="solid" ,pch = 1, col = rep(1:N, each=M), 
        main=NA ,ylab = "Foraging effort ",xlab="Time", 
        cex.lab=2.0,cex.axis=2.0)



 y_coordinates <- sapply(c(rP,rA),function(i) i*sin(times/58.0916+c(sP,sA)))

matplot(times,y_coordinates, type = "l",lty = "solid" , pch=1,col = c(rep("blue",M),rep("red",N))
        , main="Intinsic growth rates", ylab = "Rate", xlab = "Time")



{vars1 <- c(rP,rA)
vars2 <- c(sP,sA)
vars3<- c(uP,uA)
mult_one <- function(var1, var2, var3)
{
  var1*sin(times/58.0916+var2)+var3
}
y_coordinates<-mapply(mult_one,vars1, vars2, vars3)}





library(tidyverse) 
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)
solu<-as.data.frame(solution[(54*365*10+1):nrow(solution),])

solu1<-solu %>%
  filter(rowMeans(solu[,(M+2):(M+N+1)])>0.001 & rowMeans(solu[,2:(1+M)])>0.001)
  
P1<-solu1[,1:(M+1)]
A1<-solu1[,c(1,(M+2):(M+N+1))]
F1<-solu1[,c(1,(M+N+2):(2*M+N+1))]
FE<-solu1[,c(1,(2*M+N+2):(M*N+2*M+N+1))]

rP1<-y_coordinates[(54*365*10+1):nrow(solution),1:M]
rA1<-y_coordinates[(54*365*10+1):nrow(solution),(M+1):(M+N)]
as.matrix(P1)->P1
as.matrix(A1)->A1
as.matrix(F1)->F1
as.matrix(FE)->FE



matplot(rP1, type = "l",lty = "solid", lwd=2 , pch=1,col = 1:M, main=paste(" Plant : P=",M,"A=", N),
        ylab = "growth rate", xlab = "Time",xaxt="n" )
abline(h=0)

text(500,-1,"Plant growth rate")
matplot(rA1, type = "l",lty = "solid" , lwd=2, pch = 1, col=1:N,
        main=paste(" Animal : P=",M,"A=", N), ylab = "growth rate", xlab = "Time")
abline(h=0)
matplot(P1[,-1], type = "l",lty = "solid", lwd=2 , pch=1,col = 1:M, main=paste(" Plant : P=",M,"A=", N),
        ylab = "Density", xlab = "Time" )


for (i in 1:ncol(rP1)) {
  lines(rP1[, i], col = i,lty = "solid", lwd=2 , pch=1)
}

matplot(A1[,-1], type = "l",lty = "solid" , lwd=2, pch = 1, col=1:N,
        main=paste(" Animal : P=",M,"A=", N), ylab = "Density", xlab = "Time")

for (i in 1:ncol(rA1)) {
  lines(rA1[, i], col = i,lty = "solid", lwd=2 , pch=1)
}
matplot(F1[,-1], type = "l",lty = "solid" , lwd=2, pch = 1, col=1:M,
        main=paste("Floral resource: P=",M, "A=",N), 
        ylab = "Floral resource", xlab = "Time")
matplot(FE[,-1], type = "l", lwd=2,lty ="solid" ,pch = 1, col = rep(1:N, each=M), main=paste(" Animal foraging effort : P=",M,"A=", N) ,ylab = "Foraging Efforts ",
        xlab = "Time")
#Plot
par(mar = c(0.5, 4.5, 4.1, 2.0))
matplot((1:nrow(P1[,-1]))/10,P1[,-1], type = "l",lwd=2,lty = "solid" , pch = 1, col=1:M,
        main=NA, 
        ylab = "Plant density", xlab = NA,xaxt="n",cex.lab=2.0,cex.axis=2.0)
par(mar = c(4.1, 4.5, 1, 2.0))
matplot((1:nrow(P1))/10,F1[,-1],lwd=2, type = "l",lty = "solid" , pch = 1, col=1:M, 
        main=NA, ylab = "Floral resource", xlab ="Time (day)", cex.lab=2.0,cex.axis=2.0)

par(mar = c(0.5, 4.5, 4.1, 2.0))
matplot((1:nrow(P1))/10,A1[,-1], type = "l",lwd=2,lty = "solid" , pch=1,col = 1:N, 
        main=NA, ylab = "Animal density", xlab = "Time",xaxt="n",cex.lab=2.0,cex.axis=2.0)

par(mar = c(4.1, 4.5, 1, 2.1))
matplot((1:nrow(P1))/10,FE[,-1], type = "l", lwd=2,lty ="solid" ,pch = 1, col = rep(1:N, each=M), 
        main=NA ,ylab = "Foraging effort ",xlab="Time (day)", 
        cex.lab=2.0,cex.axis=2.0)

bc.t.over<-data.frame("Vis.bc"=numeric(),  "bet.bc"=numeric(), "Enc.bc"=numeric(),"XP.bc"=numeric(),"XA.bc"=numeric(),"Fi.bc"=numeric())
#bt.t.over<-data.frame("Vis.bt"=numeric(),  "bet.bt"=numeric(), "Enc.bt"=numeric(),"XP.bt"=numeric(),"XA.bt"=numeric(),"Fi.bt"=numeric())
#cor.t.over<-data.frame("Vis.cor"=numeric(), "bet.cor"=numeric(),"Enc.cor"=numeric(), "XP.cor"=numeric(),"XA.cor"=numeric(),"Fi.cor"=numeric())
for (i in seq(1,(dim(solu1)[1]-70),70)){
  XP.a<-as.matrix(P1[i,2:(M+1)],nr=M)
  XA.a<-as.matrix(A1[i,2:(N+1)],nr=N)
 
  Fi.a<-as.matrix(F1[i,2:(M+1)],nr=M)
 
  bet.a<-matrix(FE[i,2:(M*N+1)],M,N)
 
  V.a<-bet.a*(Fi.a%*%t(XA.a)) 
  
  E.a<-Fi.a%*%t(XA.a)
  
  
  XP.b<-as.matrix(P1[i+70,2:(M+1)],nr=M)
  
  XA.b<-as.matrix(A1[i+70,(2):(N+1)],nr=N)
  
  Fi.b<-as.matrix(F1[i+70,2:(M+1)],nr=M)
 
  bet.b<-matrix(ForEffMatA[i+70,],M,N)
 
  V.b<-bet.b*(Fi.b%*%t(XA.b))
 
  E.b<-Fi.b%*%t(XA.b)
  Vis.bc.value<-1-(2*sum(pmin(V.a,V.b))/(sum(V.a)+sum(V.b)))
  bet.bc.value<-1-(2*sum(pmin(bet.a,bet.b))/(sum(bet.a)+sum(bet.b)))
  Enc.bc.value<-1-(2*sum(pmin(E.a,E.b))/(sum(E.a)+sum(E.b)))
  XP.bc.value<-1-(2*sum(pmin(XP.a,XP.b))/(sum(XP.a)+sum(XP.b)))
  XA.bc.value<-1-(2*sum(pmin(XA.a,XA.b))/(sum(XA.a)+sum(XA.b)))
  Fi.bc.value<-1-(2*sum(pmin(Fi.a,Fi.b))/(sum(Fi.a)+sum(Fi.b)))
  bc.value<-data.frame("Vis.bc"=Vis.bc.value, "bet.bc"=bet.bc.value, "Enc.bc"=Enc.bc.value,"XP.bc"=XP.bc.value,
                       "XA.bc"=XA.bc.value,"Fi.bc"=Fi.bc.value)
  bc.t.over<-rbind(bc.t.over,bc.value)
  
  
  # Vis.bt.value<-betalinkr(webs2array(list(V.a=V.a, V.b=V.b)), 
  #                         partitioning="poisot",binary = FALSE)[3]
  # 
  # Vis.bt.value<-betalinkr(webs2array(list(V.a=V.a, V.b=V.b)), 
  #                         partitioning="poisot",binary = FALSE)[3]
  # bet.bt.value<-betalinkr(webs2array(list(bet.a=bet.a, bet.b=bet.b)), 
  #                         partitioning="poisot",binary = FALSE)[3]
  # Enc.bt.value<-betalinkr(webs2array(list(E.a=E.a, E.b=E.b)), 
  #                         partitioning="poisot",binary = FALSE)[3]
  # XP.bt.value<-betalinkr(webs2array(list(XP.a=XP.a, XP.b=XP.b)), 
  #                         partitioning="poisot",binary = FALSE)[3]
  # XA.bt.value<-betalinkr(webs2array(list(XA.a=XA.a, XA.b=XA.b)), 
  #                         partitioning="poisot",binary = FALSE)[3]
  # Fi.bt.value<-betalinkr(webs2array(list(Fi.a=Fi.a, Fi.b=Fi.b)), 
  #                         partitioning="poisot",binary = FALSE)[3]
  # 
  # bt.value<-data.frame("Vis.bt"=Vis.bt.value, "bet.bt"=bet.bt.value, "Enc.bt"=Enc.bt.value,"XP.bt"=XP.bt.value,
  #                      "XA.bt"=XA.bt.value,"Fi.bt"=Fi.bt.value)
  # bt.t.over<-rbind(bt.t.over,bt.value)
  
}

n1<-1
n2<-floor(seq(1,nrow(bc.t.over),length.out=4))[2]
n3<-floor(seq(1,nrow(bc.t.over),length.out=4))[3]
n4<-nrow(bc.t.over)

par(mfrow=c(3,2))
par(mar = c(3,3,1.5,1.5))

plot(bc.t.over[,1], ylab = "Bray-Curtis", xlab = "Transition", pch=16, main = "Bray-Curtis interaction",cex=2.0)
abline(v=c(n2+0.5,n3+0.5))

plot(bc.t.over[,2], ylab = "Bray-Curtis", pch=16,xlab = "Transition", main = "Bray-Curtis bet",cex=2.0)
abline(v=c(n2+0.5,n3+0.5))

plot(bc.t.over[,3], ylab = "Bray-Curtis", xlab = "Transition", pch=16, main = "Bray-Curtis Enc",cex=2.0)
abline(v=c(n2+0.5,n3+0.5))

plot(bc.t.over[,4], ylab = "Bray-Curtis", xlab = "Transition", pch=16, main = "Bray-Curtis plant ",cex=2.0)
abline(v=c(n2+0.5,n3+0.5))
plot(bc.t.over[,5], ylab = "Bray-Curtis", xlab = "Transition", pch=16, main = "Bray-Curtis animal",cex=2.0)
abline(v=c(n2+0.5,n3+0.5))



par(mfrow=c(3,2))
par(mar = c(3,3,1.5,1.5))

plot(bt.t.over[,1], ylab = "Bray", xlab = "Transition", pch=16, main = "Bray-Curtis interaction",cex=2.0)
abline(v=c(n2+0.5,n3+0.5))

plot(bt.t.over[,2], ylab = "Bray", pch=16,xlab = "Transition", main = "Bray-Curtis bet",cex=2.0)
abline(v=c(n2+0.5,n3+0.5))

plot(bt.t.over[,3], ylab = "Bray-Curtis", xlab = "Transition", pch=16, main = "Bray-Curtis Enc",cex=2.0)
abline(v=c(n2+0.5,n3+0.5))

plot(bt.t.over[,4], ylab = "Bray-Curtis", xlab = "Transition", pch=16, main = "Bray-Curtis plant ",cex=2.0)
abline(v=c(n2+0.5,n3+0.5))
plot(bt.t.over[,5], ylab = "Bray-Curtis", xlab = "Transition", pch=16, main = "Bray-Curtis animal",cex=2.0)
abline(v=c(n2+0.5,n3+0.5))
plot(bt.t.over[,6], ylab = "Bray-Curtis", xlab = "Transition", pch=16, main = "Bray-Curtis floral",cex=2.0)
abline(v=c(n2+0.5,n3+0.5))









dev.copy(jpeg,"TurnoverF3.jpeg",width = 300, height = 300,units = "mm", res = 600)
dev.off()


boxplot(tover[,c(1,7,13)],col = c("maroon"), boxwex = 0.3, ylab="Bray-Curtis turnover", 
        main="Late period", names =c("Early","Mid","Late"),
        ylim = c(0, 1),cex.lab=1.5,cex.axis=1.5,cex.main=1.5 )

boxplot(tover[,13:18],col = c("maroon",rep("palegreen",5)), boxwex = 0.5, ylab="Bray-Curtis turnover", 
        main="Late period", names =c("Vis.","FE","Enc.","Pl.","An.","Fl."),
         ylim = c(0, 1),cex.lab=1.5,cex.axis=1.5,cex.main=1.5 )
abline(v=c(1.5),lty=2)

dev.copy(jpeg,"Tover_L_bc.jpeg",width = 300, height = 300,units = "mm", res = 400)
dev.off()

abline(v=c(1.5,7.5,13.5),lty=2)
abline(v=c(6.5,12.5),lwd=2)




fitted_bc<-lm(formula = Vis.bc1 ~bet.bc1+Enc.bc1+XP.bc1 , 
               data = tover)


sum_bc<-summary.glm(fitted_bc)
sum_bc
with(summary(fitted_bc), 1 - deviance/null.deviance)





boxplot(tover[,13:18],col = c("maroon"), boxwex = 0.5, ylab="Bray-Curtis turnover", 
        main="Late period", names =c("Vis.","FE","Enc.","Pl.","An.","Fl."),
        ylim = c(0, 1),cex.lab=1.5,cex.axis=1.5,cex.main=1.5 )
boxplot(tover[,c(1,7,13)],col = c("maroon"),boxwex=0.5,
        ylab="Bray-Curtis turnover",cex.lab=1.5,cex.axis=1.5,cex.main=1.5,ylim=c(0,1), names = c("Early","Mid","Late"), xlab="Interaction turnover" )



dev.copy(jpeg,"TEST_SE.jpeg",width = 300, height = 300,units = "mm", res = 400)
dev.off()



betalinkr(webs2array(list(bet.a=bet.a, bet.b=bet.b)), 
          partitioning="poisot",binary = FALSE)
