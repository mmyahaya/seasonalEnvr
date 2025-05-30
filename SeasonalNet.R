# Load required packages
library(deSolve)
library(bipartite)
library(tidyverse)


# Define the Lotka-Volterra function
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

# Define random number generator
Urand<-function(mu_y,var_y,m,n){
  a<-mu_y
  b<-sqrt((3*var_y)/(mu_y**2))

  Urand<-a*(1+b*(2*matrix(runif(m*n,0,1),m,n)-1))
  return(Urand)
}

set.seed(1234)

M=7 #plant
N=13 #animal
set.seed(1234)
{RMatA<-matrix(1/M,M,N)

  uP<-Urand(0.1,0.02,M,1)
  uA<-Urand(0.1,0.01,N,1)

  sP<-Urand(0.1,0.2,M,1)
  sA<-Urand(0.1,0.1,N,1)
  # Intrinsic growth
  rP<-Urand(3.5,0.01,M,1)
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
solution<-ode(y=yini, times=times, func=lotka, parms=parameters)
{X<-as.matrix(solution[,2:(M+N+1)])
  Fi<-as.matrix(solution[,(M+N+2):(2*M+N+1)])
  ForEffMatA<-as.matrix(solution[,(2*M+N+2):(M*N+2*M+N+1)])
}


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



{vars1 <- c(rP,rA)
  vars2 <- c(sP,sA)
  vars3<- c(uP,uA)
  mult_one <- function(var1, var2, var3)
  {
    var1*sin(times/58.0916+var2)+var3
  }
  y_coordinates<-mapply(mult_one,vars1, vars2, vars3)}





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



