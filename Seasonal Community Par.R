library(foreach)
library(doParallel)
library(tidyverse) 
library(dplyr)

# Define the number of cores to use
detectCores()
cores<-10
# Register the parallel backend
cl <- makeCluster(cores)
registerDoParallel(cl)
M=3
N=5

# Define the functions
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


# Scaled uniform distribution
Urand<-function(mu_y,var_y,m,n){
  a<-mu_y
  b<-sqrt((3*var_y)/(mu_y**2))
  
  Urand<-a*(1+b*(2*matrix(runif(m*n,0,1),m,n)-1))
  return(Urand)
}


#Model Simulation
integrate_model<-function(itr){
  
  M=3
  N=5
  RMatA<-matrix(1/M,M,N)
  
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
  sigma_A=matrix(runif(M*N,0,.1), nr=M)
  #Consumption rate
  bet0<-RMatA
  # Initial food 
  Fi0<-matrix(runif(M,0,1), nr=M)
  # initial population
  XP0<-matrix(runif(M,0,1), nr=M)
  XA0<-matrix(runif(N,0,1), nr=N)
  #Adaptation rate
  G=matrix(2,N,1)
  X0=rbind(XP0,XA0)
  
  yini=c(c(X0),c(Fi0),c(bet0))
  times=seq(0,20000,.1)
  parameters=list(rP=rP,rA=rA,CP=CP,CA=CA,a=a,w=w,sigma_A=sigma_A,
                  sigma_P=sigma_P, G=G,sP=sP,sA=sA,uP=uP,uA=uA)
  solution<-deSolve::ode(y=yini, times=times, func=lotka, parms=parameters)
  
  #Estite variables
  X<-as.matrix(solution[,2:(M+N+1)])
  Fi<-as.matrix(solution[,(M+N+2):(2*M+N+1)])
  ForEffMatA<-as.matrix(solution[,(2*M+N+2):(M*N+2*M+N+1)])
  XPF<-as.matrix(X[length(times),1:M],nr=M)
  XAF<-as.matrix(X[length(times),(M+1):(M+N)],nr=N)
  FiF<-as.matrix(Fi[length(times),],nr=M)
  bet<-matrix(ForEffMatA[length(times),],M,N)
  
  # Extract the last favourable season, using the total plants density or animal density greater than 0.1
  solu<-as.data.frame(solution[(54*365*10+1):nrow(solution),])
  solu1<-solu %>%
    filter(rowMeans(solu[,(M+2):(M+N+1)])>0.001 & rowMeans(solu[,2:(1+M)])>0.001)
  P1<-solu1[,1:(M+1)]
  A1<-solu1[,c(1,(M+2):(M+N+1))]
  F1<-solu1[,c(1,(M+N+2):(2*M+N+1))]
  FE<-solu1[,c(1,(2*M+N+2):(M*N+2*M+N+1))]
  as.matrix(P1)->P1
  as.matrix(A1)->A1
  as.matrix(F1)->F1
  as.matrix(FE)->FE
  # Compute weekly Bray-Curtis turnover
  bc.t.over<-data.frame("Vis.bc"=numeric(),  "bet.bc"=numeric(), "Enc.bc"=numeric(),"XP.bc"=numeric(),"XA.bc"=numeric(),"Fi.bc"=numeric())
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
    #Bray-Curtis
    Vis.bc.value<-1-(2*sum(pmin(V.a,V.b))/(sum(V.a)+sum(V.b)))
    bet.bc.value<-1-(2*sum(pmin(bet.a,bet.b))/(sum(bet.a)+sum(bet.b)))
    Enc.bc.value<-1-(2*sum(pmin(E.a,E.b))/(sum(E.a)+sum(E.b)))
    XP.bc.value<-1-(2*sum(pmin(XP.a,XP.b))/(sum(XP.a)+sum(XP.b)))
    XA.bc.value<-1-(2*sum(pmin(XA.a,XA.b))/(sum(XA.a)+sum(XA.b)))
    Fi.bc.value<-1-(2*sum(pmin(Fi.a,Fi.b))/(sum(Fi.a)+sum(Fi.b)))
    bc.value<-data.frame("Vis.bc"=Vis.bc.value, "bet.bc"=bet.bc.value, "Enc.bc"=Enc.bc.value,"XP.bc"=XP.bc.value,
                         "XA.bc"=XA.bc.value,"Fi.bc"=Fi.bc.value)
    bc.t.over<-rbind(bc.t.over,bc.value)
    
  }
  
  n1<-1
  n2<-floor(seq(1,nrow(bc.t.over),length.out=4))[2]
  n3<-floor(seq(1,nrow(bc.t.over),length.out=4))[3]
  n4<-nrow(bc.t.over)
  
  turn.over<-c(colMeans(bc.t.over[n1:n2,]),colMeans(bc.t.over[(n2+1):n3,]),colMeans(bc.t.over[n3:n4,]))
  names(turn.over)<-c("Vis.bc1", "bet.bc1", "Enc.bc1", "XP.bc1", "XA.bc1", "Fi.bc1", 
                      "Vis.bc2", "bet.bc2", "Enc.bc2", "XP.bc2", "XA.bc2", "Fi.bc2", 
                      "Vis.bc3", "bet.bc3", "Enc.bc3", "XP.bc3", "XA.bc3", "Fi.bc3")
  
  structures<-data.frame("H2"=c(),"mod"=c(),"nes"=c())
  for(i in seq(1,(dim(solu1)[1]),70)){
    XA.t<-as.matrix(A1[i,2:(N+1)],nr=N)
    Fi.t<-as.matrix(F1[i,2:(M+1)],nr=M)
    bet.t<-matrix(FE[i,2:(M*N+1)],M,N)
    V.t<-bet.t*(Fi.t%*%t(XA.t))
    #Computation of network structural properties 
    H2<-bipartite::H2fun(V.t, H2_integer = FALSE)[1] #Specialisation
    mod<-bipartite::computeModules(V.t)@likelihood  # Modularity
    nes<-bipartite::nested(V.t,method = "WNODA") # Nestedness
    structures.values<-c(H2,mod,nes)
    structures<-rbind(structures,structures.values)
  }
  
  structures<-c(colMeans(structures[n1:n2,]),colMeans(structures[(n2+1):n3,]),
                colMeans(structures[n3:n4,]))
  names(structures)<-c("H2.1","mod.1","nes.1","H2.2","mod.2","nes.2",
                       "H2.3","mod.3","nes.3")
  
  output<-c(turn.over,structures,"mean_rP"=mean(c(rP)),"mean_rA"=mean(c(rA)),"mean_cP"=mean(c(CP)),
            "mean_cA"=mean(c(CA)),"mean_sigmaP"=mean(c(sigma_P)),"mean_sigmaA"=mean(c(sigma_A)),
            "mean_a"=mean(c(a)),"mean_w"=mean(c(w)),"mean_sP"=mean(c(sP)),"mean_sA"=mean(c(sA)),
            "mean_uP"=mean(c(uP)),"mean_uA"=mean(c(uA)),
            "var_rP"=var(c(rP)),"var_rA"=var(c(rA)),"var_cP"=var(c(CP)),
            "var_cA"=var(c(CA)),"var_sigmaP"=var(c(sigma_P)),"var_sigmaA"=var(c(sigma_A)),
            "var_a"=var(c(a)),"var_w"=var(c(w)),"var_sP"=var(c(sP)),"var_sA"=var(c(sA)),
            "var_uP"=var(c(uP)),"var_uA"=var(c(uA)))
  return(c(output))
  # tryCatch(
  #   expr = {
  #     
  #   },
  #   error=function(e)
  #     return(NA)
  # )
  
}
# Define the parallel loop
clusterExport(cl,c("lotka","Urand"))
system.time(t.data<-foreach(itr=rep(1:3), .combine = rbind, .packages='tidyverse') %dopar% {
  values<-integrate_model(itr)
  
})





 stopCluster(cl)


#Write file on csv
write.table(t.data, file = "FavourNet(20-30).csv", sep = ",",
            append = TRUE, quote = FALSE,
            col.names = FALSE, row.names = FALSE)


#####







