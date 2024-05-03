

solu<-as.data.frame(solution[(53*365*10+1):(54*365*10+1),])
solu1<-solu 
#%>%
#filter(rowMeans(solu[,(M+2):(M+N+1)])>0.001 & rowMeans(solu[,2:(1+M)])>0.001)

P1<-solu1[,1:(M+1)]
A1<-solu1[,c(1,(M+2):(M+N+1))]
F1<-solu1[,c(1,(M+N+2):(2*M+N+1))]
FE<-solu1[,c(1,(2*M+N+2):(M*N+2*M+N+1))]

rP1<-y_coordinates[(53*365*10+1):(54*365*10+1),1:M]
rA1<-y_coordinates[(53*365*10+1):(54*365*10+1),(M+1):(M+N)]
as.matrix(P1)->P1
as.matrix(A1)->A1
as.matrix(F1)->F1
as.matrix(FE)->FE


layout(matrix(1:6, ncol = 2), widths = 1, heights = c(0.3,0.93,1), respect = FALSE)

st<-(0:(nrow(P1)-1))/10
par(mar = c(0.5, 4.5, 2.0, 2.0))
matplot(rP1, type = "l",lty = "solid", lwd=2 , pch=1,col = 1:M, main=NA,
        ylab = "growth rate", xaxt="n",cex.lab=2.0,cex.axis=1.5 )
abline(h=0)
text(400,-1,"Plant ",cex=2.0)
par(mar = c(0.5, 4.5, 2.0, 2.0))
matplot(P1[,1],P1[,-1], type = "l",lwd=2,lty = "solid" , pch = 1, col=1:M,
        main=NA, 
        ylab = "Plant density", xlab = NA,xaxt="n",cex.lab=2.0,cex.axis=2.0)
par(mar = c(4.1, 4.5, 0.5, 2.0))
matplot(st,F1[,-1],lwd=2, type = "l",lty = "solid" , pch = 1, col=1:M, 
        main=NA, ylab = "Floral resource", xlab ="Time (day)", cex.lab=2.0,cex.axis=2.0)
par(mar = c(0.5, 4.5, 2.0, 2.0))
matplot(rA1, type = "l",lty = "solid" , lwd=2, pch = 1, col=1:N,
        main=NA, ylab = "growth rate", xaxt="n",cex.lab=2.0,cex.axis=1.5 )
abline(h=0)
text(500,-0.3,"Animal ",cex=2.0)
par(mar = c(0.5, 4.5, 2.0, 2.0))
matplot(A1[,-1], type = "l",lwd=2,lty = "solid" , pch=1,col = 1:N, 
        main=NA, ylab = "Animal density", xlab = "Time",xaxt="n",cex.lab=2.0,cex.axis=2.0)

par(mar = c(4.1, 4.5, 0.5, 2.0))
matplot(st,FE[,-1], type = "l", lwd=2,lty ="solid" ,pch = 1, col = rep(1:N, each=M), 
        main=NA ,ylab = "Foraging effort ",xlab="Time (day)", 
        cex.lab=2.0,cex.axis=2.0)

dev.copy(jpeg,"SE_Growth&Intplot_US101.jpeg",width = 300, height = 300,units = "mm", res = 600)
dev.off()
