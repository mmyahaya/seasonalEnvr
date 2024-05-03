layout(matrix(1:4, ncol = 2), widths = 1, heights = c(1,1), respect = FALSE)

par(mar = c(0.5, 4.5, 4.1, 2.0))
matplot(times,X[,1:M], type = "l",lwd=2,lty = "solid" , pch = 1, col=1:M,
        main=NA, 
        ylab = "Plant density", xlab = NA,xaxt="n",cex.lab=2.0,cex.axis=2.0)
par(mar = c(4.1, 4.5, 1, 2.0))
matplot(times,Fi,lwd=2, type = "l",lty = "solid" , pch = 1, col=1:M, 
        main=NA, ylab = "Floral resource", xlab ="Time", cex.lab=2.0,cex.axis=2.0)

par(mar = c(0.5, 4.5, 4.1, 2.0))
matplot(times,X[,(M+1):(M+N)], type = "l",lwd=2,lty = "solid" , pch=1,col = 1:N, 
        main=NA, ylab = "Animal density", xlab = "Time",xaxt="n",cex.lab=2.0,cex.axis=2.0)

par(mar = c(4.1, 4.5, 1, 2.1))
matplot(times,ForEffMatA, type = "l", lwd=2,lty ="solid" ,pch = 1, col = rep(1:N, each=M), 
        main=NA ,ylab = "Foraging effort ",xlab="Time", 
        cex.lab=2.0,cex.axis=2.0)

dev.copy(jpeg,"SE_Fullplot_US1.jpeg",width = 300, height = 225,units = "mm", res = 400)
dev.off()
#Steady
par(mar = c(0.5, 4.5, 4.1, 2.0))
matplot(times[(51*365*10+1):200001],X[(51*365*10+1):200001,1:M], type = "l",lwd=2,lty = "solid" , pch = 1, col=1:M,
        main=NA, 
        ylab = "Plant density", xlab = NA,xaxt="n",cex.lab=2.0,cex.axis=2.0)
par(mar = c(4.1, 4.5, 1, 2.0))
matplot(times[(51*365*10+1):200001],Fi[(51*365*10+1):200001,],lwd=2, type = "l",lty = "solid" , pch = 1, col=1:M, 
        main=NA, ylab = "Floral resource", xlab ="Time", cex.lab=2.0,cex.axis=2.0)

par(mar = c(0.5, 4.5, 4.1, 2.0))
matplot(times[(51*365*10+1):200001],X[(51*365*10+1):200001,(M+1):(M+N)], type = "l",lwd=2,lty = "solid" , pch=1,col = 1:N, 
        main=NA, ylab = "Animal density", xlab = "Time",xaxt="n",cex.lab=2.0,cex.axis=2.0)

par(mar = c(4.1, 4.5, 1, 2.1))
matplot(times[(51*365*10+1):200001],ForEffMatA[(51*365*10+1):200001,], type = "l", lwd=2,lty ="solid" ,pch = 1, col = rep(1:N, each=M), 
        main=NA ,ylab = "Foraging effort ",xlab="Time", 
        cex.lab=2.0,cex.axis=2.0)



dev.copy(jpeg,"SE_Steadyplot_US1_test.jpeg",width = 300, height = 300,units = "mm", res = 600)
dev.off()

#Growth season
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

dev.copy(jpeg,"SE_Growthplot_US01.jpeg",width = 300, height = 300,units = "mm", res = 600)
dev.off()


#Weekly plot
seqT<-seq(1,(dim(solu1)[1]),70)





par(mar = c(0.5, 4.5, 4.1, 2.0))
matplot((1:nrow(P1[seqT,])),P1[seqT,-1], type = "o",lwd=2,lty = "solid" , pch = 1, col=1:M,
        main=NA, 
        ylab = "Plant density", xlab = NA,xaxt="n",cex.lab=2.0,cex.axis=2.0)
par(mar = c(4.1, 4.5, 1, 2.0))
matplot((1:nrow(P1[seqT,])),F1[seqT,-1],lwd=2, type = "o",lty = "solid" , pch = 1, col=1:M, 
        main=NA, ylab = "Floral resource", xlab ="Time (Week)", cex.lab=2.0,cex.axis=2.0)

par(mar = c(0.5, 4.5, 4.1, 2.0))
matplot((1:nrow(P1[seqT,])),A1[seqT,-1], type = "o",lwd=2,lty = "solid" , pch=1,col = 1:N, 
        main=NA, ylab = "Animal density", xlab = "Time",xaxt="n",cex.lab=2.0,cex.axis=2.0)

par(mar = c(4.1, 4.5, 1, 2.1))
matplot((1:nrow(P1[seqT,])),FE[seqT,-1], type = "o", lwd=2,lty ="solid" ,pch = 1, col = rep(1:N, each=M), 
        main=NA ,ylab = "Foraging effort ",xlab="Time (Week)", 
        cex.lab=2.0,cex.axis=2.0)


dev.copy(jpeg,"SE_weekplot_bc_US101.jpeg",width = 300, height = 300,units = "mm", res = 600)
dev.off()

#### 8 plots ####
layout(matrix(1:8, ncol = 2), widths = 1, heights = c(1,1), respect = FALSE)

par(mar = c(0.5, 4.5, 4.1, 2.0))
matplot(times,X[,1:M], type = "l",lwd=2,lty = "solid" , pch = 1, col=1:M,
        main=NA, 
        ylab = "Plant density", xlab = NA,xaxt="n",cex.lab=2.0,cex.axis=2.0)
par(mar = c(4.1, 4.5, 1, 2.0))
matplot(times,Fi,lwd=2, type = "l",lty = "solid" , pch = 1, col=1:M, 
        main=NA, ylab = "Floral resource", xlab ="Time", cex.lab=2.0,cex.axis=2.0)


par(mar = c(0.5, 4.5, 4.1, 2.0))
matplot(times[(51*365*10+1):200001],X[(51*365*10+1):200001,1:M], type = "l",lwd=2,lty = "solid" , pch = 1, col=1:M,
        main=NA, 
        ylab = "Plant density", xlab = NA,xaxt="n",cex.lab=2.0,cex.axis=2.0)
par(mar = c(4.1, 4.5, 1, 2.0))
matplot(times[(51*365*10+1):200001],Fi[(51*365*10+1):200001,],lwd=2, type = "l",lty = "solid" , pch = 1, col=1:M, 
        main=NA, ylab = "Floral resource", xlab ="Time", cex.lab=2.0,cex.axis=2.0)


par(mar = c(0.5, 4.5, 4.1, 2.0))
matplot(times,X[,(M+1):(M+N)], type = "l",lwd=2,lty = "solid" , pch=1,col = 1:N, 
        main=NA, ylab = "Animal density", xlab = "Time",xaxt="n",cex.lab=2.0,cex.axis=2.0)

par(mar = c(4.1, 4.5, 1, 2.1))
matplot(times,ForEffMatA, type = "l", lwd=2,lty ="solid" ,pch = 1, col = rep(1:N, each=M), 
        main=NA ,ylab = "Foraging effort ",xlab="Time", 
        cex.lab=2.0,cex.axis=2.0)

par(mar = c(0.5, 4.5, 4.1, 2.0))
matplot(times[(51*365*10+1):200001],X[(51*365*10+1):200001,(M+1):(M+N)], type = "l",lwd=2,lty = "solid" , pch=1,col = 1:N, 
        main=NA, ylab = "Animal density", xlab = "Time",xaxt="n",cex.lab=2.0,cex.axis=2.0)

par(mar = c(4.1, 4.5, 1, 2.1))
matplot(times[(51*365*10+1):200001],ForEffMatA[(51*365*10+1):200001,], type = "l", lwd=2,lty ="solid" ,pch = 1, col = rep(1:N, each=M), 
        main=NA ,ylab = "Foraging effort ",xlab="Time", 
        cex.lab=2.0,cex.axis=2.0)

dev.copy(jpeg,"SE_Fullplot_US101.jpeg",width = 300, height = 300,units = "mm", res = 600)
dev.off()
#Steady