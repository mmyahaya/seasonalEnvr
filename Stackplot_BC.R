layout(matrix(1:6, ncol = 2), widths = 1, heights = c(1,1,1), respect = FALSE)

{par(mar = c(2,4,2,1.5))
plot(bc.t.over[,1], ylab = NA,ylim = c(0,1), xlab = "Week", pch=16, main ="Interaction",cex=2.0, xaxt="n",
     cex.lab=2.0,cex.axis=2.0,cex.main=2.0)
abline(v=c(n2+0.5,n3+0.5))

par(mar = c(2,4,2,1.5))
plot(bc.t.over[,3], ylab = NA,ylim = c(0,1),xlab = NA, pch=16, main = "Encounter rate",cex=2.0,
     cex.lab=2.0,cex.axis=2.0,cex.main=2.0, xaxt="n")
abline(v=c(n2+0.5,n3+0.5))

par(mar = c(4.1,4,2,1.5))
plot(bc.t.over[,5], ylab = NA, xlab = NA,ylim = c(0,1), pch=16, main = "Animal density",cex=2.0,
     cex.lab=2.0,cex.axis=2.0,cex.main=2.0)


abline(v=c(n2+0.5,n3+0.5))

par(mar = c(2,2,2,1.5))
plot(bc.t.over[,2], ylab = NA,ylim = c(0,1), pch=16,xlab = "Week", main ="Foraging effort",cex=2.0, xaxt="n",
     cex.lab=2.0,cex.axis=2.0,cex.main=2.0)
abline(v=c(n2+0.5,n3+0.5))


par(mar = c(2,2,2,1.5))
plot(bc.t.over[,4], ylab = NA, xlab = "Week",ylim = c(0,1), pch=16, main = "Plant density",cex=2.0, xaxt="n",
     cex.lab=2.0,cex.axis=2.0,cex.main=2.0)
abline(v=c(n2+0.5,n3+0.5))


par(mar = c(4.1,2,2,1.5))
plot(bc.t.over[,6], ylab = NA, xlab = NA, pch=16,ylim = c(0,1), main = "Floral resource abundnace ",cex=2.0,
     cex.lab=2.0,cex.axis=2.0,cex.main=2.0)
abline(v=c(n2+0.5,n3+0.5))}



dev.copy(jpeg,"SingleNturn_US101.jpeg",width = 300,
         height = 300,units = "mm", res = 600)
dev.off()



layout(matrix(1:3, ncol = 1), widths = 1,
       heights = c(1,1,1), respect = FALSE)
par(mar = c(2,4,2,1.5))
plot(bc.t.over[,7], ylab = NA,ylim = c(0,1), xlab = "Week", pch=16, main ="H2",cex=2.0, xaxt="n",
     cex.lab=2.0,cex.axis=2.0,cex.main=2.0)
abline(v=c(n2+0.5,n3+0.5))

par(mar = c(2,4,2,1.5))
plot(bc.t.over[,8], ylab = NA,ylim = c(0,1),xlab = NA, pch=16, main = "mod",cex=2.0,
     cex.lab=2.0,cex.axis=2.0,cex.main=2.0, xaxt="n")
abline(v=c(n2+0.5,n3+0.5))

par(mar = c(2,4,2,1.5))
plot(bc.t.over[,9]/100, ylab = NA,ylim = c(0,1),xlab = NA, pch=16, main = "nes",cex=2.0,
     cex.lab=2.0,cex.axis=2.0,cex.main=2.0, xaxt="n")
abline(v=c(n2+0.5,n3+0.5))

