library(reshape2)
library(car)
library(zetadiv)
library(ggplot2)
library(MuMIn)


tover<-read.csv("Turnover.csv")

USdata2<-na.omit(USdata2)
names(USdata2)<-names(USdata)
USdata3<-na.omit(USdata3)

tover<-USdata3[1:1500,]
tover<-na.omit(tover)
Vis.mat<-tover[,c(1,7,13)]
names(Vis.mat)<-c("Early","Mid","Late")

Vis.mat<-melt(Vis.mat,value.name = "Turnover",variable.name = "Period")
one.test.V<-aov(Turnover ~ Period, data = Vis.mat)
summary(one.test.V)
TukeyHSD(one.test.V)

#Early
T1<-tover[,2:6]
names(T1)<-c("FE","Enc.","Pl.","An.","Fl.")
T1<-melt(T1,value.name = "Turnover",variable.name = "Factor")
one.test.T1<-aov(Turnover ~ Factor, data = T1)
summary(one.test.T1)
TukeyHSD(one.test.T1)->Tuk1
Tuk1
write.csv(Tuk1$Factor,"Tuk1.csv",row.names = T)
#Mid
T2<-tover[,8:12]
names(T2)<-c("FE","Enc.","Pl.","An.","Fl.")
T2<-melt(T2,value.name = "Turnover",variable.name = "Factor")
one.test.T2<-aov(Turnover ~ Factor, data = T2)
summary(one.test.T2)
TukeyHSD(one.test.T2)->Tuk2
Tuk2
write.csv(Tuk2$Factor,"Tuk2.csv",row.names = T)

#Late
T3<-tover[,14:18]
names(T3)<-c("FE","Enc.","Pl.","An.","Fl.")
T3<-melt(T3,value.name = "Turnover",variable.name = "Factor")
one.test.T3<-aov(Turnover ~ Factor, data = T3)
summary(one.test.T3)
TukeyHSD(one.test.T3)->Tuk3
Tuk3
write.csv(Tuk3$Factor,"Tuk3.csv",row.names = T)


# Model comparison
maineffects.glm1 <-glm.cons(formula = Vis.bc1 ~bet.bc1+Enc.bc1+XP.bc1+XA.bc1+Fi.bc1, 
                            data = tover,cons = 1,na.action = na.pass)
mega.model.comparison1 <- dredge(maineffects.glm1)
head(mega.model.comparison1)
as.matrix(mega.model.comparison1)->mmc1
write.csv(mmc1,"mmc1.csv",row.names = F)


maineffects.glm2 <-glm.cons(formula = Vis.bc2 ~bet.bc2+Enc.bc2+XP.bc2+XA.bc2+Fi.bc2, 
                            data = tover,cons = 1,na.action = na.pass)
mega.model.comparison2 <- dredge(maineffects.glm2)
head(mega.model.comparison2)
as.matrix(mega.model.comparison2)->mmc2
write.csv(mmc2,"mmc2.csv",row.names = F)

maineffects.glm3 <-glm.cons(formula = Vis.bc3 ~bet.bc3+Enc.bc3+XP.bc3+XA.bc3+Fi.bc3, 
                            data = tover,cons = 1,na.action = na.pass)
mega.model.comparison3 <- dredge(maineffects.glm3)
head(mega.model.comparison3)
as.matrix(mega.model.comparison3)->mmc3
write.csv(mmc3,"mmc3.csv",row.names = F)
# Early


fitted_bc1<-glm.cons(formula = Vis.bc1 ~bet.bc1+Enc.bc1+XP.bc1, 
               data = tover,cons = 1,na.action = na.pass)


sum_bc1<-summary.glm(fitted_bc1)
sum_bc1
with(summary(fitted_bc1), 1 - deviance/null.deviance)


vif(fitted_bc1)->my_vif1
my_vif1
my_vif1[my_vif1>5]

#Mid 
fitted_bc2<-glm.cons(formula = Vis.bc2 ~bet.bc2+Enc.bc2+XP.bc2+XA.bc2 , 
                     data = tover,cons = 1)


sum_bc2<-summary.glm(fitted_bc2)
sum_bc2
with(summary(fitted_bc2), 1 - deviance/null.deviance)

vif(fitted_bc2)->my_vif2
my_vif2
my_vif2[my_vif2>5]

# Late
fitted_bc3<-glm.cons(formula = Vis.bc3 ~bet.bc3+Enc.bc3+XP.bc3 , 
                     data = tover,cons = 1,na.action = na.pass)


sum_bc3<-summary.glm(fitted_bc3)

sum_bc3

with(summary(fitted_bc3), 1 - deviance/null.deviance)
vif(fitted_bc3)->my_vif3
my_vif3
my_vif3[my_vif3>5]



# Disjoint contribution- Early period
comm.analysis.bc1<-data.frame("Var"=numeric(),"Perc.bc1"=numeric())
resp_bc1_name<-variable.names(fitted_bc1)[-1]
with(summary(fitted_bc1), 1 - deviance/null.deviance)->Total.glm.bc1
res="Vis.bc1"
for(n in 1:length(resp_bc1_name)){
  f<-reformulate(resp_bc1_name[-n],res)
  var.name<-resp_bc1_name[n]
  g.l.m1<-glm(formula = f, data=tover, family = quasibinomial(link = 'logit'))
  perc<-(Total.glm.bc1-with(summary(g.l.m1), 1 - deviance/null.deviance))/Total.glm.bc1
  comm.analysis.bc1<-rbind(comm.analysis.bc1,data.frame("Var"=var.name,"Perc.bc1"=perc))
}
# Mid
comm.analysis.bc2<-data.frame("Var"=numeric(),"Perc.bc2"=numeric())
resp_bc2_name<-variable.names(fitted_bc2)[-1]
with(summary(fitted_bc2), 1 - deviance/null.deviance)->Total.glm.bc2
res="Vis.bc2"
for(n in 1:length(resp_bc2_name)){
  f<-reformulate(resp_bc2_name[-n],res)
  var.name<-resp_bc2_name[n]
  g.l.m2<-glm(formula = f, data=tover, family = quasibinomial(link = 'logit'))
  perc<-(Total.glm.bc2-with(summary(g.l.m2), 1 - deviance/null.deviance))/Total.glm.bc2
  comm.analysis.bc2<-rbind(comm.analysis.bc2,data.frame("Var"=var.name,"Perc.bc2"=perc))
}
#Late
comm.analysis.bc3<-data.frame("Var"=numeric(),"Perc.bc3"=numeric())
resp_bc3_name<-variable.names(fitted_bc3)[-1]
with(summary(fitted_bc3), 1 - deviance/null.deviance)->Total.glm.bc3
res="Vis.bc3"
for(n in 1:length(resp_bc3_name)){
  f<-reformulate(resp_bc3_name[-n],res)
  var.name<-resp_bc3_name[n]
  g.l.m3<-glm(formula = f, data=tover, family = quasibinomial(link = 'logit'))
  perc<-(Total.glm.bc3-with(summary(g.l.m3), 1 - deviance/null.deviance))/Total.glm.bc3
  comm.analysis.bc3<-rbind(comm.analysis.bc3,data.frame("Var"=var.name,"Perc.bc3"=perc))
}

stat.data<-data.frame( "Phase"=c(rep("Early",5),rep("Mid",5),rep("Late",5)),
                      "Predictor"=c(rep(c("Foraging effort","Encounter rate","Plant density","Animal density","Floral resource"),3)),
                      "Percentage"=c(comm.analysis.bc1$Perc.bc1,NA,NA,comm.analysis.bc2$Perc.bc2,NA,comm.analysis.bc3$Perc.bc3,NA,NA)*100)
stat.data$Phase <- factor(stat.data$Phase, levels = c("Early", "Mid","Late"))
stat.data$Predictor<- factor(stat.data$Predictor, levels = c("Foraging effort","Encounter rate","Plant density","Animal density","Floral resource"))
stat.data<-na.omit(stat.data)
Turnv.plot<-ggplot(stat.data, aes(fill=Predictor, y=Percentage, x=Phase )) + 
  geom_bar(position="dodge", stat="identity")+ theme_classic()+
  theme(axis.text.x = element_text(size = 14),  # Adjust x-axis text size
        axis.text.y = element_text(size = 14),  # Adjust y-axis text size
        axis.title.x = element_text(size = 14),  # Adjust x-axis label size
        axis.title.y = element_text(size = 14),  # Adjust y-axis label size
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))+
  labs(y="Disjoint contribution (%)")
  #facet_grid(Phase ~ .)
Turnv.plot
ggsave("Turnv_US101.jpeg", plot =Turnv.plot ,
       width = 7, height = 5, dpi = 600)


#####


#####
#Plots
layout(matrix(1:4, ncol = 2), widths = 1, heights = c(1,1), respect = FALSE)
par(mar = c(3,4.5,2,1.5))
tplot<-boxplot(tover[,c(1,7,13)],col = c("grey"), boxwex = 0.5, ylab="Bray-Curtis turnover", 
        main="Interaction", names =c("Early","Mid", "Late"),
        ylim = c(0, 1),cex.lab=2.0,cex.axis=2.0,cex.main=2.0)


par(mar = c(3,4.5,2,1.5))


boxplot(tover[,7:12],col = c("grey",rep("white",5)), boxwex = 0.5, ylab="Bray-Curtis turnover", 
        main="Mid phase", names =c("Int.","FE","Enc.","Pl.","An.","Fl."),
        ylim = c(0, 1),cex.lab=2.0,cex.axis=2.0,cex.main=2.0 )
abline(v=c(1.5),lty=2)

boxplot(tover[,1:6],col = c("grey",rep("white",5)), boxwex = 0.5, ylab="Bray-Curtis turnover", 
        main="Early phase", names =c("Int.","FE","Enc.","Pl.","An.","Fl."),
        ylim = c(0, 1),cex.lab=2.0,cex.axis=2.0,cex.main=2.0 )
abline(v=c(1.5),lty=2)

boxplot(tover[,13:18],col = c("grey",rep("white",5)), boxwex = 0.5, ylab="Bray-Curtis turnover", 
        main="Late phase", names =c("Int.","FE","Enc.","Pl.","An.","Fl."),
        ylim = c(0, 1),cex.lab=2.0,cex.axis=2.0,cex.main=2.0 )
abline(v=c(1.5),lty=2)


dev.copy(jpeg,"ToverFull_US101.jpeg",width = 300, height = 300,units = "mm", res = 600)
dev.off()


# mtext()

library(jtools)

effect_plot(fitted_bc1,pred = bet.bc1,interval = TRUE ,plot.points = TRUE,
            jitter = 0.05) +  labs(y = "Interaction", x= "Foraging effort")+
  theme(text = element_text(size=18),axis.text.x = element_blank(), 
        axis.title.y = element_text(size = 20))->eff_plot.bb1
effect_plot(fitted_bc1,pred = Enc.bc1,interval = TRUE ,plot.points = TRUE,
            jitter = 0.05) +  labs(y = NULL, x= "Encounter rate")+
  theme(text = element_text(size=18),axis.text.x = element_blank(), 
        axis.title.y = element_text(size = 20))->eff_plot.be1
effect_plot(fitted_bc1,pred = XP.bc1,interval = TRUE ,plot.points = TRUE,
            jitter = 0.05) +  labs(y = NULL, x= "Plant density")+
  theme(text = element_text(size=18),axis.text.x = element_blank(), 
        axis.title.y = element_text(size = 20))->eff_plot.bp1




effect_plot(fitted_bc2,pred = bet.bc2,interval = TRUE ,plot.points = TRUE,
            jitter = 0.05) +  labs(y = "Interaction", x= "Foraging effort")+
  theme(text = element_text(size=18),axis.text.x = element_blank(), 
        axis.title.y = element_text(size = 20))->eff_plot.bb2
effect_plot(fitted_bc2,pred = Enc.bc2,interval = TRUE ,plot.points = TRUE,
            jitter = 0.05) +  labs(y = NULL, x= "Encounter rate")+
  theme(text = element_text(size=18),axis.text.x = element_blank(), 
        axis.title.y = element_text(size = 20))->eff_plot.be2
effect_plot(fitted_bc2,pred = XP.bc2,interval = TRUE ,plot.points = TRUE,
            jitter = 0.05) +  labs(y = NULL, x= "Plant density")+
  theme(text = element_text(size=18),axis.text.x = element_blank(), 
        axis.title.y = element_text(size = 20))->eff_plot.bp2
effect_plot(fitted_bc2,pred = XA.bc2,interval = TRUE ,plot.points = TRUE,
            jitter = 0.05) +  labs(y = NULL, x= "Animal density")+
  theme(text = element_text(size=18),axis.text.x = element_blank(), 
        axis.title.y = element_text(size = 20))->eff_plot.ba2


effect_plot(fitted_bc3,pred = bet.bc3,interval = TRUE ,plot.points = TRUE,
            jitter = 0.05) +  labs(y = "Interaction", x= "Foraging effort")+
  theme(text = element_text(size=18),axis.text.x = element_blank(), 
        axis.title.y = element_text(size = 20))->eff_plot.bb3
effect_plot(fitted_bc3,pred = Enc.bc3,interval = TRUE ,plot.points = TRUE,
            jitter = 0.05) +  labs(y = NULL, x= "Encounter rate")+
  theme(text = element_text(size=18),axis.text.x = element_blank(), 
        axis.title.y = element_text(size = 20))->eff_plot.be3
effect_plot(fitted_bc3,pred = XP.bc3,interval = TRUE ,plot.points = TRUE,
            jitter = 0.05) +  labs(y = NULL, x= "Plant density")+
  theme(text = element_text(size=18),axis.text.x = element_blank(), 
        axis.title.y = element_text(size = 20))->eff_plot.bp3




library(gridExtra)
grid.arrange(eff_plot.bb1,eff_plot.be1, eff_plot.bp1,
             eff_plot.bb2,eff_plot.be2, eff_plot.bp2,
             eff_plot.bb3,eff_plot.be2,ncol = 3)->eff.plot


grid.arrange(eff_plot.bb1,eff_plot.be1, eff_plot.bp1,
             ncol = 3)->eff.plot.1


ggsave("eff.1.jpeg", plot =eff.plot.1 ,
       width = 16, height = 9, dpi = 600)



grid.arrange(eff_plot.bb2,eff_plot.be2, eff_plot.bp2,eff_plot.ba2,
             ncol = 4)->eff.plot.2


ggsave("eff.2.jpeg", plot =eff.plot.2 ,
       width = 16, height = 9, dpi = 600)



grid.arrange(eff_plot.bb3,eff_plot.be3, eff_plot.bp3,
             ncol = 3)->eff.plot.3


ggsave("eff.3.jpeg", plot =eff.plot.3 ,
       width = 16, height = 9, dpi = 600)

cor_dat<-tover[c(1,7,13,20,21,26:30,33,35:37,39,41,42)]
corr <- round(cor(cor_dat), 2)
p.mat <- cor_pmat(cor_dat)
ggcorrplot(corr, hc.order = FALSE,
           lab = TRUE,p.mat = p.mat, insig = "blank")->USggfull
USggfull
ggsave("USggfull1.jpeg", plot =USggfull ,
       width = 16, height = 9, dpi = 600)


customise<-c("Vis.bc1"="Int. Early","Vis.bc2"="Int. Mid","Vis.bc3"="Int. Late",
             "mean_rA"="M. Animal amplitude","mean_cP"="M. Plant denity dep.",
             "mean_w"="M. decay rate","mean_sP"="M. Plant SS",
             "mean_sA"="M. Animal SS","mean_uP"="M. Plant SL","mean_uA"="M. Animal SL",
             "var_cP"="V. Plant density dep.","var_sigmaP"="V. Plant conversion eff.",
             "var_sigmaA"="V. Animal conversion eff.","var_a"="V. floral resource","var_sP"="V. Plant SS",
             "var_uP"="V. Plant SS","var_uA"="V. Animal SL")


cor_dat<-tover[c(1,7,13,20,21,26:30,33,35:37,39,41,42)]
corr <- round(cor(cor_dat), 2)
p.mat <- cor_pmat(cor_dat)
ggcorrplot(corr[,1:3], hc.order = FALSE,
           lab = TRUE)+
  scale_y_discrete(labels = customise)+
  scale_x_discrete(labels = customise)->USggsigplot


ggsave("USggsig1.jpeg", plot =USggsigplot ,
       width = 12, height = 6, dpi = 600)



vis_cor(USdata[c(1,7,13,19:42)]) + geom_text(aes(label = round(value,2)), 
                                             color = "black", size = 3)


pairs(cor_dat)



#### 50 species


