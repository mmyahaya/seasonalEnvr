library(reshape2)
library(car)
library(zetadiv)
library(ggplot2)
library(MuMIn)
library(ggcorrplot)





tover<-Net_with_structure2
tover<-na.omit(tover)
tover<-tover[1:1500,]
Vis.mat<-tover[,c("Vis.bc1","Vis.bc2","Vis.bc3")]
names(Vis.mat)<-c("Early","Mid","Late")

Vis.mat<-melt(Vis.mat,value.name = "Turnover",variable.name = "Period")
one.test.V<-aov(Turnover ~ Period, data = Vis.mat)
summary(one.test.V)
TukeyHSD(one.test.V)

#Early
T1<-tover[,c("bet.bc1", "Enc.bc1" ,"XP.bc1" , "XA.bc1" , "Fi.bc1" )]
names(T1)<-c("FE","Enc.","Pl.","An.","Fl.")
T1<-melt(T1,value.name = "Turnover",variable.name = "Factor")
one.test.T1<-aov(Turnover ~ Factor, data = T1)
summary(one.test.T1)
TukeyHSD(one.test.T1)->Tuk1
Tuk1
write.csv(Tuk1$Factor,"Tuk1.csv",row.names = T)
#Mid
T2<-tover[,c("bet.bc2", "Enc.bc2" ,"XP.bc2" , "XA.bc2" , "Fi.bc2" )]
names(T2)<-c("FE","Enc.","Pl.","An.","Fl.")
T2<-melt(T2,value.name = "Turnover",variable.name = "Factor")
one.test.T2<-aov(Turnover ~ Factor, data = T2)
summary(one.test.T2)
TukeyHSD(one.test.T2)->Tuk2
Tuk2
write.csv(Tuk2$Factor,"Tuk2.csv",row.names = T)

#Late
T3<-tover[,c("bet.bc3", "Enc.bc3" ,"XP.bc3" , "XA.bc3" , "Fi.bc3" )]
names(T3)<-c("FE","Enc.","Pl.","An.","Fl.")
T3<-melt(T3,value.name = "Turnover",variable.name = "Factor")
one.test.T3<-aov(Turnover ~ Factor, data = T3)
summary(one.test.T3)
TukeyHSD(one.test.T3)->Tuk3
Tuk3
write.csv2(Tuk3$Factor,"Tuk3.csv",row.names = T)

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
fitted_bc2<-glm.cons(formula = Vis.bc2 ~bet.bc2+Enc.bc2+XA.bc2,
                     data = tover,cons = 1)


sum_bc2<-summary.glm(fitted_bc2)
sum_bc2
with(summary(fitted_bc2), 1 - deviance/null.deviance)

vif(fitted_bc2)->my_vif2
my_vif2
my_vif2[my_vif2>5]

# Late
fitted_bc3<-glm.cons(formula = Vis.bc3 ~bet.bc3+Enc.bc3+XP.bc3+XA.bc3 ,
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

stat.data<-data.frame("Phase"=c(rep("Early",5),rep("Mid",5),rep("Late",5)),
                      "Predictor"=c(rep(c("Foraging effort","Encounter rate","Plant density","Animal density","Floral resource"),3)),
                      "Percentage"=c(comm.analysis.bc1$Perc.bc1,NA,NA,comm.analysis.bc2[1,2],NA,comm.analysis.bc2[2:3,2],NA,
                                     comm.analysis.bc3[1,2],NA,comm.analysis.bc3[2:3,2],NA)*100)
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
ggsave("Turnv_His.tiff", plot =Turnv.plot ,
       width = 7, height = 5, dpi = 600)


#####

# H2 test
H2.mat<-data.frame(tover$H2.1,tover$H2.2,tover$H2.3)
names(H2.mat)<-c("Early","Mid","Late")

one.test.V<-aov(Turnover ~ Period, data = melt(H2.mat,value.name = "Turnover",
                                               variable.name = "Period"))
summary(one.test.V)
TukeyHSD(one.test.V)

# mod test
mod.mat<-data.frame(tover$mod.1,tover$mod.2,tover$mod.3)
names(mod.mat)<-c("Early","Mid","Late")
one.test.V<-aov(Turnover ~ Period, data = melt(mod.mat,value.name = "Turnover",
                                               variable.name = "Period"))
summary(one.test.V)
TukeyHSD(one.test.V)

# nes test
nes.mat<-data.frame(tover$nes.1,tover$nes.2,tover$nes.3)
names(nes.mat)<-c("Early","Mid","Late")
one.test.V<-aov(Turnover ~ Period, data = melt(nes.mat,value.name = "Turnover",
                                               variable.name = "Period"))
summary(one.test.V)
TukeyHSD(one.test.V)

#####
#Plots
layout(matrix(1:4, ncol = 2), widths = 1, heights = c(1,1), respect = FALSE)
par(mar = c(3,4.5,2,1.5))
boxplot(tover[,c("Vis.bc1","Vis.bc2","Vis.bc3")],col = c("grey"), boxwex = 0.5, ylab="Bray-Curtis turnover",
        main="Interaction turnover", names =c("Early","Mid", "Late"),
        ylim = c(0, 1),cex.lab=2.0,cex.axis=2.0,cex.main=2.0)


par(mar = c(3,4.5,2,1.5))


boxplot(H2.mat,col = c("grey","white","white"), boxwex = 0.5, ylab="Specialisation (H'2)",
        main="Specialisation", names =c("Early","Mid", "Late"),
        ylim = c(0, 1),cex.lab=2.0,cex.axis=2.0,cex.main=2.0)



boxplot(mod.mat,col = c("grey"), boxwex = 0.5, ylab="Modularity (Q)",
        main="Modularity", names =c("Early","Mid", "Late"),
        ylim = c(0, 1),cex.lab=2.0,cex.axis=2.0,cex.main=2.0)


boxplot(nes.mat/100,col = c("grey"), boxwex = 0.5, ylab="Nestedness (WNODA)",
        main="Nestedness", names =c("Early","Mid", "Late"),
        ylim = c(0, 1),cex.lab=2.0,cex.axis=2.0,cex.main=2.0)


dev.copy(jpeg,"Turn&struc.tiff",width = 300, height = 300,units = "mm", res = 600)
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
#--------------------------------------------------

customise<-c("Vis.bc1"="Int. Early",
             "Vis.bc2"="Int. Mid",
             "Vis.bc3"="Int. Late",
             "H2.1"="Specialisation (Early)",
             "H2.2"="Specialisation (Mid)",
             "H2.3"="Specialisation (Late)",
             "mod.1"="Modularity (Early)",
             "mod.2"="Modularity (Mid)",
             "mod.3"="Modularity (Late)",
             "nes.1"="Nestedness (Early)",
             "nes.2"="Nestedness (Mid)",
             "nes.3"="Nestedness (Late)",

             "H2.c1"="c.Specialisation (Early)",
             "H2.c2"="c.Specialisation (Mid)",
             "H2.c3"="c.Specialisation (Late)",
             "mod.c1"="c.Modularity (Early)",
             "mod.c2"="c.Modularity (Mid)",
             "mod.c3"="c.Modularity (Late)",
             "nes.c1"="c.Nestedness (Early)",
             "nes.c2"="c.Nestedness (Mid)",
             "nes.c3"="c.Nestedness (Late)",

             "mean_rA"="M. Animal amplitude","mean_rP"="M. Plant amplitude",
             "mean_cP"="M. Plant denity dep.",
             "mean_w"="M. decay rate","mean_sP"="M. Plant BP",
             "mean_sA"="M. Animal BP","mean_uP"="M. Plant SL",
             "mean_uA"="M. Animal SL","var_rP"="V. Plant amplitude",
             "var_rA"="V. Animal amplitude","var_sA"="V. Animal BP",
             "var_cP"="V. Plant density dep.","var_sigmaP"="V. Plant conversion eff.",
             "var_sigmaA"="V. Animal conversion eff.","var_a"="V. floral resource","var_sP"="V. Plant BP",
             "var_uP"="V. Plant SL","var_uA"="V. Animal SL",
              "bet.bc1"="FE. (Early)", "Enc.bc1"="Enc. (Early)", "XP.bc1"="Pl. (Early)",
             "XA.bc1"="An. (Early)", "Fi.bc1"="Fl. (Early)",
             "bet.bc2"="FE. (Mid)", "Enc.bc2"="Enc. (Mid)", "XP.bc2"="Pl. (Mid)",
             "XA.bc2"="An. (Mid)", "Fi.bc2"="Fl. (Mid)",
             "bet.bc3"="FE. (Late)", "Enc.bc3"="Enc. (Late)", "XP.bc3"="Pl. (Late)"
             , "XA.bc3"="An. (Late)", "Fi.bc3"="Fl. (Late)")
seasonParm<-c("mean_rP", "mean_rA", "mean_sP", "mean_sA", "mean_uP",
              "mean_uA", "var_rP", "var_rA", "var_sP", "var_sA", "var_uP",
              "var_uA")



cor_dat<-tover[c("Vis.bc1","Vis.bc2","Vis.bc3",seasonParm)]
corr <- round(cor(cor_dat), 2)
p.mat <- cor_pmat(cor_dat)
ggcorrplot(corr[-c(4,7,10,11,13:15),1:3],
           hc.order = FALSE,lab_size = 8, tl.cex =18,
           lab = TRUE,sig.level = 0.001,p.mat = p.mat[-c(4,7,10,11,13:15),1:3], insig = "blank")+
  scale_y_discrete(labels = customise)+
  scale_x_discrete(labels = customise)->USggfull
USggfull
ggsave("Int&parmgg.tiff", plot =USggfull ,
       width = 12, height = 7, dpi = 600)


corr[rownames()]
dput(rownames(corr))
c( "mean_rP", "mean_sP", "mean_uP", "mean_uA", "var_sP")



visdat::vis_cor(tover[,c(names(tover)[1:27])]) +
  geom_text(aes(label = round(value,2)),
                                             color = "black", size = 3)+
  scale_y_discrete(labels = customise)+
  scale_x_discrete(labels = customise,position = "top")


pairs(cor_dat)

USggsigplot

#### Network structures
#c(names(tover)[19:27],seasonParm)

c.names<-c("Vis.bc1", "bet.bc1", "Enc.bc1", "XP.bc1", "XA.bc1", "Fi.bc1",
           "H2.c1", "mod.c1", "nes.c1", "Vis.bc2", "bet.bc2", "Enc.bc2",
           "XP.bc2", "XA.bc2", "Fi.bc2", "H2.c2", "mod.c2", "nes.c2", "Vis.bc3",
           "bet.bc3", "Enc.bc3", "XP.bc3", "XA.bc3", "Fi.bc3", "H2.c3",
           "mod.c3", "nes.c3")


struct.names<-c("H2.c1", "mod.c1", "nes.c1", "H2.c2", "mod.c2", "nes.c2","H2.c3",
                "mod.c3", "nes.c3")
cor_dat<-tover[,c.names[19:27]]
corr <- round(cor(cor_dat), 2)
p.mat <- cor_pmat(cor_dat)
ggcorrplot((corr[!(rownames(corr) %in% struct.names[7:9]),struct.names[7:9]]), hc.order = FALSE,
           lab = TRUE, sig.level = 0.001,pch = 8,tl.cex = 18,
           p.mat = (p.mat[!(rownames(corr) %in% struct.names[7:9]),struct.names[7:9]]),insig = c("blank"))+
  scale_y_discrete(labels = customise)+
  scale_x_discrete(labels = customise)->ggcorplot
ggcorplot
ggsave("ggcor_Net&Turn.tiff", plot =ggcorplot,
       width = 16, height = 9, dpi = 600)

visdat::vis_cor(tover[c(1:27)]) + geom_text(aes(label = round(value,2)),
                                             color = "black", size = 3)

# H2 test
H2.mat<-data.frame(tover$H2.1,tover$H2.2,tover$H2.3)
names(H2.mat)<-c("Early","Mid","Late")

one.test.V<-aov(Turnover ~ Period, data = melt(H2.mat,value.name = "Turnover",
                                               variable.name = "Period"))
summary(one.test.V)
TukeyHSD(one.test.V)

# mod test
mod.mat<-data.frame(tover$mod.1,tover$mod.2,tover$mod.3)
names(mod.mat)<-c("Early","Mid","Late")
one.test.V<-aov(Turnover ~ Period, data = melt(mod.mat,value.name = "Turnover",
                                               variable.name = "Period"))
summary(one.test.V)
TukeyHSD(one.test.V)

# nes test
nes.mat<-data.frame(tover$nes.1,tover$nes.2,tover$nes.3)
names(nes.mat)<-c("Early","Mid","Late")
one.test.V<-aov(Turnover ~ Period, data = melt(nes.mat,value.name = "Turnover",
                                               variable.name = "Period"))
summary(one.test.V)
TukeyHSD(one.test.V)





# H2 test
H2.cmat<-data.frame(tover$H2.c1,tover$H2.c2,tover$H2.c3)
names(H2.cmat)<-c("Early","Mid","Late")

one.test.V<-aov(Turnover ~ Period, data = melt(H2.cmat,value.name = "Turnover",
                                               variable.name = "Period"))
summary(one.test.V)
TukeyHSD(one.test.V)

# mod test
mod.cmat<-data.frame(tover$mod.c1,tover$mod.c2,tover$mod.c3)
names(mod.cmat)<-c("Early","Mid","Late")
one.test.V<-aov(Turnover ~ Period, data = melt(mod.cmat,value.name = "Turnover",
                                               variable.name = "Period"))
summary(one.test.V)
TukeyHSD(one.test.V)

# nes test
nes.cmat<-data.frame(tover$nes.c1,tover$nes.c2,tover$nes.c3)
names(nes.cmat)<-c("Early","Mid","Late")
one.test.V<-aov(Turnover ~ Period, data = melt(nes.cmat,value.name = "Turnover",
                                               variable.name = "Period"))
summary(one.test.V)
TukeyHSD(one.test.V)




#Plots
layout(matrix(1:4, ncol = 2), widths = 1, heights = c(1,1), respect = FALSE)
par(mar = c(3,4.5,2,1.5))
boxplot(tover[,c("Vis.bc1","Vis.bc2","Vis.bc3")],col = c("grey"), boxwex = 0.5, ylab="Bray-Curtis turnover",
        main="Interaction turnover", names =c("Early","Mid", "Late"),
        ylim = c(0, 1),cex.lab=2.0,cex.axis=2.0,cex.main=2.0)


par(mar = c(3,4.5,2,1.5))


boxplot(H2.cmat,col = c("grey"), boxwex = 0.5,
        ylab=(TeX("$\\Delta H'_2$")),
        main="Specialisation", names =c("Early","Mid", "Late"),
        ylim = c(0, 1),cex.lab=2.0,cex.axis=2.0,cex.main=2.0)



boxplot(mod.cmat,col = c("grey","white","white"), boxwex = 0.5, ylab=(TeX("$\\Delta Q$")),
        main="Modularity", names =c("Early","Mid", "Late"),
        ylim = c(0, 1),cex.lab=2.0,cex.axis=2.0,cex.main=2.0)


boxplot(nes.cmat/100,col = c("grey"), boxwex = 0.5, ylab=(TeX("$\\Delta N$")),
        main="Nestedness", names =c("Early","Mid", "Late"),
        ylim = c(0, 1),cex.lab=2.0,cex.axis=2.0,cex.main=2.0)


# boxplot(H2.mat,col = c("grey"), boxwex = 0.5, ylab="Specialisation (H'2)",
#         main="Specialisation", names =c("Early","Mid", "Late"),
#         ylim = c(0, 1),cex.lab=2.0,cex.axis=2.0,cex.main=2.0)
#
#
#
# boxplot(mod.mat,col = c("grey"), boxwex = 0.5, ylab="Modularity (Q)",
#         main="Modularity", names =c("Early","Mid", "Late"),
#         ylim = c(0, 1),cex.lab=2.0,cex.axis=2.0,cex.main=2.0)
#
#
# boxplot(nes.mat/100,col = c("grey"), boxwex = 0.5, ylab="Nestedness (WNODA)",
#         main="Nestedness", names =c("Early","Mid", "Late"),
#         ylim = c(0, 1),cex.lab=2.0,cex.axis=2.0,cex.main=2.0)

#
# dev.copy(jpeg,"box_Net.jpeg",width = 300, height = 300,units = "mm", res = 600)
# dev.off()




# Model comparison
dep<-c("H2.c1", "mod.c1", "nes.c1", "H2.c2", "mod.c2", "nes.c2", "H2.c3",
       "mod.c3", "nes.c3")

comparison.table<-c()
for(n in dep){
  glm.com <-glm(formula = reformulate(seasonParm,n),
                data = tover,na.action = na.pass)

  comparison <- dredge(glm.com)
  comparison[1,2:13]->comparison
  rownames(comparison)<-n
  comparison.table<-rbind(comparison.table,comparison)
}
# write.csv(comparison.table,"com_table.csv",row.names = T)
for(i in 1:nrow(comparison.table)){
  structure.glm<-glm(formula = reformulate(names(comparison.table[i,!is.na(comparison.table[i,])]),
                                           rownames(comparison.table[i,])),
                     data = tover, na.action = na.pass)
  print(rownames(comparison.table[i,]))
  print(summary.glm(structure.glm))
  print(with(summary(structure.glm), 1 - deviance/null.deviance))
  print("----------------------------------------------------")
}






