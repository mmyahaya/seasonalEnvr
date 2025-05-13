library(reshape2)
library(zetadiv)
library(ggplot2)
library(car)
library(MuMIn)
library(tidyverse)
library(latex2exp)




tover<-Net_with_structure2
tover<-na.omit(tover)
#tover<-tover[1:1500,]

tover<- tover %>%
  mutate(across(starts_with("nes"), ~ . / 100))

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


dep<-c("Vis.bc1", "H2.c1", "mod.c1", "nes.c1",
       "Vis.bc2", "H2.c2", "mod.c2", "nes.c2", 
       "Vis.bc3", "H2.c3", "mod.c3", "nes.c3")

turnoverParm<-c("bet.bc1", "Enc.bc1", "XP.bc1", "XA.bc1", "Fi.bc1",
                "bet.bc2", "Enc.bc2", "XP.bc2", "XA.bc2", "Fi.bc2",
                "bet.bc3", "Enc.bc3", "XP.bc3", "XA.bc3", "Fi.bc3")

#### Compute the best models for interaction and structure turnover####

comparison.table <- c()

# Define index groups for turnoverParm
index_groups <- list(1:5, 6:10, 11:15)

# Loop through all indices in dep
for (i in seq_along(dep)) {
  parm_index <- ((i - 1) %/% 4) + 1  # Determine the correct turnoverParm index group
  glm.com <- glm.cons(formula = reformulate(turnoverParm[index_groups[[parm_index]]], dep[i]),
                      data = tover, cons = 1, na.action = na.pass)
  
  comparison <- dredge(glm.com)
  comparison[1, 2:6] -> comparison
  rownames(comparison) <- dep[i]
  names(comparison) <- c("Foraging effort", "Encounter rate", 
                         "Floral resource", "Animal density", "Plant density")
  comparison.table <- rbind(comparison.table, comparison)
}


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
fitted_bc3<-glm.cons(formula = Vis.bc3 ~bet.bc3+Enc.bc3+XP.bc3,
                     data = tover,cons = 1,na.action = na.pass)


sum_bc3<-summary.glm(fitted_bc3)

sum_bc3

with(summary(fitted_bc3), 1 - deviance/null.deviance)
vif(fitted_bc3)->my_vif3
my_vif3
my_vif3[my_vif3>5]

# Specialisation
fitted_H2.c1<-glm.cons(formula = H2.c1 ~bet.bc1+XP.bc1,
                       data = tover,cons = 1,na.action = na.pass)
summary.glm(fitted_H2.c1)

with(summary(fitted_H2.c1), 1 - deviance/null.deviance)
vif(fitted_H2.c1)

fitted_H2.c2<-glm.cons(formula = H2.c2 ~bet.bc2,
                       data = tover,cons = 1,na.action = na.pass)

with(summary(fitted_H2.c2), 1 - deviance/null.deviance)
vif(fitted_H2.c2)
summary.glm(fitted_H2.c2)
fitted_H2.c3<-glm.cons(formula = H2.c3 ~bet.bc3+Enc.bc3,
                       data = tover,cons = 1,na.action = na.pass)

with(summary(fitted_H2.c3), 1 - deviance/null.deviance)
vif(fitted_H2.c3)
summary.glm(fitted_H2.c3)
#Mod

fitted_mod.c1<-glm.cons(formula = mod.c1 ~bet.bc1+XP.bc1,
                       data = tover,cons = 1,na.action = na.pass)

with(summary(fitted_mod.c1), 1 - deviance/null.deviance)
summary.glm(fitted_mod.c1)
vif(fitted_mod.c1)
fitted_mod.c2<-glm.cons(formula = mod.c2 ~bet.bc2,
                        data = tover,cons = 1,na.action = na.pass)

with(summary(fitted_mod.c2), 1 - deviance/null.deviance)
vif(fitted_mod.c2)
summary.glm(fitted_mod.c2)
fitted_mod.c3<-glm.cons(formula = mod.c3 ~XP.bc3,
                        data = tover,cons = 1,na.action = na.pass)

with(summary(fitted_mod.c3), 1 - deviance/null.deviance)
vif(fitted_mod.c3)
summary.glm(fitted_mod.c3)
# Nes
fitted_nes.c1<-glm.cons(formula = nes.c1 ~bet.bc1+Enc.bc1+XA.bc1,
                        data = tover,cons = 1,na.action = na.pass)

with(summary(fitted_nes.c1), 1 - deviance/null.deviance)
summary.glm(fitted_nes.c1)
vif(fitted_nes.c1)

fitted_nes.c2<-glm.cons(formula = nes.c2 ~XA.bc2+Fi.bc2,
                        data = tover,cons = 1,na.action = na.pass)

with(summary(fitted_nes.c2), 1 - deviance/null.deviance)
vif(fitted_nes.c2)
summary.glm(fitted_nes.c2)
fitted_nes.c3<-glm.cons(formula = nes.c3 ~bet.bc3+XA.bc3,
                        data = tover,cons = 1,na.action = na.pass)

with(summary(fitted_nes.c3), 1 - deviance/null.deviance)
vif(fitted_nes.c3)
summary.glm(fitted_nes.c3)

##### Compute disjoint contribution for interaction turnover ####

disjoint<-function(fit,res){
  comm.analysis<-data.frame("Var"=numeric(),"Perc"=numeric())
  pred<-variable.names(fit)[-1]
  Total.glm<-with(summary(fit), 1 - deviance/null.deviance)
  for(n in 1:length(pred)){
    f<-reformulate(pred[-n],res)
    var.name<-pred[n]
    var.name <- substr(var.name, 1, nchar(var.name) - 1)
    g.l.m<-glm.cons(formula = f,
            data = tover,
            cons = 1,
            na.action = na.pass)
    perc<-(Total.glm-with(summary(g.l.m), 1 - deviance/null.deviance))/Total.glm
    comm.analysis<-rbind(comm.analysis,data.frame("Var"=var.name,"Perc"=perc))
  }
  comm.analysis
}

# Disjoint contribution- Early period
comm.analysis.bc1<-disjoint(fitted_bc1,"Vis.bc1")
# Mid
comm.analysis.bc2<-disjoint(fitted_bc2,"Vis.bc2")
#Late
comm.analysis.bc3<-disjoint(fitted_bc3,"Vis.bc3")


comm.analysis<-full_join(comm.analysis.bc1,comm.analysis.bc2,
                         by = join_by(Var)) %>%
  full_join(comm.analysis.bc3, by = join_by(Var))
names(comm.analysis)<-c("Predictor","Early","Mid","Late")
comm.analysis<-comm.analysis %>%
  mutate(Predictor=case_when(
    Predictor=="bet.bc" ~ "Foraging effort",
    Predictor=="Enc.bc" ~ "Encounter rate",
    Predictor=="XP.bc" ~ "Plant density",
    Predictor=="XA.bc" ~ "Animal density",
    Predictor=="Fi.bc" ~ "Floral resource",
    TRUE ~ NA
  ))

# df %>% gather("key", "value", x, y, z) is equivalent
# to df %>% pivot_longer(c(x, y, z), names_to = "key", values_to = "value")
stat.data<-comm.analysis %>%
  pivot_longer(c(Early,Mid,Late),names_to = "Phase", values_to = "Percentage")

stat.data$Phase <- factor(stat.data$Phase, levels = c("Early", "Mid","Late"))
stat.data$Predictor<- factor(stat.data$Predictor, levels = c("Foraging effort",
                                                             "Encounter rate",
                                                             "Plant density",
                                                             "Animal density",
                                                             "Floral resource"))
stat.data<-na.omit(stat.data)
stat.data$Percentage<-stat.data$Percentage*100
Turnv.plot<-ggplot(stat.data, aes(fill=Predictor, y=Percentage, x=Phase )) +
  geom_bar(position="dodge", stat="identity")+ theme_classic()+
  theme(axis.text.x = element_text(size = 14),  # Adjust x-axis text size
        axis.text.y = element_text(size = 14),  # Adjust y-axis text size
        axis.title.x = element_text(size = 14),  # Adjust x-axis label size
        axis.title.y = element_text(size = 14),  # Adjust y-axis label size
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))+
  labs(y="Disjoint contribution (%)",
       x=TeX("Interaction turnover ($Delta V$)"))+
  scale_fill_discrete(
    labels = c("Foraging effort"=bquote(Delta ~ "Foraging effort"), 
               "Encounter rate"=bquote(Delta ~ "Encounter rate"),
               "Plant density"=bquote(Delta ~ "Plant density"),
               "Animal density"=bquote(Delta ~ "Animal density"))  # Custom math labels
  )
  #facet_grid(Phase ~ .)
Turnv.plot
ggsave("His_V1503.tiff", plot =Turnv.plot ,
       width = 7, height = 5, dpi = 600)





#### Test for differences in structure turnovers ####



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
boxplot(tover[,c("Vis.bc1","Vis.bc2","Vis.bc3")],col = c("grey"), boxwex = 0.5, 
        ylab=TeX("$Delta V$"),
        main="Interaction", names =c("Early","Mid", "Late"),
        ylim = c(0, 1),cex.lab=2.0,cex.axis=2.0,cex.main=2.0)
text(0.6,1,"(a) ",cex=2.0)


par(mar = c(3,4.5,2,1.5))


boxplot(H2.cmat,col = c("grey"), boxwex = 0.5,
        ylab=(TeX("$\\Delta H'_2$")),
        main="Specialisation", names =c("Early","Mid", "Late"),
        ylim = c(0, 1),cex.lab=2.0,cex.axis=2.0,cex.main=2.0)
text(0.6,1,"(b) ",cex=2.0)


boxplot(mod.cmat,col = c("grey","white","white"), boxwex = 0.5, ylab=(TeX("$\\Delta Q$")),
        main="Modularity", names =c("Early","Mid", "Late"),
        ylim = c(0, 1),cex.lab=2.0,cex.axis=2.0,cex.main=2.0)
text(0.6,1,"(c) ",cex=2.0)

boxplot(nes.cmat,col = c("grey"), boxwex = 0.5, ylab=(TeX("$\\Delta N$")),
        main="Nestedness", names =c("Early","Mid", "Late"),
        ylim = c(0, 1),cex.lab=2.0,cex.axis=2.0,cex.main=2.0)
text(0.6,1,"(d) ",cex=2.0)


dev.copy(jpeg,"box_Net1503.tiff",width = 300, height = 300,units = "mm", res = 600)
dev.off()


disjoint_season<-function(fit,res){
  comm.analysis<-data.frame("Var"=numeric(),"Perc"=numeric())
  pred<-variable.names(fit)[-1]
  Total.glm<-with(summary(fit), 1 - deviance/null.deviance)
  for(n in 1:length(pred)){
    f<-reformulate(pred[-n],res)
    var.name<-pred[n]
    g.l.m<-glm(formula = f,
                    data = tover)
    perc<-(Total.glm-with(summary(g.l.m), 1 - deviance/null.deviance))/Total.glm
    comm.analysis<-rbind(comm.analysis,data.frame("Var"=var.name,"Perc"=perc))
  }
  comm.analysis
}



seasonParm<-c("mean_rP", "mean_rA", "mean_sP", "mean_sA", "mean_uP",
              "mean_uA", "var_rP", "var_rA", "var_sP", "var_sA", "var_uP",
              "var_uA")
dep<-c("Vis.bc1", "H2.c1", "mod.c1", "nes.c1",
       "Vis.bc2", "H2.c2", "mod.c2", "nes.c2", 
       "Vis.bc3", "H2.c3", "mod.c3", "nes.c3")

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
                     data = tover)
  cli::cli_h2(cli::col_blue(rownames(comparison.table[i,])))
 
  print(summary.glm(structure.glm))
  
  
  cat("R_squared:",cli::col_red(round(with(summary(structure.glm), 1 - deviance/null.deviance),4)),"\n\n\n")
  
}

#### Write disjoint contribution ####

dis.table<-data.frame()
for(i in 1:nrow(comparison.table)){
  structure.glm<-glm(formula = reformulate(names(comparison.table[i,!is.na(comparison.table[i,])]),
                                           rownames(comparison.table[i,])),
                     data = tover)
  dis<-disjoint_season(structure.glm,rownames(comparison.table[i,]))
  dis$response<-rownames(comparison.table[i,])
  dis.table<-rbind(dis.table,dis)
  

}

dis.table<-dis.table %>% 
mutate(Phase = case_when(
  str_sub(response,-1) == "1" ~ "Early",
  str_sub(response,-1) == "2" ~ "Mid",
  str_sub(response,-1) == "3" ~ "Late",
  TRUE ~ ""
))

dis.table<-dis.table %>% 
  mutate(Structure = case_when(
    str_sub(response,1,3) == "Vis" ~ "Vis",
    str_sub(response,1,2) == "H2" ~ "H2",
    str_sub(response,1,3) == "mod" ~ "mod",
    str_sub(response,1,3) == "nes" ~ "nes",
    TRUE ~ ""
  ))
dis.table$Structure<-factor(dis.table$Structure, levels = c("Vis","H2","mod","nes"))

dis.table$response<-factor(dis.table$response, levels = dep)
dis.table$Phase<-factor(dis.table$Phase, levels = c("Early","Mid","Late"))
dis.table$Perc<-dis.table$Perc*100

dis_plot<-ggplot(dis.table %>% filter(!(Structure=="Vis"))) +
  geom_bar(aes(fill = Var, y = Perc, x = Structure), 
           position = "dodge", stat = "identity") + 
  theme_classic() +
  theme(axis.text.x = element_text(size = 14),  
        axis.text.y = element_text(size = 14),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),  # Adjust legend title size
        strip.text = element_text(size = 14)) +
  labs(y = "Disjoint contribution (%)", fill = "Seasonal parameter") +  # Rename legend
  facet_grid(Phase ~ ., switch = 'y') +
  
  # Custom axis labels with expressions
  scale_x_discrete(labels = c("Vis" = bquote(Delta ~ "Interaction"), 
                              "H2" = bquote(Delta ~ "Specialisation"), 
                              "mod" = bquote(Delta ~ "Modularity"),
                              "nes" = TeX("$Delta$ Nestedness"))) +
  
  # Apply Set1 palette for fill colors
  scale_fill_brewer(palette = "Set1", 
                    labels = c("mean_rP" = TeX("mean $(rho^P)$"), 
                               "mean_rA" = TeX("mean $(rho^A)$"),
                               "mean_sP" = TeX("mean $(s^P)$"), 
                               "mean_sA" = TeX("mean $(s^A)$"),
                               "mean_uP" = TeX("mean $(u^P)$"),
                               "mean_uA" = TeX("mean $(u^A)$"), 
                               "var_rP" = TeX("var $(rho^P)$"),
                               "var_rA" = TeX("var $(rho^A)$"), 
                               "var_sP" = TeX("var $(s^P)$"),
                               "var_sA" = TeX("var $(s^A)$"), 
                               "var_uP" = TeX("var $(u^P)$"),
                               "var_uA" = TeX("var $(u^A)$")))

dis_plot
 
ggsave("dis_plot.tiff", plot =dis_plot ,
       width = 10, height = 5, dpi = 600)

Vis_table<-dis.table %>% filter(Structure=="Vis")

Vis_plot<-ggplot(Vis_table, aes(fill=Var, y=Perc, x=Phase )) +
  geom_bar(position="dodge", stat="identity")+ theme_classic()+
  theme(axis.text.x = element_text(size = 14),  # Adjust x-axis text size
        axis.text.y = element_text(size = 14),  # Adjust y-axis text size
        axis.title.x = element_text(size = 14),  # Adjust x-axis label size
        axis.title.y = element_text(size = 14),  # Adjust y-axis label size
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))+
  labs(y="Disjoint contribution (%)",
       x=TeX("Interaction turnover ($Delta V$)"),
       fill = "Seasonal parameter")+
  scale_fill_brewer(palette = "Set1",
    labels  = c("mean_rP" = TeX("mean $(rho^P)$"), 
                        "mean_rA" = TeX("mean $(rho^A)$"),
                        "mean_sP" = TeX("mean $(s^P)$"), 
                        "mean_sA" = TeX("mean $(s^A)$"),
                        "mean_uP" = TeX("mean $(u^P)$"),
                        "mean_uA" = TeX("mean $(u^A)$"), 
                        "var_rP" = TeX("var $(rho^P)$"),
                        "var_rA" = TeX("var $(rho^A)$"), 
                        "var_sP" = TeX("var $(s^P)$"),
                        "var_sA" = TeX("var $(s^A)$"), 
                        "var_uP" = TeX("var $(u^P)$"),
                        "var_uA" = TeX("var $(u^A)$")))  # Custom math labels

Vis_plot

ggsave("His_vis1503.tiff", plot =Vis_plot ,
       width = 7, height = 5, dpi = 600)

#### Supplementary
layout(matrix(1:3, ncol = 1), widths = 1, heights = c(0.9,0.9,1), respect = FALSE)
par(mar = c(1.,4.5,2,1.5))

boxplot(tover[,c("bet.bc1", "Enc.bc1", "XP.bc1", "XA.bc1", "Fi.bc1")],
        col = c("grey"), boxwex = 0.5, ylab=NULL,
        main="Early phase", names =c("FE","Enc.","Pl.","An.","Fl."),
        ylim = c(0, 1),cex.lab=2.0,cex.axis=2.0,cex.main=2.0,xaxt="n" )


boxplot(tover[,c("bet.bc2", "Enc.bc2", "XP.bc2", "XA.bc2", "Fi.bc2")],
        col = c("grey"), boxwex = 0.5, ylab="Bray-Curtis turnover",
        main="Mid phase", names =c("FE","Enc.","Pl.","An.","Fl."),
        ylim = c(0, 1),cex.lab=2.0,cex.axis=2.0,cex.main=2.0,xaxt="n" )
par(mar = c(2.,4.5,2,1.5))


boxplot(tover[,c("bet.bc3", "Enc.bc3", "XP.bc3", "XA.bc3", "Fi.bc3")],
        col = c("grey"), boxwex = 0.5, ylab=NULL,
        main="Late phase", names =c("FE","Enc.","Pl.","An.","Fl."),
        ylim = c(0, 1),cex.lab=2.0,cex.axis=2.0,cex.main=2.0 )



# Specilisation
comm.analysis.H2.1<-disjoint(fitted_H2.c1,"H2.c1")

comm.analysis.H2.2<-disjoint(fitted_H2.c2,"H2.c2")

comm.analysis.H2.3<-disjoint(fitted_H2.c3,"H2.c3")

#Modularity

comm.analysis.mod.1<-disjoint(fitted_mod.c1,"mod.c1")

comm.analysis.mod.2<-disjoint(fitted_mod.c2,"mod.c2")

comm.analysis.mod.3<-disjoint(fitted_mod.c3,"mod.c3")

# nestedness

comm.analysis.nes.1<-disjoint(fitted_nes.c1,"nes.c1")

comm.analysis.nes.2<-disjoint(fitted_nes.c2,"nes.c2")

comm.analysis.nes.3<-disjoint(fitted_nes.c3,"nes.c3")

comm.analysis.H2.1$response<-"H2.c1"
comm.analysis.H2.3$response<-"H2.c3"
comm.analysis.mod.1$response<-"mod.c1"
comm.analysis.nes.1$response<-"nes.c1"
comm.analysis.nes.2$response<-"nes.c2"
comm.analysis.nes.3$response<-"nes.c3"
dis.str.table<-rbind(comm.analysis.H2.1,comm.analysis.H2.3,
      comm.analysis.mod.1,
      comm.analysis.nes.1,comm.analysis.nes.2,comm.analysis.nes.3)


dis.str.table<-dis.str.table %>% 
  mutate(Phase = case_when(
    str_sub(response,-1) == "1" ~ "Early",
    str_sub(response,-1) == "2" ~ "Mid",
    str_sub(response,-1) == "3" ~ "Late",
    TRUE ~ ""
  ))

dis.str.table<-dis.str.table %>% 
  mutate(Structure = case_when(
    str_sub(response,1,2) == "H2" ~ "H2",
    str_sub(response,1,3) == "mod" ~ "mod",
    str_sub(response,1,3) == "nes" ~ "nes",
    TRUE ~ ""
  ))


dis.str.table$Structure<-factor(dis.str.table$Structure, levels = c("H2","mod","nes"))

dis.str.table$response<-factor(dis.str.table$response, 
                               levels = c("H2.c1", "mod.c1", "nes.c1",
                                              "nes.c2", 
                                              "H2.c3", "nes.c3"))
dis.str.table$Phase<-factor(dis.str.table$Phase, levels = c("Early","Mid","Late"))
dis.str.table$Perc<-dis.str.table$Perc*100

dis_plot<-ggplot(dis.str.table) +
  geom_bar(aes(fill = Var, y = Perc, x = Structure), 
           position = "dodge", stat = "identity") + 
  theme_classic() +
  theme(axis.text.x = element_text(size = 14),  
        axis.text.y = element_text(size = 14),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),  # Adjust legend title size
        strip.text = element_text(size = 14)) +
  labs(y = "Disjoint contribution (%)", fill = "Turnover predictor") +  # Rename legend
  facet_grid(Phase ~ ., switch = 'y') +
  
  # Custom axis labels with expressions
  scale_x_discrete(labels = c("Vis" = bquote(Delta ~ "Interaction"), 
                              "H2" = bquote(Delta ~ "Specialisation"), 
                              "mod" = bquote(Delta ~ "Modularity"),
                              "nes" = TeX("$Delta$ Nestedness"))) +
  
  # Apply Set1 palette for fill colors
  scale_fill_brewer(palette = "Set1", 
                    labels = c("bet.bc" = TeX("$Delta$ Foraging effort"), 
                               "Enc.bc" = TeX("$Delta$ Encounter rate"),
                               "XP.bc" = TeX("$Delta$ Plant density"),
                               "XA.bc" = TeX("$Delta$ Animal density"),
                               "Fi.bc" = TeX("$Delta$ Floral resource")))
