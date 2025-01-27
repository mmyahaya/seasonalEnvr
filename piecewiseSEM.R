# Load library
library(piecewiseSEM)
library(zetadiv)
library(dplyr)
library(DiagrammeR)



models.1 <- psem(
  glm(formula = bet.bc1 ~ mean_rP + mean_rA + mean_sP + mean_sA + mean_uP + mean_uA +
             var_rP + var_rA + var_sP + var_sA + var_uP + var_uA,
           data = tover),
  glm(formula = Enc.bc1 ~ mean_rP + mean_rA + mean_sP + mean_sA + mean_uP + mean_uA +
             var_rP + var_rA + var_sP + var_sA + var_uP + var_uA,
           data = tover),
  glm(formula = XP.bc1 ~ mean_rP + mean_rA + mean_sP + mean_sA + mean_uP + mean_uA +
             var_rP + var_rA + var_sP + var_sA + var_uP + var_uA,
           data = tover),
  glm(formula = XA.bc1 ~ mean_rP + mean_rA + mean_sP + mean_sA + mean_uP + mean_uA +
        var_rP + var_rA + var_sP + var_sA + var_uP + var_uA,
      data = tover),
  glm(formula = Fi.bc1 ~ mean_rP + mean_rA + mean_sP + mean_sA + mean_uP + mean_uA +
        var_rP + var_rA + var_sP + var_sA + var_uP + var_uA,
      data = tover),
  

  glm.cons(formula = Vis.bc1 ~bet.bc1+Enc.bc1+XP.bc1+XA.bc1+Fi.bc1,
           data = tover,cons = 1,na.action = na.pass),
  glm.cons(formula = H2.c1 ~bet.bc1+Enc.bc1+XP.bc1+XA.bc1+Fi.bc1,
           data = tover,cons = 1,na.action = na.pass),
  glm.cons(formula = mod.c1 ~bet.bc1+Enc.bc1+XP.bc1+XA.bc1+Fi.bc1,
           data = tover,cons = 1,na.action = na.pass),
  glm.cons(formula = nes.c1 ~bet.bc1+Enc.bc1+XP.bc1+XA.bc1+Fi.bc1,
           data = tover,cons = 1,na.action = na.pass),
  
  tover
)
# Run summary
# Address conflict using conserve = T
summary(models.1, conserve = T)
plot(models.1)
model_plot<-plot(models.1, return=TRUE)


model_plot$edges_df<-model_plot$edges_df %>%
  filter(style=="solid") %>%
  filter(abs(as.numeric(label))>0.2) %>%
  mutate(color= ifelse(as.numeric(label)<0,"red",color))
model_plot$nodes_df<-model_plot$nodes_df %>%
  filter(id %in% c(model_plot$edges_df$from,model_plot$edges_df$to))

# Render the graph
render_graph(model_plot,layout =  "tree")

graph<-create_graph(
  nodes_df = model_plot$nodes_df,
  edges_df = model_plot$edges_df,
  directed = TRUE,
  graph_name = NULL,
  attr_theme = "default",
  write_backups = FALSE,
  display_msgs = FALSE
)



models.2 <- psem(
  glm(formula = bet.bc2 ~ mean_rP + mean_rA + mean_sP + mean_sA + mean_uP + mean_uA +
        var_rP + var_rA + var_sP + var_sA + var_uP + var_uA,
      data = tover),
  glm(formula = Enc.bc2 ~ mean_rP + mean_rA + mean_sP + mean_sA + mean_uP + mean_uA +
        var_rP + var_rA + var_sP + var_sA + var_uP + var_uA,
      data = tover),
  glm(formula = XP.bc2 ~ mean_rP + mean_rA + mean_sP + mean_sA + mean_uP + mean_uA +
        var_rP + var_rA + var_sP + var_sA + var_uP + var_uA,
      data = tover),
  glm(formula = XA.bc2 ~ mean_rP + mean_rA + mean_sP + mean_sA + mean_uP + mean_uA +
        var_rP + var_rA + var_sP + var_sA + var_uP + var_uA,
      data = tover),
  glm(formula = Fi.bc2 ~ mean_rP + mean_rA + mean_sP + mean_sA + mean_uP + mean_uA +
        var_rP + var_rA + var_sP + var_sA + var_uP + var_uA,
      data = tover),
  glm.cons(formula = Vis.bc2 ~bet.bc2+Enc.bc2+XP.bc2,
           data = tover,cons = 1,na.action = na.pass),
  glm.cons(formula = H2.c2 ~bet.bc2+Enc.bc2+XP.bc2+XA.bc2+Fi.bc2,
           data = tover,cons = 1,na.action = na.pass),
  glm.cons(formula = mod.c2 ~bet.bc2+Enc.bc2+XP.bc2+XA.bc2+Fi.bc2,
           data = tover,cons = 1,na.action = na.pass),
  glm.cons(formula = nes.c2 ~bet.bc2+Enc.bc2+XP.bc2+XA.bc2+Fi.bc2,
           data = tover,cons = 1,na.action = na.pass),
  tover
)
# Run summary
summary(models.2)

# Address conflict using conserve = T
summary(models.2, conserve = T)
plot(models.2)
model_plot.2<-plot(models.2, return=TRUE)


model_plot.2$edges_df<-model_plot.2$edges_df %>%
  filter(style=="solid") %>%
  filter(abs(as.numeric(label))>0.1) %>%
  mutate(color= ifelse(as.numeric(label)<0,"red",color))
model_plot.2$nodes_df<-model_plot.2$nodes_df %>%
  filter(id %in% c(model_plot.2$edges_df$from,model_plot.2$edges_df$to))

# Render the graph
render_graph(model_plot.2,layout =  "tree")




models.3 <- psem(
  glm(formula = bet.bc3 ~ mean_rP + mean_rA + mean_sP + mean_sA + mean_uP + mean_uA +
        var_rP + var_rA + var_sP + var_sA + var_uP + var_uA,
      data = tover),
  glm(formula = Enc.bc3 ~ mean_rP + mean_rA + mean_sP + mean_sA + mean_uP + mean_uA +
        var_rP + var_rA + var_sP + var_sA + var_uP + var_uA,
      data = tover),
  glm(formula = XP.bc3 ~ mean_rP + mean_rA + mean_sP + mean_sA + mean_uP + mean_uA +
        var_rP + var_rA + var_sP + var_sA + var_uP + var_uA,
      data = tover),
  glm(formula = XA.bc3 ~ mean_rP + mean_rA + mean_sP + mean_sA + mean_uP + mean_uA +
        var_rP + var_rA + var_sP + var_sA + var_uP + var_uA,
      data = tover),
  glm(formula = Fi.bc3 ~ mean_rP + mean_rA + mean_sP + mean_sA + mean_uP + mean_uA +
        var_rP + var_rA + var_sP + var_sA + var_uP + var_uA,
      data = tover),
  glm.cons(formula = Vis.bc3 ~bet.bc3+Enc.bc3+XP.bc3,
           data = tover,cons = 1,na.action = na.pass),
  glm.cons(formula = H2.c3 ~bet.bc3+Enc.bc3+XP.bc3+XA.bc3+Fi.bc3,
           data = tover,cons = 1,na.action = na.pass),
  glm.cons(formula = mod.c3 ~bet.bc3+Enc.bc3+XP.bc3+XA.bc3+Fi.bc3,
           data = tover,cons = 1,na.action = na.pass),
  glm.cons(formula = nes.c3 ~bet.bc3+Enc.bc3+XP.bc3+XA.bc3+Fi.bc3,
           data = tover,cons = 1,na.action = na.pass),
  tover
)
# Run summary
summary(models.3)

# Address conflict using conserve = T
summary(models.3, conserve = T)
plot(models.3)
model_plot.3<-plot(models.3, return=TRUE)


model_plot.3$edges_df<-model_plot.3$edges_df %>%
  filter(style=="solid") %>%
  filter(abs(as.numeric(label))>0.1) %>%
  mutate(color= ifelse(as.numeric(label)<0,"red",color))
model_plot.3$nodes_df<-model_plot.3$nodes_df %>%
  filter(id %in% c(model_plot.3$edges_df$from,model_plot.3$edges_df$to))

# Render the graph
render_graph(model_plot.3,layout =  "tree")



 #####

nodes <- create_nodes(nodes = 1:7, type = "number")

edges <- create_edges(from = c(1, 1, 2, 2, 3, 3),
                      to = c(2, 3, 4, 5, 6, 7),
                      rel = "leading to")

graph <- DiagrammeR::create_graph(nodes_df = model_plot$nodes_df,
                      edges_df = model_plot$edges_df,
                      graph_attrs = "layout = dot",
                      node_attrs = "fontname = Helvetica",
                      edge_attrs = "color = gray20"
                     )

# View the graph
render_graph(graph)




library(DiagrammeR)

# Create a graph using the DOT language with vertical orientation
graph <- "
  digraph tree {
    graph [layout = tree, rankdir = TB]
    node [shape = box]

    A -> B
    A -> C
  }
"

# Render the graph
grViz(graph)



##### demo ####
# Create fake data
set.seed(1)

data <- data.frame(
  x1 = runif(100),
  x2 = runif(100),
  y1 = runif(100),
  y2 = rpois(100, 1),
  y3 = runif(100)
)

# Create SEM using `psem`
modelList <- psem(
  lm(y1 ~ x1+x2, data),
  glm(y2 ~ x1+x2, "poisson", data),
  lm(y3 ~ y1 + y2, data),
  data
)

# Run summary
summary(modelList)

# Address conflict using conserve = T
summary(modelList, conserve = T)

# Address conflict using direction = c()
summary(modelList, direction = c("y2 <- y1"))

# Address conflict using correlated errors
modelList2 <- update(modelList, y2 %~~% y1)

summary(modelList2)

plot(modelList)


glm<-glm(formula = Vis.bc1 ~mean_rP + mean_rA + mean_sP + mean_sA + mean_uP + mean_uA +
           var_rP + var_rA + var_sP + var_sA + var_uP + var_uA,
         data = tover)

summary(glm)

with(summary(glm), 1 - deviance/null.deviance)


glm<-glm(formula = H2.c1 ~mean_rP + mean_rA + mean_sP + mean_sA + mean_uP + mean_uA +
           var_rP + var_rA + var_sP + var_sA + var_uP + var_uA,
         data = tover)

summary(glm)

with(summary(glm), 1 - deviance/null.deviance)



glm<-glm(formula = mod.c1 ~mean_rP + mean_rA + mean_sP + mean_sA + mean_uP + mean_uA +
           var_rP + var_rA + var_sP + var_sA + var_uP + var_uA,
         data = tover)

summary(glm)

with(summary(glm), 1 - deviance/null.deviance)


glm<-glm(formula = nes.c1 ~mean_rP + mean_rA + mean_sP + mean_sA + mean_uP + mean_uA +
           var_rP + var_rA + var_sP + var_sA + var_uP + var_uA,
         data = tover)

summary(glm)

with(summary(glm), 1 - deviance/null.deviance)
