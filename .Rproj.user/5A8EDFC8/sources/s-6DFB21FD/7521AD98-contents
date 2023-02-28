
# -----------------------------------------------------------------------------
# Make figure #2
# -----------------------------------------------------------------------------

which.select <- c("cmd.white.pole.2", "cmd.good.pole.4", "cmd.man.pole.3",
                  "cmd.honorable.pole.10", "cmd.young.pole.20")

corr.1 <- blog.data %>% filter(group == 1) %>% select(which.select) %>% as.matrix() %>% rcorr(type = "pearson")
corr.2 <- blog.data %>% filter(group == 2) %>% select(which.select) %>% as.matrix() %>% rcorr(type = "pearson")
corr.3 <- blog.data %>% filter(group == 3) %>% select(which.select) %>% as.matrix() %>% rcorr(type = "pearson")
corr.4 <- blog.data %>% filter(group == 4) %>% select(which.select) %>% as.matrix() %>% rcorr(type = "pearson")
corr.5 <- blog.data %>% filter(group == 5) %>% select(which.select) %>% as.matrix() %>% rcorr(type = "pearson")

# create edge.list
my_funk <- function(cor, n, pv =""){ 
  cor[lower.tri(cor, diag = TRUE)] <- NA
  el <- melt(cor) %>% na.omit()   
  colnames( el) <- c("Dim1", "Dim2", paste0("Class", n, pv) )   
  return(el)
}

el.1 <- my_funk(corr.1[[1]], n =1) 
el.2 <- my_funk(corr.2[[1]], n =2)
el.3 <- my_funk(corr.3[[1]], n =3)
el.4 <- my_funk(corr.4[[1]], n =4)
el.5 <- my_funk(corr.5[[1]], n =5)

pv.1 <- my_funk(corr.1[[3]], n =1, pv = "pv")
pv.2 <- my_funk(corr.2[[3]], n =2, pv = "pv") 
pv.3 <- my_funk(corr.3[[3]], n =3, pv = "pv")
pv.4 <- my_funk(corr.4[[3]], n =4, pv = "pv")
pv.5 <- my_funk(corr.5[[3]], n =5, pv = "pv")

pv <- left_join(pv.1, pv.2)
pv <- left_join(pv, pv.3)
pv <- left_join(pv, pv.4)
pv <- left_join(pv, pv.5)

df <- left_join(el.1, el.2)
df <- left_join(df, el.3)
df <- left_join(df, el.4)
df <- left_join(df, el.5)

df <- left_join(df, pv)

df <- df %>% 
  arrange(Dim1, Dim2) %>% 
  mutate(Dim1 = recode(Dim1, 
                       cmd.good.pole.4 = "Character",
                       cmd.man.pole.3 = "Gender",
                       cmd.white.pole.2 = "Race",
                       cmd.young.pole.20 = "Age",
                       cmd.honorable.pole.10 = "Status"),
         Dim2 = recode(Dim2, 
                       cmd.good.pole.4 = "Character",
                       cmd.man.pole.3 = "Gender",
                       cmd.white.pole.2 = "Race",
                       cmd.young.pole.20 = "Age",
                       cmd.honorable.pole.10 = "Status") ) %>% 
  mutate(Diff1 =  abs(Class1),
         Diff2 =  abs(Class2),
         Diff3 =  abs(Class3),
         Diff4 =  abs(Class4),
         Diff5 =  abs(Class5) ) 


# Which Edge has largest absolute difference from zero:
max.temp <- data.frame(Class1.max=NA, Class2.max=NA,  
                       Class3.max=NA, Class4.max=NA ,
                       Class5.max=NA )
for(i in 1:nrow(df) ){
  which <- df[i, ] %>% select(Diff1:Diff5) %>% max()
  max.temp[i, ] <- df[i, paste0("Diff", 1:5) ] == which
}


# Which Edge are above/below zero:
pos.temp <- data.frame(Class1.pos=NA, Class2.pos=NA,  
                       Class3.pos=NA, Class4.pos=NA,
                       Class5.pos=NA )

for(i in 1:nrow(df) ){
  which <- df[i, ] %>% select(Class1:Class5)
  pos.temp[i, ] <- ifelse(which>=0, TRUE, FALSE)
}

# Find the max degree for each node
node.degree <- data.frame(Node = c("Character", "Gender", "Race", "Age", "Status"),
                          Class1.node=NA, Class2.node=NA,  
                          Class3.node=NA, Class4.node=NA,
                          Class5.node=NA )

for(i in 1:5 ){
  node.degree[, i+1] <- get(paste0("corr.",i))[[1]] %>%
    as.matrix() %>%
    abs() %>% rowSums() 
}

for(i in 2:ncol(node.degree)){
  max <- node.degree[, i] %>% max()
  node.degree[, i] <- node.degree[ , i] == max
}

# Which Edge have P > 0.5:
sig.temp <- data.frame(Class1.sig=NA, Class2.sig=NA,  
                       Class3.sig=NA, Class4.sig=NA,
                       Class5.sig=NA )

for(i in 1:nrow(df) ){
  sigs <- df[i, ] %>% select(Class1pv:Class5pv)
  mins <- df[i, ] %>% select(Class1:Class5) %>% abs()
  sig.temp[i, ] <- ifelse(sigs<=0.05, ifelse(mins >= 0.15, TRUE, FALSE), FALSE)
}

# -----------------------------------------------------------------------------
# Create Stacked Dyad Plots
# -----------------------------------------------------------------------------

edges <- el.1 %>% 

    select(Dim1, Dim2) %>% 
  arrange(Dim1, Dim2) %>% 
  mutate(Dim1 = recode(Dim1, 
                       cmd.good.pole.4 = "Character",
                       cmd.man.pole.3 = "Gender",
                       cmd.white.pole.2 = "Race",
                       cmd.young.pole.20 = "Age",
                       cmd.honorable.pole.10 = "Status"),
         Dim2 = recode(Dim2, 
                       cmd.good.pole.4 = "Character",
                       cmd.man.pole.3 = "Gender",
                       cmd.white.pole.2 = "Race",
                       cmd.young.pole.20 = "Age",
                       cmd.honorable.pole.10 = "Status") ) %>% 
  cbind(max.temp, pos.temp, sig.temp)

plot_funk <- function(x, max.row, pos.row, sig.row, hi.node) {
  
  cols = ifelse( edges[i, max.row]==TRUE, "#000000", "#696969")
  cols = ifelse( edges[i, sig.row]==TRUE, cols, "#ffffff")
  wide = ifelse( edges[i, max.row]==TRUE, 2, .6)
  line = ifelse( edges[i, pos.row]==TRUE, 1, 5)
  expa <- expansion(c(.20, .20))
  
  p.temp <- x %>%
    graph_from_data_frame(directed=FALSE) %>%
    tidygraph::as_tbl_graph() %>%
    tidygraph::activate(nodes) %>% 
    mutate(node.color = case_when(name == hi.node ~ TRUE, TRUE ~ FALSE)) %>%
    ggraph(layout = "grid") + 
    # geom_edge_link(width = 1, edge_colour = edge.color)
    geom_edge_link(aes(start_cap = circle(1, 'mm'), end_cap = circle(1, 'mm')), 
                   edge_colour = cols, edge_linetype = line, width=wide ) +
    geom_node_point(aes(color = node.color), size = 16, ) +
    geom_node_label(aes(label = name)) + theme() +
    scale_x_continuous(expand = expa) +
    scale_y_continuous(expand = expa) +
    # scale_edge_color_manual(values = c("#000000", "#696969"))+
    scale_color_manual(values = c("#d2d2d2", "#000000")) +
    theme(legend.position = 'none')
  
  return(p.temp)
}

set_graph_style(plot_margin = margin(1,1,1,1), text_size = 9)

p.list.1 <- list()
for(i in 1:nrow(edges) ){
  p.list.1[[i]] <- plot_funk(x=edges[i,1:2], max.row=3, pos.row=8, sig.row=13, hi.node="Gender")
}

p.list.2 <- list()
for(i in 1:nrow(edges) ){
  x = edges[i,1:2]
  p.list.2[[i]] <- plot_funk(x=edges[i,1:2], max.row=4, pos.row=9, sig.row=14, hi.node="Gender")
}

p.list.3 <- list()
for(i in 1:nrow(edges) ){
  x = edges[i,1:2]
  p.list.3[[i]] <- plot_funk(x=edges[i,1:2], max.row=5, pos.row=10, sig.row=15, hi.node="Status")
}

p.list.4 <- list()
for(i in 1:nrow(edges) ){
  x = edges[i,1:2]
  p.list.4[[i]] <- plot_funk(x=edges[i,1:2], max.row=6, pos.row=11, sig.row=16, hi.node="Race")
}

p.list.5 <- list()
for(i in 1:nrow(edges) ){
  x = edges[i,1:2]
  p.list.5[[i]] <- plot_funk(x=edges[i,1:2], max.row=7, pos.row=12, sig.row=17, hi.node="Race")
}


fig.1 <- ggarrange(plotlist=p.list.1, ncol=1, nrow=10)
fig.1 <- annotate_figure(fig.1, top = text_grob("Class #1") )

fig.2 <- ggarrange(plotlist=p.list.2, ncol=1, nrow=10)
fig.2 <- annotate_figure(fig.2, top = text_grob("Class #2") )

fig.3 <- ggarrange(plotlist=p.list.3, ncol=1, nrow=10)
fig.3 <- annotate_figure(fig.3, top = text_grob("Class #3") )

fig.4 <- ggarrange(plotlist=p.list.4, ncol=1, nrow=10)
fig.4 <- annotate_figure(fig.4, top = text_grob("Class #4") )

fig.5 <- ggarrange(plotlist=p.list.5, ncol=1, nrow=10)
fig.5 <- annotate_figure(fig.5, top = text_grob("Class #5") )


png("figures/fig2.png", width = 12, height = 6, units = 'in', res = 500)

ggarrange(fig.1, fig.2, fig.3, fig.4, fig.5, ncol=5, nrow=1)

dev.off()