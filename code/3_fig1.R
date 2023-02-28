
# -----------------------------------------------------------------------------
# Get corr and p-value matrices
# -----------------------------------------------------------------------------

vars <- c("cmd.good.pole.4","cmd.man.pole.3","cmd.white.pole.2",
          "cmd.young.pole.20","cmd.honorable.pole.10")

corr.data1 <- blog.data[which(blog.data$group=="1"),] #Subsets
corr.data2 <- blog.data[which(blog.data$group=="2"),]
corr.data3 <- blog.data[which(blog.data$group=="3"),]
corr.data4 <- blog.data[which(blog.data$group=="4"),]
corr.data5 <- blog.data[which(blog.data$group=="5"),]

corr1 <- round(cor(corr.data1[vars]), 2) #Get bivariate corrs
corr2 <- round(cor(corr.data2[vars]), 2)
corr3 <- round(cor(corr.data3[vars]), 2)
corr4 <- round(cor(corr.data4[vars]), 2)
corr5 <- round(cor(corr.data5[vars]), 2)

pmat1 <- cor_pmat(corr.data1[vars]) #Get corr p-values
pmat2 <- cor_pmat(corr.data2[vars])
pmat3 <- cor_pmat(corr.data3[vars])
pmat4 <- cor_pmat(corr.data4[vars])
pmat5 <- cor_pmat(corr.data5[vars])

cor1 <- classes$modules[[1]]$cormat
cor2 <- classes$modules[[2]]$cormat
cor3 <- classes$modules[[3]]$cormat
cor4 <- classes$modules[[4]]$cormat
cor5 <- classes$modules[[5]]$cormat

# -----------------------------------------------------------------------------
# Get corr networks
# -----------------------------------------------------------------------------

n1 <- qgraph(cor1,
       graph = "cor",
       minimum=.15, maximum=.88, threshold="sig", sampleSize=2395,
       #Not plotting corrs < |.15|. Max non-diagonal abs(corr) = .813.
       layout = "spring", repulsion = 1.86, label.cex=2,
       posCol="black", negCol="black", negDashed=T,
       borders=T, shape = "circle", label.prop = 0.75,
       curveAll=F, edge.labels=F, edge.label.cex = 0.45, esize = 8,
       title="Class #1", labels=c("character","gender","race",
                                  "age","status"))
n2 <- qgraph(cor2,
       graph = "cor",
       minimum=.15, maximum=.88, threshold="sig", sampleSize=4650,
       #Not plotting corrs < |.15|. Max non-diagonal abs(corr) = .813.
       layout = "spring", repulsion = 1.86, label.cex=2,
       posCol="black", negCol="black", negDashed=T,
       borders=T, shape = "circle", label.prop = 0.75,
       curveAll=F, edge.labels=F, edge.label.cex = 0.45, esize = 8,
       title="Class #2", labels=c("character","gender","race",
                                  "age","status"))

n3 <- qgraph(cor3,
       graph = "cor",
       minimum=.15, maximum=.88, threshold="sig", sampleSize=1859,
       #Not plotting corrs < |.15|. Max non-diagonal abs(corr) = .813.
       layout = "spring", repulsion = 1.86, label.cex=2,
       posCol="black", negCol="black", negDashed=T,
       borders=T, shape = "circle", label.prop = 0.75,
       curveAll=F, edge.labels=F, edge.label.cex = 0.45, esize = 8,
       title="Class #3", labels=c("character","gender","race",
                                  "age","status"))

n4 <- qgraph(cor4,
       graph = "cor",
       minimum=.15, maximum=.88, threshold="sig", sampleSize=2489,
       #Not plotting corrs < |.15|. Max non-diagonal abs(corr) = .813.
       layout = "spring", repulsion = 1.86, label.cex=2,
       posCol="black", negCol="black", negDashed=T,
       borders=T, shape = "circle", label.prop = 0.75,
       curveAll=F, edge.labels=F, edge.label.cex = 0.45, esize = 8,
       title="Class #4", labels=c("character","gender","race",
                                  "age","status"))

n5 <- qgraph(cor5,
       graph = "cor",
       minimum=.15, maximum=.88, threshold="sig", sampleSize=1853,
       #Not plotting corrs < |.15|. Max non-diagonal abs(corr) = .813.
       layout = "spring", repulsion = 1.86, label.cex=2,
       posCol="black", negCol="black", negDashed=T,
       borders=T, shape = "circle", label.prop = 0.75,
       curveAll=F, edge.labels=F, edge.label.cex = 0.45, esize = 8,
       title="Class #5", labels=c("character","gender","race",
                                  "age","status"))

# -----------------------------------------------------------------------------
# Get corr heatmaps
# -----------------------------------------------------------------------------

p1 <- ggcorrplot(corr1, hc.order = F, type = "lower",
           lab = TRUE, p.mat=as.matrix(pmat1[,-1]), sig.level=0.05,
           colors=c("black","white","black")) + 
  scale_y_discrete(limits = unique(colnames(corr1)),
                   labels=c("character","gender","race","age","status")) +
  scale_x_discrete(limits = unique(rownames(corr1)),
                   labels=c("character","gender","race","age","status"))

p2 <- ggcorrplot(corr2, hc.order = F, type = "lower",
                 lab = TRUE, p.mat=as.matrix(pmat2[,-1]), sig.level=0.05,
                 colors=c("black","white","black")) + 
  scale_y_discrete(limits = unique(colnames(corr1)),
                   labels=c("character","gender","race","age","status")) +
  scale_x_discrete(limits = unique(rownames(corr1)),
                   labels=c("character","gender","race","age","status"))

p3 <- ggcorrplot(corr3, hc.order = F, type = "lower",
                 lab = TRUE, p.mat=as.matrix(pmat3[,-1]), sig.level=0.05,
                 colors=c("black","white","black")) + 
  scale_y_discrete(limits = unique(colnames(corr1)),
                   labels=c("character","gender","race","age","status")) +
  scale_x_discrete(limits = unique(rownames(corr1)),
                   labels=c("character","gender","race","age","status"))

p4 <- ggcorrplot(corr4, hc.order = F, type = "lower",
                 lab = TRUE, p.mat=as.matrix(pmat4[,-1]), sig.level=0.05,
                 colors=c("black","white","black")) + 
  scale_y_discrete(limits = unique(colnames(corr1)),
                   labels=c("character","gender","race","age","status")) +
  scale_x_discrete(limits = unique(rownames(corr1)),
                   labels=c("character","gender","race","age","status"))

p5 <- ggcorrplot(corr5, hc.order = F, type = "lower",
                 lab = TRUE, p.mat=as.matrix(pmat5[,-1]), sig.level=0.05,
                 colors=c("black","white","black")) + 
  scale_y_discrete(limits = unique(colnames(corr1)),
                   labels=c("character","gender","race","age","status")) +
  scale_x_discrete(limits = unique(rownames(corr1)),
                   labels=c("character","gender","race","age","status"))

# -----------------------------------------------------------------------------
# Put the plot together
# -----------------------------------------------------------------------------

png("figures/fig1_a.png", width = 8, height = 14, units = 'in', res = 750)
par(mfrow=c(5,1))
plot(n1)
plot(n2)
plot(n3)
plot(n4)
plot(n5)
par(mfrow=c(1,1))
dev.off()

png("figures/fig1_b.png", width = 8, height = 14, units = 'in', res = 750)
ggarrange(p1,p2,p3,p4,p5, align = "h", ncol = 1, nrow = 5)
dev.off()


