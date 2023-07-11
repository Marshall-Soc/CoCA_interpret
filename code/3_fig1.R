
# -----------------------------------------------------------------------------
# Get corr and p-value matrices
# -----------------------------------------------------------------------------

vars <- c("white_pole","man_pole","good_pole",
          "influential_pole","young_pole")

corr.data1 <- blog.data[which(blog.data$group=="1"),] #Subsets
corr.data2 <- blog.data[which(blog.data$group=="2"),]
corr.data3 <- blog.data[which(blog.data$group=="3"),]
corr.data4 <- blog.data[which(blog.data$group=="4"),]
corr.data5 <- blog.data[which(blog.data$group=="5"),]

corr.data1 <- cbind(corr.data1, classes$modules[[1]]$cmds)
corr.data2 <- cbind(corr.data2, classes$modules[[2]]$cmds)
corr.data3 <- cbind(corr.data3, classes$modules[[3]]$cmds)
corr.data4 <- cbind(corr.data4, classes$modules[[4]]$cmds)
corr.data5 <- cbind(corr.data5, classes$modules[[5]]$cmds)

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

pmat1 <- pmat1[vars,vars]
pmat2 <- pmat2[vars,vars]
pmat3 <- pmat3[vars,vars]
pmat4 <- pmat4[vars,vars]
pmat5 <- pmat5[vars,vars]

cor1 <- classes$modules[[1]]$cormat
cor2 <- classes$modules[[2]]$cormat
cor3 <- classes$modules[[3]]$cormat
cor4 <- classes$modules[[4]]$cormat
cor5 <- classes$modules[[5]]$cormat

# -----------------------------------------------------------------------------
# Get corr networks
# -----------------------------------------------------------------------------

n1 <- qgraph::qgraph(cor1,
       graph = "cor",
       minimum=.15, maximum=.88, threshold="sig", sampleSize=2395,
       #Not plotting corrs < |.15|. Max non-diagonal abs(corr) = .813.
       layout = "spring", repulsion = 1.86, label.cex=2,
       posCol="black", negCol="black", negDashed=T,
       borders=T, shape = "circle", label.prop = 0.75,
       curveAll=F, edge.labels=F, edge.label.cex = 0.45, esize = 8,
       title="", labels=c("race","gender","character",
                                  "status","age"))
n2 <- qgraph::qgraph(cor2,
       graph = "cor",
       minimum=.15, maximum=.88, threshold="sig", sampleSize=4650,
       #Not plotting corrs < |.15|. Max non-diagonal abs(corr) = .813.
       layout = "spring", repulsion = 1.86, label.cex=2,
       posCol="black", negCol="black", negDashed=T,
       borders=T, shape = "circle", label.prop = 0.75,
       curveAll=F, edge.labels=F, edge.label.cex = 0.45, esize = 8,
       title="", labels=c("race","gender","character",
                                  "status","age"))

n3 <- qgraph::qgraph(cor3,
       graph = "cor",
       minimum=.15, maximum=.88, threshold="sig", sampleSize=1859,
       #Not plotting corrs < |.15|. Max non-diagonal abs(corr) = .813.
       layout = "spring", repulsion = 1.86, label.cex=2,
       posCol="black", negCol="black", negDashed=T,
       borders=T, shape = "circle", label.prop = 0.75,
       curveAll=F, edge.labels=F, edge.label.cex = 0.45, esize = 8,
       title="", labels=c("race","gender","character",
                                  "status","age"))

n4 <- qgraph::qgraph(cor4,
       graph = "cor",
       minimum=.15, maximum=.88, threshold="sig", sampleSize=2489,
       #Not plotting corrs < |.15|. Max non-diagonal abs(corr) = .813.
       layout = "spring", repulsion = 1.86, label.cex=2,
       posCol="black", negCol="black", negDashed=T,
       borders=T, shape = "circle", label.prop = 0.75,
       curveAll=F, edge.labels=F, edge.label.cex = 0.45, esize = 8,
       title="", labels=c("race","gender","character",
                                  "status","age"))

n5 <- qgraph::qgraph(cor5,
       graph = "cor",
       minimum=.15, maximum=.88, threshold="sig", sampleSize=1853,
       #Not plotting corrs < |.15|. Max non-diagonal abs(corr) = .813.
       layout = "spring", repulsion = 1.86, label.cex=2,
       posCol="black", negCol="black", negDashed=T,
       borders=T, shape = "circle", label.prop = 0.75,
       curveAll=F, edge.labels=F, edge.label.cex = 0.45, esize = 8,
       title="", labels=c("race","gender","character",
                                  "status","age"))

# -----------------------------------------------------------------------------
# Get corr heatmaps
# -----------------------------------------------------------------------------

p1 <- ggcorrplot(corr1, hc.order = F, type = "lower",
           lab = TRUE, p.mat=pmat1, sig.level=0.05,
           colors=c("black","white","black"),
           lab_col=c(rep("white",10)), lab_size=8, tl.cex=16, pch.cex=14) + 
  scale_y_discrete(limits = unique(colnames(corr1)),
                   labels=c("race","gender","character",
                            "status","age")) +
  scale_x_discrete(limits = unique(rownames(corr1)),
                   labels=c("race","gender","character",
                            "status","age"))

p2 <- ggcorrplot(corr2, hc.order = F, type = "lower",
                 lab = TRUE, p.mat=pmat2, sig.level=0.05,
                 colors=c("black","white","black"),
                 lab_col=c(rep("white",7),rep("white",2),"black"), lab_size=8,
                tl.cex=16, pch.cex=14) +  
  scale_y_discrete(limits = unique(colnames(corr1)),
                   labels=c("race","gender","character",
                            "status","age")) +
  scale_x_discrete(limits = unique(rownames(corr1)),
                   labels=c("race","gender","character",
                            "status","age"))

p3 <- ggcorrplot(corr3, hc.order = F, type = "lower",
                 lab = TRUE, p.mat=pmat3, sig.level=0.05,
                 colors=c("black","white","black"),
                 lab_col=c("white","black",rep("white",2),"black",rep("white",2),rep("black",2),"white"), 
                 lab_size=8, tl.cex=16, pch.cex=14) + 
  scale_y_discrete(limits = unique(colnames(corr1)),
                   labels=c("race","gender","character",
                            "status","age")) +
  scale_x_discrete(limits = unique(rownames(corr1)),
                   labels=c("race","gender","character",
                            "status","age"))

p4 <- ggcorrplot(corr4, hc.order = F, type = "lower",
                 lab = TRUE, p.mat=pmat4, sig.level=0.05,
                 colors=c("black","white","black"),
                 lab_col=c("white",rep("black",2),rep("white",2),rep("black",5)), lab_size=8, 
                 tl.cex=1, pch.cex=14) +  
  scale_y_discrete(limits = unique(colnames(corr1)),
                   labels=c("race","gender","character",
                            "status","age")) +
  scale_x_discrete(limits = unique(rownames(corr1)),
                   labels=c("race","gender","character",
                            "status","age"))

p5 <- ggcorrplot(corr5, hc.order = F, type = "lower",
                 lab = TRUE, p.mat=pmat5, sig.level=0.05,
                 colors=c("black","white","black"),
                 lab_col=c("black",rep("white",3),rep("black",3),rep("white",3)), lab_size=8, 
                 tl.cex=16, pch.cex=14) + 
  scale_y_discrete(limits = unique(colnames(corr1)),
                   labels=c("race","gender","character",
                            "status","age")) +
  scale_x_discrete(limits = unique(rownames(corr1)),
                   labels=c("race","gender","character",
                            "status","age"))

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


