

# -----------------------------------------------------------------------------
# Make figure #3
# -----------------------------------------------------------------------------

corr1 <- rcorr(t(classes$modules[[1]]$cmds), type="pearson")$r
corr2 <- rcorr(t(classes$modules[[2]]$cmds), type="pearson")$r
corr3 <- rcorr(t(classes$modules[[3]]$cmds), type="pearson")$r
corr4 <- rcorr(t(classes$modules[[4]]$cmds), type="pearson")$r
corr5 <- rcorr(t(classes$modules[[5]]$cmds), type="pearson")$r

p.mat1 <- rcorr(t(classes$modules[[1]]$cmds), type="pearson")$P
p.mat2 <- rcorr(t(classes$modules[[2]]$cmds), type="pearson")$P
p.mat3 <- rcorr(t(classes$modules[[3]]$cmds), type="pearson")$P
p.mat4 <- rcorr(t(classes$modules[[4]]$cmds), type="pearson")$P
p.mat5 <- rcorr(t(classes$modules[[5]]$cmds), type="pearson")$P

png("figures/fig3_a.png", width = 11, height = 8.5, 
    units = 'in', res = 350)
corrplot(corr1, order="hclust", type="lower", hclust.method = "average",
         tl.pos="n", addgrid=NA, p.mat = p.mat1, sig.level = .05, insig = "blank",
         col=brewer.pal(n=11, name="PuOr"), title="Class #1",
         mar=c(0,0,2,0))
dev.off()

png("figures/fig3_b.png", width = 11, height = 8.5, 
    units = 'in', res = 350)
corrplot(corr2, order="hclust", type="lower", hclust.method = "average",
         tl.pos="n", addgrid=NA, p.mat = p.mat2, sig.level = .05, insig = "blank",
         col=brewer.pal(n=11, name="PuOr"), title="Class #2",
         mar=c(0,0,2,0))
dev.off()

png("figures/fig3_c.png", width = 11, height = 8.5, 
    units = 'in', res = 350)
corrplot(corr3, order="hclust", type="lower", hclust.method = "average",
         tl.pos="n", addgrid=NA, p.mat = p.mat3, sig.level = .05, insig = "blank",
         col=brewer.pal(n=11, name="PuOr"), title="Class #3",
         mar=c(0,0,2,0))
dev.off()

png("figures/fig3_d.png", width = 11, height = 8.5, 
    units = 'in', res = 350)
corrplot(corr4, order="hclust", type="lower", hclust.method = "average",
         tl.pos="n", addgrid=NA, p.mat = p.mat4, sig.level = .05, insig = "blank",
         col=brewer.pal(n=11, name="PuOr"), title="Class #4",
         mar=c(0,0,2,0))
dev.off()

png("figures/fig3_e.png", width = 11, height = 8.5, 
    units = 'in', res = 350)
corrplot(corr5, order="hclust", type="lower", hclust.method = "average",
         tl.pos="n", addgrid=NA, p.mat = p.mat5, sig.level = .05, insig = "blank",
         col=brewer.pal(n=11, name="PuOr"), title="Class #5",
         mar=c(0,0,2,0))
dev.off()

prop.test(table1[[2]], (table1[[1]] + table1[[2]]), p = 0.2,
          alternative = "greater", correct = F) #reject null

prop.test(table2[[2]], (table2[[1]] + table2[[2]]), p = 0.2,
          alternative = "greater", correct = F) #reject null

prop.test(table3[[2]], (table3[[1]] + table3[[2]]), p = 0.2,
          alternative = "greater", correct = F) #reject null

prop.test(table4[[2]], (table4[[1]] + table4[[2]]), p = 0.2,
          alternative = "greater", correct = F) #reject null

prop.test(table5[[2]], (table5[[1]] + table5[[2]]), p = 0.2,
          alternative = "greater", correct = F) #reject null