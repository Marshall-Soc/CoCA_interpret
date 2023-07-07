
# -----------------------------------------------------------------------------
# Make table #1
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

corr.bin1 <- corr1
corr.bin2 <- corr2
corr.bin3 <- corr3
corr.bin4 <- corr4
corr.bin5 <- corr5

corr.bin1[which(p.mat1 >= .05)]  <- NA
corr.bin2[which(p.mat2 >= .05)]  <- NA
corr.bin3[which(p.mat3 >= .05)]  <- NA
corr.bin4[which(p.mat4 >= .05)]  <- NA
corr.bin5[which(p.mat5 >= .05)]  <- NA

corr.bin1 <- (corr.bin1 < 0) + 0
corr.bin2 <- (corr.bin2 < 0) + 0
corr.bin3 <- (corr.bin3 < 0) + 0
corr.bin4 <- (corr.bin4 < 0) + 0
corr.bin5 <- (corr.bin5 < 0) + 0

diag(corr.bin1) <- NA
diag(corr.bin2) <- NA
diag(corr.bin3) <- NA
diag(corr.bin4) <- NA
diag(corr.bin5) <- NA

table1 <- table(corr.bin1)
table2 <- table(corr.bin2)
table3 <- table(corr.bin3)
table4 <- table(corr.bin4)
table5 <- table(corr.bin5)


df <- data.frame(matrix(ncol = 4, nrow = 5))
colnames(df) <- c("N_negative", "Total", "Prop_Negative", "Sig")
rownames(df) <- c("Class #1","Class #2","Class #3","Class #4","Class #5")

df[1,1] <- table1[[2]]
df[2,1] <- table2[[2]]
df[3,1] <- table3[[2]]
df[4,1] <- table4[[2]]
df[5,1] <- table5[[2]]

df[1,2] <- (table1[[1]] + table1[[2]])
df[2,2] <- (table2[[1]] + table2[[2]])
df[3,2] <- (table3[[1]] + table3[[2]])
df[4,2] <- (table4[[1]] + table4[[2]])
df[5,2] <- (table5[[1]] + table5[[2]])

df[1,3] <- table1[[2]]/(table1[[1]] + table1[[2]])
df[2,3] <- table2[[2]]/(table2[[1]] + table2[[2]])
df[3,3] <- table3[[2]]/(table3[[1]] + table3[[2]])
df[4,3] <- table4[[2]]/(table4[[1]] + table4[[2]])
df[5,3] <- table5[[2]]/(table5[[1]] + table5[[2]])

  #Bootstrap test
n_boot <- 1000

df_boot <- data.frame(matrix(nrow = n_boot, ncol = nrow(df)))

for (i in 1:n_boot) {
  
  for (k in 1:nrow(df)) {
    
  num_negative <- sample(0:1, size = df[k,2], replace = T)
  
  df_boot[i,k] <- table(num_negative)[[2]]
  
  }
  
}

df[1,4] <- sum(df_boot[,1] >= table1[[2]])/df[1,2]
df[2,4] <- sum(df_boot[,2] >= table2[[2]])/df[2,2]
df[3,4] <- sum(df_boot[,3] >= table3[[2]])/df[3,2]
df[4,4] <- sum(df_boot[,4] >= table4[[2]])/df[4,2]
df[5,4] <- sum(df_boot[,5] >= table5[[2]])/df[5,2]

print(df)
