
# -----------------------------------------------------------------------------
# Make table #3 (multiple group test of schema invariance)
# -----------------------------------------------------------------------------

# Create other partitions
classes2 <- graph.adjacency(classes$cormat, 
                            mode="undirected", weighted=T) %>%
  leading.eigenvector.community(., steps = 1) #
table(classes2$membership)

classes3 <- graph.adjacency(classes$cormat, 
                            mode="undirected", weighted=T) %>%
  leading.eigenvector.community(., steps = 2) #
table(classes3$membership)

classes4 <- graph.adjacency(classes$cormat, 
                            mode="undirected", weighted=T) %>%
  leading.eigenvector.community(., steps = 5) #
table(classes4$membership)


cca.group2 <- classes2$membership %>% as.data.frame()
colnames(cca.group2) <- "group2"
cca.group2$rowid <- 1:nrow(cca.group2) %>% as.character()

cca.group3 <- classes3$membership %>% as.data.frame()
colnames(cca.group3) <- "group3"
cca.group3$rowid <- 1:nrow(cca.group3)
cca.group3$rowid <- 1:nrow(cca.group3) %>% as.character()

cca.group4 <- classes4$membership %>% as.data.frame()
colnames(cca.group4) <- "group4"
cca.group4$rowid <- 1:nrow(cca.group4)
cca.group4$rowid <- 1:nrow(cca.group4) %>% as.character()

# Merge cca group labels with the dataset
# cca.group <- classes$membership %>% as.data.frame() 
# cca.group$rowid <- rownames(cca.group) # Get groups and their row IDs
# 
# colnames(cca.group) <- c("group","rowid")
# 
# blog.data$rowid <- rownames(blog.data) #Convert row IDs to a variable
# 
# blog.data <- left_join(blog.data, cca.group, by = c("article_id" = "rowid"))


blog.data <- left_join(blog.data, cca.group2, by = c("article_id" = "rowid")) 
blog.data <- left_join(blog.data, cca.group3, by = c("article_id" = "rowid")) 
blog.data <- left_join(blog.data, cca.group4, by = c("article_id" = "rowid")) 


cmds <- rbind(classes$modules[[1]]$cmds,
              classes$modules[[2]]$cmds,
              classes$modules[[3]]$cmds,
              classes$modules[[4]]$cmds,
              classes$modules[[5]]$cmds) %>%
  add_rownames(var = "article_id")

blog.data <- left_join(blog.data, cmds, by = "article_id")

# Multiple group test of schema invariance
model <- 'white_pole ~~ man_pole
          white_pole ~~ good_pole
          white_pole ~~ influential_pole
          white_pole ~~ young_pole
          man_pole ~~ good_pole
          man_pole ~~ influential_pole
          man_pole ~~ young_pole
          good_pole ~~ influential_pole
          good_pole ~~ young_pole
          influential_pole ~~ young_pole
          '
fit.diff <- cfa(model, data = blog.data, group = "group")
summary(fit.diff)

fit.same <- cfa(model, data = blog.data, group = "group", 
            group.equal = c("residuals","residual.covariances"))
summary(fit.same)

lavTestLRT(fit.same,fit.diff)

# -----------------------------------------------------------------------------
# Make table #4 (multiple group test comparing k-class partitions)
# -----------------------------------------------------------------------------

fit.diff2 <- cfa(model, data = blog.data, group = "group2")
fit.diff3 <- cfa(model, data = blog.data, group = "group3")
fit.diff4 <- cfa(model, data = blog.data, group = "group4")

for (i in list(fit.diff2,fit.diff3,fit.diff4,fit.diff)) {
  
  as.data.frame(fitMeasures(i))[c("aic","bic"),] |>
    print()
  
}
