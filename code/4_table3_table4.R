
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

classes5 <- graph.adjacency(classes$cormat, 
                            mode="undirected", weighted=T) %>%
  leading.eigenvector.community(., steps = 6) #
table(classes5$membership)

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


blog.data <- left_join(blog.data, cca.group2, by = "rowid") 
blog.data <- left_join(blog.data, cca.group3, by = "rowid") 
blog.data <- left_join(blog.data, cca.group4, by = "rowid") 

model <- 'dim =~ cmd.liberal.pole.1 + cmd.white.pole.2 + cmd.man.pole.3 + cmd.good.pole.4 + cmd.honorable.pole.10'

fit1 <- cfa(model, data = blog.data, group = "group")
fit2 <- cfa(model, data = blog.data, group = "group", group.equal = c("intercepts","loadings"))

lavTestLRT(fit1, fit2)


# -----------------------------------------------------------------------------
# Make table #3 (multiple group test comparing k-class partitions)
# -----------------------------------------------------------------------------