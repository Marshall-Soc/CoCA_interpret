
# -----------------------------------------------------------------------------
# Create semantic directions
# -----------------------------------------------------------------------------

# Merge CoCA group labels with the dataset
cca.group <- classes$membership %>% as.data.frame() 
cca.group$rowid <- rownames(cca.group) # Get groups and their row IDs

colnames(cca.group) <- c("group","rowid")

blog.data$rowid <- rownames(blog.data) #Convert row IDs to a variable

blog.data <- left_join(blog.data, cca.group, by = "rowid") #Merge

#Some mlogit models
model <- multinom(group ~ day*rating + n_ppl_article + rating*mccain +
                    rating*biden + rating*bush + rating*obama + rating*palin + 
                    rating*clinton, data = blog.data)

blog.data$female <- 0
blog.data[which(blog.data$clinton==1 | blog.data$palin==1),]$female <- 1
blog.data$female <- as.factor(blog.data$female)

model2 <- multinom(group ~ n_ppl_article + rating + female + day,
                   data = blog.data)


