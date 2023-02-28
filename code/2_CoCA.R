
# -----------------------------------------------------------------------------
# Create semantic directions
# -----------------------------------------------------------------------------

dim.list <- c("racial", "gender", "morality", "status", "age")

# initialize mat for multiple sem directions
sem.direct <- matrix(ncol = ncol(ft.wv) )

for(i in dim.list){
  
  sd.temp <- dims %>% 
    filter(dimension==i & 
             in_embeddings==TRUE) %>%
    select(add, subtract) %>%
    get_direction(., ft.wv)
  
  sem.direct <- rbind(sem.direct, sd.temp)
  # nrow(sem.direct)
  print(i)
  
}

# remove first row NAs
sem.direct <- sem.direct[-1, , drop = FALSE]
dim(sem.direct) # 20 by 300

# -----------------------------------------------------------------------------
# CoCA
# -----------------------------------------------------------------------------

classes <- CoCA(dtm = blog.dtm,
                wv = ft.wv,
                directions = sem.direct,
                filter_value = 0.05,
                filter_sig = T,
                zero_action = "drop")
