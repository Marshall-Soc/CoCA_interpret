
# Note: You can skip this script, as the results are already in the dataframe.
        # It will take a while to run

# -----------------------------------------------------------------------------
# Get tags
# -----------------------------------------------------------------------------

df.people.full <- person_entity(blog.data$documents)
df.temp.full <- df.people.full

# -----------------------------------------------------------------------------
# Some cleaning
# -----------------------------------------------------------------------------

# Replace Curly Apostrophes
for(j in 1:length(bad.names) ){
  df.temp.full <-  lapply(df.temp.full, function(x) str_replace_all(x, "’", "'") )
}

# Replace Periods
for(j in 1:length(bad.names) ){
  df.temp.full <-  lapply(df.temp.full, function(x) str_replace_all(x, fixed("."), "") )
}

# Replace Apostrophes ESS
for(j in 1:length(bad.names) ){
  df.temp.full <-  lapply(df.temp.full, function(x) str_replace_all(x, fixed("'s"), "") )
}

# Replace "
for(j in 1:length(bad.names) ){
  df.temp.full <-  lapply(df.temp.full, function(x) str_replace_all(x, fixed(' "'), '') )
}

# Replace ”
for(j in 1:length(bad.names) ){
  df.temp.full <-  lapply(df.temp.full, function(x) str_replace_all(x, fixed('”'), '') )
}

# Replace (
for(j in 1:length(bad.names) ){
  df.temp.full <-  lapply(df.temp.full, function(x) str_replace_all(x, fixed('('), '') )
}

# Replace )
for(j in 1:length(bad.names) ){
  df.temp.full <-  lapply(df.temp.full, function(x) str_replace_all(x, fixed(')'), '') )
}

# Replace double space with one
for(j in 1:length(bad.names) ){
  df.temp.full <-  lapply(df.temp.full, function(x) str_replace_all(x, "\\s+", " ") )
}


for(i in 1:nrow(bad.names) ){
  rmv <- paste0("^\\b", bad.names$bad[i],"\\b$" )
  print(rmv)
  df.temp.full <- lapply(df.temp.full, function(x) str_remove_all(x, rmv) )
}

for(j in 1:nrow(cb) ){
  
  pattern <- cb[j, 2, drop = TRUE]
  replace <- cb[j, 1, drop = TRUE]
  pat <- paste0("^\\b", pattern,"\\b$" )
  df.temp.full <- lapply(df.temp.full, function(x) str_replace_all(x, pat, replace) )
  
}

# Removes duplicate names *within* each list and blanks from cleaning
df.temp.full <- lapply(df.temp.full, setdiff, "")

# Replace double space with one
# [[13142]]
# character(0)
df.temp.full <- lapply(df.temp.full, function(x) if(identical(x, character(0))) NA_character_ else x)

# -----------------------------------------------------------------------------
# Create NER variables
# -----------------------------------------------------------------------------

df.temp.full <- df.temp.full %>% 
  melt() %>% 
  as_tibble() %>% 
  rename(named_entity = value, article_id = L1)  %>%
  mutate(article_id = as.character(article_id) ) %>%
  mutate_if(grepl('Obama',.), ~replace(., grepl('Obama', .), "Barack Obama") ) %>%
  mutate_if(grepl('McCain',.), ~replace(., grepl('McCain', .), "John McCain") ) %>% 
  mutate_if(grepl('Hillary',.), ~replace(., grepl('Hillary', .), "Hillary Clinton") ) %>% 
  group_by(article_id) %>% 
  add_count(name = "n_ppl_article") %>%
  mutate(all_name = paste0(named_entity, collapse = ";") )

blog.data <- blog.data %>% 
  tibble::rownames_to_column(var = "article_id") %>% 
  left_join(df.temp.full)

blog.data <- blog.data %>%
  mutate(mccain = ifelse(str_detect(all_name, "McCain"), TRUE, FALSE) ) %>%
  mutate(obama = ifelse(str_detect(all_name, "Obama"), TRUE, FALSE) ) %>%
  mutate(biden = ifelse(str_detect(all_name, "Biden"), TRUE, FALSE) ) %>%
  mutate(palin = ifelse(str_detect(all_name, "Palin"), TRUE, FALSE ) ) %>%
  mutate(clinton = ifelse(str_detect(all_name, "Hillary"), TRUE, FALSE) ) %>%
  mutate(bush = ifelse(str_detect(all_name, "Bush"), TRUE, FALSE) ) %>%
  mutate(named_entity = case_when(named_entity == "John"    & mccain==TRUE ~ "John McCain",  TRUE ~ named_entity)) %>%
  mutate(named_entity = case_when(named_entity == "Barry"   & obama==TRUE ~ "Barack Obama",  TRUE ~ named_entity)) %>%
  mutate(named_entity = case_when(named_entity == "Joe"     & biden==TRUE ~ "Joe Biden",  TRUE ~ named_entity)) %>% 
  mutate(named_entity = case_when(named_entity == "Sarah"   & palin==TRUE ~ "Sarah Palin",  TRUE ~ named_entity)) %>% 
  mutate(named_entity = case_when(named_entity == "Hillary" & clinton==TRUE ~ "Hillary Clinton",  TRUE ~ named_entity)) 