
# -----------------------------------------------------------------------------
# Find represantative texts
# -----------------------------------------------------------------------------

# Below are the blog posts from Class #2 that were particularly high on the 
# Gender dimension (i.e. engaging the ``masculine'' pole of the dimension and then
# the ``feminine'' pole of the dimension).
corr.data2 %>%
  arrange(man_pole) %>% 
  slice_max(man_pole, n = 7) %>% 
  select(documents, blog)
corr.data2 %>%
  arrange(man_pole) %>% 
  slice_min(man_pole, n = 7) %>% 
  select(documents, blog)


# here is a blog post from Class #5 that is high on the Race dimension
corr.data5 %>% filter(group == 5) %>% 
  arrange(white_pole) %>% 
  slice_min(white_pole, n = 9) %>% 
  select(documents, blog)

