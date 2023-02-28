
# -----------------------------------------------------------------------------
# PACKAGES
# -----------------------------------------------------------------------------

    pacman::p_load(text2map, readtext, corrplot,
                googlesheets4, dplyr, textclean, RColorBrewer,
                tm, qdapDictionaries, igraph, qgraph,
                stringr, tidyr, tidytext, lavaan, Hmisc,
                tm, nnet, entity, gmodels, rstatix, ggraph,
                effects, reshape2, margins, ggcorrplot,
                ggpubr, lsmeans, install = TRUE)

# -----------------------------------------------------------------------------
# DATA
# -----------------------------------------------------------------------------

    # Juxtaposition pairs
    dims <- readxl::read_excel("data/semantic_directions.xlsx", 
                                        sheet = "cd")

    # Blogs
    # blog.data <- readtext("https://uclspp.github.io/datasets/data/poliblogs2008.zip")
    blog.data   <- readRDS("data/blog_data.rds") # This contains all of the variables

    # Bad names
    bad.names <- readxl::read_excel("data/ner_cleaning.xlsx",
                               sheet = "bad")
    
    # To replace names
    cb <- readxl::read_excel("data/ner_cleaning.xlsx",
                                    sheet = "cb")
    
    # Embeddings
    ft.wv <- readRDS("data/ft.cc.en.300D.2M.Rds")

# -----------------------------------------------------------------------------
# PREP text and DTM
# -----------------------------------------------------------------------------

    # clean text -- this column is already made since the code takes a bit to run
    # blog.data.clean <- blog.data %>% 
    # mutate(documents_cleaned = textclean::replace_non_ascii(documents) ) %>%
    # mutate(documents_cleaned = textclean::replace_url(documents_cleaned, replacement = " ") ) %>%
    # mutate(documents_cleaned = textclean::replace_html(documents_cleaned, replacement = " ") ) %>%
    # mutate(documents_cleaned = textclean::replace_contraction(documents_cleaned, 
    #                                                     contraction.key=qdapDictionaries::contractions, 
    #                                                     ignore.case=TRUE) ) %>%
    # mutate(documents_cleaned = str_replace_all(documents_cleaned, "[[:punct:]]", " ")) %>%
    # mutate(documents_cleaned = textclean::replace_ordinal(documents_cleaned) ) %>%
    # mutate(documents_cleaned = textclean::replace_number(documents_cleaned) ) %>%
    # mutate(documents_cleaned = stringr::str_replace_all(documents_cleaned, "[\\s]+", " ") )   


    # Prep DTM
    blog.dtm <- blog.data %>% 
                group_by(article_id) %>%
                summarize(documents_cleaned = first(documents_cleaned),
                         text = first(text) ) %>%
        unnest_tokens(word, documents_cleaned, to_lower = TRUE) %>% 
        anti_join(get_stopwords(language="en", source="snowball") ) %>% 
        filter(!str_detect(word, "[0-9]+") & 
                str_length(word) > 1 ) %>% 
        dplyr::count(text, word) %>%
        cast_dtm(term = word, document = text, 
                value = n, weighting = tm::weightTf) 
    
    dim(blog.dtm) # 13246 by 67432
    # dim(removeSparseTerms(blog.dtm, .998)) # 13246  10027
    blog.dtm <- removeSparseTerms(blog.dtm, .998)  # 13246  10027
