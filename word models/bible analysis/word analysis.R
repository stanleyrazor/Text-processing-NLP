
# libs
require(pacman)
p_load(gtools, tidytext, stringr, stringi, dplyr, 
       ggplot2, ggpubr, gridExtra, fitdistrplus, ggraph, igraph)

# functions for normalizing probabilities
summalize <- function(x) x/sum(x)

# loading data and cleaning a bit to omit stop words and punctuation and numbers
txt <- read.csv('data/clean_v1.csv') |> 
  tibble() |>
  setNames(c('word', 'book')) |>
  unnest_tokens(word, word) |>
  anti_join(stop_words) |>
  mutate(num = str_detect(word, '[0123456789]')) |>
  filter(num == FALSE) |>
  dplyr::select(-num)

# confirming that book of esther doesnt mention GOD
txt |>
  filter(book == 'The Book of Esther') |>
  filter(word %in% c('lord','god'))


# Analysing the gospels ---------------------------------------------------

gospels <- txt |>
  filter(book %in% c("The Gospel According to Saint Matthew", 
                     "The Gospel According to Saint Mark" ,
                     "The Gospel According to Saint Luke",
                     "The Gospel According to Saint John"))

# searching how many times GOD has been called in all the books
mention_name <- gospels |>
  filter(word %in% c('jesus','christ')) |>
  group_by(book) |>
  count(book) |>
  ungroup() |>
  arrange(desc(n))

# TF-IDF analysis
gospels |>
  group_by(book) |>
  count(word, sort = T) |>
  ungroup()

gtd <- gospels |>
  count(book, word, sort = T) |>
  bind_tf_idf(term = word, document = book, n = n)

gtd |>
  ggplot(aes(x = tf, y = idf, col = book)) +
  geom_point() +
  labs(title = 'TF & IDF', x = 'Term frequency', y = 'inverse document frequency') +
  theme_classic2()

gtd |>
  arrange(desc(tf_idf))

gtd |> 
  arrange(desc(tf_idf)) |>
  group_by(book) |>
  top_n(10) |>
  ggplot(aes(x = reorder(word, tf_idf), y = tf_idf)) +
  geom_col(alpha = .4, fill = 'gray', col = 'black') +
  labs(title = 'TF-IDF by book', x = 'Word', y = 'TF-IDF')+
  coord_flip() +
  facet_wrap(~book, scales = 'free')+
  theme_classic()



# Gospel Bigram -----------------------------------------------------------

gospel_bigram <- read.csv('data/clean_v1.csv') |> 
  tibble() |>
  setNames(c('word', 'book')) |>
  filter(book %in% c("The Gospel According to Saint Matthew", 
                     "The Gospel According to Saint Mark" ,
                     "The Gospel According to Saint Luke",
                     "The Gospel According to Saint John")) |>
  unnest_tokens(bigram, word, token = 'ngrams', n = 2) |>
  mutate(num = str_detect(bigram, '[0123456789]')) |>
  filter(num == FALSE) |>
  dplyr::select(-num)

# Omitting stopwordsin the bigrams
bi_n.stop <- gospel_bigram |>
  separate(bigram, c('word1', 'word2'), sep = ' ') |>
  filter(
    !word1 %in% stop_words$word,
    !word2 %in% stop_words$word
  ) |>
  unite(bigram, word1, word2, sep = ' ')

# analysing

gospel_bigram <- bi_n.stop |>
  count(book, bigram, sort = T) |>
  bind_tf_idf(term = bigram, document = book, n = n)

gospel_bigram |>
  group_by(book) |>
  arrange(desc(tf_idf)) |>
  top_n(4) |>
  ggplot(aes(x = reorder(bigram, tf_idf), y = tf_idf)) +
  geom_col(alpha = .4, fill = 'gray', col = 'black') +
  labs(title = 'TF-IDF by book', x = 'bigram', y = 'TF-IDF')+
  coord_flip() +
  facet_wrap(~book, scales = 'free')+
  theme_classic()


# Network graphs for sentiment --------------------------------------------

bi_count <- gospel_bigram |>
  dplyr::select(bigram, n) |>
  separate(bigram, c('w1', 'w2'), sep = ' ')

bi_graph <- bi_count |> 
  filter(n > 5) |>
  graph_from_data_frame()

ggraph(bi_graph, layout = 'fr')+
  geom_edge_link(aes(edge_alpha = n), show.legend = F, 
                 arrow = grid::arrow(type = 'closed', length = unit(.15, 'inches')))+
  geom_node_point(color = 'lightblue', size = 5)+
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)+
  labs(title = 'Network of frequent bigrams')+
  theme_void()

