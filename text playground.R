

# Text playground ---------------------------------------------------------


# libs --------------------------------------------------------------------

require(pacman)
p_load(dplyr, tidytext, ggplot2, stringr, stringi, janeaustenr, tidyr, ggpubr,
       ggraph, widyr, wordcloud, wordcloud2, reshape2, igraph)

get_sentiments('afinn')
get_sentiments('loughran')
get_sentiments('nrc')

# Analysis 1: simple analysis ---------------------------------------------

text <- c(
  "Because I could not stop for Death -",
  "He kindly stopped for me -",
  "The Carriage held but just Ourselves -",
  "and Immortality"
)

tibble(id = 1:4,
       text = text) |>
  unnest_tokens(word, text)


# Analyzing works of jane austen ------------------------------------------

original_books <- austen_books() |>
  group_by(book) |> 
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)
                            ))) |>
  ungroup()

# unnesting
tidy_book <- original_books |>
  unnest_tokens(word, text)

# omitting stop words
tidy_book <- tidy_book |>
  anti_join(stop_words)

# counting words
tidy_book |>
  count(word,sort = T) |>
  head(20) |>
  ggplot()+
  geom_col(aes(x = reorder(word, n),y = n))+
  labs(title = 'Word count (top 20)',
       x = 'Word', y = 'Count')+
  coord_flip()+
  theme_classic()
  

# Incorporating sentiment analysis ----------------------------------------

# bing sentiments
js <- tidy_book |>
  select(book, linenumber, word) |>
  inner_join(get_sentiments('bing')) |>
  count(book, index = linenumber %/% 80, sentiment) |>
  spread(sentiment, n, fill = 0) |>
  mutate(sentiment = positive - negative)

js |>
  ggplot(aes(x = index, y = sentiment, fill = book))+
  geom_col(show.legend = F)+
  theme_classic()+
  facet_wrap(. ~ book, scales = 'free_x')

# checking word contrinbutions to the sentiments

tidy_book |>
  inner_join(get_sentiments('bing')) |>
  count(word, sentiment, sort = T) |>
  group_by(sentiment) |>
  top_n(10) |>
  ggplot()+
  geom_col(aes(x = reorder(word, n), y = n))+
  labs(title = 'Word contribution to sentiment',
       x = 'Word', y = 'word count')+
  facet_wrap( ~ sentiment, scales = 'free_y')+
  coord_flip()+
  theme_classic()

# Omitting the word miss since its used to denote a woman and not the negative one
custom_stop_words <- bind_rows(
  data.frame(word = 'miss', lexicon = 'custom'),
  stop_words
)

tidy_book |>
  anti_join(custom_stop_words) |>
  inner_join(get_sentiments('bing')) |>
  count(word, sentiment, sort = T) |>
  group_by(sentiment) |>
  top_n(10) |>
  ggplot()+
  geom_col(aes(x = reorder(word, n), y = n))+
  labs(title = 'Word contribution to sentiment',
       x = 'Word', y = 'word count')+
  facet_wrap( ~ sentiment, scales = 'free_y')+
  coord_flip()+
  theme_classic()

#word clouds
tidy_book |>
  count(word) |>
  inner_join(get_sentiments('bing')) |>
  with(wordcloud(word, n, 
                 max.words = 100,
                 colors = ifelse(sentiment == 'negative', 'tomato', 'blue')))

# comparison cloud
tidy_book |>
  inner_join(get_sentiments('bing')) |>
  count(word, sentiment, sort = T) |>
  acast(word ~ sentiment, value.var = 'n', fill = 0) |>
  comparison.cloud(colors = c('red', 'blue'),
                   max.words = 100)

# Term frequency and Inverse term frequency -------------------------------

# computing word count per book
book_words <- original_books |>
  unnest_tokens(word, text) |>
  count(book, word, sort = T) |>
  ungroup()

# computing total words in the book
total_words <- book_words |>
  group_by(book) |>
  summarise(total = sum(n))

# joining and computing term frequency
book_words <- book_words |>
  left_join(total_words) |>
  mutate(term_freq = n/total)

book_words |>
  gghistogram(x = 'term_freq', fill = 'book', bins = 60)+
  labs(title = 'Term frequency', x = 'Term frequency', y = 'Density')+
  facet_wrap(~book, scales = 'free_y')

freq_by_rank <- book_words |>
  group_by(book) |>
  mutate(rank = row_number())

freq_by_rank |>
  ggplot(aes(x = rank, y = term_freq, col = book))+
  geom_line()+
  scale_x_log10()+
  scale_y_log10()+
  labs(title = 'Zipf`s Law', x = 'Rank', y = 'Term frequency')+
  theme_classic2()

# computing the tf idf
book_words <- book_words |>
  bind_tf_idf(term = word, document = book, n = n)

book_words |>
  ggplot(aes(x = tf, y = idf, col = book)) +
  geom_point() +
  labs(title = 'TF & IDF', x = 'Term frequency', y = 'inverse document frequency') +
  theme_classic2()

book_words |>
  select(-total) |>
  arrange(desc(tf_idf))

book_words |> 
  arrange(desc(tf_idf)) |>
  group_by(book) |>
  top_n(15) |>
  ggplot(aes(x = reorder(word, tf_idf), y = tf_idf)) +
  geom_col(alpha = .4, col = 'black') +
  labs(title = 'TF-IDF by book', x = 'Word', y = 'TF-IDF')+
  coord_flip() +
  facet_wrap(~book, scales = 'free')+
  theme_classic2()


# Tokenization by n-gram --------------------------------------------------

austen_bigram <- austen_books() |>
  unnest_tokens(bigram, text, token = 'ngrams', n = 2) |>
  na.omit()

austen_bigram |>
  count(bigram, sort = T)

# seperating the bigram by word
bi_seperated <- austen_bigram |>
  separate(bigram, c('word1', 'word2'), sep = ' ')

bi_filtered <- bi_seperated |>
  filter(
    !word1 %in% stop_words$word,
    !word2 %in% stop_words$word
  )

bi_unite <- bi_filtered |>
  unite(bigram, word1, word2, sep = ' ')

bi_unite |>
  count(bigram, sort = T)

# Most frequent trigram
austen_books() |>
  unnest_tokens(trigram, text, token = 'ngrams', n = 3) |>
  na.omit() |>
  separate(trigram, c('w1', 'w2', 'w3'),sep = ' ') |>
  filter(
    !w1 %in% stop_words$word,
    !w2 %in% stop_words$word,
    !w3 %in% stop_words$word
  ) |>
  unite(trigram, w1, w2, w3, sep = ' ') |>
  count(trigram, sort = T)

# usefulness of bigrams, e.g. in checking street names
bi_filtered |>
  filter(word2 == 'street') |>
  group_by(book) |>
  count(book, word1)

# computing the tf-idf of bigrams
bi_tf_idf <- bi_unite |>
  count(book, bigram) |>
  bind_tf_idf(bigram, book, n) |>
  arrange(desc(tf_idf)) 
  
bi_tf_idf |>
  group_by(book) |>
  top_n(10) |>
  ggplot()+
  geom_col(aes(x = reorder(bigram, tf_idf), y = tf_idf)) +
  labs(title = 'Unique & important words', x = 'Bigram', y = 'TF-IDF')+
  coord_flip()+
  facet_wrap(~book, scales = 'free')+
  theme_classic2()

# checking words preceeded by not, no, never, without

bi_seperated |>
  filter(word1 %in% c('not', 'no', 'never', 'without')) |>
  inner_join(get_sentiments('bing'), by = c(word2 = 'word')) |>
  count(book, word1, word2, sentiment, sort = T) |>
  filter(sentiment == 'positive') |>
  group_by(book) |>
  top_n(10) |>
  ungroup() |>
  ggplot() +
  geom_col(aes(x = reorder(word2, n), y = n)) +
  labs(title = 'Misleading sentiments for positive class', x = 'Word', y = 'Count') +
  coord_flip() +
  facet_wrap( ~ book, scales = 'free') +
  theme_classic2()


# Network graphs for occurences -------------------------------------------

bi_count <- bi_tf_idf |>
  select(bigram, n) |>
  separate(bigram, c('w1', 'w2'), sep = ' ')

bi_graph <- bi_count |> 
  filter(n > 20) |>
  graph_from_data_frame()

ggraph(bi_graph, layout = 'fr')+
  geom_edge_link(aes(edge_alpha = n), show.legend = F, 
                 arrow = grid::arrow(type = 'closed', length = unit(.15, 'inches')))+
  geom_node_point(color = 'lightblue', size = 5)+
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)+
  labs(title = 'Network of frequent bigrams')+
  theme_light()


