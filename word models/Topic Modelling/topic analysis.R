

# Topic Modelling ---------------------------------------------------------

# libs --------------------------------------------------------------------

require(pacman)
p_load(dplyr, tidytext, ggplot2, stringr, stringi, janeaustenr, tidyr, ggpubr,
       ggraph, widyr, wordcloud, wordcloud2, reshape2, igraph,
       topicmodels)

data("AssociatedPress")

# Constructing a two-topic model
ap_2lda <- LDA(AssociatedPress, k = 2, control = set.seed(1))



# Topic-Word Probabilities ------------------------------------------------

# extracting betas: per topic per word probs
# probability of that term being generated from that topic
pp.ap <- tidy(ap_2lda, matrix = 'beta') |>
  mutate(topic = ifelse(topic == 1, 'Topic 1', 'Topic 2'))

pp.ap |> 
  group_by(topic) |>
  top_n(20, beta) |>
  ungroup() |>
  ggplot() +
  geom_col(aes(x = reorder(term, beta), y = beta))+
  labs(title = 'Per-topic-per-word probabilities',
       y = 'Probability of a term being generated from a topic',
       x = 'Word')+
  facet_wrap( ~ topic, scales = 'free_y')+
  coord_flip()+
  theme_classic()
  
# checking words with greatest difference in beta between topic 1 and 2

beta_spread <- pp.ap |>
  spread(topic, beta) |>
  filter(`Topic 1` > 1e-3 | `Topic 2` > 1e-3) |>
  mutate(`LogRatio` = log2(`Topic 2`/`Topic 1`))

beta_spread |>
  mutate(LogType = ifelse(LogRatio < 0, 
                          'Likely to occur in Topic 1',
                          'Likely to occur in Topic 2')) |>
  group_by(LogType) |>
  top_n(10, abs(LogRatio)) |>
  ggplot()+
  geom_col(aes(x = reorder(term, (LogRatio)), y = (LogRatio),
               fill = LogType), show.legend = F)+
  labs(title = 'Greatest difference in Beta',
       subtitle = 'Negative Beta difference indicates words most likely to occur \ 
       in topic 1 than in topic 2, and vice versa',
       x = 'Word', y = 'Beta difference')+
  coord_flip()+
  theme_classic()


# Document Topic Probabilities --------------------------------------------

# per document per topic probabilities are called Gamma

dd.ap <- tidy(ap_2lda, matrix = 'gamma')

dd.ap |>
  filter(document %in% 1:4) |>
  ggplot()+
  geom_boxplot(aes(x = factor(topic), y = gamma, fill = factor(topic)),
               show.legend = F)+
  facet_wrap(.~document)+
  labs(title = 'Document-Topic Distribution',
       x = 'Topic', y = 'Gamma')+
  theme_classic()

# checking documnts highly likelt yo be generated fromtopic 1/2
# cutoff probability: 0.7

dd.ap |>
  filter(topic == 1 & gamma >= .7)

dd.ap |>
  filter(topic == 2 & gamma >= .7)

# topic 1 seems highly political
tidy(AssociatedPress) |>
  filter(document == 838) |>
  arrange(desc(count))

# topic 2 seems highly health and lifestyle
tidy(AssociatedPress) |>
  filter(document == 1201) |>
  arrange(desc(count))



