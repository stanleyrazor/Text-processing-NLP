

# English probability of letters following each other ---------------------

# libs
require(pacman)
p_load(gtools, tidytext, stringr, stringi, dplyr, ggplot2,
       reshape2, ggpubr, readr)

# functions for normalizing probabilities
summalize <- function(x) x/sum(x)

# function for duplicating middle letters, to enhance continuity
duplicate_mid <- function(vec)
{
  pvec <- vec[-c(1, length(vec))]
  vrep <- rep(pvec, each = 2)
  return(c(vec[1], vrep, vec[length(vec)]))
}

# the matrix of permutations
dd <- permutations(26, 2 , letters, repeats.allowed = T)

# the text data
dictionary <- read_delim("data/dictionary.txt", 
                         delim = "\t", escape_double = FALSE, 
                         col_names = FALSE, trim_ws = TRUE)

txt <- dictionary |>
  setNames('text') |>
  mutate(text = ifelse(text == '',NA, text)) |>
  na.omit()

View(dictionary); View(txt)

# we need to omit all stop words, duplicates, numbers, punctuations.
txt_word <- txt |>
  unnest_tokens(word, text) |>
  mutate(word = str_extract(word, "[a-z]+")) |>
  na.omit() |>
  mutate(duplicate = duplicated(word)) |>
  filter(duplicate == F) |> # omitting duplicates
  dplyr::select(word)

# the matrix
txt_mat <- txt_word |>
  as.list() |> 
  stri_join_list(sep = ' ') |>
  str_split('') |>
  unlist() |>
  duplicate_mid() |>
  matrix(ncol = 2, byrow = T)

View(txt_mat)

# Marginal probability

marginal_data <- txt_word |>
  as.list() |> 
  stri_join_list(sep = ' ') |>
  str_split('') |>
  unlist() |>
  matrix(ncol = 1) |>
  data.frame() |>
  setNames('letter') |>
  group_by(letter) |>
  count() |>
  ungroup() |>
  filter(letter != ' ') |>
  mutate(n = summalize(n)) |>
  setNames(c('letter', 'Probability'))

p1 <- marginal_data |>
  ggplot()+
  geom_col(aes(x = letter, y = Probability),
           col = 'white', fill = 'blue',alpha = .5) +
  labs(title = 'Marginal probability: P(Letter)',
       x = 'Letter', y = 'Probability')+
  theme_classic()+
  coord_flip()
p1

# Joint probability data: joint for current and next --------------------------------------------------

joint_data <- txt_mat |>
  data.frame() |>
  setNames(c('now', 'nxt')) |>
  dplyr::filter(now != ' ' & nxt != ' ') |>
  group_by(now, nxt) |>
  count() |>
  ungroup() |>
  mutate(n = summalize(n)) |>
  setNames(c('now', 'nxt', 'Probability'))

View(joint_data)

p2 <- joint_data |>
  ggplot(col = 'white')+
  geom_tile(aes(x = now, y = nxt, fill = Probability),col = 'white')+
  labs(title = 'Joint Probability: P(Current, Next)',
       x = 'Current letter',
       y = 'Next letter')+
  theme_classic()
p2

# Conditional probability data: conditional on current --------------------

features <- c(letters)
for (i in 1:length(features))
{
  if (i == 1)
  {
    cc_data <- txt_mat |>
      data.frame() |>
      setNames(c('now', 'nxt')) |>
      filter(now != ' ' & nxt != ' ') |> # omitting spaces
      filter(now == features[i]) |>
      group_by(now, nxt) |>
      count() |>
      ungroup() |>
      mutate(n = summalize(n))
    
    cn_data <- txt_mat |>
      data.frame() |>
      setNames(c('now', 'nxt')) |>
      filter(now != ' ' & nxt != ' ') |> # omitting spaces
      filter(nxt == features[i]) |>
      group_by(now, nxt) |>
      count() |>
      ungroup() |>
      mutate(n = summalize(n))
  }
  else
  {
    temp_data1 <- txt_mat |>
      data.frame() |>
      setNames(c('now', 'nxt')) |>
      filter(now != ' ' & nxt != ' ') |> # omitting spaces
      filter(now == features[i]) |>
      group_by(now, nxt) |>
      count() |>
      ungroup() |>
      mutate(n = summalize(n))
    cc_data <- rbind(cc_data, temp_data1)
    
    temp_data2 <- txt_mat |>
      data.frame() |>
      setNames(c('now', 'nxt')) |>
      filter(now != ' ' & nxt != ' ') |> # omitting spaces
      filter(nxt == features[i]) |>
      group_by(now, nxt) |>
      count() |>
      ungroup() |>
      mutate(n = summalize(n))
    cn_data <- rbind(cn_data, temp_data2)
  }
}

cc_data <- cc_data |>
  setNames(c('now', 'nxt', 'Probability'))

cn_data <- cn_data |>
  setNames(c('now', 'nxt', 'Probability'))

View(cc_data)
View(cn_data)

p3 <- cc_data |>
  ggplot()+
  geom_tile(aes(x = now, y = nxt, fill = Probability), col = 'white')+
  labs(title = 'Conditional probability: P(Next | Current = *)',
       x = 'Current letter',
       y = 'Next letter')+
  theme_classic()
p3

p4 <- cn_data |>
  ggplot()+
  geom_tile(aes(x = now, y = nxt, fill = Probability), col = 'white')+
  labs(title = 'Conditional probability: P(Current | Next = *)',
       x = 'Current letter',
       y = 'Next letter')+
  theme_classic()
p4

gridExtra::grid.arrange(p1, p2, p3, p4,
                        nrow = 2)



# distribution of letters which most commonly start a word
starter <- txt_mat |>
  data.frame() |>
  setNames(c('now', 'nxt')) |>
  filter(now == ' ') |>
  group_by(nxt) |>
  count() |>
  ungroup() |>
  mutate(n = summalize(n)) |>
  setNames(c('nxt', 'Probability')) |>
  arrange(desc(Probability))

finalizer <- txt_mat |>
  data.frame() |>
  setNames(c('now', 'nxt')) |>
  filter(nxt == ' ') |>
  group_by(now, nxt) |>
  count() |>
  ungroup() |>
  mutate(n = summalize(n)) |>
  setNames(c('now', 'nxt', 'Probability')) |>
  arrange(desc(Probability))

gridExtra::grid.arrange(
  starter |>
    ggplot()+
    geom_col(aes(x = reorder(nxt, Probability), y = Probability))+
    labs(title = 'Distribution of word starters',
         x = 'Letter', y = 'Probability of starting an english word')+
    coord_flip()+
    theme_classic(),
  finalizer |>
    ggplot()+
    geom_col(aes(x = reorder(now, Probability), y = Probability))+
    labs(title = 'Distribution of word enders',
         x = '', y = 'Probability of ending an english word')+
    coord_flip()+
    theme_classic(),
  nrow = 1
)

start_end <- merge(starter, finalizer, 
                   by.x = 'nxt', by.y = 'now') |>
  select(!c('now', 'nxt.y')) |>
  setNames(c('letter', 'prob.start', 'prob.end')) |>
  reshape2::melt(id = 'letter')

# How many times does a letter appear in a word on average
colMedians <- function(x) apply(x, 2, median)
colMax <- function(x) apply(x, 2, max)
colMin <- function(x) apply(x, 2, min)

vowels <- c('a','e', 'i', 'o', 'u')
consonants <- letters[!letters %in% vowels]

freq_occ <- max_possible <- min_possible <- vector()
for (i in 1:length(letters))
{
  freq_occ[i] <- txt_word |>
    mutate(count = str_count(word, pattern = letters[i])) |>
    mutate(count = ifelse(count == 0, NA, count)) |>
    na.omit() |>
    dplyr::select(count) |>
    colMedians()
  
  max_possible[i] <- txt_word |>
    mutate(count = str_count(word, pattern = letters[i])) |>
    dplyr::select(count) |>
    colMax()
  
  min_possible[i] <- txt_word |>
    mutate(count = str_count(word, pattern = letters[i])) |>
    dplyr::select(count) |>
    colMin()
}



# construct metrics for average length of english word
# construct probability function for consonants and vowels
# come up with a word generator
# construct

# computing the average word length,modelling using the binomial dist

txt_len <- txt_word |>
  mutate(len = str_length(word)) |>
  arrange(desc(len))

txt_len |>
  ggplot()+
  geom_bar(aes(x = factor(len)))+
  labs(title = 'Distribution of word length',
       x = 'Word length', y = 'Count')+
  theme_classic()

len_vec <- txt_len |>
  select(len) |>
  unlist() |> unname()

# fitting the binomial model
model_binom <- fitdistrplus::fitdist(len_vec, distr = 'binom',
                                     start = list(prob = .5, size = length(unique(len_vec))),
                                     method = 'qme',
                                     probs = c(.25, .75))


l <- rbinom(n = 10, 
            size = as.integer(model_binom$estimate[2]),
            prob = model_binom$estimate[1])
l <- ifelse(l < 2, NA, l) |>
  na.omit() |>
  as.integer()

for (i in 1:length(l))
{
  # random starting
  w <- vector()
  s <- sample(starter[, 'nxt'] |> unlist() |> unname(),
              1, 
              prob = starter[, 'Probability'] |> unlist() |> unname())
  w <- c(s)
  
  for (ll in 2:l[i])
  {
    # come up with probability of suggesting a random letter
    u <- runif(1)
    if (u > .5)
    {
      # we suggest a letter
      # random suggestion from its tops
      temp <- cc_data |>
        filter(now != ' ' | nxt != ' ') |>
        filter(now == w[ll - 1]) |>
        arrange(desc(Probability)) |>
        select(nxt) |>
        head(5) |>
        unlist() |>
        as.character()
      w[ll] <- temp[sample(5, 1)]
    }
    else
    {
      # look-up table
      w[ll] <- cc_data |>
        filter(now != ' ' & nxt != ' ') |>
        filter(now == w[ll - 1]) |>
        arrange(desc(Probability)) |>
        select(nxt) |>
        head(1) |>
        as.character()
    }
    
    
    if (w[ll] == ' ') break
  }
  
  print(stri_join_list(list(w)))
  
}

