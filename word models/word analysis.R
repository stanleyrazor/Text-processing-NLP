

# Sentence modelling ------------------------------------------------------

# libs
require(pacman)
p_load(gtools, tidytext, janeaustenr, stringr, stringi, dplyr, 
       ggplot2, ggpubr, gridExtra, fitdistrplus)

# loading data into my environment
load("~/Documents/Text processing NLP/word models/march 10 2023.RData")


# Modelling word length ---------------------------------------------------

txt_len <- txt_word |>
  mutate(num = str_length(word))

sumtbl <- txt_len |>
  filter(num < 22) |>
  summarise(
    Min = min(num), 
    `Q1 (25%)` = quantile(num, .25),
    Mean = mean(num), 
    Median = median(num),
    `Q3 (75%)` = quantile(num, .75),
    Max = max(num),
    Skewness = DescTools::Skew(num),
    Kurtosis = DescTools::Kurt(num)
  )

t1 <- ggtexttable(round(sumtbl, 2), rows = NULL)

t2 <- txt_len |>
  filter(num < 22) |>
  ggplot()+
  geom_bar(aes(x =factor (num)))+
  labs(title = 'Word length distribution', 
       x = 'Word length', y = 'Count')+
  theme_classic()

ggarrange(t2, t1, nrow = 2,heights = c(4,1))


# Modeling word length ----------------------------------------------------


# fitting the binomial model
model_pois <- fitdist(txt_len$num, distr = 'pois')
model_gaussian <- fitdist(txt_len$num, distr = 'norm')

# ZTP model: lambda = 8.14876
# generating random numbers:
rztp <- function(rate)
{
  u = runif(n = 1, min = exp(-rate), max = 1)
  t = log(u)
  return(1 + rpois(n = 1, lambda = (rate - t)))
}

# generating random word lengths
w.l <- replicate(100, rztp(rate = 8.14876))
w.l

# Simple generation without looking at joint probs
for (wl in 1:length(w.l))
{
  # the starting letter
  start <- sample(x = starter[, 'nxt'] |> unlist() |> unname(),
                  size = 1,
                  prob = starter[, 'Probability'] |> unlist() |> unname())
  
  wrd <- start
  for (ch in 2:w.l[wl])
  {
    nxt_u <- cc_data |>
      filter(now == wrd[ch - 1])
    
    # finding the next letter
    nxt_letter <- sample(x = nxt_u[, 'nxt'] |> unlist() |> unname(),
                         size = 1,
                         prob = nxt_u[, 'Probability'] |> unlist() |> unname())
    
    wrd <- c(wrd, nxt_letter)
  }
  
  wrd <- list(wrd) |> stri_join_list()
  print(wrd)
}

# Generation of text paying attention to joint probs
for (wl in 1:length(w.l))
{
  # the starting letter
  start <- sample(x = starter[, 'nxt'] |> unlist() |> unname(),
                  size = 1,
                  prob = starter[, 'Probability'] |> unlist() |> unname())
  
  wrd <- start
  for (ch in 2:w.l[wl])
  {
    nxt_u <- cc_data |>
      filter(now == wrd[ch - 1])
    
    # a probability of acceptance based on joint probs
    u <- 1.529e-03
    j.prob <- 0
    i = 0
    while (j.prob < u)
    {
      j.prob <- joint_data |>
        filter(nxt == nxt_letter, now == wrd[ch - 1]) |>
        dplyr::select(Probability) |>
        as.numeric()
      
      # finding the next letter
      nxt_letter <- sample(x = nxt_u[, 'nxt'] |> unlist() |> unname(),
                           size = 1,
                           prob = nxt_u[, 'Probability'] |> unlist() |> unname())
      
      # condition for terminating words taking more than 10 iterations to build
      i = i + 1
      if (i > 10) {message('broken'); break}
    }
    
    wrd <- c(wrd, nxt_letter)
  }
  
  wrd <- list(wrd) |> stri_join_list()
  print(wrd)
}

i=1

x = rpois(100, 10)
m1 <- vglm(x ~ 1., family = pospoisson())


# Modelling sentence length -----------------------------------------------

# function for computing length of a sentence
len_sentence <- function(sentence) sentence |> str_split(' ') |> unlist() |> length()

# the sentence lengths
txt_sen <- txt |>
  stri_join_list(sep = ' ') |>
  str_squish() |>
  str_trim() |>
  str_split('[?.!]+ ') |>
  unlist() |>
  sapply(FUN = len_sentence) |>
  unname()

summary(txt_sen)

# omitting outliers using the three sigma rule
t.out <- (txt_sen - median(txt_sen))/(mad(txt_sen)/0.6745)
boxplot(t.out)
txt_rob_sen <- txt_sen[-which(abs(t.out) > 3)]

ggarrange(txt_sen |>
            data.frame() |>
            ggdensity(x = 'txt_sen', fill = 'lightgray', add = 'median') +
            labs(title = 'Distribution of sentence lengths',
                 x = 'Sentence length',y = 'Count')+
            theme_classic(),
          txt_rob_sen |>
            data.frame() |>
            ggdensity(x = 'txt_rob_sen', fill = 'lightgray', add = 'median') +
            labs(title = 'Distribution of sentence lengths',
                 x = 'Sentence length (cleaned)',y = 'Count')+
            theme_classic())


# fitting the binomial model
model_binom <- fitdist(txt_rob_sen, distr = 'binom',
                                     start = list(prob = .2, size = length(unique(txt_rob_sen))),
                                     method = 'qme',
                                     probs = c(.25, .75))
descdist(txt_rob_sen, boot = 200, discrete = T)

# generating random sentence lengths
sen_len <- rbinom(n = 3, 
                  size = as.integer(model_binom$estimate[2]),
                  prob = model_binom$estimate[1])

# generating a sentence
for (i in 1:length(sen_len))
{
  # random starting
  w <- vector()
  s <- sample(txt_word[, 'nxt'] |> unique(), 1)
  w <- c(s)
  
  for (ll in 2:sen_len[i])
  {
    # come up with probability of suggesting a random letter
    u <- runif(1)
    if (u > 2)
    {
      # we suggest a letter
      # random suggestion from its tops
      temp <- cc_data |>
        dplyr::filter(now == w[ll - 1]) |>
        arrange(desc(Probability)) |>
        dplyr::select(nxt) |>
        head(20) |>
        unlist() |>
        as.character()
      w[ll] <- temp[sample(20, 1)]
    }
    else
    {
      # look-up table
      temp <- cc_data |>
        dplyr::filter(now == w[ll - 1]) |>
        arrange(desc(Probability)) |>
        dplyr::select(nxt) |>
        head(5) |>
        unlist() |>
        as.character()
      w[ll] <- temp[sample(5, 1)]
    }
  }
  
  print(stri_join_list(list(w), sep = ' '))
  
}

