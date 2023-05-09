
# dir
setwd('/home/nc-workforce/Documents/Text processing NLP/word models/bible analysis')

# pkgs
require(pacman)
p_load(dplyr, ggplot2, stringr, stringi, tidytext, tidyr)

# data
bb <- read.csv('data/kjv.csv') |>
  select(-X, -gutenberg_id) |>
  filter(
    text != 'The Old Testament of the King James Version of the Bible' &
      text != 'The New Testament of the King James Bible')
View(bb)

# Function for omitting white spaces
omit_white <- function(vec) ifelse(vec == '', NA, vec)

# cleaning the data -------------------------------------------------------

# removing extra white spaces
bb <- bb |>
  mutate(text = str_squish(text) |> omit_white()) |>
  na.omit()

rownames(bb) <- 1:nrow(bb)

# Constructing the book_title object
book_title <- bb[1:66, ]
id_dup <- which(duplicated(bb) == T)

book_title_loc <- id_dup[which(bb[id_dup, ] %in% book_title == T)]
book_title_loc = book_title_loc[!book_title_loc %in% c(18157, 20435)]
bb[book_title_loc, ]

# investigate index: 18157, 20435, 22311, 24542
# the book of kings brings the issue
# bb[c(18157, 20435), ]
# bb[c(22311, 24542), ]
# index: (18157, 20435) are simply the books of samuel
# index: (22311, 24542) are the real books of kings

bb$book <- NA
for (i in 1:length(book_title_loc))
{
  start <- book_title_loc[i]
  
  if (i == length(book_title_loc)) stop = nrow(bb)
  else stop = book_title_loc[i+1] - 1
  
  bb[start:stop, 2] <- book_title[i]
}

bb <- bb |> na.omit()

# clearing up the first indexes of beginning of every book
id_alpha <- which((bb[, 1] == bb[, 2]) == T)
bb <- bb[-id_alpha, ]

rownames(bb) <- 1:nrow(bb)
write.csv(bb, 'data/clean_v1.csv', row.names = F)


