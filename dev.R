library(tidyverse)

# Word list
# https://gist.github.com/cfreshman/cdcdf777450c5b5301e439061d29694c
words <- read_csv(file = file.path("data", "wordle-allowed-guesses.txt"), col_names = "allowed", col_types = cols())

# Possible answers
# https://gist.github.com/cfreshman/a03ef2cba789d8cf00c08f767e0fad7b#file-wordle-answers-alphabetical-txt
answers <- read_csv(file = file.path("data", "wordle-answers-alphabetical.txt"), col_names = "answer", col_types = cols())

wordle <- list(
    words = words,
    answers = answers
)

saveRDS(object = wordle, file = file.path("data", "wordle.rds"))






