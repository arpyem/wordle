library(parallel)
library(tidyverse)

source("functions.R")

wordle = readRDS("data/wordle.rds")

detectCores()

# Distribution of possibilities after making a guess for each possible answer

# # This took 2 weeks to run...
# df_trim <- wordle$answers$answer %>% 
#     map(function(word) {
#         message(paste0(word, " ---------------------"))
#         recommend_guess(answer = word, possibilities = wordle$answers$answer) %>%
#             mutate(answer = word)
#     })
# 
# 
# start_time = Sys.time()
# answer = wordle$answers$answer[1]
# test = lapply(
#     X = wordle$answers$answer,
#     FUN = function(word) {
#         message(paste0(answer, ": ", word))
#         hint = wordle_feedback(guess = word, answer = answer)
#         possibilities = trim_possibilities(hint = hint, possibilities = wordle$answers$answer)
#         result = list(
#             word = word,
#             answer = answer,
#             possibilities = nrow(possibilities),
#             score = sum(hint$status)
#         )
#         return(result)
#     }
# )
# Sys.time() - start_time
# 
# start_time = Sys.time()
# answer = wordle$answers$answer[1]
# test = mclapply(
#     X = wordle$answers$answer,
#     mc.cores = 2,
#     FUN = function(word) {
#         message(paste0(answer, ": ", word))
#         hint = wordle_feedback(guess = word, answer = answer)
#         possibilities = trim_possibilities(hint = hint, possibilities = wordle$answers$answer)
#         result = list(
#             word = word,
#             answer = answer,
#             possibilities = nrow(possibilities),
#             score = sum(hint$status)
#         )
#         return(result)
#     }
# )
# Sys.time() - start_time

df_grid = expand.grid(
    wordle$answers$answer, 
    wordle$answers$answer, 
    stringsAsFactors = FALSE
) %>%
    set_names(c("guess", "answer")) %>%
    as_tibble()


start_time = Sys.time()
df_possibilities <- mcmapply(
    FUN = function(guess, answer) {
        hint = wordle_feedback(guess = guess, answer = answer)
        possibilities = trim_possibilities(hint = hint, possibilities = wordle$answers$answer)
        result = list(
            guess = guess,
            answer = answer,
            possibilities = nrow(possibilities),
            score = sum(hint$status)
        )
        return(result)
    },
    df_grid$guess,
    df_grid$answer,
    SIMPLIFY = FALSE,
    mc.cores = 2
)
Sys.time() - start_time

saveRDS(df_possibilities, "data/possibilities.rds")
