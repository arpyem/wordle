library(tidyverse)

# Word list
# https://gist.github.com/cfreshman/cdcdf777450c5b5301e439061d29694c

# words <- read_csv(file = file.path("data", "wordle-allowed-guesses.txt"), col_names = "allowed", col_types = cols())

# Possible answers
# https://gist.github.com/cfreshman/a03ef2cba789d8cf00c08f767e0fad7b#file-wordle-answers-alphabetical-txt

# answers <- read_csv(file = file.path("data", "wordle-answers-alphabetical.txt"), col_names = "answer", col_types = cols())

# wordle <- list(
#     words = words,
#     answers = answers
# )

# saveRDS(object = wordle, file = file.path("data", "wordle.rds"))


wordle <- readRDS(file = file.path("data", "wordle.rds"))

library(words)

# Check word list against answers - allowed words that don't match are likely irrelevant anyway
wordle$answers %>%
    left_join(words, by = c("answer" = "word")) %>%
    filter(is.na(answer)) %>%
    nrow() %>%
    "=="(0)

df_words <- words %>%
    inner_join(wordle$words, by = c("word" = "allowed"))

n_letters <- unique(df_words$word_length) # pretending not to hardcode

df_letters <- 1:n_letters %>%
    map(function(x) {
        letter <- paste0("letter", x)
        df_words %>%
            transmute(!!sym(letter) := substr(x = word, start = x, stop = x))
    }) %>%
    bind_cols()

# Top 3 are ahead but below that isn't all that differentiated - could take a weighted sampling approach?
df_letter_dist <- df_words %>%
    bind_cols(df_letters) %>%
    pivot_longer(cols = contains("letter"), names_to = "position", values_to = "letter") %>%
    count(letter, sort = TRUE)

df_letter_dist %>%
    mutate(letter = factor(letter, levels = rev(unique(letter)))) %>%
    ggplot(aes(x = letter, y = n)) +
    geom_col(fill = "dodgerblue2") +
    coord_flip() +
    theme_minimal()

# Doesn't account for multiple letters in the same word - should it be weighted this way regardless?
df_letter_dist %>%
    mutate(prob = n / n())


# What about the word list itself?

get_letter_dist <- function(words, n_letters = 5) {
    1:n_letters %>%
        map(function(x) {
            tibble(!!sym(paste0("letter", x)) := substr(x = words, start = x, stop = x))
        }) %>%
        bind_cols()
}

# Almost the exact same distribution
wordle$words$allowed %>%
    get_letter_dist() %>%
    bind_cols(wordle$words) %>%
    pivot_longer(cols = contains("letter"), names_to = "position", values_to = "letter") %>%
    count(letter, sort = TRUE) %>%
    mutate(letter = factor(letter, levels = rev(unique(letter)))) %>%
    ggplot(aes(x = letter, y = n)) +
    geom_col(fill = "dodgerblue2") +
    coord_flip() +
    theme_minimal()

# Answers have a different distribution - because the words are more "common"
wordle$answers$answer %>%
    get_letter_dist() %>%
    bind_cols(wordle$answers) %>%
    pivot_longer(cols = contains("letter"), names_to = "position", values_to = "letter") %>%
    count(letter, sort = TRUE) %>%
    mutate(letter = factor(letter, levels = rev(unique(letter)))) %>%
    ggplot(aes(x = letter, y = n)) +
    geom_col(fill = "dodgerblue2") +
    coord_flip() +
    theme_minimal()

wordle$answers$answer %>%
    get_letter_dist() %>%
    bind_cols(wordle$answers) %>%
    pivot_longer(cols = contains("letter"), names_to = "position", values_to = "letter") %>%
    count(position, letter, sort = TRUE) %>%
    mutate(letter = factor(letter, levels = rev(unique(letter)))) %>%
    ggplot(aes(x = letter, y = n)) +
    geom_col(fill = "dodgerblue2") +
    coord_flip() +
    facet_wrap(~position) +
    theme_minimal()


df_answer_letters <- wordle$answers$answer %>%
    get_letter_dist() %>%
    bind_cols(wordle$answers) %>%
    pivot_longer(cols = contains("letter"), names_to = "position", values_to = "letter")

df_answer_letters_freq <- df_answer_letters %>%
    count(letter, sort = TRUE)

# Answers weighted by letter frequency
df_answer_freq <- df_answer_letters %>%
    left_join(df_answer_letters_freq, by = "letter") %>%
    group_by(answer) %>%
    summarise(score = sum(n)) %>%
    arrange(desc(score)) %>%
    print.data.frame()

# Example answer


string_to_vector <- function(word) {
    strsplit(x = word, split = "")[[1]]
}

answer <- wordle$answers$answer[49] %>% string_to_vector()

guess <- df_answer_freq %>%
    head(1) %>%
    pull(answer) %>%
    string_to_vector()

answer <- "borno" %>% string_to_vector()

guess <- "broke" %>% string_to_vector()
guess <- "brook" %>% string_to_vector()
guess <- "brooo" %>% string_to_vector()
guess <- "oboro" %>% string_to_vector()


wordle_feedback <- function(guess, answer, l = length(answer), to_vector = TRUE) {
    
    if (to_vector) {
        guess <- string_to_vector(guess)
        answer <- string_to_vector(answer)
    }
    
    if (length(guess) != l) stop("invalid guess")
    
    df <- tibble(
        position = 1:l,
        guess = guess,
        answer = answer
    )
    
    df_g <- df %>% 
        filter(guess == answer) %>%
        mutate(status = 1) %>%
        select(position, status)
    
    df_i <- df %>% filter(guess != answer)
    
    df_match <- df_i %>%
        select(position, answer) %>%
        full_join(
            df_i %>% select(position, guess),
            by = c("answer" = "guess"),
            suffix = c("_answer", "_guess")
        ) %>%
        group_by(position_guess) %>%
        slice(1) %>%
        ungroup() %>%
        na.omit() %>%
        group_by(position_answer) %>%
        slice(1) %>%
        ungroup()
    
    df_y <- df_match %>% 
        select(position = position_guess) %>%
        mutate(status = 0)
    
    df_status <- df %>%
        left_join(
            df_g %>% bind_rows(df_y), 
            by = "position"
        ) %>%
        mutate(status = replace_na(status, -1))
    
    return(df_status)
    
}

wordle_feedback(guess = "brooo", answer = "borno")
wordle_feedback(guess = "boroo", answer = "borno")
wordle_feedback(guess = "oboro", answer = "borno")
wordle_feedback(guess = "boroo", answer = "borno")
wordle_feedback(guess = "broke", answer = "borno")
wordle_feedback(guess = "banko", answer = "borno")



answer <- wordle$answers$answer[34]

df_answer_freq$answer %>%
    head(20) %>%
    map(~wordle_feedback(guess = .x, answer = answer))




