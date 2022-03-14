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


wordle_feedback <- function(
    guess, 
    answer, 
    l = length(answer), 
    correct_position = 3, 
    correct_letter = 1, 
    incorrect = 0, 
    to_vector = TRUE
) {
    
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
        mutate(status = correct_position) %>%
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
        mutate(status = correct_letter)
    
    df_status <- df %>%
        left_join(
            bind_rows(df_g, df_y), 
            by = "position"
        ) %>%
        mutate(status = replace_na(status, incorrect))
    
    return(df_status)
    
}

wordle_feedback(guess = "brooo", answer = "borno")
wordle_feedback(guess = "boroo", answer = "borno")
wordle_feedback(guess = "oboro", answer = "borno")
wordle_feedback(guess = "boroo", answer = "borno")
wordle_feedback(guess = "broke", answer = "borno")
wordle_feedback(guess = "banko", answer = "borno")


# Trim possibilities based on guess
hint <- wordle_feedback(guess = "sleil", answer = "swill")
hint

df_possibilities <- wordle$answers$answer %>%
    map_df(function(word) {
        word %>%
            string_to_vector() %>%
            set_names(1:5)
    }) %>%
    mutate(word = wordle$answers$answer)
    # map_df(function(word) {
    #     tibble(
    #         word = word,
    #         letter = string_to_vector(word)
    #     )
    # }) %>%
    # group_by(word) %>%
    # mutate(position = row_number()) %>%
    # ungroup()


g <- hint %>% filter(status == 3)
y <- hint %>% filter(status == 1)

remainder <- df_possibilities
for (i in 1:nrow(g)) {
    remainder <- remainder %>% 
        filter(!!sym(as.character(g$position[i])) == g$guess[i]) %>%
        select(-!!sym(as.character(g$position[i])))
}

# TODO yellow with one letter will match options with multiple of the same letter (unless the position was correct)
possibilities <- remainder %>%
    pivot_longer(cols = !word, names_to = "position", values_to = "letter") %>%
    filter(letter %in% y$guess) %>%
    distinct(word)


trim_possibilities <- function(hint, possibilities) {
    
    df_possibilities <- possibilities %>%
        map_df(function(word) {
            word %>%
                string_to_vector() %>%
                set_names(1:5)
        }) %>%
        mutate(word = possibilities)
    
    # Correct guess
    if (all(hint$status == 3)) {
        trimmed_words <- paste0(hint$guess, collapse = "")
        return(trimmed_words)
    }
    
    remainder <- df_possibilities
    
    # Trim to words with correct positions first
    g <- hint %>% filter(status == 3)
    
    if (nrow(g) > 0) {
        for (i in 1:nrow(g)) {
            remainder <- remainder %>% 
                filter(!!sym(as.character(g$position[i])) == g$guess[i]) %>%
                select(-!!sym(as.character(g$position[i])))
        }
    }
    
    # Then trim to words containing correct letters
    y <- hint %>% filter(status == 1)
    
    trimmed_words <- remainder %>%
        pivot_longer(cols = !word, names_to = "position", values_to = "letter") %>%
        mutate(position = as.integer(position))
    
    if (nrow(y) > 0) {
        trimmed_words <- trimmed_words %>%
            group_by(word) %>%
            
            # Filter to words containing the correct letters
            filter(all(y$guess %in% letter)) %>%
            
            # Remove words where we know the letter is in the wrong spot
            inner_join(
                y %>% select(position, guess), 
                by = "position"
            ) %>%
            filter(!any(letter == guess)) %>%
            ungroup()
    }
    
    trimmed_words2 <- remainder %>%
        filter(word %in% trimmed_words$word) %>%
        pivot_longer(cols = !word, names_to = "position", values_to = "letter") %>%
        mutate(position = as.integer(position))
    
    # Then trim to words that do not have the incorrect guessed letters in the incorrect location
    x <- hint %>% filter(status == 0)
    
    if (nrow(x) > 0) {
        trimmed_words3 <- trimmed_words2 %>%
            left_join(x, by = "position") %>%
            mutate(guess = replace_na(guess, "NA")) %>%
            group_by(word) %>%
            filter(!any(guess %in% letter)) %>%
            ungroup()
    }
    
    trimmed <- trimmed_words3 %>% distinct(word)
    
    return(trimmed)
}


answer <- "swill"
first_guess <- "stake"

trimmed_possibilities <- trim_possibilities(hint = wordle_feedback(guess = first_guess, answer = answer), possibilities = wordle$answers$answer)


trimmed_guess <- df_answer_freq %>%
    inner_join(trimmed_possibilities, by = c("answer" = "word")) %>%
    filter(score == max(score)) %>%
    head(1) %>%
    pull(answer)

trim_possibilities(hint = wordle_feedback(guess = trimmed_guess, answer = answer), possibilities = trimmed_possibilities$word)

# Max eliminations
set.seed(34534)
elimination <- trimmed_possibilities %>%
    # sample_n(20) %>%
    pull(word) %>%
    map(function(guess) {
        message(guess)
        p <- trim_possibilities(hint = wordle_feedback(guess = guess, answer = answer), possibilities = trimmed_possibilities$word)
        tibble(guess = guess, possibilities = nrow(p))
    }) %>%
    bind_rows()

next_guess <- elimination %>%
    arrange(possibilities) %>%
    left_join(df_answer_freq, by = c("guess" = "answer")) %>%
    filter(possibilities == min(possibilities, na.rm = TRUE)) %>%
    filter(score == max(score, na.rm = TRUE)) %>%
    head(1) %>%
    pull(guess)



trimmed_possibilities2 <- trim_possibilities(hint = wordle_feedback(guess = next_guess, answer = answer), possibilities = trimmed_possibilities$word)





answer <- sample(wordle$answers$answer, 1)
guess <- "stare"

hint <- wordle_feedback(guess, answer)
trimmed_possibilities <- trim_possibilities(hint, wordle$answers$answer)

recommend_guess <- function(answer, possibilities) {
    possibilities %>%
        map(function(guess) {
            message(guess)
            p <- trim_possibilities(hint = wordle_feedback(guess, answer), possibilities = trimmed_possibilities$word)
            tibble(guess = guess, possibilities = nrow(p))
        }) %>%
        bind_rows()
}

df_recommend <- recommend_guess(answer, trimmed_possibilities$word)

df_recommend %>%
    filter(possibilities == min(possibilities, na.rm = TRUE)) %>%
    left_join(df_answer_freq, by = c("guess" = "answer"))%>%
    filter(score == max(score, na.rm = TRUE)) %>%
    head(1) %>%
    pull(guess)



# Distribution of possibilities after making a guess for each possible answer

# This took 2 weeks to run...
df_trim <- wordle$answers$answer %>% 
    map(function(word) {
        message(paste0(word, " ---------------------"))
        recommend_guess(answer = word, possibilities = wordle$answers$answer) %>%
            mutate(answer = word)
    })
    
saveRDS(df_trim, "data/possibilities.rds")

df_trim <- df_trim %>%
    bind_rows() %>%
    filter(!is.na(possibilities))

df_trim %>%
    group_by(guess) %>%
    summarise(mean = mean(possibilities), median = median(possibilities), max = max(possibilities)) %>%
    ungroup() %>%
    arrange(mean)

df_trim %>%
    ggplot(aes(x = guess, y = possibilities)) +
    geom_boxplot()

df_trim %>%
    filter(guess == "store") %>%
    arrange(possibilities)




answer = "brine"
hint = wordle_feedback(guess = "winey", answer = answer)
trimmed_possibilities = trim_possibilities(hint = hint, possibilities = wordle$answers$answer)
df_trim %>%
    inner_join(trimmed_possibilities, by = c("answer" = "word"))







# Example of single guess giving a solution
guess = string_to_vector("begin")
hint = tibble(
    position = 1:5,
    guess = guess,
    status = c(1, 1, 1, 0, 0)
)
trimmed_possibilities = trim_possibilities(hint = hint, possibilities = wordle$answers$answer)


df_possibilities <- wordle$answers$answer %>%
    map_df(function(word) {
        word %>%
            string_to_vector() %>%
            set_names(1:5)
    }) %>%
    mutate(word = wordle$answers$answer)

y <- hint %>% filter(status == 1)

trimmed_words <- df_possibilities %>%
    pivot_longer(cols = !word, names_to = "position", values_to = "letter") %>%
    mutate(position = as.integer(position))

trimmed_words <- trimmed_words %>%
    group_by(word) %>%
    
    # Filter to words containing the correct letters
    filter(all(y$guess %in% letter)) %>%
    
    # Remove words where we know the letter is in the wrong spot
    inner_join(
        y %>% select(position, guess), 
        by = "position"
    ) %>%
    filter(!any(letter == guess)) %>%
    ungroup()



# Bugfix

guess = "issue"
hint = tibble(
    position = 1:5,
    guess = string_to_vector(guess),
    status = c(0,1,0,0,1)
)
trim_possibilities(hint, wordle$answers$answer)
