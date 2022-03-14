tests = readRDS(file.path("tests", "values.rds"))

tests

trim_possibilities(
    hint = tests$g1$hint, 
    possibilities = wordle$answers$answer, 
    status_values = c("s1", "s2", "s3")
)


df_possibilities <- possibilities %>%
    map_df(function(word) {
        word %>%
            string_to_vector() %>%
            set_names(1:5)
    }) %>%
    mutate(word = possibilities)

if (all(hint$status == status_values[3])) {
    trimmed_words <- tibble(word = paste0(hint$guess, collapse = ""))
    return(trimmed_words)
}

y <- hint %>% filter(status == status_values[2])

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

trimmed_words <- remainder %>%
    filter(word %in% trimmed_words$word) %>%
    pivot_longer(cols = !word, names_to = "position", values_to = "letter") %>%
    mutate(position = as.integer(position))

# Then trim to words that do not have the incorrect guessed letters in the incorrect location
x <- hint %>% filter(status == status_values[1])


# need to fix logic for guesses with multiple of one letter for this part
# e.g. "issue"
# if you guess one "s" in the incorrect position, all words with the second "s" get removed in this step
if (nrow(x) > 0) {
    trimmed_words <- trimmed_words %>%
        left_join(x, by = "position") %>%
        mutate(guess = replace_na(guess, "NA")) %>%
        group_by(word) %>%
        filter(!any(guess %in% letter)) %>%
        ungroup()
}


# First remove letters that were already found in the word 
# This prevents guessing a word with more than one of the same letter from filtering all words that have even one of that letter
trimmed_words %>%
    anti_join(y, by = c("letter" = "guess")) %>%
    left_join(x, by = "position") %>%
    mutate(guess = replace_na(guess, "NA")) %>%
    group_by(word) %>%
    filter(!any(guess %in% letter)) %>%
    ungroup()



