
# Get hints from a wordle guess, given a specific answer
# Feedback is in numeric form and can be used as a weighting and changed
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


# Use wordle feedback to trim the list of possible answers
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
        
        # By word...
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
    
    trimmed_words <- trimmed_words %>% distinct(word)
    
    return(trimmed_words)
}