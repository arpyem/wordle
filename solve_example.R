guess = "store"

hint1 = data.frame(
    position = 1:5,
    guess = string_to_vector(guess),
    status = c(1, 0, 0, 0, 3),
    stringsAsFactors = FALSE
)

p1 = trim_possibilities(hint1, wordle$answers$answer)

print.data.frame(p1)

guess2 = "aside"

hint2 = data.frame(
    position = 1:5,
    guess = string_to_vector(guess2),
    status = c(1, 1, 0, 0, 3),
    stringsAsFactors = FALSE
)

p2 = trim_possibilities(hint2, p1$word)

print.data.frame(p1)

guess3 = "pause"

hint3 = data.frame(
    position = 1:5,
    guess = string_to_vector(guess3),
    status = c(1, 3, 0, 3, 3),
    stringsAsFactors = FALSE
)

p3 = trim_possibilities(hint3, p2$word)

print.data.frame(p1)






# 2022-03-11

guess = "stare"

hint1 = data.frame(
    position = 1:5,
    guess = string_to_vector(guess),
    status = c(0, 1, 1, 0, 0),
    stringsAsFactors = FALSE
)

p1 = trim_possibilities(hint1, wordle$answers$answer)

print.data.frame(p1)


guess2 = "tango"

hint2 = data.frame(
    position = 1:5,
    guess = string_to_vector(guess2),
    status = c(1, 3, 0, 0, 0),
    stringsAsFactors = FALSE
)

p2 = trim_possibilities(hint2, p1$word)

print.data.frame(p2)


guess3 = "match"

hint3 = data.frame(
    position = 1:5,
    guess = string_to_vector(guess3),
    status = c(0, 3, 3, 3, 3),
    stringsAsFactors = FALSE
)

p3 = trim_possibilities(hint3, p2$word)

print.data.frame(p3)

# logic to check alll remaining words only differ in the first letter, try to eliminate the letters in the first position
# this avoids situations where you have x guesses left / y words that have the same last 4 letters, which can be very unfavorable8



# 2022-03-12

guess = "stare"

hint1 = data.frame(
    position = 1:5,
    guess = string_to_vector(guess),
    status = c(0, 1, 1, 0, 0),
    stringsAsFactors = FALSE
)

p1 = trim_possibilities(hint1, wordle$answers$answer)

print.data.frame(p1)


guess2 = "adopt"

hint2 = data.frame(
    position = 1:5,
    guess = string_to_vector(guess2),
    status = c(1, 1, 1, 0, 1),
    stringsAsFactors = FALSE
)

p2 = trim_possibilities(hint2, p1$word)

print.data.frame(p2)





# 2022-03-13

guess = "spire"

hint1 = data.frame(
    position = 1:5,
    guess = string_to_vector(guess),
    status = c(1, 0, 0, 0, 0),
    stringsAsFactors = FALSE
)

p1 = trim_possibilities(hint1, wordle$answers$answer)

print.data.frame(p1)


guess2 = "toast"

hint2 = data.frame(
    position = 1:5,
    guess = string_to_vector(guess2),
    status = c(0, 3, 0, 1, 0),
    stringsAsFactors = FALSE
)

p2 = trim_possibilities(hint2, p1$word)

print.data.frame(p2)


guess3 = "focus" # Correct

