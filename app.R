library(tidyverse)
library(shiny)
library(shinyjs)
library(DT)


# Set seed based on date - only affects the starting guess
set.seed(as.numeric(Sys.Date()))


# Data
wordle <- readRDS(file = file.path("data", "wordle.rds"))


# Functions 
source("functions.R")


# TODO add 6th guess (for closure)
# TODO messaging for when possibility table is empty - invalid hint (or missing words)


# UI ========================================================================================================================================
ui <- fluidPage(
    title = "Wordle Helper",
    useShinyjs(),
    includeCSS(path = "www/wordle.css"),
    tags$head(
        tags$script(src = "utilities.js")
    ),
    
    # Content
    div(
        
        div(
            
            # Info
            div(
                actionButton(inputId = "help", label = "Help"),
                class = "rowc mb"
            ),
            
            
            # UI: GUESSES ----
            div(
                
                # Guess 1 ----
                div(
                    id = "div-g1",
                    div(
                        selectInput(
                            inputId = "g1", 
                            label = "Guess 1",
                            choices = sort(c(wordle$words$allowed, wordle$answers$answer)), 
                            selected = sample(wordle$answers$answer, 1),
                            width = "150px"
                        ),
                        uiOutput(outputId = "ui_g1"),
                        div(actionButton(inputId = "submit_g1", label = "Guess"), style = "margin: 5px;"),
                        class = "input-guess"
                    )
                ),
                
                # Guess 2 ----
                hidden(div(
                    id = "div-g2",
                    div(
                        selectInput(
                            inputId = "g2", 
                            label = "Guess 2",
                            choices = NULL, 
                            width = "150px"
                        ),
                        uiOutput(outputId = "ui_g2"),
                        div(actionButton(inputId = "submit_g2", label = "Guess"), style = "margin: 5px;"),
                        class = "input-guess"
                    )
                )),
                
                # Guess 3 ----
                hidden(div(
                    id = "div-g3",
                    div(
                        selectInput(
                            inputId = "g3", 
                            label = "Guess 3",
                            choices = NULL, 
                            width = "150px"
                        ),
                        uiOutput(outputId = "ui_g3"),
                        div(actionButton(inputId = "submit_g3", label = "Guess"), style = "margin: 5px;"),
                        class = "input-guess"
                    )
                )),
                
                # Guess 4 ----
                hidden(div(
                    id = "div-g4",
                    div(
                        selectInput(
                            inputId = "g4", 
                            label = "Guess 4",
                            choices = NULL, 
                            width = "150px"
                        ),
                        uiOutput(outputId = "ui_g4"),
                        div(actionButton(inputId = "submit_g4", label = "Guess"), style = "margin: 5px;"),
                        class = "input-guess"
                    )
                )),
                
                # Guess 5 ----
                hidden(div(
                    id = "div-g5",
                    div(
                        selectInput(
                            inputId = "g5", 
                            label = "Guess 5",
                            choices = NULL, 
                            width = "150px"
                        ),
                        uiOutput(outputId = "ui_g5"),
                        div(actionButton(inputId = "submit_g5", label = "Guess"), style = "margin: 5px;"),
                        class = "input-guess"
                    )
                )),
                
                # Guess 6 ----
                hidden(div(
                    id = "div-g6",
                    div(
                        selectInput(
                            inputId = "g6", 
                            label = "Guess 6",
                            choices = NULL, 
                            width = "150px"
                        ),
                        uiOutput(outputId = "ui_g6"),
                        div(actionButton(inputId = "submit_g6", label = "Guess"), style = "margin: 5px;"),
                        class = "input-guess"
                    )
                )),
                
                class = "input-guesses"
            ),
            
            
            # UI: POSSIBILITIES ----
            div(
                DTOutput(outputId = "t_possibilities"),
                class = "possibility-table"
            ),
            
            # UI: SOLUTION ----
            div(
                id = "div-solution",
                uiOutput(outputId = "ui_solution")
            )
            
        )
        
    ),
    
    verbatimTextOutput(outputId = "test")
    
)



# SERVER =============================================================================================================================
server <- function(input, output, session) {
    
    # Testing ----
    output$test <- renderPrint({
        req(NULL)
        list(
            current_guess = rv$current_guess,
            current_possibilities = current_possibilities()
        )
    })
    
    
    # Reactive Values ----
    rv <- reactiveValues(
        submit = NULL,
        current_guess = 1,
        solution1 = NA,
        solution2 = NA,
        solution3 = NA,
        solution4 = NA,
        solution5 = NA,
        solution6 = NA,
        tests = list(),
        usage = readRDS(file = file.path("tests", "usage.rds")),
        start_time = Sys.time()
    )
    
    # observe({
    #     runjs()
    # }) %>%
    #     bindEvent(input$g1)
    
    
    
    # GUESS 1 ----
    
    # Render letter boxes
    output$ui_g1 <- renderUI({
        
        letter_boxes <- input$g1 %>%
            toupper() %>%
            string_to_vector() %>%
            imap(function(letter, i) {
                id <- paste0("g1l", i)
                
                runjs(paste0(
                    id, " = new Letter('", id, "', ", i - 1, "); ", 
                    "guess_status_1[", i - 1, "] = ", id, ".status; ",
                    "Shiny.setInputValue('g1_status', guess_status_1); "
                ))
                
                div(
                    id = id,
                    letter,
                    class = "letter s1",
                    onclick = paste0("update_status(", id, ", guess_status_1, 'g1_status');")
                )
            })
        
        div(
            letter_boxes,
            class = "guess"
        )
    })
    
    
    # Get hint
    hint1 <- reactive({
        req(length(input$g1_status) == 5)
        tibble(
            position = 1:5,
            guess = string_to_vector(input$g1),
            status = input$g1_status
        )
    })
    
    
    # Trim possible guesses based on hint
    p1 <- eventReactive(input$submit_g1, {
        
        p <- trim_possibilities(
            hint = hint1(), 
            possibilities = wordle$answers$answer, 
            status_values = c("s1", "s2", "s3")
        )
        
        if (length(p[["word"]]) == 1) {
            rv$solution1 <- p[["word"]]
        } else {
            rv$solution1 <- NA
        }
        
        return(p)
    })
    
    
    
    # GUESS 2 ----
    
    # Trim options for possible second guesses
    observeEvent(p1(), {
        if (is.na(rv$solution1)) {
            updateSelectInput(session = session, inputId = "g2", choices = p1()[["word"]])
            shinyjs::show(id = "div-g2", anim = TRUE)
        } else {
            shinyjs::hide(id = "div-g2", anim = TRUE)
        }
    })
    
    
    
    # Render letter boxes for second guess
    output$ui_g2 <- renderUI({
        letter_boxes <- input$g2 %>%
            toupper() %>%
            string_to_vector() %>%
            imap(function(letter, i) {
                id <- paste0("g2l", i)
                
                runjs(paste0(
                    id, " = new Letter('", id, "', ", i - 1, "); ", 
                    "guess_status_2[", i - 1, "] = ", id, ".status; ",
                    "Shiny.setInputValue('g2_status', guess_status_2); "
                ))
                
                div(
                    id = id,
                    letter,
                    class = "letter s1",
                    onclick = paste0("update_status(", id, ", guess_status_2, 'g2_status');")
                )
            })
        
        div(
            letter_boxes,
            class = "guess"
        )
    })
    
    
    # Get hint from second guess
    hint2 <- reactive({
        req(length(input$g2_status) == 5)
        tibble(
            position = 1:5,
            guess = string_to_vector(input$g2),
            status = input$g2_status
        )
    })
    
    
    # Trim possibilities for next answer
    p2 <- eventReactive(input$submit_g2, {
        
        p <- trim_possibilities(
            hint = hint2(), 
            possibilities = p1()[["word"]], 
            status_values = c("s1", "s2", "s3")
        )
        
        if (length(p[["word"]]) == 1) {
            rv$solution2 <- p[["word"]]
        } else {
            rv$solution2 <- NA
        }
        
        return(p)
    })
    
    
    
    # GUESS 3 ------------------------------------------------------------
    
    # Trim options for possible third guesses
    observeEvent(p2(), {
        if (is.na(rv$solution2)) {
            updateSelectInput(session = session, inputId = "g3", choices = p2()[["word"]])
            shinyjs::show(id = "div-g3", anim = TRUE)
        } else {
            shinyjs::hide(id = "div-g3", anim = TRUE)
        }
    })
    
    
    
    # Render letters for guess 3
    output$ui_g3 <- renderUI({
        
        letter_boxes <- input$g3 %>%
            toupper() %>%
            string_to_vector() %>%
            imap(function(letter, i) {
                id <- paste0("g3l", i)
                
                runjs(paste0(
                    id, " = new Letter('", id, "', ", i - 1, "); ", 
                    "guess_status_3[", i - 1, "] = ", id, ".status; ",
                    "Shiny.setInputValue('g3_status', guess_status_3); "
                ))
                
                # onclick(id = id, runjs(paste0("update_status(", id, ");"))) 
                
                div(
                    id = id,
                    letter,
                    class = "letter s1",
                    onclick = paste0("update_status(", id, ", guess_status_3, 'g3_status');")
                )
            })
        
        div(
            letter_boxes,
            class = "guess"
        )
        
    })
    
    
    hint3 <- reactive({
        req(length(input$g3_status) == 5)
        tibble(
            position = 1:5,
            guess = string_to_vector(input$g3),
            status = input$g3_status
        )
    })
    
    p3 <- eventReactive(input$submit_g3, {
        
        p <- trim_possibilities(
            hint = hint3(), 
            possibilities = p2()[["word"]], 
            status_values = c("s1", "s2", "s3")
        )
        
        if (length(p[["word"]]) == 1) {
            rv$solution3 <- p[["word"]]
        } else {
            rv$solution3 <- NA
        }
        
        return(p)
    })
    
    
    
    # GUESS 4 ------------------------------------------------------------
    
    # Trim options for possible fourth guesses
    observeEvent(p3(), {
        if (is.na(rv$solution3)) {
            updateSelectInput(session = session, inputId = "g4", choices = p3()[["word"]])
            shinyjs::show(id = "div-g4", anim = TRUE)
        } else {
            shinyjs::hide(id = "div-g4", anim = TRUE)
        }
    })
    
    
    
    # Render letters for guess 4
    output$ui_g4 <- renderUI({
        
        letter_boxes <- input$g4 %>%
            toupper() %>%
            string_to_vector() %>%
            imap(function(letter, i) {
                id <- paste0("g4l", i)
                
                runjs(paste0(
                    id, " = new Letter('", id, "', ", i - 1, "); ", 
                    "guess_status_4[", i - 1, "] = ", id, ".status; ",
                    "Shiny.setInputValue('g4_status', guess_status_4); "
                ))
                
                div(
                    id = id,
                    letter,
                    class = "letter s1",
                    onclick = paste0("update_status(", id, ", guess_status_4, 'g4_status');")
                )
            })
        
        div(
            letter_boxes,
            class = "guess"
        )
        
    })
    
    
    hint4 <- reactive({
        req(length(input$g4_status) == 5)
        tibble(
            position = 1:5,
            guess = string_to_vector(input$g4),
            status = input$g4_status
        )
    })
    
    p4 <- eventReactive(input$submit_g4, {
        
        p <- trim_possibilities(
            hint = hint4(), 
            possibilities = p3()[["word"]], 
            status_values = c("s1", "s2", "s3")
        )
        
        if (length(p[["word"]]) == 1) {
            rv$solution4 <- p[["word"]]
        } else {
            rv$solution4 <- NA
        }
        
        return(p)
    })
    
    
    
    # GUESS 5 ------------------------------------------------------------
    
    # Trim options for possible fifth guesses
    observeEvent(p4(), {
        if (is.na(rv$solution4)) {
            updateSelectInput(session = session, inputId = "g5", choices = p4()[["word"]])
            shinyjs::show(id = "div-g5", anim = TRUE)
        } else {
            shinyjs::hide(id = "div-g5", anim = TRUE)
        }
    })
    
    
    # Render letters
    output$ui_g5 <- renderUI({
        
        letter_boxes <- input$g5 %>%
            toupper() %>%
            string_to_vector() %>%
            imap(function(letter, i) {
                id <- paste0("g5l", i)
                
                runjs(paste0(
                    id, " = new Letter('", id, "', ", i - 1, "); ", 
                    "guess_status_5[", i - 1, "] = ", id, ".status; ",
                    "Shiny.setInputValue('g4_status', guess_status_5); "
                ))
                
                div(
                    id = id,
                    letter,
                    class = "letter s1",
                    onclick = paste0("update_status(", id, ", guess_status_5, 'g5_status');")
                )
            })
        
        div(
            letter_boxes,
            class = "guess"
        )
        
    })
    
    
    hint5 <- reactive({
        req(length(input$g5_status) == 5)
        tibble(
            position = 1:5,
            guess = string_to_vector(input$g5),
            status = input$g5_status
        )
    })
    
    p5 <- eventReactive(input$submit_g5, {
        
        p <- trim_possibilities(
            hint = hint5(), 
            possibilities = p4()[["word"]], 
            status_values = c("s1", "s2", "s3")
        )
        
        if (length(p[["word"]]) == 1) {
            rv$solution5 <- p[["word"]]
        } else {
            rv$solution5 <- NA
        }
        
        return(p)
    })
    
    
    
    # GUESS 6 ------------------------------------------------------------
    
    # Trim options for possible last guesses
    observeEvent(p5(), {
        if (is.na(rv$solution5)) {
            updateSelectInput(session = session, inputId = "g6", choices = p5()[["word"]])
            shinyjs::show(id = "div-g6", anim = TRUE)
        } else {
            shinyjs::hide(id = "div-g6", anim = TRUE)
        }
    })
    
    
    # Render letters
    output$ui_g6 <- renderUI({
        
        letter_boxes <- input$g6 %>%
            toupper() %>%
            string_to_vector() %>%
            imap(function(letter, i) {
                id <- paste0("g6l", i)
                
                runjs(paste0(
                    id, " = new Letter('", id, "', ", i - 1, "); ", 
                    "guess_status_6[", i - 1, "] = ", id, ".status; ",
                    "Shiny.setInputValue('g4_status', guess_status_6); "
                ))
                
                div(
                    id = id,
                    letter,
                    class = "letter s1",
                    onclick = paste0("update_status(", id, ", guess_status_6, 'g6_status');")
                )
            })
        
        div(
            letter_boxes,
            class = "guess"
        )
        
    })
    
    
    hint6 <- reactive({
        req(length(input$g6_status) == 5)
        tibble(
            position = 1:5,
            guess = string_to_vector(input$g6),
            status = input$g6_status
        )
    })
    
    p6 <- eventReactive(input$submit_g6, {
        
        p <- trim_possibilities(
            hint = hint6(), 
            possibilities = p5()[["word"]], 
            status_values = c("s1", "s2", "s3")
        )
        
        if (length(p[["word"]]) == 1) {
            rv$solution6 <- p[["word"]]
        } else {
            rv$solution6 <- NA
        }
        
        return(p)
    })
    
    
    
    # Possibilities ----

    # Get current guess    
    observeEvent(input$submit_g1, {
        rv$current_guess <- 1
    })
    
    observeEvent(input$submit_g2, {
        rv$current_guess <- 2
    })
    
    observeEvent(input$submit_g3, {
        rv$current_guess <- 3
    })
    
    observeEvent(input$submit_g4, {
        rv$current_guess <- 4
    })
    
    observeEvent(input$submit_g5, {
        rv$current_guess <- 5
    })
    
    observeEvent(input$submit_g6, {
        rv$current_guess <- 6
    })
    
    
    # Show/hide inputs based on current guess
    observeEvent(rv$current_guess, {
        max_guesses <- 6
        1:(rv$current_guess + 1) %>% map(~shinyjs::show(id = paste0("div-g", .x), anim = TRUE))
        if (rv$current_guess < (max_guesses - 1)) {
            (rv$current_guess + 2):max_guesses %>% map(~shinyjs::hide(id = paste0("div-g", .x), anim = TRUE))
            (rv$current_guess + 2):max_guesses %>% map(~updateSelectInput(session = session, inputId = paste0("g", .x), choices = NULL, selected = ""))
        }
    }, ignoreInit = TRUE)
    
    current_possibilities <- reactive({
        req(rv$current_guess)
        eval(parse(text = paste0("p", rv$current_guess, "()")))
    })
    
    output$t_possibilities <- renderDT({
        validate(need(nrow(current_possibilities()) > 0, "No possibilities found"))
        current_possibilities() %>%
            rename(Possibilities = word) %>%
            datatable(
                options = list(
                    pageLength = nrow(.),
                    dom = "t",
                    scrollX = TRUE,
                    scrollY = min(500, 45 * nrow(.)),
                    columnDefs = list(
                        list(className = "dt-center", targets = "_all")
                    )
                ), rownames = FALSE, 
                filter = "top", 
                style = "bootstrap", 
                selection = "single"
            )
    })
    
    # update current guess input based on table selection
    observeEvent(input$t_possibilities_rows_selected, {
        updateSelectInput(
            session = session, 
            inputId = paste0("g", rv$current_guess + 1), 
            selected = current_possibilities()[["word"]][input$t_possibilities_rows_selected]
        )
    })
    
    
    # Solution ----
    # test case: WHINY, ABASE (s3,s1,s1,s2,s1)
    solution <- reactive({
        solutions <- c(
            rv$solution1,
            rv$solution2,
            rv$solution3,
            rv$solution4,
            rv$solution5,
            rv$solution6
        ) %>%
            na.omit()
        if (length(solutions) > 0) {
            head(solutions, 1)
        }
    })
    
    output$ui_solution <- renderUI({
        if (!is.null(solution())) {
            letter_boxes <- solution() %>% 
                toupper() %>%
                string_to_vector() %>% 
                map(function(letter) {
                    div(letter, class = "letter s3")
                })
            div(
                div("SOLUTION", class = "solution"),
                div(letter_boxes, class = "guess"),
            )
        }
    })
    
    
    
    # Help ----
    
    observeEvent(input$help, {
        showModal(modalDialog(
            title = NULL,
            div(
                div(
                    tags$p(
                        "This is a tool to help you solve the game ", 
                        tags$a("Wordle", href = "https://www.nytimes.com/games/wordle/index.html", target = "_blank"),
                        ". For more information about Wordle, see the ",
                        tags$a("Wordle FAQ", href = "https://help.nytimes.com/hc/en-us/articles/360029050872-Word-Games-and-Logic-Puzzles#h_01FVGCB2Z00ZQMDMCYWBPWJNXB", target = "_blank"),
                        "."
                    ),
                    class = "rowc mb"
                ),
                div(
                    tags$h2("How to use the helper:"),
                    tags$ol(
                        tags$li("Select the word you guessed in Wordle."),
                        tags$li("Click on the letters to match the hint Wordle gave you."),
                        tags$li("Click on Submit hint to see possible answers."),
                        tags$li("Click on a row in the table of possibilities, or search for your next guess in the dropdown."),
                        tags$li("When there is only one possibility left, you found the answer."),
                        tags$li("If there are no possibilities in the table and you have not found the answer yet, double check that the hints match what Wordle shows.")
                    ),
                    class = "rowc mb"
                ),
                
                div(
                    tags$h3("Example:", style = "text-align: center"),
                    wordle_example(word = "spear", style = "font-size: 2em; margin-bottom: 15px"),
                    div(
                        div("E", class = "letter s3"),
                        div(" is in the answer and in the correct position"),
                        class = "rowc mb"
                    ),
                    div(
                        div("P", class = "letter s2"),
                        div(" is in the answer but is not in the correct position"),
                        class = "rowc mb"
                    ),
                    div(
                        div(
                            div("S", class = "letter s1"),
                            div("A", class = "letter s1"),
                            div("R", class = "letter s1"),
                            class = "rowc"
                        ),
                        div(" are not anywhere in the answer"),
                        class = "rowc mb"
                    )
                )
                
            ), 
            easyClose = TRUE,
            size = "m"
        ))
    })
    
    
    
    # Cache Tests ----
    
    observe({
        
        rv$tests$g1 <- list(
            guess = input$g1,
            status = input$g1_status,
            hint = hint1(),
            possibilities = p1()
        )
        
        rv$tests$g2 <- list(
            guess = input$g2,
            status = input$g2_status,
            hint = hint2(),
            possibilities = p2()
        )
        
        rv$tests$g3 <- list(
            guess = input$g3,
            status = input$g3_status,
            hint = hint3(),
            possibilities = p3()
        )
        
        rv$tests$g4 <- list(
            guess = input$g4,
            status = input$g4_status,
            hint = hint4(),
            possibilities = p4()
        )
        
    })
    
    onSessionEnded(function() {
        isolate({
            saveRDS(object = rv$tests, file = file.path("tests", "values.rds"))
            saveRDS(object = c(rv$start_time, rv$usage), file = file.path("tests", "usage.rds"))
        })
    })
    
}

shinyApp(ui, server)