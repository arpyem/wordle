library(tidyverse)
library(shiny)
library(shinyjs)
library(DT)


# Data
wordle <- readRDS(file = file.path("data", "wordle.rds"))


# Functions 
source("functions.R")


# TODO check if there is only one possibility and show as answer


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
            
            # Guesses
            div(
                
                # Guess 1 ----
                div(
                    id = "div-g1",
                    
                    div(
                        div(
                            selectInput(
                                inputId = "g1", 
                                label = "Guess 1",
                                choices = sort(c(wordle$words$allowed, wordle$answers$answer)), 
                                selected = sample(wordle$answers$answer, 1),
                                width = "100%"
                            ),
                            style = "width: 10%; min-width: 150px; max-width: 300px"
                        ),
                        class = "rowc"
                    ),
                    
                    uiOutput(outputId = "ui_g1"),
                    
                    div(
                        actionButton(inputId = "submit_g1", label = "Submit hint"),
                        class = "rowc mb"
                    ),
                    
                    div(
                        div(
                            DTOutput(outputId = "t1"),
                            class = "possibility-table"
                        ),
                        class = "rowc mb"
                    ),
                    
                    class = "mb"
                ),
                
                # Guess 2 ----
                div(
                    id = "div-g2",
                    
                    div(
                        div(
                            selectInput(
                                inputId = "g2", 
                                label = "Guess 2",
                                choices = NULL, 
                                width = "100%"
                            ),
                            style = "width: 10%; min-width: 150px; max-width: 300px"
                        ),
                        class = "rowc"
                    ),
                    
                    uiOutput(outputId = "ui_g2"),
                    
                    div(
                        actionButton(inputId = "submit_g2", label = "Submit hint"),
                        class = "rowc mb"
                    ),
                    
                    div(
                        id = "div-t2",
                        div(
                            DTOutput(outputId = "t2"),
                            class = "possibility-table"
                        ),
                        class = "rowc mb hidden"
                    ),
                    
                    class = "hidden mb"
                ),
                
                # Guess 3 ----
                div(
                    id = "div-g3",
                    
                    div(
                        div(
                            selectInput(
                                inputId = "g3", 
                                label = "Guess 3",
                                choices = NULL, 
                                width = "100%"
                            ),
                            style = "width: 10%; min-width: 150px; max-width: 300px"
                        ),
                        class = "rowc"
                    ),
                    
                    uiOutput(outputId = "ui_g3"),
                    
                    div(
                        actionButton(inputId = "submit_g3", label = "Submit hint"),
                        class = "rowc mb"
                    ),
                    
                    div(
                        id = "div-t3",
                        div(
                            DTOutput(outputId = "t3"),
                            class = "possibility-table"
                        ),
                        class = "rowc mb hidden"
                    ),
                    
                    class = "hidden mb"
                ),
                
                # Guess 4 ----
                div(
                    id = "div-g4",
                    
                    div(
                        div(
                            selectInput(
                                inputId = "g4", 
                                label = "Guess 4",
                                choices = NULL, 
                                width = "100%"
                            ),
                            style = "width: 10%; min-width: 150px; max-width: 300px"
                        ),
                        class = "rowc"
                    ),
                    
                    uiOutput(outputId = "ui_g4"),
                    
                    div(
                        actionButton(inputId = "submit_g4", label = "Submit hint"),
                        class = "rowc mb"
                    ),
                    div(
                        id = "div-t4",
                        div(
                            DTOutput(outputId = "t4"),
                            class = "possibility-table"
                        ),
                        class = "rowc mb hidden"
                    ),
                    
                    class = "hidden mb"
                )
                
            ),
            
            # Progress ----
            div(
                uiOutput(outputId = "ui_progress"),
                class = "guess-progress"
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
        # list(
        #     guess_1 = input$g1,
        #     status_1 = input$g1_status,
        #     hint1 = hint1(),
        #     p1 = p1(),
        #     guess_2 = input$g2,
        #     status_2 = input$g2_status,
        #     hint2 = hint2(),
        #     p2 = p2(),
        #     guess_3 = input$g3,
        #     status_3 = input$g3_status,
        #     guess_4 = input$g4,
        #     status_4 = input$g4_status
        # )
        all_guesses()
    })
    
    
    # Reactive Values ----
    rv <- reactiveValues()
    
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
            class = "guess mb"
        )
    })
    
    
    # Get hint
    hint1 <- reactive({
        req(input$g1_status)
        tibble(
            position = 1:5,
            guess = string_to_vector(input$g1),
            status = factor(input$g1_status)
        )
    })
    
    
    # Trim possible guesses based on hint
    p1 <- eventReactive(input$submit_g1, {
        message("finding possible answers")
        trim_possibilities(
            hint = hint1(), 
            possibilities = wordle$answers$answer, 
            status_values = c("s1", "s2", "s3")
        )
    })
    
    
    # Render table of possible second guesses
    output$t1 <- renderDT({
        p1() %>%
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
    
    
    
    # GUESS 2 ----
    
    # Trim options for possible second guesses
    observeEvent(p1(), {
        removeCssClass(id = "div-g2", class = "hidden")
        removeCssClass(id = "div-t2", class = "hidden")
        updateSelectInput(session = session, inputId = "g2", choices = p1()[["word"]])
    })
    
    # Update selected guess if a row in the table is clicked
    observeEvent(input$t1_rows_selected, {
        selected <- p1()[["word"]][input$t1_rows_selected]
        updateSelectInput(session = session, inputId = "g2", selected = selected)
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
            class = "guess mb"
        )
    })
    
    
    # Get hint from second guess
    hint2 <- reactive({
        req(input$g2_status)
        tibble(
            position = 1:5,
            guess = string_to_vector(input$g2),
            status = factor(input$g2_status)
        )
    })
    
    
    # Trim possibilities for next answer
    p2 <- eventReactive(input$submit_g2, {
        message("finding possible second answers")
        trim_possibilities(
            hint = hint2(), 
            possibilities = p1()[["word"]], 
            status_values = c("s1", "s2", "s3")
        )
    })
    
    
    # Render possibility table
    output$t2 <- renderDT({
        p2() %>%
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
    
    
    # GUESS 3 ------------------------------------------------------------
    
    # Trim options for possible third guesses
    observeEvent(p2(), {
        removeCssClass(id = "div-g3", class = "hidden")
        removeCssClass(id = "div-t3", class = "hidden")
        updateSelectInput(session = session, inputId = "g3", choices = p2()[["word"]])
    })
    
    # Update selected guess if a row in the table is clicked
    observeEvent(input$t2_rows_selected, {
        selected <- p2()[["word"]][input$t2_rows_selected]
        updateSelectInput(session = session, inputId = "g3", selected = selected)
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
            class = "guess mb"
        )
        
    })
    
    
    hint3 <- reactive({
        req(input$g3_status)
        tibble(
            position = 1:5,
            guess = string_to_vector(input$g3),
            status = factor(input$g3_status)
        )
    })
    
    p3 <- eventReactive(input$submit_g3, {
        message("finding possible third answers")
        trim_possibilities(
            hint = hint3(), 
            possibilities = p2()[["word"]], 
            status_values = c("s1", "s2", "s3")
        )
    })
    
    output$t3 <- renderDT({
        p3() %>%
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
    
    
    # GUESS 4 ------------------------------------------------------------
    
    # Trim options for possible fourth guesses
    observeEvent(p3(), {
        removeCssClass(id = "div-g4", class = "hidden")
        removeCssClass(id = "div-t4", class = "hidden")
        updateSelectInput(session = session, inputId = "g4", choices = p3()[["word"]])
    })
    
    # Update selected guess if a row in the table is clicked
    observeEvent(input$t3_rows_selected, {
        selected <- p3()[["word"]][input$t3_rows_selected]
        updateSelectInput(session = session, inputId = "g4", selected = selected)
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
            class = "guess mb"
        )
        
    })
    
    
    hint4 <- reactive({
        req(input$g4_status)
        tibble(
            position = 1:5,
            guess = string_to_vector(input$g4),
            status = factor(input$g4_status)
        )
    })
    
    p4 <- eventReactive(input$submit_g4, {
        message("finding possible fourth answers")
        trim_possibilities(
            hint = hint4(), 
            possibilities = p3()[["word"]], 
            status_values = c("s1", "s2", "s3")
        )
    })
    
    output$t4 <- renderDT({
        p4() %>%
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
    
    
    # PROGRESS ----
    
    # All guesses
    all_guesses <- eventReactive(c(input$submit_g1, input$submit_g2, input$submit_g3, input$submit_g4), {
        list(
            g1 = list(
                letters = string_to_vector(input$g1),
                status = input$g1_status
            ),
            g2 = list(
                letters = string_to_vector(input$g2),
                status = input$g2_status
            ),
            g3 = list(
                letters = string_to_vector(input$g3),
                status = input$g3_status
            ),
            g4 = list(
                letters = string_to_vector(input$g4),
                status = input$g4_status
            )
        )
    })
    
    
    output$ui_progress <- renderUI({
        all_guesses() %>%
            map(function(guess) {
                letter_boxes <- map2(guess$letters, guess$status, function(letter, status) {
                    div(
                        toupper(letter),
                        class = paste("letter", status)
                    )
                })
                
                div(letter_boxes, class = "guess")
            })
    })
    
    
    # Help ----
    
    observeEvent(input$help, {
        showModal(modalDialog(
            title = "Wordle Helper",
            div(
                div(
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
                    div("Correct letter and position", class = "s-info s3"),
                    div("Correct letter, incorrect position", class = "s-info s2"),
                    div("Incorrect letter and position", class = "s-info s1"),
                    class = "rowc mb"
                ),
            ), 
            easyClose = TRUE,
            size = "m"
        ))
    })
    
    
}

shinyApp(ui, server)