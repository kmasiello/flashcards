library(shiny)
library(bslib)
library(reactable)

# Read vocabulary from CSV file
read_vocabulary <- function() {
  if (file.exists("vocabulary.csv")) {
    vocab <- read.csv("vocabulary.csv", stringsAsFactors = FALSE)
    required_cols <- c("word", "definition", "synonyms", "example")
    if (all(required_cols %in% colnames(vocab))) {
      return(vocab)
    } else {
      stop("CSV must have columns: word, definition, synonyms, example")
    }
  } else {
    sample_vocab <- data.frame(
      word = c(
        "serendipity",
        "ubiquitous",
        "ephemeral",
        "ameliorate",
        "perspicacious"
      ),
      definition = c(
        "The occurrence of events by chance in a happy way",
        "Present, appearing, or found everywhere",
        "Lasting for a very short time",
        "To make something better; improve",
        "Having keen insight; mentally sharp"
      ),
      synonyms = c(
        "chance, luck, fortune",
        "widespread, universal, prevalent",
        "temporary, fleeting, brief",
        "improve, enhance, better",
        "insightful, perceptive, astute"
      ),
      example = c(
        "Finding my lost keys while looking for my phone was pure serendipity.",
        "Smartphones have become ubiquitous in modern society.",
        "The beauty of cherry blossoms is ephemeral, lasting only a few weeks.",
        "The new policies were designed to ameliorate working conditions.",
        "Her perspicacious analysis revealed the hidden flaws in the plan."
      ),
      stringsAsFactors = FALSE
    )
    write.csv(sample_vocab, "vocabulary.csv", row.names = FALSE)
    return(sample_vocab)
  }
}

vocabulary <- read_vocabulary()

ui <- page_sidebar(
  title = "Vocabulary Flashcards",
  theme = bs_theme(preset = "bootstrap"),

  sidebar = sidebar(
    width = 300,
    h4("Word → Definition Mode"),
    p("Click 'Show Definition' to reveal the definition and synonyms."),
    br(),
    actionButton("new_card", "New Card", class = "btn-primary w-100"),
    actionButton("shuffle", "Shuffle Deck", class = "btn-info w-100 mt-2"),
    br(),
    br(),
    h5("Progress"),
    textOutput("progress_text"),
    br(),
    h5("Quick Stats"),
    p(paste("Total words:", nrow(vocabulary))),
    textOutput("cards_studied")
  ),

  # Main flashcard
  card(
    full_screen = TRUE,
    card_header("Flashcard"),
    div(
      style = "min-height: 400px; padding: 20px; text-align: center;",

      h2(
        textOutput("current_word"),
        style = "color: #495057; margin-bottom: 40px;"
      ),

      div(
        style = "margin-bottom: 30px;",
        conditionalPanel(
          condition = "!input.definition_revealed",
          actionButton(
            "reveal",
            "Show Definition",
            class = "btn-outline-primary btn-lg"
          )
        ),
        conditionalPanel(
          condition = "input.definition_revealed",
          actionButton("correct", "✓ Got it!", class = "btn-success me-3"),
          actionButton("incorrect", "✗ Keep studying", class = "btn-warning")
        )
      ),

      conditionalPanel(
        condition = "input.definition_revealed",
        div(
          style = "border: 2px solid #dee2e6; border-radius: 10px; padding: 20px; margin: 20px 0; text-align: left;",
          h5("Definition:"),
          p(
            textOutput("definition_text"),
            style = "font-style: italic; margin-bottom: 15px;"
          ),
          h5("Synonyms:"),
          p(
            textOutput("synonyms_text"),
            style = "color: #666; margin-bottom: 15px;"
          ),
          h5("Example Usage:"),
          p(
            textOutput("example_text"),
            style = "color: #495057; font-style: italic;"
          )
        )
      ),

      # Hidden input to track state
      checkboxInput("definition_revealed", "", FALSE, width = "0px") |>
        tagAppendAttributes(style = "position: absolute; left: -9999px;")
    )
  ),

  # Collapsible word list below
  accordion(
    accordion_panel(
      "Word List",
      div(
        style = "max-height: 400px; overflow-y: auto;",
        reactableOutput("word_table")
      )
    ),
    open = FALSE
  )
)

server <- function(input, output, session) {
  values <- reactiveValues(
    deck = sample(nrow(vocabulary)),
    position = 0,
    cards_studied = 0,
    correct_count = 0,
    current_index = NULL
  )

  get_new_card <- function() {
    if (values$position >= length(values$deck)) {
      values$deck <- sample(nrow(vocabulary))
      values$position <- 1
    } else {
      values$position <- values$position + 1
    }
    values$current_index <- values$deck[values$position]
    updateCheckboxInput(session, "definition_revealed", value = FALSE)
  }

  observeEvent(input$new_card, {
    get_new_card()
  })

  observeEvent(input$reveal, {
    updateCheckboxInput(session, "definition_revealed", value = TRUE)
  })

  observeEvent(input$shuffle, {
    values$deck <- sample(nrow(vocabulary))
    values$position <- 0
    showNotification("Deck shuffled!", type = "message")
  })

  observeEvent(input$incorrect, {
    current_word <- vocabulary$word[values$current_index]
    values$cards_studied <- values$cards_studied + 1
    showNotification(
      paste("Keep studying", current_word, "📚"),
      type = "warning"
    )
    get_new_card()
  })

  observeEvent(input$correct, {
    current_word <- vocabulary$word[values$current_index]
    values$cards_studied <- values$cards_studied + 1
    values$correct_count <- values$correct_count + 1
    showNotification(
      paste0("Good job on ", current_word, "! 🚀"),
      type = "warning"
    )
    get_new_card()
  })

  output$current_word <- renderText({
    if (!is.null(values$current_index)) {
      vocabulary$word[values$current_index]
    } else {
      ""
    }
  })

  output$definition_text <- renderText({
    if (!is.null(values$current_index)) {
      vocabulary$definition[values$current_index]
    } else {
      ""
    }
  })

  output$synonyms_text <- renderText({
    if (!is.null(values$current_index)) {
      vocabulary$synonyms[values$current_index]
    } else {
      ""
    }
  })

  output$example_text <- renderText({
    if (!is.null(values$current_index)) {
      vocabulary$example[values$current_index]
    } else {
      ""
    }
  })

  output$progress_text <- renderText({
    if (values$cards_studied > 0) {
      accuracy <- round(values$correct_count / values$cards_studied * 100)
      paste0(
        "Accuracy: ",
        accuracy,
        "% (",
        values$correct_count,
        "/",
        values$cards_studied,
        ")"
      )
    } else {
      "No cards studied yet"
    }
  })

  output$cards_studied <- renderText({
    paste("Cards studied:", values$cards_studied)
  })

  output$word_table <- renderReactable({
    reactable(
      vocabulary[, c("word", "definition", "synonyms")],
      searchable = TRUE,
      sortable = TRUE,
      striped = TRUE,
      highlight = TRUE,
      bordered = TRUE,
      compact = TRUE,
      defaultPageSize = 10,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(5, 10, 15, 20),
      columns = list(
        word = colDef(
          name = "Word",
          minWidth = 120,
          style = list(fontWeight = "bold")
        ),
        definition = colDef(
          name = "Definition",
          minWidth = 200,
          style = list(fontSize = "14px")
        ),
        synonyms = colDef(
          name = "Synonyms",
          minWidth = 150,
          style = list(fontSize = "14px", color = "#666")
        )
      )
    )
  })

  observeEvent(
    TRUE,
    {
      get_new_card()
    },
    once = TRUE
  )
}

shinyApp(ui, server)
