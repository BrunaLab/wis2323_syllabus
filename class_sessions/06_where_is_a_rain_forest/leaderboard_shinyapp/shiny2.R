
library(shiny)
library(ggplot2)
library(tidyverse)
# path<-"class_sessions/06_where_is_a_rain_forest/leaderboard_shinyapp/shiny2.R"
# runApp(path, display.mode = "showcase")
# --- Configuration ---
groups <- c("1 name1", "2 name2", "3 name3", "4 name4", "5 name5","6 name6", "7 name7", "8 name8", "9 name9", "10 name10", "11 name11", "12 name12")
n_questions <- 15
max_points_per_question <- 10  # Adjust as needed




scores_file <- "class_sessions/06_where_is_a_rain_forest/leaderboard_shinyapp/quiz_scores.csv"

# Load scores from CSV on startup if it exists
scores <- reactiveVal({
  if (file.exists(scores_file)) {
    mat <- as.matrix(read.csv(scores_file, row.names = 1))
    colnames(mat) <- paste0("Q", 1:n_questions)
    mat
  } else {
    mat <- matrix(0, nrow = length(groups), ncol = n_questions)
    rownames(mat) <- groups
    colnames(mat) <- paste0("Q", 1:n_questions)
    mat
  }
})

# Infer current question from saved data on startup
current_q <- reactiveVal({
  if (file.exists(scores_file)) {
    mat <- as.matrix(read.csv(scores_file, row.names = 1))
    # Find the first column that is all zeros
    answered <- which(apply(mat, 2, function(col) any(col != 0)))
    if (length(answered) == 0) 1 else max(answered) + 1
  } else {
    1
  }
})








ui <- fluidPage(
  titlePanel("Quiz Score Tracker"),
  sidebarLayout(
    sidebarPanel(
      width = 1,  # Add this line — default is 4
      h4(textOutput("question_label")),
      lapply(groups, function(s) {
        numericInput(
          inputId = paste0("score_", gsub(" ", "_", s)),
          label = s,
          value = 0,
          min = 0,
          max = max_points_per_question
        )
      }),
      br(),
      actionButton("submit_question", "Submit Question", class = "btn-primary"),
      br(), br(),
      # actionButton("reset", "Reset All", class = "btn-danger"),
      # br(), br(),
      uiOutput("progress_text")
    ),
    mainPanel(
      width = 11,  # Add this line — default is 4
      plotOutput("score_plot", height = "400px"),
      br(),
      tableOutput("score_table")
    )
  )
)

server <- function(input, output, session) {
  
  
  # Label for current question
  output$question_label <- renderText({
    if (current_q() <= n_questions) {
      paste("Enter points for Question", current_q())
    } else {
      "All questions complete!"
    }
  })
  
  # Progress indicator
  output$progress_text <- renderUI({
    q <- current_q()
    if (q <= n_questions) {
      tagList(
        p(paste("Question", q, "of", n_questions)),
        div(
          style = "background:#ddd; border-radius:4px;",
          div(
            style = paste0(
              "width:", round((q - 1) / n_questions * 100), "%;",
              "background:#0021A5; height:12px; border-radius:4px;"
            )
          )
        )
      )
    } else {
      p(strong("Quiz complete! 🎉"), style = "color:green;")
    }
  })
  
  # Submit question scores
  observeEvent(input$submit_question, {
    
    q <- current_q()
    if (q > n_questions) return()
    
    mat <- scores()
    for (s in groups) {
      id <- paste0("score_", gsub(" ", "_", s))
      mat[s, q] <- input[[id]]
    }
    scores(mat)
    write.csv(mat, scores_file)
    # Reset inputs to 0 for next question
    for (s in groups) {
      id <- paste0("score_", gsub(" ", "_", s))
      updateNumericInput(session, id, value = 0)
    }
    
    current_q(q + 1)
  })
  
  # Reset everything
  observeEvent(input$reset, {
    mat <- matrix(0, nrow = length(groups), ncol = n_questions)
    rownames(mat) <- groups
    scores(mat)
    write.csv(mat, scores_file)
    current_q(1)
    for (s in groups) {
      id <- paste0("score_", gsub(" ", "_", s))
      updateNumericInput(session, id, value = 0)
    }
  })
  
  # Bar chart of total scores
  output$score_plot <- renderPlot({
    mat <- scores()
    totals <- rowSums(mat)
    df <- data.frame(Group = groups, Total = totals)
    df<- df |> 
      mutate(Group=ordered(Group, levels = groups))
    
    ggplot(df, aes(x = Group, y = Total, fill = Group)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      geom_text(aes(label = Total), vjust = -0.5, size = 5) +
      ylim(0, n_questions * max_points_per_question * 1.1) +
      labs(
        title = paste("Scores after", max(0, current_q() - 1), "of", n_questions, "questions"),
        x = NULL, y = "Total Points"
      ) +
      # scale_fill_brewer(palette = "Set2") +
      theme_minimal(base_size = 14)+
      coord_flip()
  })
  
  
  # Score table
  # output$score_table <- renderTable({
  #   mat <- scores()
  #   df <- as.data.frame(mat)
  #   colnames(df) <- paste0("Q", 1:n_questions)
  #   df$Total <- rowSums(mat)
  #   df$Group <- groups
  #   df[, c("Group", paste0("Q", 1:n_questions), "Total")]
  # })
}

shinyApp(ui = ui, server = server)