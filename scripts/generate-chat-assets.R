suppressWarnings(suppressPackageStartupMessages({
  library(bslib)
  library(ellmer)
  library(htmltools)
  library(shiny)
  library(shinychat)
  library(webshot2)
}))

asset_dir <- file.path("assets", "generated")
if (!dir.exists(asset_dir)) {
  dir.create(asset_dir, recursive = TRUE)
}
stopifnot(dir.exists(asset_dir))

tool_call <- function(id, call) {
  stopifnot(
    length(id) == 1,
    is.list(call),
    length(call$name) == 1,
    is.list(call$arguments),
    length(call$intent) == 1
  )

  request <- ContentToolRequest(
    id = id,
    name = call$name,
    arguments = call$arguments
  )
  card <- contents_shinychat(
    ContentToolResult(
      value = "",
      request = request
    )
  )
  card$intent <- call$intent
  card$expanded <- TRUE
  card$show_request <- TRUE
  as.tags(card)
}

bash_command <- function(command) {
  stopifnot(length(command) == 1)
  list(name = "bash", arguments = list(command = command), intent = command)
}

edit_file_command <- function(path, instruction) {
  stopifnot(length(path) == 1, length(instruction) == 1)
  list(
    name = "edit_file",
    arguments = list(path = path, instruction = instruction),
    intent = paste(path, instruction, sep = "\n")
  )
}

chat_tool <- function(call) {
  stopifnot(is.list(call))
  list(type = "tool", call = call)
}

chat_note <- function(text) {
  stopifnot(length(text) == 1)
  list(type = "note", text = text)
}

render_chat <- function(filename, assistant_text, steps) {
  stopifnot(
    length(filename) == 1,
    length(assistant_text) == 1,
    is.list(steps),
    length(steps) >= 1
  )

  rendered_steps <- list()
  tool_index <- 0
  for (step in steps) {
    stopifnot(is.list(step), length(step$type) == 1)
    if (identical(step$type, "tool")) {
      tool_index <- tool_index + 1
      rendered_steps <- c(
        rendered_steps,
        list(tool_call(paste0("call_", tool_index), step$call))
      )
    } else if (identical(step$type, "note")) {
      rendered_steps <- c(
        rendered_steps,
        list(div(class = "reasoning-note", tags$p(step$text)))
      )
    } else {
      stop("unsupported chat step type: ", step$type)
    }
  }

  messages <- list(
    list(
      role = "user",
      content = "Explore the `sales.csv` dataset, tell me something new and interesting about it."
    ),
    list(
      role = "assistant",
      content = do.call(
        tagList,
        c(
          list(tags$p(assistant_text)),
          rendered_steps
        )
      )
    )
  )

  ui <- page_fillable(
    theme = bs_theme(
      bg = "#141821",
      fg = "#f5f7fb",
      primary = "#315cf6"
    ),
    tags$style(HTML(
      "
      body {
        margin: 0;
        background: #141821;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
      }
      .shot-stage {
        min-height: 100vh;
        display: grid;
        place-items: center;
        padding: 24px;
      }
      .chat-card {
        width: 780px;
        height: 640px;
        background: #f7f8fb;
        border: 1px solid #dce2ec;
        border-radius: 8px;
        padding: 24px;
        box-shadow: 0 30px 90px rgba(0, 0, 0, .30);
      }
      .chat-card shiny-chat-container {
        --bs-body-bg: #f7f8fb;
        --bs-body-color: #172033;
        --bs-body-color-rgb: 23, 32, 51;
        --bs-secondary-color: #626b7a;
        --bs-border-color: #dce2ec;
        --bs-primary: #315cf6;
        --bs-primary-rgb: 49, 92, 246;
        --bs-code-color: #173a9d;
        --shiny-chat-user-message-bg: #315cf6;
        height: 590px;
        color: #172033;
      }
      .chat-card .shiny-chat-user-message {
        color: #ffffff;
      }
      .chat-card .shiny-chat-message:not(.shiny-chat-user-message) {
        color: #172033;
      }
      .chat-card code {
        color: #173a9d;
        background: #edf1ff;
        border-radius: 5px;
        padding: 2px 6px;
      }
      .chat-card shiny-tool-request,
      .chat-card shiny-tool-result {
        margin: .5rem 0;
        font-size: .88rem;
      }
      .chat-card .tool-intent {
        color: #334155;
        font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
        font-size: .82rem;
        font-style: normal;
        opacity: .9;
        white-space: pre-line;
      }
      .chat-card .reasoning-note {
        border-left: 3px solid #aab7cf;
        color: #425069;
        margin: .55rem 0;
        padding: .05rem 0 .05rem .75rem;
      }
      .chat-card .reasoning-note p {
        font-size: .9rem;
        margin: 0;
      }
    "
    )),
    div(
      class = "shot-stage",
      div(
        class = "chat-card",
        chat_ui(
          "chat",
          messages = messages,
          placeholder = "Ask about the R project...",
          width = "100%",
          height = "590px",
          fill = FALSE
        )
      )
    )
  )

  appshot(
    shinyApp(ui, function(input, output, session) {}),
    file = file.path(asset_dir, filename),
    vwidth = 900,
    vheight = 740,
    selector = ".chat-card",
    expand = 4,
    delay = 1
  )
}


inline_command <- paste(
  'Rscript -e "library(readr); library(dplyr);"',
  "df <- read_csv('sales.csv');",
  "glimpse(df);",
  "summary(df$revenue)"
)

heredoc_command <- paste(
  c(
    "cat <<'EOF' > /tmp/analysis.R",
    "library(readr)",
    "library(dplyr)",
    "df <- read_csv('sales.csv')",
    "df <- filter(df, !is.na(revenue))",
    "print(glimpse(df))",
    "EOF",
    "Rscript /tmp/analysis.R"
  ),
  collapse = "\n"
)

render_chat(
  "chat-rscript-e.png",
  "I'll inspect the data first, so I'll send R a quick one-off command.",
  list(chat_tool(bash_command(inline_command)))
)

render_chat(
  "chat-heredoc-script.png",
  "I'll inspect the data first, so I'll send R a quick one-off command.",
  list(
    chat_tool(bash_command(inline_command)),
    chat_note(
      "The result gives enough context to take the next step. This is turning into reusable setup, so I should move it into a script."
    ),
    chat_tool(bash_command(heredoc_command))
  )
)

render_chat(
  "chat-rerun-loop.png",
  "Now I'll iterate: change the file, run it, inspect the result, repeat.",
  list(
    chat_tool(bash_command("Rscript /tmp/analysis.R")),
    chat_tool(edit_file_command(
      "/tmp/analysis.R",
      "Add model diagnostics."
    )),
    chat_tool(bash_command("Rscript /tmp/analysis.R"))
  )
)
