pacman::p_load(
  shiny, shinyjs, shinyFiles, shinyalert, shinyWidgets, shinydashboard, shinydashboardPlus, shinyStorePlus,
  httr, rsvg, waiter, htmltools, magrittr, rclipboard, gptchatteR, DiagrammeR, DiagrammeRsvg
)

waiting_screen_1 <- tagList(
  spin_flower(),
  h4("DiAGrammy is analyzing your request...")
)

waiting_screen_2 <- tagList(
  spin_flower(),
  h4("DiAGrammy is coding your diagram using DiagrammeR...")
)

waiting_screen_3 <- tagList(
  spin_flower(),
  h4("DiAGrammy is editing your diagram...")
)

shinyApp(
  ui = dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE, collapsed = TRUE, minified = FALSE, width = "0px"),
    dashboardBody(
      initStore(),
      useShinyjs(),
      rclipboardSetup(),
      useWaiter(),
      fluidRow(
        div(
          style = "display: flex; justify-content: center; align-items: center; height: 100%; margin-bottom: 15px;",
          includeHTML("logo.svg")
        ),
        box(
          title = "Request a Diagram", status = "primary", solidHeader = TRUE,
          column(
            width = 12,
            div(
              style = "display: flex; flex-direction: column; margin-bottom: 10px;",
              tags$label("OpenAI API key:"),
              passwordInput(
                inputId = "API",
                placeholder = ""
              ),
              tags$small(
                style = "color: grey; margin-top: 5px; margin-bottom: 5px;",
                HTML("Visit the <a href='https://platform.openai.com/account/api-keys' target='_blank'>OpenAI website</a> to manage your API keys.")
              )
            )
          ),
          column(
            width = 12,
            div(
              style = "display: flex; flex-direction: column; margin-bottom: 10px;",
              tags$label("What type of diagram are you looking for?"),
              tags$textarea(
                id = "complete",
                style = "margin-bottom: 0; padding: 5px;",
                placeholder = "e.g. history of the universe from the big bang to humans with dates [10]",
                title = "Enter the type of diagram you are looking for here"
              ),
              tags$small(
                style = "color: grey; margin-top: 5px; margin-bottom: 5px;",
                "[n] is the number of nodes (default is < 10)."
              )
            )
          ),
          materialSwitch(inputId = "edit", label = "Edit mode", status = "primary", value = FALSE),
          actionBttn(
            inputId = "gpt",
            label = "Request",
            color = "success",
            style = "material-flat",
            icon = icon("paper-plane"),
            block = TRUE
          ),
          sidebar = boxSidebar(
            id = "controls",
            selectInput("prompt", "Diagram template:", choices = c("specify nodes", "freeform", "linear", "heirarchical", "cyclical", "mediation")),
            sliderInput("temperature", "Model temperature:",
              min = 0, max = 1, step = 0.1, value = 0.5
            ),
            sliderInput("max_tokens", "Max tokens:",
              min = 500, max = 4000, step = 100, value = 1000
            ),
          ),
          width = 12
        ),
      ),
      fluidRow(
        userBox(
          title = userDescription(
            title = "DiAGrammy",
            subtitle = "The bot who turns your words into diagrams!",
            type = 2,
            image = "https://cdn-icons-png.flaticon.com/128/3398/3398643.png"
          ),
          width = 12,
          status = "primary",
          boxToolSize = "lg",
          maximizable = TRUE,
          "Don't mind me, I'm just dreaming about diagrams.",
          htmlOutput("diagram"),
          verbatimTextOutput("code"),
          hidden(uiOutput("clip")),
          hidden(downloadButton(outputId = "down", label = "PNG")),
          footer = tags$a("Please visit my repository to learn more.", href = "https://github.com/dylanpieper/DiAGrammy")
        ),
      ),
    ),
    title = "DiAGrammy"
  ),
  server = function(input, output, session) {
    myString <- reactiveValues(output = NULL)

    observeEvent(input$edit, {
      if (input$edit) {
        updateTextInput(session, "complete", placeholder = "e.g., make the nodes a different shape")
      } else {
        updateTextInput(session, "complete", placeholder = "e.g. history of the universe from the big bang to humans with dates [10]")
      }
    })

    observeEvent(input$gpt, {
      if (input$edit) {
        if (is.null(input$API) || input$API == "") {
          showModal(modalDialog(
            title = "Error",
            "Please provide an OpenAI API key.",
            easyClose = TRUE,
            footer = tagList(
              actionButton("ok", "OK", onclick = "Shiny.onInputChange('modal_close', true)")
            )
          ))
        } else {
          waiter_show(html = waiting_screen_3, color = "black")

          res <- httr::GET("https://api.openai.com/v1/engines/davinci/completions", add_headers(Authorization = paste0("Bearer ", input$API)))

          if (res$status_code == 401) {
            waiter_hide()

            output$diagram <- renderUI(HTML("<br> <b><p style='color: red;'>Sorry, your API key was invalid. Please try again.</p></b>"))
          } else {
            prompt_1 <- paste("Act as a graphviz DOT formatter, stylist, and colorizer. Use the `DiagrammeR::grViz` function in R to edit a simple diagram and make it beautiful - specifically,", input$complete, ". Print one line of code in one markdown source pane with no comments, headers, or context. Do not add } after \") at the end of the code.")

            chatter.create(max_tokens = as.integer(input$max_tokens), temperature = as.integer(input$temperature))
            completion_1 <- chatter.chat(paste(prompt_1, myString$output), return_response = TRUE)
            completion_1_extract <- completion_1$choices[[1]]
            completion_1_clean <- gsub("```|\\{r\\}|library\\(DiagrammeR\\)|<pre>|\\n", "", completion_1_extract)

            error <- try(eval(parse(text = completion_1_clean)), silent = TRUE)

            if (any(class(error) == "try-error")) {
              output$diagram <- renderUI(HTML("<br> <b><p style='color: red;'>Sorry, I wrote R code that won't run. Please try again.</p></b>"))
              output$code <- renderText(paste(parse(text = completion_1_clean)))
            } else {
              output$diagram <- renderUI(eval(parse(text = completion_1_clean)))
              output$code <- renderText(paste(parse(text = completion_1_clean)))
            }

            show("clip")
            show("down")

            output$clip <- renderUI({
              rclipButton(
                inputId = "clipbtn",
                label = "Copy code",
                clipText = parse(text = completion_1_clean),
                icon = icon("clipboard")
              )
            })

            output$down <- downloadHandler(
              filename = paste("DiAGrammy", Sys.time(), ".png"),
              content = function(file) {
                graph <- eval(parse(text = completion_1_clean))
                graph %>%
                  export_svg() %>%
                  charToRaw() %>%
                  rsvg_png(file)
              }
            )

            waiter_hide()
          }
        }
      } else {
        if (is.null(input$API) || input$API == "") {
          showModal(modalDialog(
            title = "Error",
            "Please provide an OpenAI API key.",
            easyClose = TRUE,
            footer = tagList(
              actionButton("ok", "OK", onclick = "Shiny.onInputChange('modal_close', true)")
            )
          ))
        } else {
          waiter_show(html = waiting_screen_1, color = "black")

          res <- httr::GET("https://api.openai.com/v1/engines/davinci/completions", add_headers(Authorization = paste0("Bearer ", input$API)))

          if (res$status_code == 401) {
            waiter_hide()

            output$diagram <- renderUI(HTML("<br> <b><p style='color: red;'>Sorry, your API key was invalid. Please try again.</p></b>"))
          } else {
            chatter.auth(input$API)

            chatter.create(max_tokens = as.integer(input$max_tokens), temperature = as.integer(input$temperature))

            prompt_1 <- "Describe a directed acyclic graph based on a user’s topic. The description should have at least n nodes. I specify n in my input by writing [n], less than 10 being the default value. Connect the nodes in the system and explain the directionality between them in excruciating detail. Reiterate the user's topic and n nodes (example: \"topic [n]\")."

            if (input$prompt == "specify nodes") {
              prompt_2 <- "Act as a Graphviz DOT generator who creates meaningful diagrams. Use the `DiagrammeR::grViz` function in R. The diagram should have at least n nodes. I specify n in my input by writing [n], less than 10 being the default value (example: \"The water cycle [8]\"). Each node is indexed by a number to reduce the size of the output with no styling. The code should be valid, bugless. Print one line of code in one markdown source pane with no comments, headers, or context. Format node labels with no hyphens, punctuation, or special characters. Example code: grViz(\"digraph model { ... }\")"
            } else if (input$prompt == "freeform") {
              prompt_2 <- "Use the `DiagrammeR::grViz` function in R to code meaningful diagrams. Print one line of code in one markdown source pane with no comments, headers, or context. Format node labels with no hyphens, punctuation, or special characters. Make the text readable. Example: grViz(\"digraph model { ... }\")"
            } else if (input$prompt == "linear") {
              prompt_2 <- "Use the `DiagrammeR::grViz` function in R to code meaningful linear diagrams. Print one line of code in one markdown source pane with no comments, headers, or context. Format node labels with no hyphens, punctuation, or special characters. Make the text readable. Example: grViz(\"digraph linear_model {  graph[layout = dot]    node[shape = box, fontsize = 14]  example[label = 'Example Diagram']  gpt[label = 'GPT']  request[label = 'Requested Diagram']    edge[dir = 'forward', arrowhead = 'vee', fontsize = 12]  example -> gpt  gpt -> request}\")"
            } else if (input$prompt == "heirarchical") {
              prompt_2 <- "Use the `DiagrammeR::grViz` function in R to code meaningful diagrams. Print one line of code in one markdown source pane with no comments, headers, or context. Format node labels with no hyphens, punctuation, or special characters. Make the text readable. Example: grViz(\"digraph model { 1 [label = 'Computers']; 2 [label = 'Desktops']; 3 [label = 'Laptops']; 4 [label = 'Monitors']; 5 [label = 'Printers']; 6 [label = 'Storage']; 7 [label = 'Hard Drives']; 8 [label = 'Solid State Drives']; 9 [label = 'Networking']; 10 [label = 'Routers']; 1 -> 2; 1 -> 3; 2 -> 4; 2 -> 5; 3 -> 6; 6 -> 7; 6 -> 8; 1 -> 9; 9 -> 10;}\")"
            } else if (input$prompt == "cyclical") {
              prompt_2 <- "Use the `DiagrammeR::grViz` function in R to code meaningful cyclical diagrams. Print one line of code in one markdown source pane with no comments, headers, or context. Format node labels with no hyphens, punctuation, or special characters. Make the text readable. Example: grViz(\"digraph cyclical_model { Birth -> Growth; Growth -> Maturity; Maturity -> Reproduction; Reproduction -> Death; Reproduction -> Birth; }\")"
            } else if (input$prompt == "mediation") {
              prompt_2 <- "Use the `DiagrammeR::grViz` function in R to code meaningful mediation model diagrams. Print one line of code in one markdown source pane with no comments, headers, or context. Format node labels with no hyphens, punctuation, or special characters. Make the text readable. Example: grViz(\"digraph mediation_model {  graph[layout = dot]    node[shape = box, fontsize = 14]  example[label = 'Example Diagram']  gpt[label = 'GPT']  request[label = 'Requested Diagram']    edge[dir = 'forward', arrowhead = 'vee', fontsize = 12]  example -> gpt  gpt -> request    edge[dir = 'none', arrowhead = 'vee', fontsize = 12]  example -> request}\")"
            }

            chatter.feed(prompt_1)
            completion_1 <- chatter.chat(input$complete, return_response = TRUE)

            waiter_show(html = waiting_screen_2, color = "black")
            chatter.create(max_tokens = as.integer(input$max_tokens), temperature = as.integer(input$temperature))
            chatter.feed(prompt_2)
            completion_2 <- chatter.chat(completion_1$choices[[1]], return_response = TRUE)
            completion_2_extract <- completion_2$choices[[1]]
            completion_2_clean <- gsub("```|\\{r\\}|library\\(DiagrammeR\\)|<pre>|\\n", "", completion_2_extract)

            error <- try(eval(parse(text = completion_2_clean)), silent = TRUE)

            if (any(class(error) == "try-error")) {
              output$diagram <- renderUI(HTML("<br> <b><p style='color: red;'>Sorry, I wrote R code that won't run. Please try again.</p></b>"))
              output$code <- renderText(paste(parse(text = completion_2_clean)))
              show("clip")
            } else {
              output$diagram <- renderUI(eval(parse(text = completion_2_clean)))
              output$code <- renderText(paste(parse(text = completion_2_clean)))

              myString$output <- paste(parse(text = completion_2_clean)) # gsub("\"", "\\\\\"", paste(parse(text = completion_2_clean)))

              show("clip")
              show("down")

              output$clip <- renderUI({
                rclipButton(
                  inputId = "clipbtn",
                  label = "Copy code",
                  clipText = parse(text = completion_2_clean),
                  icon = icon("clipboard")
                )
              })
            }

            output$down <- downloadHandler(
              filename = paste("DiAGrammy", Sys.time(), ".png"),
              content = function(file) {
                graph <- eval(parse(text = completion_2_clean))
                graph %>%
                  export_svg() %>%
                  charToRaw() %>%
                  rsvg_png(file)
              }
            )

            waiter_hide()
          }
        }
      }
    })

    observeEvent(input$modal_close, {
      removeModal()
    })

    setupStorage(appId = "DiAGrammy", inputs = TRUE)
  }
)
