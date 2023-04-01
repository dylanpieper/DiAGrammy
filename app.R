library(httr)
library(shiny)
library(shinyjs)
library(shinyFiles)
library(shinyalert)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyStorePlus)
library(rsvg)
library(waiter)
library(magrittr)
library(rclipboard)
library(gptchatteR)
library(DiagrammeR)
library(DiagrammeRsvg)

waiting_screen_1 <- tagList(
  spin_flower(),
  h4("DiAGrammy is analyzing your request...")
)

waiting_screen_2 <- tagList(
  spin_flower(),
  h4("DiAGrammy is coding your diagram using DiagrammeR...")
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
              tags$input(
                id = "API", type = "text",
                style = "margin-bottom: 0; padding: 5px;",
                title = "Enter your OpenAI API key here"
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
              min = 1000, max = 4000, step = 100, value = 1000
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

  server = function(input, output) {
    observeEvent(input$gpt, {
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

          prompt1 <- "Describe a directed acyclic graph based on a user’s topic. The diagram should have at least n nodes. I specify n in my input by writing [n], less than 10 being the default value. Connect the nodes in the system and explain the directionality between them in excruciating detail. Reiterate the user's topic and n nodes (example: \"topic [n]\")."

          if (input$prompt == "specify nodes") {
            prompt2 <- "Act as a Graphviz DOT generator who creates meaningful diagrams. Use the `DiagrammeR::grViz` function in R. The diagram should have at least n nodes. I specify n in my input by writing [n], less than 10 being the default value (example: \"The water cycle [8]\"). Each node is indexed by a number to reduce the size of the output with no styling. The code should be valid, bugless. Print one line of code in one markdown source pane with no comments, headers, or context. Format node strings with no hyphens, punctuation, or special characters. Example code: grViz(\"digraph model { ... }\")"
          } else if (input$prompt == "freeform") {
            prompt2 <- "Use the `DiagrammeR::grViz` function in R to code meaningful diagrams. Print one line of code in one markdown source pane with no comments, headers, or context. Format node strings with no hyphens, punctuation, or special characters. Adjust the layout to make the text readable. Example: grViz(\"digraph model { ... }\")"
          } else if (input$prompt == "linear") {
            prompt2 <- "Use the `DiagrammeR::grViz` function in R to code meaningful linear diagrams. Print one line of code in one markdown source pane with no comments, headers, or context. Format node strings with no hyphens, punctuation, or special characters. Adjust the layout to make the text readable. Example: grViz(\"digraph linear_model {  graph[layout = dot]    node[shape = box, fontsize = 14]  example[label = 'Example Diagram']  gpt[label = 'GPT']  request[label = 'Requested Diagram']    edge[dir = 'forward', arrowhead = 'vee', fontsize = 12]  example -> gpt  gpt -> request}\")"
          } else if (input$prompt == "heirarchical") {
            prompt2 <- "Use the `DiagrammeR::grViz` function in R to code meaningful heirarchical diagrams. Print one line of code in one markdown source pane with no comments, headers, or context. Format node strings with no hyphens, punctuation, or special characters. Adjust the layout to make the text readable. Example: grViz(\"digraph heirarchical_model {  node[shape = box]  CEO -> Manager1  CEO -> Manager2  CEO -> Manager3  Manager1 -> Team1  Manager1 -> Team2  Manager2 -> Team3  Manager2 -> Team4  Manager3 -> Team5  Team1 -> Employee1  Team1 -> Employee2  Team2 -> Employee3  Team2 -> Employee4  Team3 -> Employee5  Team3 -> Employee6  Team4 -> Employee7  Team4 -> Employee8  Team5 -> Employee9  Team5 -> Employee10}\")"
          } else if (input$prompt == "cyclical") {
            prompt2 <- "Use the `DiagrammeR::grViz` function in R to code meaningful cyclical diagrams. Print one line of code in one markdown source pane with no comments, headers, or context. Format node strings with no hyphens, punctuation, or special characters. Adjust the layout to make the text readable. Example: grViz(\"digraph cyclical_model { Birth -> Growth; Growth -> Maturity; Maturity -> Reproduction; Reproduction -> Death; Reproduction -> Birth; }\")"
          } else if (input$prompt == "mediation") {
            prompt2 <- "Use the `DiagrammeR::grViz` function in R to code meaningful mediation model diagrams. Print one line of code in one markdown source pane with no comments, headers, or context. Format node strings with no hyphens, punctuation, or special characters. Adjust the layout to make the text readable. Example: grViz(\"digraph mediation_model {  graph[layout = dot]    node[shape = box, fontsize = 14]  example[label = 'Example Diagram']  gpt[label = 'GPT']  request[label = 'Requested Diagram']    edge[dir = 'forward', arrowhead = 'vee', fontsize = 12]  example -> gpt  gpt -> request    edge[dir = 'none', arrowhead = 'vee', fontsize = 12]  example -> request}\")"
          }

          chatter.feed(prompt1)

          completion1 <- chatter.chat(input$complete, return_response = TRUE)

          waiter_show(html = waiting_screen_2, color = "black")

          chatter.create(max_tokens = 1000)

          chatter.feed(prompt2)

          completion2 <- chatter.chat(completion1$choices[[1]], return_response = TRUE)

          completion2_extract <- completion2$choices[[1]]
          
          completion2_clean <- gsub("```|\\{r\\}|library\\(DiagrammeR\\)|<pre>|\\n", "", completion2_extract)

          error <- try(eval(parse(text = completion2_clean)), silent = TRUE)

          if (any(class(error) == "try-error")) {
            output$diagram <- renderUI(HTML("<br> <b><p style='color: red;'>Sorry, I wrote R code that won't run. Please try again.</p></b>"))
            output$code <- renderPrint(parse(text = completion2_clean))
            show("clip")
          } else {
            output$diagram <- renderUI(eval(parse(text = completion2_clean)))
            output$code <- renderPrint(parse(text = completion2_clean))
            show("clip")
            show("down")

            output$clip <- renderUI({
              rclipButton(
                inputId = "clipbtn",
                label = "Copy code",
                clipText = parse(text = completion2_clean),
                icon = icon("clipboard")
              )
            })
          }

          output$down <- downloadHandler(
            filename = paste("DiAGrammy", Sys.time(), ".png"),
            content = function(file) {
              graph <- eval(parse(text = completion2_clean))
              graph %>%
                export_svg() %>%
                charToRaw() %>%
                rsvg_png(file)
            }
          )

          waiter_hide()
        }
      }
    })

    observeEvent(input$modal_close, {
      removeModal()
    })

    setupStorage(appId = "DiAGrammy", inputs = TRUE)
  }
)
