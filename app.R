library(shiny)
library(shinyjs)
library(shinyFiles)
library(shinyalert)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
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
      useShinyjs(),
      rclipboardSetup(),
      useWaiter(),
      fluidRow(
        box(
          title = "Request a Diagram", status = "primary", solidHeader = TRUE,
          textAreaInput("complete", "What type of diagram are you looking for?", placeholder = "the water cycle [8]"),
          "[n] = number of nodes",
          br(),br(),
          selectInput("prompt", "Select diagram template", choices = c("specify nodes", "freeform", "linear", "heirarchical", "cyclical", "mediation")),
          actionBttn(
            inputId = "gpt",
            label = "Request",
            color = "success",
            style = "material-flat",
            icon = icon("paper-plane"),
            block = TRUE
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
    # Show the alert on startup of the app
    observe({
      shinyalert(
        title = "OpenAI API Key",
        type = "input",
        inputId = "API",
        confirmButtonText = "Set",
        closeOnEsc = FALSE,
        closeOnClickOutside = FALSE,
        callbackR = function(x) {
          chatter.auth(x)
        }
      )
    })

    # Ask GPT to write R code
    observeEvent(input$gpt, {
      waiter_show(html = waiting_screen_1, color = "black")

      chatter.create(max_tokens = 1000)

      prompt1 <- "Describe a directed acyclic graph based on a user’s topic. The diagram should have at least n nodes. I specify n in my input by writing [n], less than 10 being the default value. Connect the nodes in the system and explain the directionality between them in excruciating detail. Reiterate the user's topic and n number of nodes (example: \"topic [n]\")."
      
      if (input$prompt == "specify nodes") {
        prompt2 <- "Act as a Graphviz DOT generator who creates stylish and meaningful diagrams. Use the `DiagrammeR::grViz` function in R to code the diagrams. The diagram should have at least n nodes. I specify n in my input by writing [n], less than 10 being the default value (example: \"The water cycle [8]\"). Each node is indexed by a number to reduce the size of the output, should not include any styling. Use layout=neato, overlap=false, node [shape=rectangle] as parameters. The code should be valid, bugless. Print one markdown source code pane with no comments, headers, or context. Format node strings with no hyphens, punctuation, or special characters. Example code: grViz(\"digraph model { ... }\")"
      } else if (input$prompt == "freeform") {
        prompt2 <- "Use the `DiagrammeR::grViz` function in R to code stylish and meaningful diagrams. Print one markdown source code pane with no comments, headers, or context. Format node strings with no hyphens, punctuation, or special characters. Adjust the layout to make the text readable. Example: grViz(\"digraph model { ... }\")"
      } else if (input$prompt == "linear") {
        prompt2 <- "Use the `DiagrammeR::grViz` function in R to code stylish and meaningful diagrams. Print one markdown source code pane with no comments, headers, or context. Format node strings with no hyphens, punctuation, or special characters. Adjust the layout to make the text readable. Example: grViz(\"digraph mediation_model {  graph[layout = dot]    node[shape = box, fontsize = 14]  example[label = 'Example Diagram']  gpt[label = 'GPT']  request[label = 'Requested Diagram']    edge[dir = 'forward', arrowhead = 'vee', fontsize = 12]  example -> gpt  gpt -> request}\")"
      } else if (input$prompt == "heirarchical"){
        prompt2 <- "Use the `DiagrammeR::grViz` function in R to code stylish and meaningful diagrams. Print one markdown source code pane with no comments, headers, or context. Format node strings with no hyphens, punctuation, or special characters. Adjust the layout to make the text readable. Example: grViz(\"digraph heirarchy_model {  node[shape = box]  CEO -> Manager1  CEO -> Manager2  CEO -> Manager3  Manager1 -> Team1  Manager1 -> Team2  Manager2 -> Team3  Manager2 -> Team4  Manager3 -> Team5  Team1 -> Employee1  Team1 -> Employee2  Team2 -> Employee3  Team2 -> Employee4  Team3 -> Employee5  Team3 -> Employee6  Team4 -> Employee7  Team4 -> Employee8  Team5 -> Employee9  Team5 -> Employee10}\")"
      } else if (input$prompt == "cyclical"){
        prompt2 <- "Use the `DiagrammeR::grViz` function in R to code stylish and meaningful diagrams. Print one markdown source code pane with no comments, headers, or context. Format node strings with no hyphens, punctuation, or special characters. Adjust the layout to make the text readable. Example: grViz(\"digraph cyclical_model { Birth -> Growth; Growth -> Maturity; Maturity -> Reproduction; Reproduction -> Death; Reproduction -> Birth; }\")"
      } else if (input$prompt == "mediation") {
        prompt2 <- "Use the `DiagrammeR::grViz` function in R to code stylish and meaningful diagrams. Print one markdown source code pane with no comments, headers, or context. Format node strings with no hyphens, punctuation, or special characters. Adjust the layout to make the text readable. Example: grViz(\"digraph mediation_model {  graph[layout = dot]    node[shape = box, fontsize = 14]  example[label = 'Example Diagram']  gpt[label = 'GPT']  request[label = 'Requested Diagram']    edge[dir = 'forward', arrowhead = 'vee', fontsize = 12]  example -> gpt  gpt -> request    edge[dir = 'none', arrowhead = 'vee', fontsize = 12]  example -> request}\")"
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

        # Add clipboard buttons
        output$clip <- renderUI({
          output$clip <- renderUI({
            rclipButton(
              inputId = "clipbtn",
              label = "Copy code",
              clipText = parse(text = completion2_clean),
              icon = icon("clipboard")
            )
          })
        })

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
      }

      waiter_hide()
    })
  }
)
