library(shiny)
library(shinyFiles)
library(shinyalert)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(gptchatteR)
library(DiagrammeR)
library(DiagrammeRsvg)
library(waiter)

waiting_screen_1 <- tagList(
  spin_flower(),
  h4("DiAGrammy is analyzing your request in excruciating detail...")
) 

waiting_screen_2 <- tagList(
  spin_flower(),
  h4("DiAGrammy is coding your graph using DiagrammeR...")
) 

shinyApp(
  ui = dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE, collapsed = TRUE, minified = FALSE, width = "0px"),
    dashboardBody(
      useWaiter(),
      fluidRow(
        box(
          title = "Request a Diagram", status = "primary", solidHeader = TRUE,
          textAreaInput("complete", "What type of diagram are you looking for?"),
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
          footer = tags$a("Please visit my repository to learn more.", href="https://github.com/dylanpieper/DiAGrammy")
        ),
      ),
    ),
    title = "DiAGrammy"
  ),
  server = function(input, output) {
    # Show the alert on startup of the app
    observe({
      shinyalert(title = "Set your OpenAI API Key",
                 type = "input",
                 inputId = "API",
                 confirmButtonText = "Set",
                 closeOnEsc = FALSE,
                 closeOnClickOutside = FALSE,
                 callbackR = function(x) {
                   chatter.auth(x)
                 })
    })
    
    # Ask GPT to write R code
    observeEvent(input$gpt, {
      
      waiter_show(html = waiting_screen_1, color = "black")
      
      chatter.create(max_tokens = 1000)
      
      prompt1 <- "You are ConnectGPT, a large language model trained by OpenAI to provide a written description of a directed acyclic graph. Based on the user’s topic, you will understand the connections between the nodes of the system and explain them in excruciating detail."
      prompt2 <- "You are DiagramGPT, a large language model trained by OpenAI to provide coding assistance in R. You use the `DiagrammeR::grViz` function to generate code for stylized flowchart diagrams based on text input. You will only print one markdown source code pane with no comments, headers, or context. Do not use a piping approach using %>%. Be careful to format node strings with no hyphens, punctuation, or special characters. Adjust the layout to make the text more readable. Remove \\ from } \\"
      
      chatter.feed(prompt1)
      completion1 <- chatter.chat(input$complete, return_response=TRUE)
      
      waiter_show(html = waiting_screen_2, color = "black")
      
      chatter.create(max_tokens = 1000)
      
      chatter.feed(prompt2)
      completion2 <- chatter.chat(completion1$choices[[1]], return_response=TRUE)
      completion2_extract <- completion2$choices[[1]]
      completion2_clean <- gsub("```|\\{r\\}|library\\(DiagrammeR\\)|<pre>|\\n", "", completion2_extract)
      
      error <- try(eval(parse(text = completion2_clean)), silent = TRUE)
      
      if(any(class(error) == "try-error")){
        output$diagram <- renderUI(HTML("<br> <b><p style='color: red;'>Sorry...I could not evaluate the R code. Please try again.</p></b>"))
      }else{
        output$diagram <- renderUI(eval(parse(text = completion2_clean)))
        output$code <- renderPrint(parse(text = completion2_clean)) # Fix: returns expression; Add feature: copy and paste
      }
      
      waiter_hide()

    })
    
})