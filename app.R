library(shiny)

histogramUI <- function(id) {
  tagList(
    selectInput(NS(id, "var"), "Variable", choices = names(mtcars)),
    numericInput(NS(id, "bins"), "bins", value = 10, min = 1),
    plotOutput(NS(id, "hist")),
    shiny::verbatimTextOutput(NS(id, "testing"))
  )
}

histogramServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive(mtcars[[input$var]])
    output$hist <- renderPlot({
      hist(data(), breaks = input$bins, main = input$var)
    }, res = 96)
    output$testing <- renderText({
      # root_session <- session
      # while (inherits(root_session, "session_proxy")) {
      #   root_session <- .subset2(root_session, "parent")
      # }
      # names(
      #   root_session$input
      # )
      names(.subset2(session, "overrides"))
    })
  })
}

ui <- fluidPage(
  shinyfocus_js_dependency(),
  histogramUI("hist1"),
  shiny::textInput("another_text", "More text"),
  shiny::actionButton("go_button", "Go!"),
  shiny::verbatimTextOutput("testing")
)
server <- function(input, output, session) {
  histogramServer("hist1")

  output$testing <- shiny::renderText(
    input[[shiny::NS("shinyfocuspkg", "active_element")]]
    # names(input)
  )

  shiny::observeEvent(
    if (
      !is.null(input[[shiny::NS("shinyfocuspkg", "active_element")]]) &&
      input[[shiny::NS("shinyfocuspkg", "active_element")]] == "hist1-var-selectized"
    ) {
      "focused"
    },
    shiny::updateTextInput(
      session,
      "another_text",
      value = input[[shiny::NS("shinyfocuspkg", "active_element")]]
    )
  )

  shiny::observeEvent(
    input$go_button,
    shiny::updateTextInput(
      session,
      "another_text",
      value = "new value"
    )
  )
}
shinyApp(ui, server)
