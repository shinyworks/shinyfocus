library(shinyfocus)

changeUI <- function(id) {
  shiny::tagList(
    shiny::textInput(shiny::NS(id, "observe_me"), "Observe me"),
    shiny::textOutput(NS(id, "changing"))
  )
}
changeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    observe_focus(
      "observe_me",
      output$changing <- shiny::renderText(
        paste(
          "You entered observe_me! Here's a random number:",
          runif(1)
        )
      )
    )
  })
}

shiny::shinyApp(
  ui = shiny::fluidPage(
    shinyfocus_js_dependency(),
    changeUI("change_module"),
    shiny::textInput("another_input", "Another input"),
    shiny::actionButton("go_button", "Go!")
  ),
  server = function(input, output, session) {
    changeServer("change_module")
  }
)
