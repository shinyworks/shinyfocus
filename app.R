# This app demonstrates some of the functionality of this package.

# See ?shiny::loadSupport
options(shiny.autoload.r = FALSE)

pkgload::load_all(
  export_all = FALSE,
  helpers = FALSE,
  attach_testthat = FALSE,
  quiet = TRUE
)

ui <- shiny::fluidPage(
  shinyfocus_js_dependency(),
  shiny::column(
    2,
    shiny::textInput("name", "Name:"),
    shiny::textInput("title", "Title:")
  ),
  shiny::column(
    2,
    shiny::textOutput("explanation")
  )
)

server <- function(input, output, session) {
  on_focus(
    "name",
    output$explanation <- shiny::renderText({
      "Enter the name you want me to call you. It will be converted to Title Case."
    })
  )

  on_focus(
    "title",
    output$explanation <- shiny::renderText({
      "Describe your role in 10 characters or fewer."
    })
  )

  on_blur(
    "name",
    shiny::updateTextInput(
      inputId = "name",
      value = stringr::str_to_title(shiny::isolate(input$name))
    )
  )

  on_blur(
    "title",
    {
      if (nchar(input$title) > 10) {
        shiny::updateTextInput(
          inputId = "title",
          value = paste(
            "Typer of",
            nchar(input$title),
            "Characters"
          )
        )
      }
    }
  )

}

shiny::shinyApp(ui, server)
