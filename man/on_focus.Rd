% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/observe.R
\name{on_focus}
\alias{on_focus}
\title{Respond when input gains focus}
\usage{
on_focus(
  id,
  handler_expr,
  ...,
  priority = 99999,
  session = shiny::getDefaultReactiveDomain()
)
}
\arguments{
\item{id}{The ID string of an input.}

\item{handler_expr}{The expression to trigger whenever the specified input
gains focus. This expression is quoted and executed in the calling
environment.}

\item{...}{
  Arguments passed on to \code{\link[shiny:observeEvent]{shiny::observeEvent}}
  \describe{
    \item{\code{label}}{A label for the observer or reactive, useful for debugging.}
    \item{\code{suspended}}{If \code{TRUE}, start the observer in a suspended state. If
\code{FALSE} (the default), start in a non-suspended state.}
    \item{\code{autoDestroy}}{If \code{TRUE} (the default), the observer will be
automatically destroyed when its domain (if any) ends.}
    \item{\code{ignoreNULL}}{Whether the action should be triggered (or value
calculated, in the case of \code{eventReactive}) when the input event expression
is \code{NULL}. See Details.}
    \item{\code{once}}{Whether this \code{observeEvent} should be immediately destroyed
after the first time that the code in \code{handlerExpr} is run. This
pattern is useful when you want to subscribe to a event that should only
happen once.}
  }}

\item{priority}{An integer that controls the priority with which the observer
should be executed. It often makes sense for this priority to be very high
to avoid conflicts.}

\item{session}{The session (aka domain) in which the observer will be created
and executed. The default is almost always desired.}
}
\value{
A shiny observer (see \code{\link[shiny:observe]{shiny::observe()}}).
}
\description{
Set up a \code{\link[shiny:observeEvent]{shiny::observeEvent()}} observer to trigger when the named input
gains focus.
}
\examples{
## Only run examples in interactive R sessions
if (interactive()) {
  # App 1: A relatively simple ui without modules.
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shinyfocus_js_dependency(),
      shiny::textInput("input1", "Input 1"),
      shiny::textInput("input2", "Input 2"),
      shiny::actionButton("go_button", "Go!"),
      shiny::textOutput("focusing")
    ),
    server = function(input, output, session) {
      # Update the value in focusing whenever input1 has focus.
      on_focus(
        "input1",
        output$focusing <- shiny::renderText(
          paste(
            "You entered input1 at",
            Sys.time()
          )
        )
      )
    }
  )

  # App 2: With module.
  focusUI <- function(id) {
    shiny::tagList(
      shiny::textInput(shiny::NS(id, "observe_me"), "Observe me"),
      shiny::textOutput(NS(id, "focusing"))
    )
  }
  focusServer <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      on_focus(
        "observe_me",
        output$focusing <- shiny::renderText(
          paste(
            "You entered observe_me at",
            Sys.time()
          )
        )
      )
    })
  }

  shiny::shinyApp(
    ui = shiny::fluidPage(
      shinyfocus_js_dependency(),
      focusUI("focus_module"),
      shiny::textInput("another_input", "Another input"),
      shiny::actionButton("go_button", "Go!")
    ),
    server = function(input, output, session) {
      focusServer("focus_module")
    }
  )
}
}
\seealso{
Other observers: 
\code{\link{on_blur}()},
\code{\link{on_focus_change}()}
}
\concept{observers}
