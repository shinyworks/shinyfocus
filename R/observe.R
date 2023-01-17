#' Find the main session
#'
#' This function escapes from a module (or submodule, etc) to find the root
#' session.
#'
#' @param session A session object. Probably always use the default.
#'
#' @return The first session that isn't a "session_proxy".
#' @keywords internal
.root_session <- function(session = shiny::getDefaultReactiveDomain()) {
  while (inherits(session, "session_proxy")) {
    session <- .subset2(session, "parent")
  }
  return(session)
}

.active_element <- function(session = shiny::getDefaultReactiveDomain()) {
  return(
    .root_session(session)$input[[shiny::NS("shinyfocuspkg", "active_element")]]
  )
}

.previous_element <- function(session = shiny::getDefaultReactiveDomain()) {
  return(
    .root_session(session)$input[[shiny::NS("shinyfocuspkg", "previous_element")]]
  )
}

#' Respond when input changes focus
#'
#' Set up a [shiny::observeEvent()] observer to trigger when the named input
#' gains or loses focus.
#'
#' @inheritDotParams shiny::observeEvent handlerExpr handler.env handler.quoted
#'   label suspended domain autoDestroy ignoreNULL ignoreInit once
#' @param id The ID string of an input.
#' @param change_on A character indicating whether the observer should update
#'   when the input becomes focused and/or when the input becomes blurred.
#' @param priority An integer that controls the priority with which the observer
#'   should be executed. It often makes sense for this priority to be very high
#'   to avoid conflicts.
#' @param session The session in which the observer will be created. The default
#'   is almost always desired.
#'
#' @return A shiny observer (see [shiny::observe()]).
#' @export
#' @family observers
#'
#' @examples
#' if (interactive()) {
#'   # App 1: A relatively simple ui without modules.
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(
#'       shinyfocus_js_dependency(),
#'       shiny::textInput("input1", "Input 1"),
#'       shiny::textInput("input2", "Input 2"),
#'       shiny::actionButton("go_button", "Go!"),
#'       shiny::textOutput("changing")
#'     ),
#'     server = function(input, output, session) {
#'       # Update the value in "changing" whenever input1 gains or loses focus.
#'       observe_focus_change(
#'         "input1",
#'         output$changing <- shiny::renderText(
#'           paste(
#'             "You entered or left input1 at",
#'             Sys.time()
#'           )
#'         )
#'       )
#'     }
#'   )
#'
#'   # App 2: With module.
#'   changeUI <- function(id) {
#'     shiny::tagList(
#'       shiny::textInput(shiny::NS(id, "observe_me"), "Observe me"),
#'       shiny::textOutput(NS(id, "changing"))
#'     )
#'   }
#'   changeServer <- function(id) {
#'     shiny::moduleServer(id, function(input, output, session) {
#'       observe_focus_change(
#'         "observe_me",
#'         output$changing <- shiny::renderText(
#'           paste(
#'             "You entered or left observe_me at",
#'             Sys.time()
#'           )
#'         )
#'       )
#'     })
#'   }
#'
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(
#'       shinyfocus_js_dependency(),
#'       changeUI("change_module"),
#'       shiny::textInput("another_input", "Another input"),
#'       shiny::actionButton("go_button", "Go!")
#'     ),
#'     server = function(input, output, session) {
#'       changeServer("change_module")
#'     }
#'   )
#' }
observe_focus_change <- function(id,
                                 ...,
                                 change_on = c("focus", "blur"),
                                 priority = 99999,
                                 session = shiny::getDefaultReactiveDomain()) {
  change_on <- match.arg(change_on, several.ok = TRUE)
  value_focused <- NULL
  value_blurred <- NULL

  if ("focus" %in% change_on) {
    value_focused <- "focused"
  }
  if ("blur" %in% change_on) {
    value_blurred <- "blurred"
  }

  shiny::withReactiveDomain(
    session,
    {
      # Define input and output in case they're referenced in the handler.
      input <- session$input
      output <- session$output

      shiny::observeEvent(
        {
          if (!is.null(.active_element())) {
            if (.active_element() == session$ns(id)) {
              value_focused
            } else if (.previous_element() == session$ns(id)) {
              value_blurred
            }
          }
        },
        priority = priority,
        ...
      )
    }
  )
}

#' Respond when input gains focus
#'
#' Set up a [shiny::observeEvent()] observer to trigger when the named input
#' gains focus.
#'
#' @inheritParams observe_focus_change
#' @inheritDotParams shiny::observeEvent handlerExpr handler.env handler.quoted
#'   label suspended domain autoDestroy ignoreNULL ignoreInit once
#'
#' @return A shiny observer (see [shiny::observe()]).
#' @export
#' @family observers
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'   # App 1: A relatively simple ui without modules.
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(
#'       shinyfocus_js_dependency(),
#'       shiny::textInput("input1", "Input 1"),
#'       shiny::textInput("input2", "Input 2"),
#'       shiny::actionButton("go_button", "Go!"),
#'       shiny::textOutput("focusing")
#'     ),
#'     server = function(input, output, session) {
#'       # Update the value in focusing whenever input1 has focus.
#'       observe_focus(
#'         "input1",
#'         output$focusing <- shiny::renderText(
#'           paste(
#'             "You entered input1 at",
#'             Sys.time()
#'           )
#'         )
#'       )
#'     }
#'   )
#'
#'   # App 2: With module.
#'   focusUI <- function(id) {
#'     shiny::tagList(
#'       shiny::textInput(shiny::NS(id, "observe_me"), "Observe me"),
#'       shiny::textOutput(NS(id, "focusing"))
#'     )
#'   }
#'   focusServer <- function(id) {
#'     shiny::moduleServer(id, function(input, output, session) {
#'       observe_focus(
#'         "observe_me",
#'         output$focusing <- shiny::renderText(
#'           paste(
#'             "You entered observe_me at",
#'             Sys.time()
#'           )
#'         )
#'       )
#'     })
#'   }
#'
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(
#'       shinyfocus_js_dependency(),
#'       focusUI("focus_module"),
#'       shiny::textInput("another_input", "Another input"),
#'       shiny::actionButton("go_button", "Go!")
#'     ),
#'     server = function(input, output, session) {
#'       focusServer("focus_module")
#'     }
#'   )
#' }
observe_focus <- function(id,
                          ...,
                          priority = 99999,
                          session = shiny::getDefaultReactiveDomain()) {
  return(
    observe_focus_change(
      id = id,
      ...,
      change_on = "focus",
      priority = priority,
      session = session
    )
  )
}

#' Respond when input loses focus
#'
#' Set up a [shiny::observeEvent()] observer to trigger when the named input
#' loses focus.
#'
#' @inheritParams observe_focus_change
#' @inheritDotParams shiny::observeEvent handlerExpr handler.env handler.quoted
#'   label suspended domain autoDestroy ignoreNULL ignoreInit once
#'
#' @return A shiny observer (see [shiny::observe()]).
#' @export
#' @family observers
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'   # App 1: A relatively simple ui without modules.
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(
#'       shinyfocus_js_dependency(),
#'       shiny::textInput("input1", "Input 1"),
#'       shiny::textInput("input2", "Input 2"),
#'       shiny::actionButton("go_button", "Go!"),
#'       shiny::textOutput("blurring")
#'     ),
#'     server = function(input, output, session) {
#'       # Update the value in blurring whenever input1 loses focus.
#'       observe_blur(
#'         "input1",
#'         output$blurring <- shiny::renderText(
#'           paste(
#'             "You left input1 at",
#'             Sys.time()
#'           )
#'         )
#'       )
#'     }
#'   )
#'
#'   # App 2: With module.
#'   blurUI <- function(id) {
#'     shiny::tagList(
#'       shiny::textInput(shiny::NS(id, "observe_me"), "Observe me"),
#'       shiny::textOutput(NS(id, "blurring"))
#'     )
#'   }
#'   blurServer <- function(id) {
#'     shiny::moduleServer(id, function(input, output, session) {
#'       observe_blur(
#'         "observe_me",
#'         output$blurring <- shiny::renderText(
#'           paste(
#'             "You left observe_me at",
#'             Sys.time()
#'           )
#'         )
#'       )
#'     })
#'   }
#'
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(
#'       shinyfocus_js_dependency(),
#'       blurUI("blur_module"),
#'       shiny::textInput("another_input", "Another input"),
#'       shiny::actionButton("go_button", "Go!")
#'     ),
#'     server = function(input, output, session) {
#'       blurServer("blur_module")
#'     }
#'   )
#' }
observe_blur <- function(id,
                         ...,
                         priority = 99999,
                         session = shiny::getDefaultReactiveDomain()) {
  return(
    observe_focus_change(
      id = id,
      ...,
      change_on = "blur",
      priority = priority,
      session = session
    )
  )
}
