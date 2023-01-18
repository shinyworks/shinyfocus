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

#' Read the active_element from root input
#'
#' @inheritParams .root_session
#'
#' @return The value of `shinyfocuspkg-active_element` as a character scalar (or
#'   NULL).
#' @keywords internal
.active_element <- function(session = shiny::getDefaultReactiveDomain()) {
  return(
    .root_session(session)$input[[
      shiny::NS("shinyfocuspkg", "active_element")
    ]]
  )
}

#' Read the previous_element from root input
#'
#' @inheritParams .root_session
#'
#' @return The value of `shinyfocuspkg-previous_element` as a character scalar
#'   (or NULL).
#' @keywords internal
.previous_element <- function(session = shiny::getDefaultReactiveDomain()) {
  return(
    .root_session(session)$input[[
      shiny::NS("shinyfocuspkg", "previous_element")
    ]]
  )
}

#' Respond when input changes focus
#'
#' Set up a [shiny::observeEvent()] observer to trigger when the named input
#' gains or loses focus.
#'
#' @inheritDotParams shiny::observeEvent label suspended autoDestroy ignoreNULL
#'   ignoreInit once
#' @param id The ID string of an input.
#' @param handler_expr The expression to call whenever the specified input
#'   changes focus. This expression is quoted and executed in the calling
#'   environment.
#' @param change_on A character indicating whether the observer should update
#'   when the input becomes focused and/or when the input becomes blurred.
#' @param priority An integer that controls the priority with which the observer
#'   should be executed. It often makes sense for this priority to be very high
#'   to avoid conflicts.
#' @param session The session (aka domain) in which the observer will be created
#'   and executed. The default is almost always desired.
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
#'       on_focus_change(
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
#'       on_focus_change(
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
on_focus_change <- function(id,
                            handler_expr,
                            ...,
                            change_on = c("focus", "blur"),
                            priority = 99999,
                            session = shiny::getDefaultReactiveDomain()) {
  change_on <- match.arg(change_on, several.ok = TRUE)
  handler_expr <- rlang::enquo(handler_expr)
  value_focused <- NULL
  value_blurred <- NULL

  if ("focus" %in% change_on) {
    value_focused <- "focused"
  }
  if ("blur" %in% change_on) {
    value_blurred <- "blurred"
  }

  return(
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
      handlerExpr = handler_expr,
      handler.quoted = TRUE,
      priority = priority,
      domain = session,
      ...
    )
  )
}

#' Respond when input gains focus
#'
#' Set up a [shiny::observeEvent()] observer to trigger when the named input
#' gains focus.
#'
#' @inheritParams on_focus_change
#' @inheritDotParams shiny::observeEvent label suspended autoDestroy ignoreNULL
#'   ignoreInit once
#' @param handler_expr The expression to call whenever the specified input gains
#'   focus. This expression is quoted and executed in the calling environment.
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
#'       on_focus(
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
#'       on_focus(
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
on_focus <- function(id,
                     handler_expr,
                     ...,
                     priority = 99999,
                     session = shiny::getDefaultReactiveDomain()) {
  return(
    on_focus_change(
      id = id,
      handler_expr = handler_expr,
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
#' @inheritParams on_focus_change
#' @inheritDotParams shiny::observeEvent label suspended autoDestroy ignoreNULL
#'   ignoreInit once
#' @param handler_expr The expression to call whenever the specified input loses
#'   focus. This expression is quoted and executed in the calling environment.
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
#'       on_blur(
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
#'       on_blur(
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
on_blur <- function(id,
                    handler_expr,
                    ...,
                    priority = 99999,
                    session = shiny::getDefaultReactiveDomain()) {
  return(
    on_focus_change(
      id = id,
      handler_expr = handler_expr,
      ...,
      change_on = "blur",
      priority = priority,
      session = session
    )
  )
}
