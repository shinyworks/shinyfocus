#' Add the shinyfocus JavaScript to shiny
#'
#' Add the `shinyfocus.js` script to a shiny app exactly once. Adding this
#' script setups up the object used to detect changes in focus.
#'
#' @return An [htmltools::htmlDependency()], which shiny uses to add the
#'   `shinyfocus.js` script exactly once.
#' @export
#'
#' @examples
#' shinyfocus_js_dependency()
shinyfocus_js_dependency <- function() {
  return(
    htmltools::htmlDependency(
      name = "shinyfocus",
      version = "0.0.1",
      src = "js",
      package = "shinyfocus",
      script = "shinyfocus.js"
    )
  )
}
