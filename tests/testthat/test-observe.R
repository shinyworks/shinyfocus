library(shinytest2)

test_that("change app works", {
  skip_on_cran()

  appdir <- test_path("apps", "change")
  app <- shinytest2::AppDriver$new(
    appdir,
    name = "change",
    seed = 12345,
    screenshot_args = FALSE
  )
  on.exit(app$stop())

  app$expect_values(screenshot_args = FALSE)
  app$set_inputs(
    `shinyfocuspkg-active_element` = "change_module-observe_me",
    allow_no_input_binding_ = TRUE
  )
  app$expect_values(screenshot_args = FALSE)
  app$set_inputs(
    `shinyfocuspkg-previous_element` = "change_module-observe_me",
    allow_no_input_binding_ = TRUE
  )
  app$set_inputs(
    `shinyfocuspkg-active_element` = "another_input",
    allow_no_input_binding_ = TRUE
  )
  app$expect_values(screenshot_args = FALSE)
  app$set_inputs(
    `shinyfocuspkg-previous_element` = "another_input",
    allow_no_input_binding_ = TRUE
  )
  app$set_inputs(
    `shinyfocuspkg-active_element` = "go_button",
    allow_no_input_binding_ = TRUE
  )
  app$click("go_button")
  app$expect_values(screenshot_args = FALSE)
  app$set_inputs(
    `shinyfocuspkg-previous_element` = "go_button",
    allow_no_input_binding_ = TRUE
  )
  app$set_inputs(
    `shinyfocuspkg-active_element` = "another_input",
    allow_no_input_binding_ = TRUE
  )
  app$expect_values(screenshot_args = FALSE)
  app$set_inputs(
    `shinyfocuspkg-previous_element` = "another_input",
    allow_no_input_binding_ = TRUE
  )
  app$set_inputs(
    `shinyfocuspkg-active_element` = "change_module-observe_me",
    allow_no_input_binding_ = TRUE
  )
  app$expect_values(screenshot_args = FALSE)
})

test_that("focus app works", {
  skip_on_cran()

  appdir <- test_path("apps", "focus")
  app <- shinytest2::AppDriver$new(
    appdir,
    name = "focus",
    seed = 12345,
    screenshot_args = FALSE
  )
  on.exit(app$stop())

  app$expect_values(screenshot_args = FALSE)
  app$set_inputs(
    `shinyfocuspkg-active_element` = "change_module-observe_me",
    allow_no_input_binding_ = TRUE
  )
  app$expect_values(screenshot_args = FALSE)
  app$set_inputs(
    `shinyfocuspkg-previous_element` = "change_module-observe_me",
    allow_no_input_binding_ = TRUE
  )
  app$set_inputs(
    `shinyfocuspkg-active_element` = "another_input",
    allow_no_input_binding_ = TRUE
  )
  app$expect_values(screenshot_args = FALSE)
  app$set_inputs(
    `shinyfocuspkg-previous_element` = "another_input",
    allow_no_input_binding_ = TRUE
  )
  app$set_inputs(
    `shinyfocuspkg-active_element` = "go_button",
    allow_no_input_binding_ = TRUE
  )
  app$click("go_button")
  app$expect_values(screenshot_args = FALSE)
  app$set_inputs(
    `shinyfocuspkg-previous_element` = "go_button",
    allow_no_input_binding_ = TRUE
  )
  app$set_inputs(
    `shinyfocuspkg-active_element` = "another_input",
    allow_no_input_binding_ = TRUE
  )
  app$expect_values(screenshot_args = FALSE)
  app$set_inputs(
    `shinyfocuspkg-previous_element` = "another_input",
    allow_no_input_binding_ = TRUE
  )
  app$set_inputs(
    `shinyfocuspkg-active_element` = "change_module-observe_me",
    allow_no_input_binding_ = TRUE
  )
  app$expect_values(screenshot_args = FALSE)
})

test_that("blur app works", {
  skip_on_cran()

  appdir <- test_path("apps", "blur")
  app <- shinytest2::AppDriver$new(
    appdir,
    name = "blur",
    seed = 12345,
    screenshot_args = FALSE
  )
  on.exit(app$stop())

  app$expect_values(screenshot_args = FALSE)
  app$set_inputs(
    `shinyfocuspkg-active_element` = "change_module-observe_me",
    allow_no_input_binding_ = TRUE
  )
  app$expect_values(screenshot_args = FALSE)
  app$set_inputs(
    `shinyfocuspkg-previous_element` = "change_module-observe_me",
    allow_no_input_binding_ = TRUE
  )
  app$set_inputs(
    `shinyfocuspkg-active_element` = "another_input",
    allow_no_input_binding_ = TRUE
  )
  app$expect_values(screenshot_args = FALSE)
  app$set_inputs(
    `shinyfocuspkg-previous_element` = "another_input",
    allow_no_input_binding_ = TRUE
  )
  app$set_inputs(
    `shinyfocuspkg-active_element` = "go_button",
    allow_no_input_binding_ = TRUE
  )
  app$click("go_button")
  app$expect_values(screenshot_args = FALSE)
  app$set_inputs(
    `shinyfocuspkg-previous_element` = "go_button",
    allow_no_input_binding_ = TRUE
  )
  app$set_inputs(
    `shinyfocuspkg-active_element` = "another_input",
    allow_no_input_binding_ = TRUE
  )
  app$expect_values(screenshot_args = FALSE)
  app$set_inputs(
    `shinyfocuspkg-previous_element` = "another_input",
    allow_no_input_binding_ = TRUE
  )
  app$set_inputs(
    `shinyfocuspkg-active_element` = "change_module-observe_me",
    allow_no_input_binding_ = TRUE
  )
  app$expect_values(screenshot_args = FALSE)
})
