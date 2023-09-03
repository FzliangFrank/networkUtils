testServer(
  mod_visNet_server,
  # Add here your module params
  args = list(
    graph_rct = reactive(create_demo_graph())
  )
  , {
    ns <- session$ns
    expect_true(
      inherits(ns, "function")
    )
    expect_true(
      grepl(id, ns(""))
    )
    expect_true(
      grepl("test", ns("test"))
    )
    # Here are some examples of tests you can
    # run on your module
    # - Testing the setting of inputs
    session$setInputs(edit = TRUE)
    expect_true(input$edit == TRUE)
    # - If ever your input updates a reactiveValues
    # - Note that this reactiveValues must be passed
    # - to the testServer function via args = list()
    # expect_true(r$x == 1)
    # - Testing output
    # expect_true(inherits(output$tbl$html, "html"))
})

test_that("module ui-modification works", {
  ui <- mod_visNetInteraction_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_visNetInteraction_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})
test_that("module ui-interaction works", {
  ui <- mod_visNetModification_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_visNetModification_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

