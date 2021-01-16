test_that("The Jakpost's chart template works", {

  x <- dplyr::tibble(
    year = seq(2010, 2019, by = 1),
    val = seq(2, 20, by = 2)
  )

  chart_id <- "SbzoE"

  DatawRappr::dw_data_to_chart(x, chart_id)

  jakpost_style(
    chart_key = chart_id,
    chart_type = "line",
    y = x$val,
    y_axis_ticks = c(breaks = 4, addition = 0),
    headline = "use_test",
    author = "Dzulfiqar Fathur Rahman",
    footnote = stringr::str_c(sample(letters, 5), collapse = ", ")
  )

  DatawRappr::dw_publish_chart(chart_id)

})
