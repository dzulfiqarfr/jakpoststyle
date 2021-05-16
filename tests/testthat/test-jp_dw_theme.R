test_that("The Jakarta Post theme", {

  x <- dplyr::tibble(
    month = seq(
      lubridate::ymd("2019-04-01"),
      lubridate::ymd("2020-10-01"),
      by = "3 month"
    ),
    val = seq(-12, 6, 3)
  )

  chart_id <- "SbzoE"

  DatawRappr::dw_data_to_chart(x, chart_id)

  expect_output(
    jp_dw_theme(
      chart_id,
      author = "Howard Wolowitz",
      footnote = stringr::str_c("Last updated on ", format(Sys.time(), "%b %d, %Y"))
    ),
    stringr::str_c("Chart ", chart_id, " succesfully updated.")
  )

})
