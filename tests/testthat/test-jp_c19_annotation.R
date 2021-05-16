test_that("COVID-19 annotations", {

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
    jp_dw_c19_annotation(
      chart_id,
      y_position = 6,
      lty = "dotted"
    ),
    stringr::str_c("Chart ", chart_id, " succesfully updated.")
  )

})
