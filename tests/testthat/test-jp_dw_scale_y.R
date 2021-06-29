test_that("Y-axis format", {

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
    jp_dw_scale_y(
      chart_id,
      y_var = "val",
      max = 6,
      min = -12,
      num_ticks = 6
    ),
    stringr::str_c("Chart ", chart_id, " succesfully updated.")
  )

})
