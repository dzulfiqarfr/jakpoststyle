test_that("Y-axis scale has been successfully modified", {

  x <- dplyr::tibble(
    month = seq(
      lubridate::ymd("2020-01-01"),
      lubridate::ymd("2020-10-01"),
      by = "month"
    ),
    val = seq(2, 20, by = 2)
  )

  chart_id <- "SbzoE"

  DatawRappr::dw_data_to_chart(x, chart_id)

  expect_output(
    jp_scale_y(
      chart_id,
      y = "val",
      scale_y_max = 20,
      scale_y_min_rule = 0,
      scale_y_increment = 5
    ),
    stringr::str_c("Chart ", chart_id, " succesfully updated.")
  )

})
