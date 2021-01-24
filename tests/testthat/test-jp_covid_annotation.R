test_that("COVID-19 annotation has been successfully added", {

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
    jp_covid_annotation(
      chart_id,
      covid_text_y = 20
    ),
    stringr::str_c("Chart ", chart_id, " succesfully updated.")
  )

})
