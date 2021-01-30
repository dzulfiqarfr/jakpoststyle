httptest::with_mock_api({

  test_that("Jakpost custom theme has been successfully applied", {

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
      jakpost_style(
        chart_id,
        author = "Dzulfiqar Fathur Rahman",
        footnote = stringr::str_c("Last tested on ", format(Sys.time(), "%b %d, %Y"))
      ),
      stringr::str_c("Chart ", chart_id, " succesfully updated.")
    )

  })

})
