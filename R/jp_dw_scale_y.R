#' Modify y-axis
#'
#' Customize the scale limits and breaks for a y-axis that maps a continuous variable.
#'
#' @param chart_id String. Datawrapper chart id.
#' @param y_var String. The variable name mapped on the y-axis.
#' @param max Integer. The upper limit of the scale. Defaults to
#' the highest value in the variable.
#' @param min The lower limit of the scale. Defaults to
#' the lowest value in the variable. Also takes an integer and “truncated”,
#' which leaves some one-third space of the plot area at the bottom empty
#' for a chart that doesn’t start at zero.
#' @param increment Integer. Increment of the breaks.
#'
#' @seealso \code{\link[DatawRappr]{dw_edit_chart}} for the underlying function.
#'
#' @examples
#' \dontrun{
#' jp_dw_scale_y(
#'   chart_id = "4BcD3",
#'   y_var = "lifeExp",
#'   min = 0,
#'   increment = 20
#' )
#' }
#'
#' @export
jp_dw_scale_y <- function(
  chart_id,
  y_var,
  max = max(y_var, na.rm = TRUE),
  min = "min",
  increment
) {

  # Chart type
  chart_type <- DatawRappr::dw_retrieve_chart_metadata(chart_id)$content$type

  # Chart type library
  line_chart_type <- c("d3-lines", "d3-area")
  col_chart_type <- c("column-chart", "grouped-column-chart", "stacked-column-chart")

  # Stop for other chart types
  if (!any(chart_type %in% c(line_chart_type, col_chart_type))) {
    stop(
      stringr::str_c(
        "The `jakpoststyle` package is not optimized for ",
        chart_type,
        ". Some features may not work."
      )
    )
  }

  # Variable mapped to the y-axis
  scale_y_var <- DatawRappr::dw_data_from_chart(chart_id)[[y_var]]

  # Rules for y-axis
  range <- c(
    if (min == "truncated") {
      round((3 * min(scale_y_var, na.rm = TRUE) - max) / 2)
    } else if (min == "min") {
      round(min(scale_y_var, na.rm = TRUE))
    } else {
      min
    },
    round(max)
  )

  # Custom range and ticks for line chart
  scale_y_line <- list(
    `custom-range-y` = range,
    `custom-ticks-y` = stringr::str_c(
      seq(range[1], range[2], by = increment),
      collapse = ", "
    )
  )

  # Custom range and ticks for column chart
  scale_y_col <- list(
    `custom-range` = range,
    `custom-ticks` = stringr::str_c(
      seq(range[1], range[2], by = increment),
      collapse = ", "
    )
  )

  # Modify y-axis
  DatawRappr::dw_edit_chart(
    chart_id = chart_id,
    visualize = if (chart_type %in% line_chart_type) {
      scale_y_line
    } else {
      scale_y_col
    }
  )

}
