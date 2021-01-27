#' Customize y-axis of Datawrapper charts
#'
#' Customize the y-axis limits and major breaks of Datawrapper charts.
#'
#' @param chart_id Datawrapper chart id.
#' @param y Variable mapped to the y-axis, as a string.
#' @param scale_y_max Maximum value of the y-axis range.
#' Defaults to the highest value in the variable.
#' @param scale_y_min_rule Minimum value of the y-axis range.
#' Defaults to "min", which sets the lowest value in the variable
#' as the start of the scale. "truncated" leaves some one-third space
#' of the plot area empty between the lowest value in the variable
#' and the start of the scale.
#' @param scale_y_round Number of decimal places for all values in the y-axis.
#' Negative values round to a power of ten.
#' @param scale_y_increment Increment of the y-axis breaks.
#'
#' @seealso \code{\link[DatawRappr]{dw_edit_chart}} and \code{\link[base]{round}}
#' for the underlying functions.
#'
#' @examples
#' \dontrun{
#' jp_scale_y(
#'   chart_id = "4BcD3",
#'   y = "lifeExp",
#'   scale_y_min_rule = 0,
#'   scale_y_round = -2, # Rounds to the nearest hundred
#'   scale_y_increment = 20
#' )
#' }
#'
#' @export
jp_scale_y <- function(
  chart_id,
  y,
  scale_y_max = max(y),
  scale_y_min_rule = "min",
  scale_y_round,
  scale_y_increment
) {

  # Chart type
  chart_type <- DatawRappr::dw_retrieve_chart_metadata(chart_id)$content$type


  # Chart type libraries
  line_type <- c("d3-lines", "d3-area")
  col_type <- c("column-chart", "grouped-column-chart", "stacked-column-chart")

  # Stop for other chart types
  if (!any(chart_type %in% c(line_type, col_type))) {
    stop(
      stringr::str_c(
        "Unable to apply jp_scale_y() to ",
        chart_type,
        ". The function is not optimized for this chart type."
      )
    )
  }

  # Variable mapped to the y-axis
  scale_y_var <- DatawRappr::dw_data_from_chart(chart_id)[[y]]


  # Rules for y-axis
  range <- c(
    if (scale_y_min_rule == "truncated") {
      round((3 * min(scale_y_var) - scale_y_max) / 2, digits = scale_y_round)
    } else if (scale_y_min_rule == "min") {
      round(min(scale_y_var), digits = scale_y_round)
    } else {
      scale_y_min_rule
    },
    round(scale_y_max, scale_y_round)
  )


  # Custom range and ticks for line chart
  scale_y_line <- list(
    `custom-range-y` = range,
    `custom-ticks-y` = stringr::str_c(
      seq(range[1], range[2], by = scale_y_increment),
      collapse = ", "
    )
  )


  # Custom range and ticks for column chart
  scale_y_col <- list(
    `custom-range` = range,
    `custom-ticks` = stringr::str_c(
      seq(range[1], range[2], by = scale_y_increment),
      collapse = ", "
    )
  )


  # Modify y-axis
  DatawRappr::dw_edit_chart(
    chart_id = chart_id,
    visualize = if (chart_type %in% line_type) {
      scale_y_line
    } else {
      scale_y_col
    },
    theme = "datawrapper",
    language = "en-US"
  )

}
