#' Customize y-axis of Datawrapper charts
#'
#' Customize the y-axis limits and major breaks of Datawrapper charts.
#'
#' @param chart_id Datawrapper chart id.
#' @param y Variable mapped to the y-axis.
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
#'   y = gapminder$lifeExp,
#'   scale_y_min_rule = 0,
#'   scale_y_round = -2, # Rounds to the nearest hundred
#'   scale_y_increment = 20
#' )
#' }
#'
#' @export
jp_scale_y <- function(chart_id, y, scale_y_max = max(y),
                       scale_y_min_rule = "min", scale_y_round,
                       scale_y_increment
) {

  # Rules for y-axis
  y_axis_range <- c(
    if (scale_y_min_rule == "truncated") {
      round((3 * min(y) - scale_y_max) / 2, digits = scale_y_round)
    } else if (scale_y_min_rule == "min") {
      round(min(y), digits = scale_y_round)
    } else {
      scale_y_min_rule
    },
    round(scale_y_max, scale_y_round)
  )

  # Modify y-axis
  DatawRappr::dw_edit_chart(
    chart_id = chart_id,
    visualize = list(
      `custom-range-y` = y_axis_range,
      `custom-ticks-y` = stringr::str_c(
        seq(y_axis_range[1], y_axis_range[2], by = scale_y_increment),
        collapse = ", "
      )
    ),
    theme = "datawrapper",
    language = "en-US"
  )

}
