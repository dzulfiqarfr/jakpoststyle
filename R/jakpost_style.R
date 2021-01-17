#' Apply The Jakarta Post's unofficial style for Datawrapper chart
#'
#' Edit properties of a Datawrapper chart with an unofficial style of
#' The Jakarta Post using the `DatawRappr::dw_edit_chart()` function.
#'
#' @param chart_id A character of Datawrapper chart id.
#' @param chart_type The type of the chart (see chart types section).
#' @param y The variable mapped on the y axis.
#' @param scale_y_min_limit An option to customize the y axis range. Defaults to 0.
#' "truncated" adds a space between the lower limit of the axis range and the minimum value of the variable.
#' "min" sets the minimum value of the variable as the lower limit of the axis range.
#' @param scale_y_max_limit_add The default upper limit of the y axis range is the highest value in the variable.
#' This argument takes a value to raise the upper limit by adding it to the default value.
#' @param axis_ticks_y This argument takes two values for `breaks` and `addition`.
#' The first is a value to set the breaks of the y axis. The second value adds to the upper limit of the axis.
#' @param headline The title of the chart.
#' @param subtitle The subtitle of the chart.
#' @param author The byline of the chart.
#' @param source The data source.
#' @param source_url The link to the data source.
#' @param footnote A footnote to the chart.
#'
#' @return A message telling whether a chart has been successfully updated or not.
#'
#' @note Chart types
#' This function takes the following custom inputs to define the chart types:
#' - "line" (which translates into "d3-lines")
#' - "column" (which translates into "column-chart")
#' - "bar" (which translates into "d3-bars")
#' - "area" (which translates into "d3-area")
#' - "scatter_plot" (which translates into "d3-scatter-plot")
#' - "stacked_bar" (which translates into "d3-bars-stacked")
#'
#' Otherwise the `chart_type` argument takes any values accepted by Datawrapper
#' (see the Chart Types section in the `DataWrappr::dw_edit_chart()` documentation).
#'
#' @seealso \code{\link[DatawRappr]{dw_edit_chart}} for the underlying function.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' jakpost_style(
#'   chart_id = "4BcD3",
#'   chart_type = "line",
#'   y = gapminder$gdpPercap,
#'   scale_y_min_limit = 0,
#'   scale_y_max_limit_add = 500,
#'   axis_ticks_y = c(1000, -40),
#'   headline = "Test",
#'   subtitle = "Test",
#'   author = "Dzulfiqar Fathur Rahman",
#'   source = "Gapminder",
#'   source_url = ""
#' )
#' }
jakpost_style <- function(
  chart_id,
  chart_type,
  y,
  scale_y_min_limit = 0,
  scale_y_max_limit_add = 0,
  axis_ticks_y = c(breaks = 0, addition = 0),
  headline = "Title",
  subtitle = "Description",
  author,
  source = "",
  source_url = "",
  footnote = ""
) {

  DatawRappr::dw_edit_chart(
    chart_id = chart_id,

    type = if (chart_type == "line") {
      "d3-lines"
    } else if (chart_type == "column") {
      "column-chart"
    } else if (chart_type == "bar") {
      "d3-bars"
    } else if (chart_type == "area") {
      "d3-area"
    } else if (chart_type == "scatter_plot") {
      "d3-scatter-plot"
    } else if (chart_type == "stacked_bar") {
      "d3-bars-stacked"
    } else {
      chart_type
    },

    visualize = list(
      'base-color' = "#1d81a2",
      'x-grid' = "ticks",

      'custom-range-y' = range <- c(
        if (scale_y_min_limit == "truncated") {
          floor((3 * min(y) - max(y)) / 2)
        } else if (scale_y_min_limit == "min") {
          round(y)
        } else {
          0
        },
        round(ceiling(max(y)) + scale_y_max_limit_add)
      ),

      'custom-ticks-y' = stringr::str_c(
        seq(
          range[1],
          range[2] + axis_ticks_y[[2]],
          by = axis_ticks_y[[1]]
        ),
        collapse = ", "
      ),

      'y-grid-format' = "0,0.[00]",
      'y-grid' = "on",
      'y-grid-labels' = "inside",
      'y-grid-label-align' = "right",
      'labeling' = "off",
      'label-colors' = "false",
      'show-tooltips' = "true"
    ),
    title = headline,
    intro = subtitle,
    byline = stringr::str_c("JP/", author),
    annotate = stringr::str_c(
      footnote,
      stringr::str_c(
      '<b style="',
      'float:right; ',
      'margin: 0px; ',
      'width: 100px; ',
      'height: 12px; ',
      'background: ',
      'url(http://www.pressdisplay.com/res/en-us/g22480/t217484168/2/images/se-jakarta_hd_logo.png); ',
      'background-size: 100px 12px;',
      '"></b>'
      ),
      sep = dplyr::if_else(footnote != "", " ", "")
    ),
    source_name = source,
    source_url  = source_url,
    theme = "datawrapper",
    language = "en-US"
  )

}
