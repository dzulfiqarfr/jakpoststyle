#' Apply The Jakarta Post theme
#'
#' Add a prefix to the byline, the Post’s logo to the chart and modify axes and
#' the grid lines, in addition to other minor changes.
#'
#' @param chart_id String. Datawrapper chart id.
#' @param author String. Who creates the chart?
#' @param footnote String. Footnote to the chart.
#' @param logo Logical. Defaults to TRUE. When FALSE, the function does not add
#' The Jakarta Post’s logo to the chart.
#' @param ... Other arguments passed on to \code{\link[DatawRappr]{dw_edit_chart}},
#'   including `intro`, `source_name` and `source_url`.
#'
#' @seealso \code{\link[DatawRappr]{dw_edit_chart}} for the underlying function.
#'
#' @examples
#' \dontrun{
#' jp_dw_theme(
#'   chart_id = "4BcD3",
#'   author = "Leonard Hofstadter",
#'   intro = "Indonesia's GDP per capita (in US dollars)",
#'   source_name = "Gapminder"
#' )
#' }
#'
#' @export
jp_dw_theme <- function(
  chart_id,
  author,
  footnote = "",
  logo = TRUE,
  ...
) {

  # Chart type
  chart_type <- DatawRappr::dw_retrieve_chart_metadata(chart_id)$content$type

  # Chart type library
  line_chart_type <- c("d3-lines", "d3-area")
  col_chart_type <- c("column-chart", "grouped-column-chart", "stacked-column-chart")

  # Warning for other chart types
  if (!any(chart_type %in% c(line_chart_type, col_chart_type))) {
    warning(
      stringr::str_c(
        "The `jakpoststyle` package is not optimized for ",
        chart_type,
        ". Some features may not work."
      )
    )
  }

  # Theme
  DatawRappr::dw_edit_chart(
    chart_id = chart_id,
    language = "en-US",
    theme = "datawrapper",
    publish = list(blocks = list(`download-image` = TRUE)),
    visualize = list(
      `x-grid` = "ticks",
      `grid-lines-x` = list(
        type = "ticks",
        enabled = TRUE
      ),
      `y-grid` = "on",
      labeling = "off",
      `label-colors` = "false",
      `show-tooltips` = "true",
      `y-grid-format` = "0,0.[00]",
      `y-grid-labels` = "inside",
      yAxisLabels = list(
        enabled = TRUE,
        alignment = "right",
        placement = "inside"
      ),
      valueLabels = list(
        show = "hover",
        enabled = TRUE,
        placement = "inside"
      ),
      categoryLabels = list(enabled = TRUE, position = "color-key"),
      `y-grid-label-align` = "right"
    ),
    byline = stringr::str_c("JP/", author),
    annotate = stringr::str_c(
      footnote,
      if (logo == TRUE) {
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
        )
      } else {
        ""
      },
      sep = dplyr::if_else(footnote != "", " ", "")
    ),
    ...
  )

}
