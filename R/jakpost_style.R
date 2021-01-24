#' Apply The Jakarta Post theme to Datawrapper charts
#'
#' Use the The Jakarta Post custom theme to edit Datawrapper chart properties.
#'
#' @param chart_id Datawrapper chart id.
#' @param author Byline of the chart.
#' @param footnote Footnote to the chart.
#' @param ... Other arguments passed on to \code{\link[DatawRappr]{dw_edit_chart}},
#' including `intro`, `source_name` and `source_url` for chart subtitle,
#' source and source's link, respectively.
#'
#' @seealso \code{\link[DatawRappr]{dw_edit_chart}} for the underlying function.
#'
#' @examples
#' \dontrun{
#' jakpost_style(
#'   chart_id = "4BcD3",
#'   author = "Dzulfiqar Fathur Rahman",
#'   intro = "Indonesia's gross domestic product (GDP) per capita (in US dollars)",
#'   source_name = "Gapminder"
#' )
#' }
#'
#' @export
jakpost_style <- function(
  chart_id,
  author,
  footnote = "",
  ...
) {

  # Custom theme
  DatawRappr::dw_edit_chart(
    chart_id = chart_id,
    visualize = list(
      `x-grid` = "ticks",
      `y-grid` = "on",
      `y-grid-format` = "0,0.[00]",
      `y-grid-labels` = "inside",
      `y-grid-label-align` = "right",
      `labeling` = "off",
      `label-colors` = "false",
      `show-tooltips` = "true"
    ),
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
    language = "en-US",
    ...
  )

}
