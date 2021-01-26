#' Add COVID-19 pandemic annotation to Datawrapper charts
#'
#' Add line and text annotations for COVID-19 pandemic to Datawrapper charts.
#'
#' @param chart_id Datawrapper chart id.
#' @param date Date of first coronavirus case(s) being reported.
#' Defaults to "2020/03", in which Indonesia reported its first two confirmed cases.
#' @param covid_text_y Position of the COVID-19 text annotation on the y-axis.
#' @param text_color Color of the text annotation. Defaults to "#888".
#' @param text_bold Make the text annotation bold. Defaults to `FALSE`.
#' @param line_color Color of the line annotation. Defaults to "#888".
#' @param line_type Line type for the line annotation.
#' Defaults to "solid". "dotted" gives dotted line. "dashed" gives dashed line.
#' @param line_width Width of the line annotation, ranging from 1 to 3.
#' Defaults to 1.
#'
#' @seealso \code{\link[DatawRappr]{dw_edit_chart}} for the underlying function.
#'
#' @examples
#' \dontrun{
#' jp_covid_annotation(
#'   chart_id = "4BcD3",
#'   covid_text_y = 6,
#'   text_color = "#ee493a",
#'   text_bold = TRUE,
#'   line_color = "#ee493a",
#'   line_type = "dashed",
#'   line_width = 2
#' )
#' }
#'
#' @export
jp_covid_annotation <- function(
  chart_id,
  date = "2020/03",
  covid_text_y = NA,
  text_color = "#888",
  text_bold = FALSE,
  line_color = "#888",
  line_type = "solid",
  line_width = 1
) {

  # COVID-19 text annotation
  text_covid <- list(
    list(
      x = date,
      y =  covid_text_y,
      bg = FALSE,
      dx = 5,
      dy = 15,
      bold = text_bold,
      size = 10,
      text = "COVID-19\npandemic &#8594;",
      align = "ml",
      color = text_color,
      width = 25,
      italic = FALSE,
      underline = FALSE,
      showMobile = TRUE,
      showDesktop = TRUE,
      connectorLine = list(
        type = "straight",
        circle = FALSE,
        stroke = 1,
        enabled = FALSE,
        arrowHead = "lines",
        circleStyle = "solid",
        circleRadius = 15,
        inheritColor = FALSE,
        targetPadding = 4
      ),
      mobileFallback = FALSE
    )
  )

  # COVID-19 line annotation
  line_covid <-  list(
    list(
      x0 = date,
      type = "x",
      color = line_color,
      display = "line",
      opacity = 100,
      strokeType = line_type,
      strokeWidth = line_width
    )
  )

  # Add the text and line annotations
  DatawRappr::dw_edit_chart(
    chart_id = chart_id,
    visualize = list(
      `range-annotations` = line_covid,
      `text-annotations` = text_covid
    ),
    language = "en-US"
  )

}
