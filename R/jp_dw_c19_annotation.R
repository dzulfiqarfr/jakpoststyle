#' Add COVID-19 annotations
#'
#' Add line and text annotations to highlight the COVID-19 pandemic in
#' a chart that maps a date variable on the x-axis and a continuous variable
#' on the y-axis.
#'
#' @param chart_id String. Datawrapper chart id.
#' @param x_position String. The line and text annotations’ position on the x-axis.
#' Defaults to “2020/03”.
#' @param y_position Integer. The text annotation’s position on the y-axis.
#' @param text_bold Logical. Defaults to FALSE.
#' @param lty String. The type of line annotation. Defaults to “dotted”.
#' Also takes “solid” and “dashed”.
#' @param lwd Integer. The width of line annotation, which ranges from 1 to 3.
#' Defaults to 2.
#'
#' @seealso \code{\link[DatawRappr]{dw_edit_chart}} for the underlying function.
#'
#' @examples
#' \dontrun{
#' jp_dw_c19_annotation(
#'   chart_id = "4BcD3",
#'   y_position = 6,
#'   text_bold = TRUE,
#'   lty = "dashed",
#'   lwd = 2
#' )
#' }
#'
#' @export
jp_dw_c19_annotation <- function(
  chart_id,
  x_position = "2020/03",
  y_position = NA,
  text_bold = FALSE,
  lty = "dotted",
  lwd = 2
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

  # Existing range annotations
  existing_ra <- DatawRappr::dw_retrieve_chart_metadata(chart_id)$content$metadata$visualize$`range-annotations`

  # COVID-19 text annotation
  text_c19_annotation <- list(
    list(
      x = x_position,
      y =  y_position,
      bg = TRUE,
      dx = 5,
      dy = 15,
      bold = text_bold,
      size = 10,
      text = "COVID-19\npandemic &#8594;",
      align = "ml",
      color = "#90a4ae",
      width = 50,
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
  line_c19_annotation <-  list(
    x0 = x_position,
    y0 = x_position,
    type = "x",
    color = "#90a4ae",
    display = "line",
    opacity = 100,
    strokeType = lty,
    strokeWidth = lwd
  )

  # Add axis line annotation to existing list of annotations
  existing_ra[[length(existing_ra) + 1]] <- line_c19_annotation

  # Add text and line annotations to chart
  DatawRappr::dw_edit_chart(
    chart_id = chart_id,
    visualize = list(
      `text-annotations` = text_c19_annotation,
      `range-annotations` = if (is.null(existing_ra) == FALSE) {
        existing_ra
      } else if (is.null(existing_ra) == TRUE) {
        list(line_c19_annotation)
      } else {
        list()
      }
    )
  )

}
