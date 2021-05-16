#' Modify x-axis
#'
#' Customize the axis tick format and add an axis line to a chart with a y-axis
#' that doesn’t start at zero.
#'
#' @param chart_id String. Datawrapper chart id.
#' @param format String. Defaults to “auto”. Also takes “monthly” and
#' “quarterly” for date variables.
#' @param axis_line Logical. Defaults to FALSE. When TRUE, adds a black axis line.
#'
#' @seealso \code{\link[DatawRappr]{dw_edit_chart}} for the underlying function.
#'
#' @examples
#' \dontrun{
#' jp_dw_scale_x(
#'   chart_id = "4BcD3",
#'   format = "quarterly",
#'   axis_line = TRUE
#' )
#' }
#'
#' @export
jp_dw_scale_x <- function(
  chart_id,
  format = "auto",
  axis_line = FALSE
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

  # X-axis tick format
  format <- if (format == "monthy") {
    "YYYY|MMM"
  } else if (format == "quarterly") {
    "YYYY|[Q]Q"
  } else {
    format
  }

  # Y-axis lower limit
  y_axis_low_lim <- DatawRappr::dw_retrieve_chart_metadata(chart_id)$content$metadata$visualize$`custom-range-y`[[1]]

  # Existing range annotations
  existing_ra <- DatawRappr::dw_retrieve_chart_metadata(chart_id)$content$metadata$visualize$`range-annotations`

  # Axis line
  axis_line_annotation <- list(
    y0 = y_axis_low_lim + 0.075,
    type = "y",
    color = "#000000",
    display = "line",
    opacity = 100,
    strokeType = "solid",
    strokeWidth = 3
  )

  # Add axis line annotation to existing list of annotations
  existing_ra[[length(existing_ra) + 1]] <- axis_line_annotation

  # Modify x-axis
  DatawRappr::dw_edit_chart(
    chart_id = chart_id,
    visualize = list(
      `x-tick-format` = format,
      `range-annotations` = if (axis_line == TRUE && is.null(existing_ra) == FALSE) {
        existing_ra
      } else if (axis_line == TRUE && is.null(existing_ra) == TRUE) {
        list(axis_line_annotation)
      } else {
        list()
      }
    )
  )

}
