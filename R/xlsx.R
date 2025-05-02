
#' @describeIn write_xlsx report
#' @param report `report` object
#' @param report_data `character(1)` name of the field in `report` that contains the data
#' @param columns `list` of named expressions mapping column names to their values,
#' will be evaluated in the environment of `attendance_report[[data]]`
#' @inheritDotParams write_xlsx filename group currency overwrite
write.xlsx_report <- function(report, report_data, columns = NULL, ...) {
  columns <- rlang::enexpr(columns)
  assert_true(call_name(columns) == "list")

  output <- setDT(report[[report_data]])[,eval(columns)]
  write_xlsx(output, ...)

  NextMethod()

}

#' write_xlsx
#'
#' @description
#' Convenience wrapper around `openxlsx::write.xlsx` to handle some common formatting tasks:
#' - Column headers are title-cased and bolded
#' - Column widths are calculated based on the string representations of the data
#' - Date columns are formatted as DATE instead of LONGDATE
#'
#' And some optional additions:
#' - Band columns by group
#' - Specify currency columns
#'
#' @param data data to write to the spreadsheet
#' @param filename filename to write
#' @param group character name of column to be used for grouping/banding the data. Band color will change when column value changes.
#' @param currency character vector of column to be used for grouping/banding the data. Band color will change when column value changes.
#' @param ... not used
#' @inheritParams openxlsx::saveWorkbook
#'
#' @return filename of written xlsx
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @importFrom openxlsx addStyle createStyle setColWidths
#' @importFrom purrr map_lgl map_int
#' @importFrom lubridate is.Date is.POSIXct
write_xlsx <- function(data, filename = tempfile("write_xlsx", fileext = ".xlsx"),
                       group = NULL, currency = NULL, overwrite = FALSE, ...) {
  . <- NULL

  if(!is.null(group))
    expect_character(group, len = 1)
  if(!is.null(currency))
    expect_character(currency)

  wb <- createWorkbook()
  addWorksheet(wb, "Sheet 1")

  old_colnames <- colnames(data)
  colnames(data) <-
    # snakecase to spaces
    gsub("_"," ",colnames(data)) %>%
    # camelcase to spaces
    {gsub("([a-z])([A-Z])","\\1 \\2",.)} %>%
    # capitalize first letter
    {gsub("^(.)","\\U\\1",.,perl=T)} %>%
    tools::toTitleCase()

  # bold the first row
  writeData(wb,1,data,
            rowNames = FALSE,
            headerStyle = createStyle(textDecoration = "bold"))

  colnames(data) <- old_colnames

  # format date columns
  addStyle(wb,1,createStyle(numFmt = "DATE"),
           rows = 1:nrow(data) + 1,
           cols = which(map_lgl(data, ~is.Date(.))),
           gridExpand = TRUE,
           stack = TRUE)

  # calculate column widths
  col_widths <- map_int(data, ~max(c(nchar(format(.)),0),na.rm=T))
  colname_widths <- nchar(colnames(data))
  setColWidths(wb,1,seq_along(data),pmax(col_widths, colname_widths) + 3)

  # banding groups
  if(!is.null(group)) {
    band_lengths <- rle(data[[group]])$lengths
    banding <- Vectorize(rep.int)(c(T,F),band_lengths) %>% unlist
    addStyle(wb, 1, createStyle(fgFill = "lightgray"),
             rows = which(banding) + 1,
             cols = 1:ncol(data),
             gridExpand = TRUE,
             stack = TRUE)
  }

  # currency columns
  if(!is.null(currency)) {
    addStyle(wb, 1, createStyle(numFmt = "CURRENCY"),
             rows = 1:nrow(data) + 1,
             cols = match(currency, colnames(data)),
             gridExpand = TRUE,
             stack = TRUE)
  }

  openxlsx::saveWorkbook(wb, filename, overwrite = overwrite)

  filename
}
