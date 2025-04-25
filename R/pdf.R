#' pdf_plot
#'
#' Add a plot to a pdf by generating R markdown, intended to be used within the `expr` argument to [write_pdf]
#'
#' @param plot output from ggplot or other plot that can be `print`ed
#' @param title character optional title of the plot
#' @param subtitle character subtitle of the plot
#'
#' @export
#' @examples
#' library(ggplot2)
#' p <- ggplot(data.frame(x=runif(100))) + geom_histogram(aes(x))
#' write_pdf(pdf_plot(p, title = "Sample plot", subtitle = "histogram of random numbers"))
#'
pdf_plot <- function(plot, title = NULL, subtitle = NULL) {
  cat("\n\\clearpage\n")
  cat(paste("\n###",title,"  \n"))
  cat(paste("\n",subtitle,"  \n"))
  print(plot)
}

#' pdf_table
#'
#' Add a table to a pdf by generating R markdown, intended to be used  within the `expr` argument to [write_pdf].
#' Thin wrapper around [knitr::kable] that generates latex output.
#'
#' @param x data frame to export
#' @inheritDotParams knitr::kable digits row.names col.names align caption label format.args
#'
#' @export
pdf_table <- function(x, ...) {
   knitr::kable(x, format = "latex", escape = TRUE, longtable = TRUE, booktabs = TRUE)
}

#' write_pdf
#'
#' @param expr expression to execute to create the body of the pdf'
#' @param output_file character filename to write to, defaults to a temporary file
#' @param preamble filename of the yaml preamble used by pandoc for generating the pdf
#' @param .title character title for the header of the pdf; see the preamble for how this variable is passed to pandoc
#' @param .author character author for the header of the pdf; see the preamble for how this variable is passed to pandoc
#' @param .date character date for the header of the pdf; see the preamble for how this variable is passed to pandoc
#' @param .classoption character tex classoption; see the preamble for how this variable is passed to pandoc
#' @param .papersize character tex pagesize; see the preamble for how this variable is passed to pandoc
#' @param .mainfont character tex mainfont; see the preamble for how this variable is passed to pandoc
#' @param .geometry character tex geometry; see the preamble for how this variable is passed to pandoc
#' @param .fontsize character tex fontsize; see the preamble for how this variable is passed to pandoc
#' @param fig.width integer knitr figure width, passed to [knitr::opts_chunk]
#' @param fig.height integer knitr figure height, passed to [knitr::opts_chunk]
#' @returns character filename of pdf file
#' @export
#' @importFrom lubridate today
#' @importFrom rlang enquo eval_tidy
write_pdf <- function(expr, output_file = tempfile(fileext = ".pdf"),
                      .title = NULL, .author = NULL, .date = today(),
                      .classoption = "landscape", .papersize = "legal", .mainfont = "Arial", .geometry = "margin=0.5in", .fontsize = "9pt",
                      fig.width = 14, fig.height = 7.5,
                      preamble = system.file("pdf_memoir-preamble.Rmd", package = "tessireport")) {

  expr = rlang::enquo(expr)

  # read preamble thru knitr::knit(text) to embed variables
  pre <- knitr::knit(text = readLines(preamble))

  # write out call
  rmd_file <- tempfile(fileext = ".Rmd")

  setup <- quote(knitr::opts_chunk$set(echo = FALSE, cache = FALSE, message = FALSE, warning = FALSE, error = FALSE, fig.width = fig.width, fig.height = fig.height))

  writeLines(c(pre,'``` {r setup, echo = F}',deparse(setup),'```','``` {r results="asis"}','eval_tidy(expr)','```'), rmd_file, sep = '\n')

  rmarkdown::render(rmd_file, output_file = output_file, clean = TRUE)

  output_file
}
