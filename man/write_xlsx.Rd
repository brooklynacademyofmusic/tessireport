% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xlsx.R
\name{write_xlsx}
\alias{write_xlsx}
\title{write_xlsx}
\usage{
write_xlsx(
  data,
  filename = tempfile("write_xlsx", fileext = ".xlsx"),
  group = NULL,
  currency = NULL,
  overwrite = FALSE,
  ...
)
}
\arguments{
\item{data}{data to write to the spreadsheet}

\item{filename}{filename to write}

\item{group}{character name of column to be used for grouping/banding the data. Band color will change when column value changes.}

\item{currency}{character vector of column to be used for grouping/banding the data. Band color will change when column value changes.}

\item{overwrite}{If \code{TRUE}, overwrite any existing file.}

\item{...}{not used}
}
\value{
filename of written xlsx
}
\description{
Convenience wrapper around \code{openxlsx::write.xlsx} to handle some common formatting tasks:
\itemize{
\item Column headers are title-cased and bolded
\item Column widths are calculated based on the string representations of the data
\item Date columns are formatted as DATE instead of LONGDATE
}

And some optional additions:
\itemize{
\item Band columns by group
\item Specify currency columns
}
}
