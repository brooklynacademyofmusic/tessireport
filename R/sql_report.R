#' sql_report
#'
#' Report that wraps the functionality of [tessilake::read_sql] and [send_xlsx]
#'
#' @param sql_report sql_report object
#' @inheritDotParams tessilake::read_sql freshness primary_keys
#' @inheritDotParams send_xlsx subject body emails basename
#' @inheritDotParams write_xlsx group currency
#' @inheritDotParams mailR::send.mail html inline
#' @name sql_report
#' @examples
#' \dontrun{
#'  run(sql_report,
#'       query = "select * from my_table",
#'       subject = "Email subject",
#'       body = "Email body",
#'       emails = "me@me.com"
#'  )
#' }
#' @export
run.sql_report <- process.sql_report <- write.sql_report <-  function(sql_report, ...) NextMethod()

#' @export
sql_report <- report(class="sql_report")

#' @describeIn sql_report reads sql_report data
#' @param query character sql query to generate the report
#' @importFrom tessilake read_sql
#' @importFrom checkmate expect_character
#' @importFrom dplyr collect
#' @importFrom utils modifyList
#' @export
read.sql_report <- function(sql_report, query, ...) {
  . <- NULL

  expect_character(query, len = 1)

  # compute args from the formals of `read_sql` and `...` plus `query` and enforcing non-incremental loads
  args <- modifyList(rlang::list2(...), list(query=query, incremental=FALSE, overwrite=TRUE)) %>%
    .[intersect(names(.), rlang::fn_fmls_names(tessilake::read_sql))]

  sql_report$data <- do.call(read_sql,args) %>% collect %>% setDT

  NextMethod()
}


#' @describeIn sql_report sends sql_report data
#'
#' @importFrom tessilake read_sql
#' @export
output.sql_report <- function(sql_report, query, ...) {

  # do this in a `local` block so that the name of the object gets passed on :)
  local({
    sql_report <- sql_report$data
    send_xlsx(table = sql_report, ...)
  })

  NextMethod()
}

