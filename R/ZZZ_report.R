#' report
#'
#' @description Base S3 class for report.
#'
#' @param x a list
#' @param class an optional additional subclass
#' @param ... additional parameters for subclassed methods
#'
#' @rdname report-class
#' @name report
#' @export
#'
report = function(x = list(),class=character()) {
  stopifnot(is.list(x))
  structure(x, class=c(class,"report",class(x)))
}

#' @describeIn report composable argument for report classes
#'
#' @param report_x report
#' @param report_y report
#' @return report that contains data from `report_x` and `report_y` and inherits from both `report_x` and `report_y`
#'
#' @export
`%+%` = function(report_x,report_y) {
  class_x_tail <- tail(class(report_x),2)
  class_x <- head(class(report_x),-2)
  class_y_tail <- tail(class(report_y),2)
  class_y <- head(class(report_y),-2)

  structure(as.list(modifyList(report_x,report_y)),class = unique(c(class_x,class_y,class_x_tail,class_y_tail)))
}

default_function <- function(fun_name) {
  function(x, ...) {
    if(!"NextMethod" %in% sapply(rlang::trace_back()$call,rlang::call_name))
      warning(paste0("No `",fun_name,"` function defined for object of class (",paste(class(x),collapse = ", "),"), doing nothing"))
    x
  }
}

#' @export
#' @describeIn report tests if x is an report object
is_report = function(x) inherits("report")

#' @export
#' @describeIn report generic function to dispatch for data reading
read = function(...) UseMethod("read")
#' @export
#' @describeIn report does thing but print a warning, intended to be overloaded by subclass
read.report = default_function("read")

#' @export
#' @describeIn report generic function to dispatch for data processing
process = function(...) UseMethod("process")
#' @export
#' @describeIn report does thing but print a warning, intended to be overloaded by subclass
process.report = default_function("process")

#' @export
#' @describeIn report prints the names and contents of the report object
print.report = function(x, ...) {
  cli::cli_h1(paste(class(x)[1],"with contents:"))
  cli::cli_ul()
  purrr::imap(x, ~{
    cli::cli_li(cli::col_cyan(.y))
    cli::cli_bullets(c(" " = paste0(names(.x),collapse=", ")))
    })
}

#' @export
#' @describeIn report generic fucntion to dispatch for data output
output = function(...) UseMethod("output")
#' @export
#' @describeIn report does thing but print a warning, intended to be overloaded by subclass
output.report = default_function("output")

#' @export
#' @describeIn report generic function to dispatch for data saving
write = function(...) UseMethod("write")
#' @export
#' @describeIn report does thing but print a warning, intended to be overloaded by subclass
write.report = default_function("write")

#' @export
#' @describeIn report generic function to dispatch for report running
run = function(...) UseMethod("run")
#' @export
#' @describeIn report run all methods in order: read -> process -> write -> output
#' @importFrom rlang try_fetch abort cnd_signal is_call call_name
run.report = function(x,...) {
  try_fetch(x %>%
    read(...) %>%
    process(...) %>%
    write(...) %>%
    output(...),
    error = \(e) {
      if(is_call(e$call) && call_name(e$call) %||% "" == "UseMethod")
        abort(c("Did you forget to return a `report` object from `read`, `process`, `output` or `write`?",
                       "Hint: you can use NextMethod() to work with inherited methods as well."),
                     parent = e)
      cnd_signal(e)
    })
}
