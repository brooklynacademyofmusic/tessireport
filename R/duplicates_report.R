#' @export
duplicates_report <- report(class="duplicates_report")

#' duplicates_report
#'
#' Sends an email containing a spreadsheet of duplicates for review
#' @name duplicates_report
NULL

#' @describeIn duplicates_report read the duplicates data output by `tessistream::duplicates_stream`
#' @importFrom tessilake read_cache
#' @export
read.duplicates_report <- function(duplicates_report, ...) {
  duplicates_report$data <- read_cache("duplicates_stream","stream") %>%
    collect %>% setDT

  NextMethod()
}

#' @describeIn duplicates_report segments the duplicates data based on the map provided
#' by `routing` and emails excel spreadsheets using [send_xlsx]
#' @export
#' @param duplicates_report duplicates_report object
#' @param routing named list, names are email addresses and values are vectors of
#' customer numbers. Any duplicate pair that contains a matching customer number is
#' emailed to the named email address.
#' @inheritDotParams send_email body
#' @importFrom checkmate assert_list
output.duplicates_report <- function(duplicates_report, routing = NULL, ...) {
  keep_customer_no <- delete_customer_no <- NULL

  assert_list(routing, types = "integerish", any.missing = FALSE)

  purrr::imap(routing, \(customer_nos, email) {

    duplicates <- duplicates_report$data[keep_customer_no %in% customer_nos |
                                         delete_customer_no %in% customer_nos,]
    setcolorder(duplicates, c("keep_customer_no","delete_customer_no",
                              "keep_reason",
                              "fname","lname"))

    duplicates[,c("customer_no","i.customer_no") := NULL]

    setnames(duplicates,
             c("fname","lname"),
             c("first_name","last_name"))

    if(nrow(duplicates) > 0)
      rlang::inform(paste("Sending email to",email,"with",nrow(duplicates),
                    "duplicates."))

      send_xlsx(duplicates,
                subject = paste("Duplicates Report", Sys.Date()),
                emails = c(config::get("tessiflow.email"), email),
                ...
      )

  })

  NextMethod()
}

#' @export
process.duplicates_report <- write.duplicates_report <- function(...) {NextMethod()}
