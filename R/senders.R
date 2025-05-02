#' send_email
#'
#' @param emails email addresses (first will be sender)
#' @param subject character subject of the email
#' @param smtp named list, should contain `hostname` and (optionally) `port`, which defaults to 25.
#'  Additional elements will get merged with `...` and passed to curl, see [curl::curl_options] for more details.
#' @param body html of message or a list containing [sendmailR::mime_part] objects.
#' @param attachments named vector, names are the attachment names, values are the filenames to attach.
#' @importFrom checkmate assert_character test_character test_list assert check_character check_list
#' @importFrom sendmailR sendmail mime_part mime_part_html
#' @importFrom purrr keep_at imap
#' @inheritParams sendmailR::sendmail
#' @export
send_email <- function(subject, body = paste("Sent by", Sys.info()["nodename"]),
                       emails = config::get("tessiflow.email"),
                       smtp = config::get("tessiflow.smtp"),
                       attachments = NULL, engine = "curl",
                       ...
) {
  assert_character(subject, len = 1)
  assert(
    check_character(body, len = 1),
    check_list(body, "mime_part")
  )
  assert_character(attachments, null.ok = T)

  if (!test_character(emails, min.len = 1)) {
    stop("Set tessiflow.email to the sender (first email) and list of recipients for messages")
  }
  if (!test_list(smtp)) {
    stop("Set tessiflow.smtp to a list containing the `hostname` and optionally `port` of the smtp server")
  }

  if (typeof(body) == "character")
    body <- mime_part_html(body)

  if (length(attachments))
    body <- c(body, imap(attachments,\(x,n) mime_part(x,n)))

  dots <- modifyList(list(...),list(
                     from = emails[[1]],
                     to = emails,
                     subject = subject,
                     msg = body,
                     control = list(smtpServer=smtp$hostname,smtpPort=smtp$port),
                     engine = engine,
                     engineopts = keep_at(smtp,names(curl::curl_options()))))

  do.call(sendmail,dots)
}



#' send_file
#'
#' Helpers for sending a file via email
#'
#' @param name name of the file to use in the email. Defaults to the name of the table or the inputted filename.
#' A timestamp and extension will be appended and passed on to [send_email] as the name of `attachments`.
#' @inheritParams send_email
#' @inheritDotParams send_email subject body emails smtp engine
#' @export
send_file <- function(filename, name = basename(filename),
                      subject = paste(name, Sys.Date()), ...) {
  assert_character(filename, len = 1)

  name <- paste0(file_path_sans_ext(name),"_",Sys.Date(),
                 ".",file_ext(filename))

  send_email_args <- modifyList(list(attachments = setNames(filename,name),
                                     subject = subject),
                                rlang::list2(...))

  do.call(send_email,send_email_args)

}

#' @describeIn send_file Simple wrapper for [send_file] and [write_xlsx]
#' @inheritParams write_xlsx
#' @param table data.table to send
#' @inheritDotParams write_xlsx group currency
#' @inheritDotParams send_email subject body emails
#' @export
send_xlsx <- function(table,
                      basename = format(substitute(table)), ...) {
  assert_data_table(table)

  filename <- write_xlsx(table, ...)

  send_file(filename, basename = basename, ...)
}

#' @param report `report` object
#' @param report_filename `character(1)` name of the field that contains the filename
#' @inheritDotParams send_file name
#' @describeIn send_file report definition for sending a file by email
#' @importFrom checkmate assert_data_table assert_character assert_file_exists
#' @importFrom tools file_path_sans_ext file_ext
#' @export
output.email_report <- function(report, report_filename = "filename", ...) {
  assert_character(report_filename, len = 1)
  assert_character(report[[report_filename]], len = 1)
  assert_file_exists(report[[report_filename]])

  send_file_args <- modifyList(list(name = class(report)[[1]]), list2(...))

  do.call(send_file, c(report[[report_filename]], send_file_args))

  NextMethod()

}
