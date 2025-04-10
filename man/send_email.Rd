% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/senders.R
\name{send_email}
\alias{send_email}
\title{send_email}
\usage{
send_email(
  subject,
  body = paste("Sent by", Sys.info()["nodename"]),
  emails = config::get("tessiflow.email"),
  smtp = config::get("tessiflow.smtp"),
  attachments = NULL,
  engine = "curl",
  ...
)
}
\arguments{
\item{subject}{character subject of the email}

\item{body}{html of message or a list containing \link[sendmailR:mime_part]{sendmailR::mime_part} objects.}

\item{emails}{email addresses (first will be sender)}

\item{smtp}{named list, should contain \code{hostname} and (optionally) \code{port}, which defaults to 25.
Additional elements will get merged with \code{...} and passed to curl, see \link[curl:curl_options]{curl::curl_options} for more details.}

\item{attachments}{named vector, names are the attachment names, values are the filenames to attach.}

\item{engine}{One of: \itemize{
\item{\code{"internal"} for the internal smtp transport (default).}
\item{\code{"curl"} for the use of curl for transport. Enable if you need STARTTLS/SSL
and/or SMTP authentication. See \code{curl::\link[curl]{send_mail}}.}
\item{\code{"debug"} sendmail returns a RFC2822 formatted email message without sending it.}
}}

\item{...}{...}
}
\description{
send_email
}
