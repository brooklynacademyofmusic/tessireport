% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/p2_segments_and_tags.R
\docType{data}
\name{p2_segments_and_tags}
\alias{p2_segments_and_tags}
\alias{read.p2_segments_and_tags}
\alias{process.p2_segments_and_tags}
\alias{output.p2_segments_and_tags}
\title{p2_segments_and_tags}
\format{
An object of class \code{p2_segments_and_tags} (inherits from \code{report}, \code{list}) of length 0.
}
\usage{
p2_segments_and_tags

\method{read}{p2_segments_and_tags}(data, ...)

\method{process}{p2_segments_and_tags}(
  data,
  n = 50,
  segment_regex = "^Segment of",
  tag_regex = "(?!.*RSVP|.*\\\\(Keep\\\\))\\\\d{6,}",
  ...
)

\method{output}{p2_segments_and_tags}(data, emails = config::get("tessiflow.email"), body = NULL, ...)
}
\arguments{
\item{data}{report object}

\item{...}{additional parameters passed on to \link{send_email}}

\item{n}{number of segments and tags to return}

\item{segment_regex}{Perl-compatible regular expression for filtering segment names}

\item{tag_regex}{Perl-compatible regular expression for filtering tag names}

\item{emails}{email addresses (first will be sender)}

\item{body}{html of message or a list containing \link[sendmailR:mime_part]{sendmailR::mime_part} objects.}
}
\description{
Report of Prospect2 segments and tags for review
}
\section{Methods (by generic)}{
\itemize{
\item \code{read(p2_segments_and_tags)}: load Prospect2 segments and tags

\item \code{process(p2_segments_and_tags)}: filter Prospect2 segments and tags

\item \code{output(p2_segments_and_tags)}: send an email with spreadsheets of segments and tags

}}
\keyword{datasets}
