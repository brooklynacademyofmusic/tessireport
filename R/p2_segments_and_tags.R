#' p2_segments_and_tags
#'
#' Report of Prospect2 segments and tags for review
#'
#' @param data report object
#'
#' @export
p2_segments_and_tags <- report(class="p2_segments_and_tags")
p2_query_api <- tessistream:::p2_query_api

#' @export
#' @describeIn p2_segments_and_tags load Prospect2 segments and tags
read.p2_segments_and_tags <- function(data, ...) {

  updated_timestamp <- NULL

  keys <- c("segments","tags")
  for(key in keys) {
    value <- p2_query_api(file.path(tessistream:::api_url,"api/3/",key))[[key]]

    value$updated_timestamp <- unlist(value$updated_timestamp)
    setkey(value,updated_timestamp)

    data[[key]] <- value
  }

  NextMethod()
}

#' @param segment_regex Perl-compatible regular expression for filtering segment names
#' @param tag_regex Perl-compatible regular expression for filtering tag names
#'
#' @export
#' @describeIn p2_segments_and_tags filter Prospect2 segments and tags
process.p2_segments_and_tags <- function(data,
                                         n = 50,
                                         segment_regex = "^Segment of",
                                         tag_regex = "(?!.*RSVP|.*\\(Keep\\))\\d{6,}", ...) {

  . <- name <- tag <- created_timestamp <- seriesid <- hidden <- id <- NULL

  data$segments <- data$segments[grepl(segment_regex,name, perl = T) &
                                   seriesid == 0 & hidden == 0,
                                 .(id,name,created_timestamp)] %>% first(n)
  data$tags <- data$tags[grepl(tag_regex,tag, perl = T),
                         .(id,tag,created_timestamp)] %>% first(n)

  NextMethod()
}


#' @inheritParams send_email
#' @param ... additional parameters passed on to [send_email]
#' @export
#' @describeIn p2_segments_and_tags send an email with spreadsheets of segments and tags
output.p2_segments_and_tags <- function(data, emails = config::get("tessiflow.email"), body = NULL, ...) {

    filenames <- sapply(data,write_xlsx)

    send_email(subject = paste("P2 segments and tags",Sys.Date()),
               emails = emails,
               body = body %||% paste("Sent by", Sys.info()["nodename"]),
               attachments = setNames(filenames,paste0(c("segments","tags"),"_",Sys.Date(),".xlsx")), ...)

    NextMethod()
}
