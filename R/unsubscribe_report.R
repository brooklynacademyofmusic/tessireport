#' @export
unsubscribe_report <- report(class="unsubscribe_report")

#' unsubscribe_report
#'
#' Reports on unsubscribes and bad addresses for a group of customers
#'
#' @param unsubscribe_report unsubscribe_report object
#' @param ... not used
#' @param customers integer vector of customer numbers to load
#' @name unsubscribe_report
#' @examples
#' \dontrun{
#' run(unsubscribe_report,
#'     customers = c(1,2,3),
#'     routing_rules = list(grepl("GOV", constituencies) ~ list("eleszynski@bam.org"),
#'                          grepl("CP\\d", constituencies) ~ list("lmcgee@bam.org"),
#'                          grepl("\\+",constituencies) ~ list(c("apratama@bam.org","jhindle@bam.org")),
#'                          TRUE ~ list(c("kburke@bam.org","esearles@bam.org"))))
#' }
#'
NULL

#' @describeIn unsubscribe_report reads unsubscribe_report data
#' Loads data from
#' * p2: unsubscribes and hard bounces by list (uses p2_stream_enriched from tessistream)
#' * tessi: emails, addresses, logins, memberships, constituencies, MGOs (attributes)
#' @importFrom checkmate assert_class assert_integerish
#' @importFrom tessilake read_cache read_tessi
#' @importFrom dplyr filter collect
#' @importFrom data.table setDT
#' @export
read.unsubscribe_report <- function(unsubscribe_report, customers, ...) {
  keyword_desc <- customer_no <- NULL

  assert_class(unsubscribe_report, "unsubscribe_report")
  assert_integerish(customers)

  # load email/unsubscribe data
  unsubscribe_report$email_events <- read_cache("p2_stream_enriched","stream")
  unsubscribe_report$emails <- read_tessi("emails")

  # load address data
  unsubscribe_report$addresses <- read_tessi("addresses")

  # load login data
  unsubscribe_report$logins <- read_tessi("logins")

  # load MGO data
  unsubscribe_report$MGOs <- read_tessi("attributes") %>%
    filter(grepl("Major Gifts|PSD signatory",keyword_desc))

  # load constituency data
  unsubscribe_report$constituencies <- read_tessi("constituencies")

  # load membership data
  unsubscribe_report$memberships <- read_tessi("memberships")

  # load name data
  unsubscribe_report$customers <- read_tessi("customers")

  for(obj in names(unsubscribe_report))
    unsubscribe_report[[obj]] <-
                filter(unsubscribe_report[[obj]], customer_no %in% customers) %>%
                collect %>% setDT

  unsubscribe_report
}

#' @describeIn unsubscribe_report process unsubscribe_report data
#' * Checks if emails are unsubscribed, bounced, or don't exist
#' * Checks if mailing addresses have a NCOA flag or don't exist
#' * Checks if the customer primary login doesn't match the primary email
#' * Checks if customers are inactive
#' * Adds identifying info: MGO, constituencies, membership expiration date and level
#' @importFrom dplyr left_join if_else
#' @importFrom stats na.omit
#' @export
process.unsubscribe_report <- function(unsubscribe_report, ...) {
  . <- primary_ind <- customer_no <- timestamp <- event_subtype <- listid <- list_name <- last_updated_by <- last_update_dt <-
    login <- i.last_update_dt <- inactive_desc <- inactive_reason_desc <- keyword_value <- constituency_short_desc <- expr_dt <-
    memb_amt <- current_status_desc <- fname <- lname <- memb_level <- NULL

  assert_class(unsubscribe_report, "unsubscribe_report")

  # Email issues
  primary_emails <- unsubscribe_report$emails[primary_ind == "Y"]
  latest_email_events <- setkey(unsubscribe_report$email_events,email,timestamp) %>%
                               .[email %in% primary_emails$address, .SD[.N], by = list(email, ifelse(event_subtype == "Unsubscribe", listid, NA)),
                                 .SDcols = colnames(unsubscribe_report$email_events)]
  bad_emails <- rbind(latest_email_events[grepl("Unsub", event_subtype),
                               .(customer_no, message = paste(event_subtype,"from",list_name), timestamp)],
                      latest_email_events[grepl("Bounce", event_subtype),
                                          .(customer_no, message = event_subtype, timestamp)])

  missing_emails <- unsubscribe_report$customers[!primary_emails,on = "customer_no",
                               .(customer_no, message = "No primary email")]

  # Mailing address issues
  primary_addresses <- unsubscribe_report$addresses[primary_ind == "Y"]
  bad_addresses <- primary_addresses[last_updated_by == "NCOA$DNM",
                                .(customer_no, message = "Bad primary mailing address", timestamp = last_update_dt)]
  missing_addresses <- unsubscribe_report$customers[!primary_addresses,on = "customer_no",
                                .(customer_no, message = "No primary mailing address")]

  # Login issues
  bad_logins <- primary_emails[unsubscribe_report$logins[primary_ind == "Y"], on = "customer_no"][trimws(tolower(login)) != trimws(tolower(address)),
                 .(customer_no, message = "Email does not match login", timestamp = pmax(last_update_dt, i.last_update_dt))]

  # Inactive customers
  inactive <- unsubscribe_report$customers[inactive_desc != "Active",
                               .(customer_no, message = paste("Customer is inactive:", inactive_reason_desc), timestamp = last_update_dt)]

  #Additional info
  MGOs <- unsubscribe_report$MGOs[,.(MGOs = paste(keyword_value, collapse = ", ")), by = "customer_no"]
  constituencies <- unsubscribe_report$constituencies[,.(constituencies = paste(constituency_short_desc, collapse = ", ")), by = "customer_no"]
  latest_memberships <- setkey(unsubscribe_report$memberships,expr_dt,memb_amt) %>%
    .[!current_status_desc %in% c("Cancelled", "Deactivated"), .SD[.N], by = "customer_no"]
  name <- unsubscribe_report$customers[,.(name = paste(na.omit(c(fname, lname)), collapse = " ")), by = "customer_no"]

  unsubscribe_report$report <- rbind(bad_emails, missing_emails, bad_addresses, missing_addresses, bad_logins, inactive,
               fill = TRUE) %>%
    left_join(latest_memberships[,.(customer_no, memb_level, expr_dt)], by = "customer_no") %>%
    left_join(MGOs, by = "customer_no") %>%
    left_join(constituencies, by = "customer_no") %>%
    left_join(name, by = "customer_no") %>%
    left_join(primary_emails[,.(customer_no, email = address)], by = "customer_no")

  unsubscribe_report

}





#' @describeIn unsubscribe_report send the unsubscribe_report as emails to MGOs
#'
#' Data is filtered so that all events after `since` are returned and all events for members with expiration dates
#' between `since` and `until` are returned
#' @param since date, start date for filtering the returned data
#' @param until date, end date for filtering the returned data
#' @param routing_rules list of formulas to be used for routing, as for [dplyr::case_when]. Can refer to any columns returned by [process.unsubscribe_report]. See **Note** below
#' @note
#' For example, this value for routing_rules:
#' ```
#' list(grepl("GOV", constituencies) ~ list("eleszynski@bam.org"),
#'      grepl("CP\\d", constituencies) ~ list("lmcgee@bam.org"),
#'      grepl("\\+",constituencies) ~ list(c("apratama@bam.org","jhindle@bam.org")),
#'      TRUE ~ list(c("kburke@bam.org","esearles@bam.org")))
#' ```
#' Routes based on the following rules:
#' * GOV -> send to Government Affairs (eleszynski)
#' * CP# -> send to Strategic Partnerships (lmcgee)
#' * ??+ -> send to Patron Program (apratama and jhindle)
#' * Other -> send to Dev Ops (kburke and esearles)
#' @importFrom tessilake read_sql
#' @importFrom dplyr case_when
#' @inheritParams send_xlsx
#' @export
output.unsubscribe_report <- function(unsubscribe_report, since = Sys.Date() - 30, until = Sys.Date() + 30,
                                      routing_rules = list(TRUE ~ list(config::get("tessiflow.email"))),
                                      name = "contact_warning",
                                      subject = paste("Contact warning report for",Sys.Date()),
                                      ...) {
  . <- customer_no <- timestamp <- expr_dt <- message <- email <-
    memb_level <- MGOs <- constituencies <- fname <- lname <- userid <- NULL

  assert_class(unsubscribe_report, "unsubscribe_report")

  filtered_report <- unsubscribe_report$report[between(timestamp, since, until) | between(expr_dt, since, until) |
                                                 is.na(timestamp),
                                   .(`Tessi #`=customer_no, name, email, message, timestamp, memb_level,
                                     expr_dt, MGOs, constituencies, .I)]

  ### Routing rules
  routing_rules <- substitute(routing_rules) # needed to strip environment information from the formula
  constituency_routing <- filtered_report[,.(routing_email = unlist(case_when(!!!eval(routing_rules)))), by = I]

  filtered_report <- filtered_report[constituency_routing, on = "I"][,I:=NULL]

  split(filtered_report, by = "routing_email", keep.by = FALSE) %>%
    purrr::iwalk(\(table,email) send_xlsx(table,
                                          name = name,
                                          subject = subject,
                                          emails = c(config::get("tessiflow.email"),email),
                                          ...))

  unsubscribe_report

}
