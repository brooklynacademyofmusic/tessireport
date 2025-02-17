#' @export
p2_orphans_report <- report(class="p2_orphans_report")

#' p2_orphans_report
#'
#' Sends an email containing a plot of recent orphans and a spreadsheet of all orphans.
#' @name p2_orphans_report
#' @importFrom ggplot2 ggplot geom_histogram aes scale_fill_brewer theme_minimal
#' @importFrom dplyr case_when
#' @importFrom lubridate ddays
#' @importFrom grDevices dev.off png
#' @importFrom tessilake tessi_customer_no_map
#' @importFrom tessistream p2_orphans tessi_changed_emails p2_resolve_orphan
#' @importFrom purrr map
#' @importFrom dplyr coalesce
#' @param p2_orphans_report p2_orphans_report object
#' @inheritParams tessilake::read_tessi
#' @param ... not used
#' @export
run.p2_orphans_report <- function(p2_orphans_report, freshness = 0, ...) {
  . <- type <- timestamp <- id <- from <- to <- customer_no.x <- expr_dt <- memb_level <-
    last_updated_by <- customer_no <- i.customer_no <- NULL

  p2_orphans <- p2_orphans()
  tessi_emails <- tessi_changed_emails(since = 0, freshness = freshness)
  customer_no_map <- tessi_customer_no_map(freshness = freshness)

  if(nrow(p2_orphans) > 0 && nrow(tessi_emails) > 0) {
    # last change from `from`
    p2_orphan_events <- tessi_emails[p2_orphans,on=c("from"="address")] %>%
      .[,customer_no := coalesce(customer_no, i.customer_no)] %>%
      merge(customer_no_map, by = "customer_no", suffixes = c(".tessi_emails",""))

    p2_orphan_events[,type:=case_when(trimws(last_updated_by) %in% c("popmulti","addage") ~ "web",
                                      trimws(last_updated_by) %in% c("sqladmin","sa") ~ "merge",
                                      TRUE ~ "client")]
  } else {
    p2_orphan_events <- data.table(timestamp = lubridate::POSIXct(), group_customer_no = integer())
  }

  png(image_file <- tempfile(fileext = ".png"))
  ggplot(p2_orphan_events[timestamp>'2022-08-01']) +
    geom_histogram(aes(timestamp,
                       fill=type),
                   binwidth=ddays(7)) +
    scale_fill_brewer(type="qual") +
    theme_minimal() -> p

  print(p)
  dev.off()

  memberships <- read_tessi("memberships", c("expr_dt","memb_level","customer_no")) %>%
    collect() %>% setDT() %>% .[,.SD[.N], by="group_customer_no"]

  p2_orphan_events <- merge(p2_orphan_events,memberships,all.x=T,by="group_customer_no",suffixes=c("",".memberships"))

  has_been_updated <- p2_orphan_events %>% split(seq_len(nrow(.))) %>%
    map(~p2_resolve_orphan(.$from, .$to, customer_no = .$customer_no, dry_run = FALSE))

  writeLines(paste0("<img src='",image_file,"'>"), html_file <- tempfile())

  send_xlsx(
    table = p2_orphan_events[,.(
      timestamp = as.Date(timestamp),
      "customer_#" = customer_no,
      p2_id = id,
      from_email = from,
      to_email = to,
      expiration_date = as.Date(expr_dt),
      member_level = memb_level,
      has_been_updated,
      change_type = type,
      last_updated_by
    )],
    subject = paste("P2 Orphan Report :",lubridate::today()),
    body = html_file,
    emails = "ssyzygy@bam.org",
    attachments = setNames(nm = paste0("p2_ophan_report_",lubridate::today(),".xlsx")),
  )

}
