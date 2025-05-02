
# unsubscribe_report ------------------------------------------------------

#' make_unsubscribe_report_fixtures
#'
#' Make fixtures for unsubscribe_report testing
#'
#' @importFrom dplyr collect filter
#' @importFrom tessilake read_cache
make_unsubscribe_report_fixtures <- function() {
  customer_no <- NULL
  report <- list()
  report$email_events <- read_cache("p2_stream_enriched","deep","stream") %>% filter(customer_no %in% c(8993321,8992917,8992918)) %>% collect
  report$emails <- read_tessi("emails") %>% filter(customer_no %in% c(8993321,8992917,8992918)) %>% collect
  report$addresses <- read_tessi("addresses") %>% filter(customer_no %in% c(8993321,8992917,8992918)) %>% collect
  report$logins <- read_tessi("logins") %>% filter(customer_no %in% c(8993321,8992917,8992918)) %>% collect
  report$MGOs <- read_tessi("attributes") %>% filter(customer_no %in% c(8993321,8992917,8992918)) %>% collect
  report$constituencies <- read_tessi("constituencies") %>% filter(customer_no %in% c(8993321,8992917,8992918)) %>% collect
  report$memberships <- read_tessi("memberships") %>% filter(customer_no %in% c(8993321,8992917,8992918)) %>% collect
  report$customers <- read_tessi("customers") %>% filter(customer_no %in% c(8993321,8992917,8992918)) %>% collect

  saveRDS(report,rprojroot::find_testthat_root_file("unsubscribe_report.Rds"))
}


# attendance_report -------------------------------------------------------

make_attendance_report_fixtures <- function(n = 1000) {
  withr::local_envvar(R_CONFIG_FILE="")
  fixtures <- list()

  tables = setdiff(eval(formals(read.attendance_report)$tables),"customers")

  fixtures[tables] <- lapply(tables,read_tessi)

  fixtures$scans = read_sql(paste("select top",n,"* from T_ATTENDANCE a where ticket_no is null"))
  fixtures$order_detail <- dplyr::slice_tail(fixtures$order_detail, n=n)
  fixtures$seats <- dplyr::semi_join(fixtures$seats, fixtures$order_detail,
                              by=c("perf_no","seat_no"))
  fixtures$performances <- dplyr::semi_join(fixtures$performances,
                                            fixtures$scans, by = "perf_no")

  for(table in names(fixtures)) {
    fixtures[[table]] <- fixtures[[table]] %>% dplyr::slice_tail(n = n) %>%
      collect %>% setDT
    customer_nos <- sample(seq(n),
                           nrow(fixtures[[table]]),
                           replace = T)
    if("customer_no" %in% names(fixtures[[table]]))
      fixtures[[table]][,customer_no := customer_nos]

    if("group_customer_no" %in% names(fixtures[[table]]))
      fixtures[[table]][,group_customer_no := customer_nos]

  }

  fixtures$customers <- data.table(customer_no = seq(n),
                                   group_customer_no = seq(n),
                                   display_name = paste("Customer",seq(n)),
                                   sort_name = paste0(seq(n),"/Customer"))

  fixtures$vips <- data.table(list_no=1, customer_no=seq(n))

  saveRDS(fixtures,rprojroot::find_testthat_root_file("attendance_report.Rds"))

}


# contributions_model -----------------------------------------------------

make_contributions_model_fixtures <- function(n = 10000, rebuild = FALSE) {

  withr::local_envvar(R_CONFIG_FILE="")
  withr::local_package("mockery")

  if (rebuild) {
    stream <- read_cache("stream","stream",include_partition=T) %>%
      filter(timestamp < Sys.Date())
    max_partition = summarise(stream,max(partition)) %>% collect %>% as.numeric %>% as.POSIXct
    stream_subset <- filter(stream, partition == max_partition) %>% dplyr::compute()
    stub(contributions_dataset,"read_cache",mock(stream_subset,data.frame(date = numeric(0))))
    contributions_dataset(rebuild_dataset = T)
  }


  fixture <- read_cache("dataset","contributions_model") %>%
    dplyr::slice_sample(n = n) %>%
    collect %>% setDT

  # anonymize
  fixture[,`:=`(group_customer_no = .I,
                customer_no = .I,
                email = NULL,
                street1 = NULL,
                street2 = NULL,
                subscriberid = NULL
                )] %>% as.data.frame() %>%
    arrow::write_parquet("tests/testthat/test-contributions_model.parquet")

}
