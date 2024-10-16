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

make_contributions_model_fixtures <- function(n = 10000, rebuild = FALSE) {

  withr::local_envvar(R_CONFIG_FILE="")

  if (rebuild)
    contributions_dataset(rebuild_dataset = T)

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
                )] %>%
    arrow::write_parquet("tests/testthat/test-contributions_model.parquet")

}
