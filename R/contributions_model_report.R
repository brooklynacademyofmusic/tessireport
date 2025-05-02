#' @title contributions_model_report
#' @description emailed report of predictions from [contributions_model]
#' @export
#' @name contributions_model_report
contributions_model_report <- report(class=c("contributions_model_report"))


#' @export
#' @importFrom dplyr arrange desc group_by across
#' @describeIn contributions_model_report Read data for contributions_model_report
#' @param report `contributions_model_report` object
#' @param ... not used
read.contributions_model_report <- function(report, ...) {

  . <- prob.TRUE <- truth <- score <- event <- timestamp <- expr_dt <- memb_amt <-
    group_customer_no <- tck_amt <- customer_no <- order_dt <- explanation <- NULL

  report$predictions <- read_cache("predictions","contributions_model") %>%
    collect %>% setDT %>%
    .[, .(score = max(prob.TRUE), event = any(truth == T)), by = "group_customer_no"] %>%
    .[score > .75 & !event]

  report$address_stream <- read_cache("address_stream_full","stream") %>% arrange(timestamp)
  report$memberships <- read_tessi("memberships") %>% arrange(expr_dt,desc(memb_amt))
  report$customers <- read_tessi("customers") %>% group_by(group_customer_no) %>% filter(group_customer_no == customer_no)
  report$tickets <- read_tessi("tickets") %>% group_by(group_customer_no) %>%
    summarise(tck_amt = sum(tck_amt, na.rm=T), tck_dt = max(order_dt, na.rm=T))

  report$explanations <- readRDS(cache_primary_path("shapley.Rds","contributions_model")) %>%
    .[,.(group_customer_no,explanation = lapply(explanation,parse_shapley))]

  for (t in names(report))
    report[[t]] <- filter(report[[t]], group_customer_no %in% report$predictions$group_customer_no) %>%
      collect %>% setDT %>%
      .[,last(.SD),by="group_customer_no"]

  NextMethod()
}

#' @export
#' @describeIn contributions_model_report Process data for contributions_model_report
process.contributions_model_report <- function(report, ...) {

  . <- group_customer_no <- score <- display_name_short <- last_gift_dt <- tck_dt <- tck_amt <-
    postal_code <- memb_level <- expr_dt <- address_median_income_level <- address_pro_score_level <-
    address_capacity_level <- address_properties_level <- address_donations_level <- explanation <- NULL

  report$data <- purrr::reduce(report,merge,by="group_customer_no",all=T) %>%
    .[,.(household_no = group_customer_no, score, name = display_name_short, last_gift_dt, last_ticket_dt = tck_dt,
         ticket_amt = tck_amt, postal_code = substr(postal_code,1,5),
         membership_level = memb_level, membership_expr_dt = expr_dt,
         census_median_income = address_median_income_level,
         iwave_pro_score = address_pro_score_level, iwave_capacity = address_capacity_level,
         iwave_properties = address_properties_level, iwave_donations = address_donations_level,
         explanation)] %>%
    setorder(-score)

  NextMethod()
}

#' @export
#' @describeIn contributions_model_report Send spreadsheet for contributions_model_report
#' @inheritParams send_email
output.contributions_model_report <- function(report, subject = paste("Contributions model", Sys.Date()),
                                              body = paste("Sent by", Sys.info()["nodename"]), emails = config::get("tessiflow.email"), ...) {

  send_xlsx(report$data, subject = subject, body = body, emails = emails, name = "contributions_model",
            currency = c("ticket_amt","census_median_income","iwave_capacity","iwave_donations","iwave_properties"))
  NextMethod()
}

#' @export
write.contributions_model_report <- function(...) {NextMethod()}
