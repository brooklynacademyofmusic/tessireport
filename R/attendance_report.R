#' @export
attendance_report <- report(class="attendance_report")

#' attendance_report
#'
#' Sends an email containing a spreadsheet and pdf of attendees
#' @name attendance_report
NULL


mgos <- read_tessi("attributes") %>% collect %>% setDT %>%
  .[grepl("Major Gift",keyword_desc),
    .(mgos = paste0(paste0(keyword_value,ifelse(grepl("Prospect",keyword_desc),"*","")),
             collapse=", ")),by="group_customer_no"]

constituencies <- read_tessi("constituencies") %>% collect %>% setDT %>%
  .[!grepl("Ticket|Subscriber|AutoRenew|Patron Services",constituency_desc),
    .(constituencies = paste0(constituency_desc,
                              collapse = ", ")), by = "group_customer_no"]

#' @describeIn attendance_report load data for `attendance_report`
#'
#' @param attendance_report `attendance_report` object
#' @param list_no `integer(1)` list number to load customers from
#' @param tables `character` vector of tables to load from, as defined by [tessilake::tessi_list_tables]
#' @param since `POSIXct` date from which to query performances
#' @param until `POSIXct` date up to which to query performances
#' @param refresh `logical` whether or not to force refresh the data (default is to refresh)
#' @importFrom checkmate assert_integerish assert_character assert_posixct assert_logical
#' @importFrom lubridate force_tz tz
#' @export
read.attendance_report <- function(attendance_report, list_no,
                                   tables = c("orders","order_detail","special_activities","performances",
                                              "constituencies","customers","seats","attributes"),
                                   since = Sys.time(), until = as.POSIXct(Sys.Date() + 3),
                                   refresh = TRUE) {

  assert_integerish(list_no,len=1)
  assert_character(tables,min.len=1)
  assert_posixct(since,len=1)
  assert_posixct(until,len=1)
  assert_logical(refresh,len=1)

  freshness <- ifelse(refresh, 0, ddays(7))

  # vips: customer_no
  # TODO: create a tessilake::read_list helper
  attendance_report$vips <- read_sql(paste("select * from t_list_contents where list_no =",list_no),
                                     freshness = freshness, incremental = !refresh, overwrite = refresh) %>% collect %>% setDT
  # scans: customer_no, perf_no
  # TODO: create a tessilake::attendance table
  attendance_report$scans = read_sql("select * from T_ATTENDANCE a where ticket_no is null",
                                     freshness = freshness, incremental = !refresh, overwrite = refresh) %>%
    mutate(perf_dt = force_tz(attend_dt,""))

  tables <- c(tables,"scans")

  attendance_report[tables] <- lapply(tables,\(table_name) {
    table <- read_tessi(table_name, freshness = freshness, incremental = !refresh, overwrite = refresh)

    # work with special activities
    if("sp_act_dt" %in% names(table))
      table <- mutate(table,perf_dt = sp_act_dt)

    if("perf_dt" %in% names(table))
      table <- mutate(table,perf_dt = force_tz(perf_dt,"")) %>%
        filter(perf_dt >= !!since & perf_dt <= !!until)

    # connect to recipient
    if("recipient_no" %in% names(table))
      table <- mutate(table,customer_no = coalesce(recipient_no, customer_no))

    if("customer_no" %in% names(table))
      table <- filter(table,customer_no %in% attendance_report$vips$customer_no)

    table
  })

  NextMethod()
}

#' @describeIn attendance_report process data for `attendance_report`
#' @note
#' * **orders**  : order_no, custom_1
#' * **order_detail** : order_no, sli_status_desc, seating_status_desc, perf_no,
#' perf_dt, perf_desc, recipient_display_name, customer_no, order_ship_method_desc
#' * **activities** : customer_no, activity_desc, status_desc
#' * **performances** : perf_no, perf_desc, perf_dt
#' * **constituencies** : customer_no, constituency_desc
#' * **customers** : customer_no, display_name
#' * **performance_seating** : perf_no, seat_no, seat_row, seat_num
#' * **attributes** : customer_no, keyword_desc, keyword_value
#' * **scans** : customer_no, perf_no
#' @export
#' @importFrom data.table setkey
#' @importFrom checkmate assert_data_frame assert_names
#' @param ... additional data frames to append to the output on `group_customer_no`
process.attendance_report <- function(attendance_report,
  sli_statuses = c(
    "Unseated, Unpaid", "Seated, Unpaid", "Seated, Paid", "Unseated, Paid",
    "Upgraded", "Ticketed, Paid"
  ),
  special_activity_statuses = c("Attended", "Accepted", "Tentative"),
  ...
  ) {

  data <- list2(...)

  lapply(data,assert_data_frame)
  lapply(data,\(.) assert_names(names(.),must.include="group_customer_no"))

  attendance_report <- lapply(attendance_report, \(.) collect(.) %>% setDT)

  makelist <- \(l,s) paste0(sort(unique(l)), collapse = s)

  orders <- attendance_report$order_detail %>%
    # filter sli statuses
    .[sli_status_desc %in% sli_statuses] %>%
    setkey(detail_sli_no) %>% .[,last(.SD), by=c("perf_no","seat_no")] %>%
    merge(attendance_report$orders, by="order_no", suffixes=c("",".orders")) %>%
    merge(attendance_report$seats, by=c("perf_no","seat_no"), suffixes=c("",".seats")) %>%

    # build seat names
    .[,`:=`(seat_row = trimws(seat_row), seat_num = trimws(seat_num))] %>%
    tessistream::setunite("seat_desc", seat_row, seat_num, sep = "", na.rm=T) %>%
    .[,seat_desc := makelist(seat_desc, ", "),
      by = c("customer_no", "perf_no", "zone_desc")] %>%

    # prepend zone info
    .[,zone_short_desc := abbreviate(zone_desc, use.classes=F)] %>%
    tessistream::setunite("seats", zone_short_desc, seat_desc, sep = ": ", na.rm=T)

  scans <- attendance_report$scans %>%
    merge(attendance_report$customers, by="customer_no", all.x = T,
          suffixes = c("",".customers")) %>%
    merge(attendance_report$performances, by = "perf_no", all.x=T,
          suffixes = c(".scans","")) %>%
    .[,seats := paste0(admission_adult, " seat",
                       ifelse(admission_adult > 1,"s",""))]

  activities <- attendance_report$special_activities %>%
    .[trimws(status_desc) %in% special_activity_statuses] %>%
    tessistream::setunite("perf_desc", activity_desc, perf_desc, sep = ": ", na.rm = T) %>%
    .[,seats := paste0(num_attendees, " seat",
                     ifelse(num_attendees > 1,"s",""))]

  # build it...
  output <- rbind(orders = orders, activities = activities, scans = scans,
                  fill = T, idcol = "source") %>%
    merge(attendance_report$customers, by="customer_no", all.x = T,
          suffixes = c("",".customers")) %>%
    .[,.(id = makelist(coalesce(order_no,activity_no,id),", "),
         name = first(trimws(coalesce(recipient_display_name,display_name))),
         sort_name = first(sort_name),
         perf_desc = makelist(perf_desc, ", "),
         perf_dt = makelist(if_else(perf_dt==lubridate::floor_date(perf_dt,"day"),
                                     format(perf_dt,"%b %d"),format(perf_dt,"%b %d %I:%M %p")),", "),
         status = makelist(coalesce(sli_status_desc, status_desc), ", "),
         ship_method = makelist(order_ship_method_desc,", "),
         seats = makelist(seats,"; "),
         source = paste0(source,collapse=", ")),
      by = .(group_customer_no,
             date = lubridate::floor_date(perf_dt, "day"))]

  purrr::reduce(data, merge, all.x=T, .init = output)
}

#' @export
output.attendance_report <- function(attendance_report,
                                     columns <- c("customer" = "group_customer_no",
                                                  "order" = "id",
                                                  "name" = "name",
                                                  "performance" = "perf_desc",
                                                  "time" = "perf_dt",
                                                  "status" = "status",
                                                  "ship_method" = "ship_method",
                                                  "seats" = "seats"),...) {
    kable(transmute(output,customer_no,order_no,names,mgo,perf_dt,ticket_status=sli_status_desc,ship_method,perf_desc,seats,constituencies),
          format="latex",longtable=T,booktabs=T,escape=T) %>%
      kable_styling(latex_options=c("striped","repeat_header")) %>%
      group_rows(index=table(as_date(output$perf_dt_raw))) %>%
      column_spec(c(1,2,9),width=".75in") %>%
      column_spec(c(3,4,6,7),width="1in") %>%
      column_spec(c(8),width="1.25in") %>%
      column_spec(c(3,10),width="1.75in")
}
