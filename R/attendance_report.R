#' attendance_report
#'
#' Sends an email containing a spreadsheet and pdf of attendees
#' @name attendance_report
#' @include report.R
#' @export
attendance_report <- report(class=c("attendance_report","email_report"))

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
                                   tables = c("order_detail","special_activities","performances",
                                              "customers","seats"),
                                   since = Sys.time(), until = as.POSIXct(Sys.Date() + 3),
                                   refresh = TRUE, ...) {

  perf_dt <- attend_dt <- sp_act_dt <- customer_no <- recipient_no <- NULL

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
    mutate(perf_dt = attend_dt)


  attendance_report[tables] <- lapply(tables,\(table_name) {
    read_tessi(table_name, freshness = freshness, incremental = !refresh, overwrite = refresh)
  })

  attendance_report[names(attendance_report)] <- lapply(attendance_report, \(table) {
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
#' * **order_detail** : order_no, sli_status_desc, seating_status_desc, perf_no,
#' perf_dt, perf_desc, recipient_display_name, customer_no, order_ship_method_desc
#' * **activities** : customer_no, activity_desc, status_desc
#' * **performances** : perf_no, perf_desc, perf_dt
#' * **customers** : customer_no, display_name, sort_name
#' * **performance_seating** : perf_no, seat_no, seat_row, seat_num
#' * **scans** : customer_no, perf_no
#' @export
#' @importFrom data.table setkey
#' @importFrom checkmate assert_list assert_names
#' @importFrom rlang list2
#'
#' @param filter `expression` to filter the output by, as with [rlang::eval_tidy]
#' @param formats `character(2)` vector of date and date-time formats
#' @param append `list` of additional data to append to the output on shared columns
process.attendance_report <- function(attendance_report,
                                      filter =
    sli_status_desc %in% c(
      "Unseated, Unpaid", "Seated, Unpaid", "Seated, Paid", "Unseated, Paid",
      "Upgraded", "Ticketed, Paid"
    ) |
    status_desc %in% c(
      "Attended", "Accepted", "Tentative"
    ),
  formats = c("%b %d","%b %d %I:%M %p"),
  append = NULL, ...
  ) {

  . <- sli_status_desc <- status_desc <- detail_sli_no <- seat_row <- seat_num <- seat_desc <-
    zone_desc <- zone_short_desc <- customer_no <- group_customer_no <- admission_adult <-
    activity_desc <- perf_desc <- num_attendees <- recipient_display_name <- display_name <-
    sort_name <- perf_dt <- order_ship_method_desc <- name <- seats <- ship_method <-
    source <- order_no <- NULL

  assert_character(formats,len=2)

  assert_list(append, null.ok = T)
  lapply(append,tessilake:::assert_dataframeish)

  attendance_report[names(attendance_report)] <- lapply(attendance_report, \(.) collect(.) %>% setDT)

  makelist <- \(l,s) paste0(sort(unique(trimws(l))), collapse = s)

  orders <- attendance_report$order_detail %>%
    setkey(detail_sli_no) %>% .[,last(.SD), by=c("perf_no","seat_no")] %>%
    merge(attendance_report$seats, by=c("perf_no","seat_no"), suffixes=c("",".seats"), all.x = T) %>%

    # build seat names
    .[,`:=`(seat_row = trimws(seat_row), seat_num = trimws(seat_num))] %>%
    tessistream::setunite("seat_desc", seat_row, seat_num, sep = "", na.rm=T) %>%
    .[,seat_desc := makelist(seat_desc, ", "),
      by = c("customer_no", "perf_no", "zone_desc")] %>%

    # prepend zone info
    .[,zone_short_desc := abbreviate(zone_desc, use.classes=F)] %>%
    tessistream::setunite("seats", zone_short_desc, seat_desc, sep = ": ", na.rm=T)

  scans <- attendance_report$scans %>%
    merge(attendance_report$customers[,.(customer_no,group_customer_no)],
          by="customer_no", all.x = T,
          suffixes = c("",".customers")) %>%
    merge(attendance_report$performances, by = "perf_no", all.x=T,
          suffixes = c(".scans","")) %>%
    .[,seats := paste0(admission_adult, " seat",
                       ifelse(admission_adult > 1,"s",""))]

  activities <- attendance_report$special_activities %>%
    tessistream::setunite("perf_desc", activity_desc, perf_desc, sep = ": ", na.rm = T) %>%
    .[,seats := paste0(num_attendees, " seat",
                     ifelse(num_attendees > 1,"s",""))]

  # build it...
  output <- rbind(orders = orders, activities = activities, scans = scans,
                  fill = T, idcol = "source") %>%
    merge(attendance_report$customers, by="customer_no", all.x = T,
          suffixes = c("",".customers")) %>%
    .[eval(rlang::enexpr(filter)),
      .(name = first(trimws(coalesce(recipient_display_name,display_name))),
         sort_name = first(sort_name),
         perf_desc = makelist(perf_desc, ", "),
         perf_dt = makelist(if_else(perf_dt==lubridate::floor_date(perf_dt,"day"),
                                    format(perf_dt,formats[1]),
                                    format(perf_dt,formats[2])),", "),
         status = makelist(coalesce(sli_status_desc, status_desc), ", "),
         ship_method = makelist(order_ship_method_desc,", "),
         seats = makelist(seats,"; "),
         source = paste0(source,collapse=", ")),
      by = .(group_customer_no,order_no,
             date = lubridate::floor_date(perf_dt, "day"))]

  attendance_report$output <- purrr::reduce(append,
                                            \(x,y) merge(setkey(x,NULL),setkey(y,NULL),all.x = T),
                                            .init = output)

  NextMethod()
}

#' @param attendance_report
#'
#' @param columns `list` of named expressions mapping column names to their values,
#' will be evaluated in the environment of `attendance_report$output`
#' @param column_widths `double` vector of column widths in inches; must be the same
#' length as `columns`
#' @param ... additional parameters passed on to [pdf_table]
#'
#' @export
#' @describeIn attendance_report output a pdf of attendance
#' @importFrom kableExtra kable_styling group_rows column_spec
#' @importFrom rlang call_args_names call_name
#' @importFrom checkmate assert_true
write.attendance_report <- function(attendance_report,
                                     columns = list(`customer #` = group_customer_no,
                                                  `order #` = coalesce(as.character(order_no),""),
                                                  name = name,
                                                  performance = perf_desc,
                                                  time = perf_dt,
                                                  status = status,
                                                  `ship method` = ship_method,
                                                  seats = seats),
                                     column_widths = c(.75,.75,1.75,1,
                                                       1,1,1,1),
                                     ...) {

    . <- sort_name <- perf_dt <- date <- group_customer_no <- order_no <-
      name <- perf_desc <- perf_dt <- status <- ship_method <- seats <- NULL

    columns <- rlang::enexpr(columns)
    setkey(attendance_report$output,date,sort_name,perf_dt)
    assert_true(length(call_args_names(columns)) ==
                  length(column_widths))
    assert_true(call_name(columns) == "list")

    if(nrow(attendance_report$output) > 0) {

      table <- pdf_table(attendance_report$output[,eval(columns)]) %>%
          kable_styling(latex_options=c("striped","repeat_header")) %>%
          group_rows(index=table(attendance_report$output[,date]))

      write_pdf_args <- list2(...) %>%
        .[intersect(names(.), rlang::fn_fmls_names(write_pdf))]

      attendance_report$filename <- do.call(write_pdf,
        c(quote(purrr::reduce2(seq_along(column_widths), paste0(column_widths,"in"),
                     column_spec, .init = table)),
          write_pdf_args)
      )

    }

    NextMethod()

}
