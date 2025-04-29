withr::local_package("mockery")
withr::local_package("checkmate")

fixtures <- readRDS(rprojroot::find_testthat_root_file("attendance_report.Rds")) %>%
  lapply(arrow::as_arrow_table)

test_that("read.attendance_report reads from all tables and returns an `attendance_report` object", {

  tables <- formals(read.attendance_report)$tables %>% eval
  read_sql <- mock(fixtures$vips,fixtures$scans)
  read_tessi <- do.call(mock, fixtures[tables])
  stub(read.attendance_report, "read_sql", read_sql)
  stub(read.attendance_report, "read_tessi", read_tessi)

  report <- read(attendance_report,0, since = as.POSIXct("1900-01-01"), until = as.POSIXct("2100-01-01"))

  expect_names(names(report),permutation.of = c(tables, "vips", "scans"))
  expect_class(report,"attendance_report")
  for(table in c(tables,"vips","scans")) {
    expect_gt(nrow(report[[!!table]]),0)
  }
})

test_that("read.attendance_report force refreshes iff `refresh` = T", {

  tables <- formals(read.attendance_report)$tables %>% eval
  read_sql <- mock(fixtures$vips,fixtures$scans)
  read_tessi <- do.call(mock, fixtures[tables])
  stub(read.attendance_report, "read_sql", read_sql)
  stub(read.attendance_report, "read_tessi", read_tessi)

  report <- read(attendance_report,0,refresh=T)

  expect_equal(purrr::map_int(mock_args(read_sql),"freshness"),rep(0,2))
  expect_equal(purrr::map_int(mock_args(read_tessi),"freshness"),rep(0,length(tables)))
  expect_equal(purrr::map_lgl(mock_args(read_sql),"incremental"),rep(F,2))
  expect_equal(purrr::map_lgl(mock_args(read_tessi),"incremental"),rep(F,length(tables)))
  expect_equal(purrr::map_lgl(mock_args(read_sql),"overwrite"),rep(T,2))
  expect_equal(purrr::map_lgl(mock_args(read_tessi),"overwrite"),rep(T,length(tables)))

  read_sql <- mock(fixtures$vips,fixtures$scans)
  read_tessi <- do.call(mock, fixtures[tables])
  stub(read.attendance_report, "read_sql", read_sql)
  stub(read.attendance_report, "read_tessi", read_tessi)
  report <- read(attendance_report,0,refresh=F)

  expect_equal(purrr::map_int(mock_args(read_sql),"freshness"),rep(7*86400,2))
  expect_equal(purrr::map_int(mock_args(read_tessi),"freshness"),rep(7*86400,length(tables)))
  expect_equal(purrr::map_lgl(mock_args(read_sql),"incremental"),rep(T,2))
  expect_equal(purrr::map_lgl(mock_args(read_tessi),"incremental"),rep(T,length(tables)))
  expect_equal(purrr::map_lgl(mock_args(read_sql),"overwrite"),rep(F,2))
  expect_equal(purrr::map_lgl(mock_args(read_tessi),"overwrite"),rep(F,length(tables)))

})

test_that("read.attendance_report returns the correct timezone and filters by timestamp", {

  tables <- formals(read.attendance_report)$tables %>% eval
  read_sql <- mock(fixtures$vips,fixtures$scans)
  read_tessi <- do.call(mock, fixtures[tables])
  stub(read.attendance_report, "read_sql", read_sql)
  stub(read.attendance_report, "read_tessi", read_tessi)

  since = quantile(collect(fixtures$performances)$perf_dt,.25)
  until = quantile(collect(fixtures$performances)$perf_dt,.75)

  report <- read(attendance_report,0,since = since, until = until)

  for(table in names(report)) {
    report[[table]] <- report[[table]] %>% collect %>% setDT
    if("perf_dt" %in% names(report[[table]])) {
      expect_equal(report[[!!table]][perf_dt < since], report[[table]][integer(0)])
      expect_equal(report[[!!table]][perf_dt > until], report[[table]][integer(0)])
      expect_equal(report[[!!table]][perf_dt >= since & perf_dt <= until], report[[table]])
      expect_equal(tz(report[[!!table]][,perf_dt]), Sys.timezone())
    }
  }

})

test_that("read.attendance_report filters by customer_no", {

  tables <- formals(read.attendance_report)$tables %>% eval
  read_sql <- mock(data.table(customer_no=seq(10)),fixtures$scans)
  read_tessi <- do.call(mock, fixtures[tables])
  stub(read.attendance_report, "read_sql", read_sql)
  stub(read.attendance_report, "read_tessi", read_tessi)

  report <- read(attendance_report,0)

  for(table in names(report)) {
    report[[table]] <- report[[table]] %>% collect %>% setDT
    if("customer_no" %in% names(report[[table]])) {
      expect_equal(report[[!!table]][customer_no > 10], report[[table]][integer(0)])
      expect_equal(report[[!!table]][customer_no <= 10], report[[table]])
    }
  }

})

# process.attendance_report -----------------------------------------------
test_that("process.attendance_report combines data into an output table and returns an updated `attendance_report` object", {
  tables <- formals(read.attendance_report)$tables %>% eval
  read_sql <- mock(fixtures$vips,fixtures$scans)
  read_tessi <- do.call(mock, fixtures[tables])
  stub(read.attendance_report, "read_sql", read_sql)
  stub(read.attendance_report, "read_tessi", read_tessi)

  report <- read(attendance_report,0) %>% process

  expect_data_frame(report$output)
  expect_class(report,"attendance_report")

})

test_that("process.attendance_report output contains expected columns and rows", {
  tables <- formals(read.attendance_report)$tables %>% eval
  read_sql <- mock(fixtures$vips,fixtures$scans)
  read_tessi <- do.call(mock, fixtures[tables])
  stub(read.attendance_report, "read_sql", read_sql)
  stub(read.attendance_report, "read_tessi", read_tessi)

  report <- read(attendance_report,0) %>% process

  columns <- c("order_no", "date", "group_customer_no", "name","sort_name",
               "perf_desc","perf_dt","status","ship_method","seats","source")

  withr::local_package("lubridate")

  basis <- rbind(
    report$order_detail[,c("customer_no","perf_dt")],
    report$special_activities[,c("customer_no","perf_dt")],
    report$scans[,c("customer_no","perf_dt")]) %>%
    mutate(perf_dt = floor_date(perf_dt,"day"))

  expect_data_frame(report$output)
  expect_names(names(report$output), permutation.of = columns)
  expect_equal(nrow(report$output),nrow(unique(basis)))

  # all columns are filled
  for (col in columns) {
    expect_equal(report$output[is.na(get(!!col))], report$output[integer(0)])
  }

})

test_that("process.attendance_report creates seat summaries", {
  tables <- formals(read.attendance_report)$tables %>% eval
  read_sql <- mock(fixtures$vips,fixtures$scans)
  read_tessi <- do.call(mock, fixtures[tables])
  stub(read.attendance_report, "read_sql", read_sql)
  stub(read.attendance_report, "read_tessi", read_tessi)

  report <- read(attendance_report,0)

  report$seats <- CJ(perf_no = 1,
                     seat_row = LETTERS,
                     seat_num = seq(10)) %>%
    .[,seat_no := .I]

  report$order_detail <- fixtures$order_detail %>% collect %>% setDT %>%
    head(nrow(report$seats)) %>%
    .[,`:=`(customer_no = rep(seq(10),26),
            group_customer_no = rep(seq(10),26),
            order_no = 1, perf_no = 1,
            seat_no = .I,
            zone_desc = rep(paste("Section",c("A","B","C")),length.out=.N),
            sli_status_desc = "Ticketed, Paid")]

  report <- process(report)

  setkey(report$output,group_customer_no)
  expect_equal(report$output[1,seats],
               paste0(
                c(paste("SecA:",paste0(LETTERS[seq(1,26,by=3)],1,collapse=", ")),
                  paste("SecB:",paste0(LETTERS[seq(2,26,by=3)],1,collapse=", ")),
                  paste("SecC:",paste0(LETTERS[seq(3,26,by=3)],1,collapse=", "))),
                collapse = "; "))

})

if(!interactive()) {

test_that("process.attendance_report filters by sli_status and special_activity_status", {
  tables <- formals(read.attendance_report)$tables %>% eval
  read_sql <- mock(fixtures$vips,fixtures$scans)
  read_tessi <- do.call(mock, fixtures[tables])
  stub(read.attendance_report, "read_sql", read_sql)
  stub(read.attendance_report, "read_tessi", read_tessi)

  sli_statuses <- fixtures$order_detail$sli_status_desc %>% as.vector %>% trimws %>% unique
  sa_statuses <- fixtures$special_activities$status_desc %>% as.vector %>% trimws %>% unique

  report <- read(attendance_report,0,since=as.POSIXct("1900-01-01"),until=as.POSIXct("2100-01-01")) %>%
    process(sli_statuses = sli_statuses[1], special_activity_statuses = sa_statuses[1])

  expect_gt(nrow(report$output),0)
  expect_equal(report$output[status %in% sli_statuses[-1]], report$output[integer(0)])
  expect_equal(report$output[status %in% sa_statuses[-1]], report$output[integer(0)])
  expect_true(report$output[,any(sli_statuses[1] %in% status)])
  expect_true(report$output[,any(sa_statuses[1] %in% status)])

})

}

test_that("process.attendance_report appends data from ...", {
  tables <- formals(read.attendance_report)$tables %>% eval
  read_sql <- mock(fixtures$vips,fixtures$scans)
  read_tessi <- do.call(mock, fixtures[tables])
  stub(read.attendance_report, "read_sql", read_sql)
  stub(read.attendance_report, "read_tessi", read_tessi)

  email <- data.table(group_customer_no = seq(1000),
                      email = paste0(sample(letters,1000,replace=T),"@bam.org"))

  report <- read(attendance_report,0) %>% process(append = list(email = email))

  expect_gt(nrow(report$output),0)
  expect_names(names(report$output), must.include = "email")
  expect_equal(report$output[is.na(email)],report$output[integer(0)])

})

# write.attendance_report -------------------------------------------------

test_that("write.attendance_report assembles pdf output from process.attendance_report", {
  tables <- formals(read.attendance_report)$tables %>% eval
  read_sql <- mock(fixtures$vips,fixtures$scans)
  read_tessi <- do.call(mock, fixtures[tables])
  stub(read.attendance_report, "read_sql", read_sql)
  stub(read.attendance_report, "read_tessi", read_tessi)

  report <- read(attendance_report,0) %>% process
  # faster test
  report$output <- report$output[1:10]
  pdf_table <- mock(tessireport::pdf_table(report$output))
  write_pdf <- mock()
  stub(write.attendance_report, "write_pdf", write_pdf)
  stub(write.attendance_report, "pdf_table", pdf_table)

  write(report)

  expect_length(mock_args(pdf_table),1)
  expect_snapshot(mock_args(pdf_table)[1],variant="write.attendance_report.pdf_table")
  expect_length(mock_args(write_pdf),1)
  expect_snapshot(mock_args(write_pdf)[1],variant="write.attendance_report.write_pdf")
})

test_that("write.attendance_report uses the column name mapping in columns to define the output", {
  tables <- formals(read.attendance_report)$tables %>% eval
  read_sql <- mock(fixtures$vips,fixtures$scans)
  read_tessi <- do.call(mock, fixtures[tables])
  stub(read.attendance_report, "read_sql", read_sql)
  stub(read.attendance_report, "read_tessi", read_tessi)

  report <- read(attendance_report,0) %>% process
  # faster test
  report$output <- report$output[1:10]
  pdf_table <- mock(tessireport::pdf_table(report$output))
  write_pdf <- mock()
  stub(write.attendance_report, "write_pdf", write_pdf)
  stub(write.attendance_report, "pdf_table", pdf_table)

  write(report,
        columns = list(id = paste0("id#",group_customer_no)),
        column_widths = 1)

  expect_length(mock_args(pdf_table),1)
  expect_equal(mock_args(pdf_table)[[1]][[1]],
               data.table(id = paste0("id#",report$output$group_customer_no)))
})

