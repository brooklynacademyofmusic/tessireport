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

  report <- read(attendance_report,0)

  expect_names(names(report),permutation.of = c(tables, "vips", "scans"))
  expect_class(report,"attendance_report")
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
      expect_equal(tz(report[[!!table]][,perf_dt]), "")
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
