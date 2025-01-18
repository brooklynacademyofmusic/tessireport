withr::local_package("checkmate")
withr::local_package("mockery")

# read.sql_report ---------------------------------------------------------

test_that("read.sql_report queries the database", {
  table <- data.table(a = runif(1000))
  stub(read.sql_report, "read_sql", table)

  report <- read(sql_report,"select * from test_table")

  expect_length(report,1)
  expect_equal(report$data, table)
})

test_that("read.sql_report passes parameters on to read_sql", {
  table <- data.table(a = runif(1000))
  read_sql <- mock(table)
  stub(read.sql_report, "read_sql", read_sql)

  report <- read(sql_report,"select * from test_table", freshness = -999)

  expect_length(mock_args(read_sql),1)
  expect_equal(mock_args(read_sql)[[1]][["freshness"]],-999)
})

# output.sql_report -------------------------------------------------------

test_that("output.sql_report sends an excel file", {
  sql_report$data <- data.table(a = runif(1000))
  send_xlsx <- mock()
  stub(output.sql_report, "send_xlsx", send_xlsx)

  output(sql_report)

  expect_length(mock_args(send_xlsx),1)
  expect_equal(mock_args(send_xlsx)[[1]][["table"]], sql_report$data)
})

test_that("output.sql_report passes on additional parameters to send_xlsx", {
  sql_report$data <- data.table(a = runif(1000))
  send_xlsx <- mock()
  stub(output.sql_report, "send_xlsx", send_xlsx)

  output(sql_report, subject = "subject", body = "body")

  expect_length(mock_args(send_xlsx),1)
  expect_equal(mock_args(send_xlsx)[[1]][["table"]], sql_report$data)
  expect_equal(mock_args(send_xlsx)[[1]][["subject"]], "subject")
  expect_equal(mock_args(send_xlsx)[[1]][["body"]], "body")
})

# run.sql_report ----------------------------------------------------------

test_that("run.sql_report passes on parameters", {
  table <- data.table(a = runif(1000))
  read_sql <- mock(table)
  send_xlsx <- mock()
  stub(read.sql_report, "read_sql", read_sql)
  stub(output.sql_report, "send_xlsx", send_xlsx)
  stub(run.report, "read.sql_report", read.sql_report)
  stub(run.report, "output.sql_report", output.sql_report)

  run(sql_report, "select * from test_table", freshness = -999, subject = "subject", body = "body")

  expect_length(mock_args(read_sql),1)
  expect_equal(mock_args(read_sql)[[1]][["query"]],"select * from test_table")
  expect_equal(mock_args(read_sql)[[1]][["freshness"]],-999)

  expect_length(mock_args(send_xlsx),1)
  expect_equal(mock_args(send_xlsx)[[1]][["table"]], table)
  expect_equal(mock_args(send_xlsx)[[1]][["subject"]], "subject")
  expect_equal(mock_args(send_xlsx)[[1]][["body"]], "body")
})

