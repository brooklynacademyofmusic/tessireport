withr::local_package("checkmate")
withr::local_package("mockery")

customers <- c(8993321,8992917,8992918)
fixture <- readRDS(rprojroot::find_testthat_root_file("unsubscribe_report.Rds"))


# read.unsubscribe_report -------------------------------------------------

test_that("read.unsubscribe_report loads email, address, MGO, membership, and login data", {

  stub(read.unsubscribe_report, "read_cache", fixture[[1]])
  stub(read.unsubscribe_report, "read_tessi", do.call(mock,fixture[-1]))

  report <- read(unsubscribe_report, customers)

  expect_names(names(report), must.include = c("memberships", "MGOs", "emails", "email_events", "addresses", "logins",
                                               "constituencies", "customers"))
  expect_true(all(sapply(report,nrow) > 0))
})

# process.unsubscribe_report ----------------------------------------------

test_that("process.unsubscribe_report identifies email unsubscribes and bounces", {
  stub(read.unsubscribe_report, "read_cache", fixture[[1]])
  stub(read.unsubscribe_report, "read_tessi", do.call(mock,fixture[-1]))

  report <- read(unsubscribe_report, customers)

  report$email_events[,`:=`(event_subtype = rep(c("Open","Click","Send"), length.out = .N),
                            listid = NA)]
  setorder(report$email_events, customer_no, timestamp)
  report$email_events[,`:=`(I=1:.N,N=.N), by = "customer_no"]
  report$email_events[I==N, `:=`(event_subtype = c("Hard Bounce","Soft Bounce","Unsubscribe"),
                                 listid = c(NA,NA,1))]
  report$email_events[I==N-1,`:=`(event_subtype = c("Hard Bounce","Soft Bounce","Unsubscribe"),
                                  listid = c(NA,NA,2))]

  report <- process(report)

  expect_equal(report$report[grepl("Unsubscribe", message),.N], 2) # list 1 and list 2
  expect_equal(report$report[grepl("Bounce", message),.N], 2)
})

test_that("process.unsubscribe_report identifies missing emails", {
  stub(read.unsubscribe_report, "read_cache", fixture[[1]])
  stub(read.unsubscribe_report, "read_tessi", do.call(mock,fixture[-1]))

  report <- read(unsubscribe_report, customers)
  report$emails <- report$emails[customer_no != 8992918]
  report <- process(report)

  expect_equal(report$report[grepl("No primary email", message),.N], 1)
})

test_that("process.unsubscribe_report identifies bad addresses", {
  stub(read.unsubscribe_report, "read_cache", fixture[[1]])
  stub(read.unsubscribe_report, "read_tessi", do.call(mock,fixture[-1]))

  report <- read(unsubscribe_report, customers)
  report$addresses[,last_updated_by:=c("me","you","NCOA$DNM")]
  report <- process(report)

  expect_equal(report$report[grepl("Bad primary mailing address", message),.N], 1)
})

test_that("process.unsubscribe_report identifies missing addresses", {
  stub(read.unsubscribe_report, "read_cache", fixture[[1]])
  stub(read.unsubscribe_report, "read_tessi", do.call(mock,fixture[-1]))

  report <- read(unsubscribe_report, customers)
  report$addresses <- report$addresses[customer_no != 8992918]
  report <- process(report)

  expect_equal(report$report[grepl("No primary mailing address", message),.N], 1)
})

test_that("process.unsubscribe_report identifies non-matching logins", {
  stub(read.unsubscribe_report, "read_cache", fixture[[1]])
  stub(read.unsubscribe_report, "read_tessi", do.call(mock,fixture[-1]))

  report <- read(unsubscribe_report, customers)
  report$logins[,primary_ind:="Y"]
  report <- process(report)

  expect_equal(report$report[grepl("Email does not match login", message),.N], 2)
})

test_that("process.unsubscribe_report identifies inactive customers", {
  stub(read.unsubscribe_report, "read_cache", fixture[[1]])
  stub(read.unsubscribe_report, "read_tessi", do.call(mock,fixture[-1]))

  report <- read(unsubscribe_report, customers)
  report$customers[1,inactive_desc := "Inactive"]
  report <- process(report)

  expect_equal(report$report[grepl("Customer is inactive", message),.N], 1)
})

test_that("process.unsubscribe_report has one row per issue", {
  stub(read.unsubscribe_report, "read_cache", fixture[[1]])
  stub(read.unsubscribe_report, "read_tessi", do.call(mock,fixture[-1]))

  report <- read(unsubscribe_report, customers)

  # add a dummy customer
  report$customers <- rbind(report$customers,report$customers[1][,customer_no:=0])
  # mock some email events
  report$email_events[,`:=`(event_subtype = rep(c("Open","Click","Send"), length.out = .N),
                            listid = NA)]
  report$email_events[,email := sample(unique(report$emails$address),.N,replace=T)]
  setorder(report$email_events, email, timestamp)
  report$email_events[,`:=`(I=1:.N,N=.N), by = "email"]
  # set the last events
  report$email_events[I==N, `:=`(event_subtype = c("Hard Bounce","Soft Bounce","Unsubscribe"),
                                 listid = c(NA,NA,3))]
  # set the second-to-last events
  report$email_events[I==N-1,`:=`(event_subtype = c("Send","Send","Unsubscribe"),
                                  listid = c(NA,NA,2))]
  # set the address statuses
  report$addresses[,last_updated_by:=c("me","you","NCOA$DNM")]
  # enable one login
  report$logins[1,primary_ind:="Y"]
  # disable one customer
  report$customers[1,inactive_desc := "Inactive"]

  report <- process(report)

  expect_equal(report$report[,.N],9) # 4 bad emails, 1 missing, 1 bad mailing address, 1 missing, 1 bad login, 1 inactive.
  expect_equal(report$report[grepl("Unsubscribe",message),.N],2)
  expect_equal(report$report[grepl("Bounce",message),.N],2)
  expect_equal(report$report[grepl("No primary.+email",message),.N],1)
  expect_equal(report$report[grepl("No primary.+address",message),.N],1)
  expect_equal(report$report[grepl("Bad.+address",message),.N],1)
  expect_equal(report$report[grepl("Customer.+inactive",message),.N],1)
  expect_equal(report$report[grepl("Email.+login",message),.N],1)

})

test_that("process.unsubscribe_report includes timestamp, membership, MGO, name, email, and constituency data", {
  stub(read.unsubscribe_report, "read_cache", fixture[[1]])
  stub(read.unsubscribe_report, "read_tessi", do.call(mock,fixture[-1]))

  report <- read(unsubscribe_report, customers)
  report <- process(report)

  expect_names(colnames(report$report), permutation.of = c("message", "timestamp", "memb_level", "expr_dt", "customer_no", "MGOs",
                                                           "name", "constituencies", "email"))
  expect_false(any(sapply(report$report,\(x) all(is.na(x)))))
})

# output.unsubscribe_report -----------------------------------------------

test_that("output.unsubscribe_report filters based on since and until", {
  stub(read.unsubscribe_report, "read_cache", fixture[[1]])
  stub(read.unsubscribe_report, "read_tessi", do.call(mock,fixture[-1]))

  send_xlsx <- mock(cycle = TRUE)
  stub(output.unsubscribe_report, "send_xlsx", send_xlsx)
  stub(output.unsubscribe_report, "read_sql", data.table(fname = NA, lname = NA, userid = NA))

  report <- read(unsubscribe_report, customers) %>% process(report)
  nrows <- nrow(report$report)
  report$report$timestamp <- c(Sys.Date() - 1, Sys.Date() + seq(2, nrows))
  report$report$expr_dt <- c(Sys.Date() + 1, Sys.Date() + seq(2, nrows))

  output(report, since = Sys.Date(), until = Sys.Date()) # 0
  output(report, since = Sys.Date() - 1, Sys.Date()) # 1
  output(report, since = Sys.Date(), Sys.Date() + 1) # 1

  expect_length(mock_args(send_xlsx), 2)
  expect_equal(nrow(mock_args(send_xlsx)[[1]][[1]]),1)
  expect_equal(nrow(mock_args(send_xlsx)[[2]][[1]]),1)


})

test_that("output.unsubscribe_report routes based on constituency", {
  stub(read.unsubscribe_report, "read_cache", fixture[[1]])
  stub(read.unsubscribe_report, "read_tessi", do.call(mock,fixture[-1]))

  send_xlsx <- mock(cycle = TRUE)
  stub(output.unsubscribe_report, "send_xlsx", send_xlsx)

  report <- read(unsubscribe_report, customers) %>% process(report)
  nrows <- nrow(report$report)
  report$report$timestamp <- Sys.time()
  report$report$constituencies <- c("a", "b", "c", rep(NA, nrows - 3))

  output(report, routing_rules = list(constituencies == "a" ~ list("person_a"),
                                      constituencies == "b" ~ list(c("person_b","person_c")),
                                      constituencies == "c" ~ list("person_c"),
                                      TRUE ~ list("default")))

  expect_length(mock_args(send_xlsx), 4)
  expect_equal(nrow(mock_args(send_xlsx)[[1]][[1]]), 1)
  expect_equal(nrow(mock_args(send_xlsx)[[2]][[1]]), 1)
  expect_equal(nrow(mock_args(send_xlsx)[[3]][[1]]), 2)
  expect_equal(nrow(mock_args(send_xlsx)[[4]][[1]]), nrows - 3)
  expect_equal(mock_args(send_xlsx)[[1]][["emails"]][2], "person_a")
  expect_equal(mock_args(send_xlsx)[[2]][["emails"]][2], "person_b")
  expect_equal(mock_args(send_xlsx)[[3]][["emails"]][2], "person_c")
  expect_equal(mock_args(send_xlsx)[[4]][["emails"]][2], "default")

})
