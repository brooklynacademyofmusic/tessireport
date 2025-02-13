
# dataset_chunk_write -----------------------------------------------------

test_that("dataset_chunk_write writes out a chunk of data", {
  tessilake::local_cache_dirs()

  dataset <- data.frame(
    rowid = 1:6,
    group_customer_no = rep(1:2,each=3),
    event_type = c("Ticket","Contribution","Contribution"),
    contributionAmt = 50,
    event = T,
    timestamp = rep(c(Sys.Date()-10,Sys.Date()-.001),each=3))

  dataset_chunk_write(dataset, "year", "test")

  dataset <- read_cache("dataset","test") %>% collect

  expect_equal(nrow(dataset),6)
  expect_equal(dataset[1,"event"][[1]],TRUE)
  expect_equal(dataset[1,"group_customer_no"][[1]],1)

})

test_that("dataset_chunk_write appends to existing data", {
  tessilake::local_cache_dirs()

  dataset <- data.frame(
    rowid = 1:6,
    group_customer_no = rep(1:2,each=3),
    event_type = c("Ticket","Contribution","Contribution"),
    contributionAmt = 1:6,
    event = T,
    timestamp = rep(c(Sys.Date()-10,Sys.Date()-.001),each=3))

  dataset_chunk_write(dataset, "year", "test")

  dataset <- data.frame(
    rowid = 1:6,
    group_customer_no = rep(1:2,each=3),
    event_type = c("Ticket","Contribution","Contribution"),
    contributionAmt = 1:6,
    event = T,
    timestamp = rep(c(Sys.Date()+10,Sys.Date()+.001),each=3))

  expect_warning(dataset_chunk_write(dataset, "year", "test"),"primary_keys not given")

  dataset <- read_cache("dataset","test") %>% collect

  expect_equal(nrow(dataset),12)
  expect_equal(dataset[1,"event"][[1]],TRUE)
  expect_equal(dataset[1,"group_customer_no"][[1]],1)

})


test_that("dataset_chunk_write over multiple chunks produces the same results as run once", {
  tessilake::local_cache_dirs()

  dataset <- rbind(
    data.frame(
      rowid = 1:6,
      group_customer_no = rep(1:2,each=3),
      event_type = c("Ticket","Contribution","Contribution"),
      contributionAmt = 1:6,
      event = T,
      timestamp = Sys.Date() + seq(6)),
    data.frame(
      rowid = 7:12,
      group_customer_no = rep(1:2,each=3),
      event_type = c("Ticket","Contribution","Contribution"),
      contributionAmt = 1:6,
      event = T,
      timestamp = Sys.Date() + seq(7,12))) %>% arrow::as_arrow_table()

  dataset_chunk_write(dataset, "year", "test")

  dataset_test <- read_cache("dataset","test") %>% collect

  expect_equal(nrow(dataset_test),12)
  setkey(dataset_test,group_customer_no,timestamp)
  expect_equal(dataset_test[,contributionAmt],c(NA,1,2,3,1,2,NA,4,5,6,4,5))
  expect_equal(dataset_test[,timestamp],rep(c(0,1,2,6,7,8)*86400,2))

  dataset <- rbind(
    data.frame(
      rowid = 1:6,
      group_customer_no = rep(1:2,each=3),
      event_type = c("Ticket","Contribution","Contribution"),
      contributionAmt = 1:6,
      event = T,
      timestamp = Sys.Date() + seq(6)),
    data.frame(
      rowid = 7:12,
      group_customer_no = rep(1:2,each=3),
      event_type = c("Ticket","Contribution","Contribution"),
      contributionAmt = 1:6,
      event = T,
      timestamp = Sys.Date() + seq(7,12))) %>% arrow::as_arrow_table()

  expect_warning(dataset_chunk_write(dataset, "year", "test", rows = data.table(rowid=7:12)),"primary_keys not given")

  expect_equal(read_cache("dataset","test") %>% collect %>% setkey(group_customer_no,timestamp),
               dataset_test)

})

# dataset_rollback_event ---------------------------------------------------

test_that("dataset_rollback_event rolls back data matching `columns`", {
  dataset <- data.table(event = runif(1000)<.1,
                        leaky_data = seq(1000),
                        other_data = seq(1000),
                        by = 1)
  dataset_e <- copy(dataset)

  dataset_rollback_event(dataset, rollback_cols = "leaky_data", by = "by")

  expect_equal(dataset[event != T], dataset_e[event != T])
  expect_equal(dataset[event == T, leaky_data], dataset_e[event == T, ifelse(leaky_data-1 == 0, NA, leaky_data-1)])
})

test_that("dataset_rollback_event respects group boundaries", {
  dataset <- data.table(event = runif(1000)<.1,
                        leaky_data = seq(1000),
                        other_data = seq(1000),
                        by = sample(10,1000,replace = T))
  dataset_e <- copy(dataset)

  dataset_rollback_event(dataset, rollback_cols = "leaky_data", by = "by")

  expect_equal(dataset[event != T], dataset_e[event != T])
  expect_equal(dataset[event == T & leaky_data >= other_data],dataset[integer(0)])
  for (i in seq(10)) {
    expect_in(dataset[by == i, leaky_data], c(NA,dataset_e[by == i, leaky_data]))
  }
})


# dataset_normalize_timestamp ----------------------------------------------

test_that("dataset_normalize_timestamps replaces all data matching `columns` with offsets", {
  dataset <- data.table(timestamp = seq(as.POSIXct("2020-01-01"), Sys.time(), by = "day"),
                        timestamp_max = seq(as.POSIXct("2020-01-01"), Sys.time(), by = "day")-1,
                        by = 1)

  dataset_normalize_timestamps(dataset, by = "by")

  expect_equal(dataset[,as.double(timestamp)], seq(0,nrow(dataset)-1)*86400)
  expect_equal(dataset[,as.double(timestamp_max)], rep(1,nrow(dataset)))

})

test_that("dataset_normalize_timestamps respects group boundaries", {
  dataset <- data.table(timestamp = seq(as.POSIXct("2020-01-01"), Sys.time(), by = "day"),
                        timestamp_max = seq(as.POSIXct("2020-01-01"), Sys.time(), by = "day")-1)

  dataset[,by := sample(10,.N,replace = T)]
  dataset_e <- copy(dataset)

  timestamp_mins <- dataset[,min(timestamp), by = by]
  dataset_normalize_timestamps(dataset, by = "by")

  expect_equal(dataset[timestamp_mins,timestamp,on="by"],
               dataset_e[timestamp_mins,as.double(timestamp-V1),on="by"])
  expect_equal(dataset[timestamp_mins,as.double(timestamp_max),on="by"],
               rep(1,nrow(dataset)))

})
