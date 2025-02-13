
#' dataset_chunk_write
#'
#' Write out a chunk of a larger `dataset`, using Hadoop partition=`partition` nomenclature,
#' saving it in the cache dir `dataset_name`. The chunk is identified by the `rowid` column in `rows`,
#' which is attached to the columns of the dataset identified by `cols`. Features matching
#' the regular expression in `rollback` are rolled back one row, and all timestamps are normalized
#' by [dataset_normalize_timestamps].
#'
#' @param dataset `data.frameish` dataset to load from, must contain an index column and cannot
#' be an arrow_dplyr_query
#' @param partition `character`|`integer` identifying the partition the chunk will be saved in
#' @param dataset_name `character` cache directory where the partition will be saved in
#' @param rows [data.table] identifying rows of the dataset to load; will be appended to dataset
#' @param cols `character` columns of the dataset to add to partition
#' @inheritDotParams dataset_rollback_event by event rollback_cols
#' @inheritDotParams dataset_normalize_timestamps timestamp_cols
#' @importFrom checkmate assert_names assert_false test_class assert_vector assert_character assert_data_table
#' @return NULL
dataset_chunk_write <- function(dataset, partition,
                                dataset_name,
                                rows = data.table(rowid=seq_len(nrow(dataset))),
                                cols = colnames(dataset),
                                ...) {

  . <- group_customer_no <- timestamp <- NULL

  assert_names(names(dataset), must.include = c("timestamp","rowid",cols))
  assert_false(test_class(dataset,"arrow_dplyr_query"))
  assert_vector(partition, len = 1)
  assert_character(dataset_name, len = 1)
  assert_data_table(rows)

  # in order to do rollbacks and timestamp normalization we need the most recent previous row per customer
  # and the first row per customer...
  dataset_key <- dataset %>% select(all_of(c("group_customer_no","timestamp","rowid"))) %>%
    collect %>% setDT
  dataset_chunk <- dataset_key[rows, on = "rowid"]

  dataset_chunk[,date := timestamp]
  dataset_key[,date := timestamp + 1e-9]
  setkey(dataset_key,group_customer_no,timestamp)

  previous_rows <- dataset_key[dataset_chunk,.(rowid,timestamp,group_customer_no),on=c("group_customer_no","date"), roll = Inf]
  first_rows <- dataset_key[,first(.SD),by=c("group_customer_no"),.SDcols = c("timestamp","rowid")]

  # load rows of dataset
  dataset_chunk <- rbind(first_rows,previous_rows,dataset_chunk,fill=T) %>%
    .[!is.na(rowid),last(.SD),by="rowid"] %>%
    setkey(group_customer_no,timestamp)
  dataset <- dataset %>% filter(rowid %in% dataset_chunk$rowid) %>%
    select(all_of(c("rowid",cols))) %>% collect %>% setDT %>%
    .[dataset_chunk, on = "rowid"]

  # normalize names for mlr3
  setnames(dataset, names(dataset), \(.) gsub("\\W","_",.))

  dataset <- dataset_rollback_event(dataset = dataset, ...)
  dataset[,date := timestamp]
  dataset <- dataset_normalize_timestamps(dataset = dataset, ...)

  # remove added rows
  dataset[, rowid := dataset_chunk$rowid]
  dataset <- dataset[rows,on="rowid"]

  # limit to input columns
  dataset <- dataset[,union(gsub("\\W","_",cols),c(colnames(rows),"date","rowid")),with=F]
  dataset[,`:=`(partition = partition)]

  tessilake::write_cache(dataset, "dataset", dataset_name, partition = "partition",
                         incremental = TRUE, date_column = "date", sync = FALSE, prefer = "from")

}


#' dataset_rollback_event
#'
#' Rolls back the data in `columns` for rows flagged by `event` to prevent data leaks during training.
#'
#' @param dataset data.table of data to roll back
#' @param rollback_cols character vector of columns to roll back
#' @param event character column name containing a logical feature that indicates events to rollback
#' @param by character column name to group the table by
#' @param ... not used
#'
#' @return rolled back data.table
#' @importFrom checkmate assert_data_table assert_names assert_logical
#' @importFrom dplyr lead lag
dataset_rollback_event <- function(dataset, event = "event",
                                   rollback_cols = setdiff(colnames(dataset),
                                                     c(by,event,"timestamp")),
                                   by = "group_customer_no", ...) {

  i <- by_i <- . <- NULL

  assert_data_table(dataset)

  # normalize names
  rollback_cols <- gsub("\\W","_",rollback_cols)
  # can't rollback `by` or `event`
  rollback_cols <- setdiff(rollback_cols, c(by, event))

  assert_names(names(dataset),must.include = c(event,rollback_cols,by))
  assert_logical(dataset[,event,with=F][[1]])

  dataset[,i := seq_len(.N)]
  dataset[,by_i := seq_len(.N), by = by]

  event_ <- event
  rm(event)

  setkeyv(dataset,c(by,"by_i"))

  rollback <- dataset[lead(get(event_)) == T, c(rollback_cols,by,"by_i"),with=F] %>% .[,by_i := by_i+1] %>%
    rbind(dataset[get(event_) == T & by_i == 1, c(by, "by_i"), with = F], fill = T)
  dataset[rollback, (rollback_cols) := mget(paste0("i.",rollback_cols)), on = c(by, "by_i")]

  setkey(dataset,i)

  dataset[,`:=`(by_i = NULL, i = NULL)]

}

#' dataset_normalize_timestamps
#'
#' Replaces the date-times in `columns` with integer offsets from `timestamp`, and replaces `timestamp`
#' with the integer offset from the minimum `timestamp` per group identified by `by`.
#'
#' @param dataset data.table of data to normalize
#' @param timestamp_cols character vector of columns to normalize; defaults to all columns with a name containing the word `timestamp`
#' @param by character column name to group the table by
#' @param ... not used
#' @importFrom checkmate assert_data_table assert_names
#' @return normalized data.table
dataset_normalize_timestamps <- function(dataset,
                                         timestamp_cols = grep("timestamp", colnames(dataset), value=T, ignore.case = T),
                                        by = "group_customer_no", ...) {
  timestamp <- min_timestamp <- NULL

  # normalize names
  timestamp_cols <- gsub("\\W","_",timestamp_cols) %>% setdiff("timestamp")

  assert_data_table(dataset)
  assert_names(names(dataset), must.include = c("timestamp",timestamp_cols,by))
  assert_data_table(dataset[,c("timestamp",timestamp_cols), with = F],types=c("Date","POSIXct"))

  dataset[,(timestamp_cols) := lapply(.SD, \(c) as.numeric(as.POSIXct(timestamp)-as.POSIXct(c))), .SDcols = timestamp_cols]
  dataset[,min_timestamp := min(as.POSIXct(timestamp), na.rm = T), by = by]
  dataset[,timestamp := as.numeric(as.POSIXct(timestamp) - min_timestamp)]
  dataset[,min_timestamp := NULL]
  dataset
}
