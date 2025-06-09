withr::local_package("checkmate")
withr::local_package("mockery")

fixtures <- readRDS(rprojroot::find_testthat_root_file("test-performance_map_report.Rds")) %>%
  lapply(arrow::as_arrow_table)

# read.performance_map --------------------------------------------------------------------

test_that("read.performance_map returns a subset of data based on filters", {

  read_tessi <- mock(fixtures$performances, fixtures$order_detail,cycle=T)
  stub(read.performance_map,"read_tessi",read_tessi)
  perf_dts <- fixtures$performances$perf_dt %>% as.vector

  report <- read(performance_map_report,
       since = quantile(perf_dts,.25),
       until = quantile(perf_dts,.75))

  expect_gt(report$performances[,.N],0)
  expect_gt(report$tickets[,.N],0)
  expect_true(all(report$performances$perf_dt >= quantile(perf_dts,.25)))
  expect_true(all(report$performances$perf_dt <= quantile(perf_dts,.75)))
  expect_true(all(report$tickets$perf_dt >= quantile(perf_dts,.25)))
  expect_true(all(report$tickets$perf_dt <= quantile(perf_dts,.75)))

  report <- read(performance_map_report,
                 filter_expr = grepl("Opera",theater_desc),
                 since = as.Date("1900-01-01"),
                 until = as.Date("2100-01-01"))
  expect_gt(report$performances[,.N],0)
  expect_gt(report$tickets[,.N],0)
  expect_match(report$performances$theater_desc,"Opera",all=T)

})


# process -----------------------------------------------------------------

test_that("process.performance_map returns cluster data", {
  read_tessi <- mock(fixtures$performances, fixtures$order_detail,cycle=T)
  stub(read.performance_map,"read_tessi",read_tessi)
  expect_warning(
    report <- read(performance_map_report,
       since = as.Date("1900-01-01"),
       until = as.Date("2100-01-01")) %>% process(n_clusters=2),
    "non-integer")

  expect_names(names(report),must.include=c("production_summary",
                                            "production_groups",
                                            "production_map",
                                            "production_clustering"))

  expect_class(report$production_clustering,"hclust")
  # number of groups must equal n_clusters
  expect_equal(sort(unique(report$production_groups$group)),c(1,2))
  # two dimensional PCA with names and ids
  expect_names(names(report$production_map),must.include=c("V1","V2",
                                                           "prod_season_desc",
                                                           "prod_season_no"))
  # summary data for use in reporting
  expect_names(names(report$production_summary),must.include=c("prod_season_desc",
                                                               "prod_season_no",
                                                               "perf_dt"))

  # we have matching data between cluster, groups, and map
  expect_names(labels(report$production_clustering),
               permutation.of = report$production_groups$prod_season_desc)
  expect_names(labels(report$production_clustering),
               permutation.of = report$production_map$prod_season_desc)
  expect_names(report$production_summary$prod_season_desc,
               must.include = labels(report$production_clustering))

})

# write -------------------------------------------------------------------

test_that("write.performance_map writes out a pdf and is ready for emailing", {
  read_tessi <- mock(fixtures$performances, fixtures$order_detail,cycle=T)
  stub(read.performance_map,"read_tessi",read_tessi)
  expect_warning(
    report <- read(performance_map_report,
                   since = as.Date("1900-01-01"),
                   until = as.Date("2100-01-01")) %>%
      process(n_clusters=2) %>%
      write(n_clusters=2),
    "non-integer")

  expect_names(names(report),must.include="filename")
})
