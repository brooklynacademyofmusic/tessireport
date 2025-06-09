#' @name performance_map_report
#' @title performance_map_report
#' @description Hierarchical clustering of performances, using audience crossover
#' data to calculate the effective "distance" between performances.
#' * Distance is calculated using Simpson's distance metric
#' * Distance is corrected by time using a binomial regression on time between
#' performances
#' * Clustering is hierarchical using Ward D2 distance
#' * Mapping is classic multidimensional scaling, i.e. PCA
#' @export
performance_map_report <- report(list(),c("performance_map","email_report"))

#' @param report `report` object
#' @param since `POSIXct` performance data on/after this date will be returned
#' @param until `POSIXct` performance data on/before this date will be returned
#' @param filter_expr `expression` to use when filtering performances, i.e.
#' `read_tessi("performances") %>% filter(!!filter_expr)`
#' @param ... not used
#' @export
#' @importFrom tessilake read_tessi
#' @importFrom dplyr filter collect
#' @importFrom tidyselect all_of
#' @describeIn performance_map_report read data for performance map
read.performance_map <- function(report,since = Sys.Date()-365*5,
                                        until = Sys.Date()+365,
                                        filter = NULL, ...) {

  filter_expr = rlang::enexpr(filter_expr)

  report$performances = read_tessi("performances") %>%
    filter(perf_dt >= since & perf_dt <= until) %>%
    filter(!!filter_expr) %>%
    collect %>% setDT

  report$tickets = read_tessi("order_detail", select = c(
    "perf_desc","perf_dt","perf_no","seat_no","prod_season_no","customer_no")) %>%
    filter(perf_dt >= since & perf_dt <= until) %>%
    filter(perf_no %in% report$performances$perf_no) %>%
    collect %>% setDT

  NextMethod()

}


# prodData = o[,.(perf_desc=first(perf_desc),perf_dt=mean(perf_dt,na.rm=TRUE),inventory=n_distinct(perf_no,seat_no)),by="prod_season_no"]
# seasonData = s[,.(prod_season_no,season_desc)] %>% distinct %>% .[,`:=`(season_desc=stringr::str_remove(season_desc,"\\s*\\d{2}FY\\s*"),
#                                                                         season_fyear=stringr::str_extract(season_desc,"\\d{2}FY"))]


#' @param n_clusters `integer` number of clusters to make
#'
#' @export
#' @describeIn performance_map_report analyze and cluster performances
#' @importFrom data.table dcast
#' @importFrom parallelDist parDist
#' @importFrom boot logit inv.logit
#' @importFrom stats cmdscale
#' @importFrom dendextend cutree
process.performance_map <- function(report, n_clusters = 8, ...) {

  # Limit this to real tickets and summarize
  training_tickets = report$tickets[group_customer_no>=200,.(T),
                                    by=c("group_customer_no",
                                         "prod_season_no")]

  # Filter the tickets by repeat customers, and filter the productions by
  # least 10 repeat customers to limit noise
  repeat_customers = training_tickets[,.N,by="group_customer_no"][N>1]
  training_tickets = training_tickets[repeat_customers,on="group_customer_no"] %>%
    .[,N:=.N,by="prod_season_no"] %>% .[N>10] %>% .[,N:=NULL]

  # Build table of customer attendance
  productions_by_customer = training_tickets %>%
    dcast(prod_season_no~group_customer_no,value.var="T",fill=F)

  productions_by_time = report$performances[,.(perf_dt=as.numeric(median(perf_dt))),
                                            by="prod_season_no"]

  # Calculate distances
  production_distance = parDist(
    as.matrix(productions_by_customer,rownames = T),
    method="simpson")
  production_time_distance = parDist(
    as.matrix(productions_by_time,rownames = T))

  # Time correction for production distance -
  # to determine relationship between the time difference between the performances
  # and the simpson distance.
  production_distance = as.matrix(production_distance) %>%
    reshape2::melt(value.name="dist",varnames = c("prod1","prod2"))
  production_time_distance = as.matrix(production_time_distance) %>%
    reshape2::melt(value.name="time",varnames = c("prod1","prod2"))

  production_distance = merge(production_distance,production_time_distance)
  dist_time_model = glm(dist~sqrt(time),data=production_distance,family="binomial")

  setDT(production_distance)
  production_distance[,corr := stats::predict(dist_time_model,
                                              newdata=.SD,type = "response")]

  # Adjust the distance by the the correction
  production_distance_corr <- production_distance[,dist_corr :=
      inv.logit(logit(dist)-logit(corr))] %>%
    dcast(prod1~prod2,value.var="dist_corr") %>%
    as.matrix(rownames=T) %>% as.dist


  # Prepare output
  report$production_summary <- report$performances[,.(
    prod_season_desc=first(prod_season_desc),
    perf_dt=median(perf_dt)), by=prod_season_no]

  # Clustering
  clustering <- hclust(production_distance_corr,method="ward.D2")

  report$production_groups <- cutree(clustering,n_clusters,
                                     order_clusters_as_data=F) %>%
    data.table(group = ., prod_season_no = as.integer(names(.))) %>%
    merge(report$production_summary)

  report$production_map <- cmdscale(production_distance_corr) %>%
    as.data.table(keep.rownames = "prod_season_no") %>%
    .[,prod_season_no := as.integer(prod_season_no)] %>%
    merge(report$production_summary)

  clustering$labels = report$production_summary[data.table(
    prod_season_no=as.integer(clustering$labels)),
    prod_season_desc,
    on="prod_season_no"]
  report$production_clustering <- clustering

  NextMethod()

}

#' @param highlight_since `POSIXct` performances on/after this date will be highlighted in the returned plots
#' @export
#' @importFrom stats as.dendrogram
#' @importFrom dendextend color_labels color_branches cutree
#' @importFrom ggplot2 scale_color_brewer ggplot aes geom_point theme_minimal
#' scale_x_continuous scale_y_continuous scale_color_manual scale_alpha_manual
#' @importFrom colorspace diverging_hsv
#' @importFrom ggrepel geom_text_repel
#' @describeIn performance_map_report write pdf of performance maps and an file for importing into Tessitura
write.performance_map <- function(report, n_clusters = 8, highlight_since = Sys.Date() - 365, ...) {

  palette <- if(n_clusters <= 8) {
    scale_color_brewer(palette=2,type="qual")$palette
  } else {
    rainbow_hcl
  }

  highlight <- report$performances[perf_dt>highlight_since,
                                   unique(prod_season_desc)]
  report$production_map[,highlight := prod_season_desc %in% highlight]

  report$filename <- write_pdf({
    pdf_plot(title = "Family tree of performances",
             subtitle = paste("Performances since",format(highlight_since,"%D"),"are marked in bold"),
             report$production_clustering %>% as.dendrogram %>%
               color_labels(k=n_clusters,col=palette) %>%
               color_branches(k=n_clusters,col=palette) %>%
               dendextend::set("by_labels_branches_lwd",highlight) %>%
               dendextend::set("labels_cex",.4) %>%
               plot)

    pdf_plot(title = "Scatterplot of performance (PCA)",
             subtitle = paste("Performances since",format(highlight_since,"%D"),"are marked with triangles"),
             report$production_map %>%
                ggplot(aes(V1,V2,
                           color=as.factor(cutree(report$production_clustering,
                                                  n_clusters,
                                                  order_clusters_as_data=F)[prod_season_desc]))) +
                geom_point(aes(shape=highlight,
                               alpha=highlight)) +
                geom_text_repel(aes(label=prod_season_desc),cex=1,max.overlaps=25) +
                theme_minimal(base_size = 8) + theme(legend.position = "none") +
                scale_color_manual(values = palette(n_clusters)) +
                scale_alpha_manual(values = c(.5,1)))

    report$production_groups %>% split(.$group) %>% purrr::walk(~print(pdf_table(.)))
  })

  NextMethod()

}

