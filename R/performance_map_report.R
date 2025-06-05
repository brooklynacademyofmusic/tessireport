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
                                        filter_expr = NULL, ...) {

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
#' @importFrom reshape2 melt
#' @importFrom boot logit inv.logit
#' @importFrom stats cmdscale
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

  # Time correction for production distance -- used a lm fit lm(log(1.00001-V1)~V2) to determine relationship between the time difference between the performances
  # and the simpson distance.
  #
  production_distance = as.matrix(production_distance) %>%
    melt(value.name="dist",varnames = c("prod1","prod2"))
  production_time_distance = as.matrix(production_time_distance) %>%
    melt(value.name="time",varnames = c("prod1","prod2"))

  production_distance = merge(production_distance,production_time_distance)
  dist_time_model = glm(dist~sqrt(time),data=production_distance,family="binomial")

  setDT(production_distance)
  production_distance[,corr := stats::predict(dist_time_model,
                                              newdata=.SD,type = "response")]

  production_distance_corr <- production_distance[,dist_corr :=
      inv.logit(logit(dist)-logit(corr))] %>%
    dcast(prod1~prod2,value.var="dist_corr") %>%
    as.matrix(rownames=T) %>% as.dist

  production_summary <- report$performances[,.(
    prod_season_desc=first(prod_season_desc),
    fyear=first(fyear)), by=prod_season_no]

  report$production_clustering <- hclust(production_distance_corr,method="ward.D2")
  report$production_clustering$labels = production_summary[data.table(
    prod_season_no=as.integer(report$production_clustering$labels)),
    prod_season_desc,
    on="prod_season_no"]

  report$production_groups <- cutree(clustering,12) %>%
    data.table(group = ., prod_season_no = as.integer(names(.))) %>%
    merge(production_summary)

  report$production_map <- cmdscale(production_distance_corr) %>%
    as.data.table(keep.rownames = "prod_season_no") %>%
    .[,prod_season_no := as.integer(prod_season_no)] %>%
    merge(production_summary)

  NextMethod()

}

#' @export
write.performance_map <- function(report, ...) NextMethod()

write.performance_map_report <- function(report) {
  #' \clearpage
  #' ###### Family tree of 20FY-25FY performances
  #' 25FY performances are marked in bold
  #+ dendrogram, echo = FALSE, fig.width = 14, fig.height = 7.5

  cluster.ward %>% as.dendrogram %>%
    color_labels(k=8,col=scale_color_brewer(palette=2,type="qual",direction = -1)$palette) %>%
    color_branches(k=8,col=scale_color_brewer(palette=2,type="qual",direction = -1)$palette) %>%
    dendextend::set("by_labels_branches_lwd",prodData[seasonData[season_fyear=="25FY"],
                                                      perf_desc,on="prod_season_no"]) %>%
    dendextend::set("labels_cex",.4) %>% plot

  #' \clearpage
  #' ###### Scatterplot of 20FY-25FY Lives (Metric Multidimensional Scaling)
  #' 25FY is marked with triangles
  #+ scatter 1, echo = FALSE, fig.width = 14, fig.height = 7.5

  prodMap %>%
    ggplot(aes(V1,V2,size=inventory,color=as.factor(cutree(cluster.ward,8,order_clusters_as_data = FALSE)[perf_desc]))) +
    geom_point(aes(shape=season_fyear=="25FY",
                   alpha=if_else(season_fyear=="25FY",1,.5))) +
    geom_text_repel(aes(label=perf_desc),cex=1,max.overlaps=25) +
    theme_minimal(base_size = 8) + theme(legend.position = "none") +
    scale_x_continuous(trans = "reverse")+#, name = "theater : dance") +
    scale_y_continuous(trans = "reverse")+#, name = "avantgarde : traditional") +
    scale_colour_brewer(palette=2,type="qual",direction = -1) +
    scale_size_continuous(trans="sqrt") +
    scale_alpha_identity()

  #' \clearpage
  #' ###### Customer heatmap for Live performance 20FY-25FY
  #' Customers who bought tickets to NWF 25FY are most interested in shows in the bright hex boxes
  #+ heatmap,  echo = FALSE, fig.width = 14, fig.height = 7.5

  customerMap %>%
    filter(group_customer_no %in% customerMap[season_fyear == "25FY",group_customer_no] &
             season_fyear < "25FY") %>%
    ggplot(aes(V1,V2)) +
    geom_hex(aes(weight=1/inventory),bins=c(20,10)) +

    geom_point(data=prodMap,
               aes(V1,V2,color=as.factor(cutree(cluster.ward,8,order_clusters_as_data = FALSE)[perf_desc]),
                   shape=season_fyear=="25FY",
                   alpha=if_else(season_fyear=="25FY",1,.5))) +
    geom_text_repel(data=prodMap,aes(label=perf_desc),color="black",cex=1,max.overlaps=25) +

    theme_minimal(base_size = 8) + theme(legend.position = "none") +
    scale_x_continuous(trans = "reverse")+#, name = "theater : dance") +
    scale_y_continuous(trans = "reverse")+#, name = "avantgarde : traditional") +
    scale_fill_distiller(palette = "YlGnBu",labels = scales::percent)  +
    scale_alpha_identity()

  production_groups %>% split(.$group) %>% purrr::imap(\(data,i) {
    paste(c(deparse1(data$perf_desc),
            deparse1(data$prod_season_no)),
          collapse = "\n")
  }) %>% paste(collapse = "\n\n") %>%
    stringr::str_replace_all("(\\d+)L(,|\\))","\\1\\2") %>%
    stringr::str_replace_all("c\\(","(") %>%
    write(filename_txt)

}

