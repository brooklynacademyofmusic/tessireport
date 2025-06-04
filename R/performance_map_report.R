#' @name performance_map_report
#' @title performance_map_report
#' @description Hierarchical clustering of performances, using audience crossover
#' data to calculate the effective "distance" between performances
performance_map_report <- report(list(),c("performance_map","email_report"))

#' @export
#' @describeIn performance_map_report read data for performance map
read.performance_map_report <- function(report) {
  ticketStream <- read_cache("ticket_stream", "stream") %>%
    filter(event_subtype == "Live Performance") %>% collect %>% setDT

  tickets = ticketStream

  o = read_tessi("order_detail",
                 select = c("perf_desc","perf_dt","perf_no","seat_no",
                            "prod_season_no")) %>% collect %>% setDT

  s = read_tessi("performances") %>% collect %>% setDT

  # tickets = tickets[group_customer_no>=200,.(T),by=c("group_customer_no","prod_season_no")]
  prodData = o[,.(perf_desc=first(perf_desc),perf_dt=mean(perf_dt,na.rm=TRUE),inventory=n_distinct(perf_no,seat_no)),by="prod_season_no"]
  seasonData = s[,.(prod_season_no,season_desc)] %>% distinct %>% .[,`:=`(season_desc=stringr::str_remove(season_desc,"\\s*\\d{2}FY\\s*"),
                                                                          season_fyear=stringr::str_extract(season_desc,"\\d{2}FY"))]
}

#' @export
#' @describeIn performance_map_report analyze and cluster performances
process.performance_map_report <- function(report) {


  # Limit this to tickets that we care about
  trainingTickets = tickets[group_customer_no>=200 & timestamp>today()-dyears(5),.(T),by=c("group_customer_no","prod_season_no")]

  # Filter the tickets by repeat customers, and filter the productions by at least 10 repeat customers
  repeatCustomers = trainingTickets[,.N,by="group_customer_no"][N>1]
  trainingTickets = trainingTickets[group_customer_no %in% repeatCustomers$group_customer_no] %>%
    .[,N:=.N,by="prod_season_no"] %>% .[N>=10,.(prod_season_no,group_customer_no,T)]

  # Build table of customer attendance
  productions_by_customer = trainingTickets %>% dcast(prod_season_no~group_customer_no,fill=F) %>%
    .[,prod_season_no := as.integer(as.character(prod_season_no))]
  # Build table of performance dates
  production_dates = prodData[productions_by_customer,.(prod_season_no,as.integer(perf_dt)),on="prod_season_no"]

  # Calculate distances
  production_distance = parDist(x=as.matrix(productions_by_customer[,-1]) %>%
                                  `rownames<-`(productions_by_customer[,1][[1]]),method="simpson")
  production_time_distance = parDist(x=as.matrix(production_dates[,-1]) %>%
                                       `rownames<-`(production_dates[,1][[1]]))

  # Time correction for production distance -- used a lm fit lm(log(1.00001-V1)~V2) to determine relationship between the time difference between the performances
  # and the simpson distance.
  #
  production_distance2 = as.matrix(production_distance) %>%
    `rownames<-`(productions_by_customer$prod_season_no) %>%
    `colnames<-`(productions_by_customer$prod_season_no) %>% reshape2::melt(value.name="dist")
  production_time_distance2 = as.matrix(production_time_distance) %>%
    `rownames<-`(productions_by_customer$prod_season_no) %>%
    `colnames<-`(productions_by_customer$prod_season_no) %>% reshape2::melt(value.name="time")

  dist_time = merge(production_distance2,production_time_distance2,by=c("Var1","Var2"))
  sample_frac(dist_time,.1) %>% ggplot() + geom_density_2d_filled(aes(sqrt(time),boot::logit(dist)),bins=100) + theme(legend.position="none")

  # This regression is pretty stable across different subsets of seasons so let's just go with it.
  linear_model = glm(dist~sqrt(time),data=dist_time,family="binomial")
  summary(linear_model)

  dist_time_corr = cbind(dist_time,corr=predict(linear_model,newdata=dist_time,type="response")) %>% setDT %>%
    .[is.na(corr),corr:=0] %>%
    .[is.na(dist),dist:=1] %>%
    .[,dist := dist-corr+1]
  # production_distance2_corr = as.matrix(production_distance_corr) %>%
  #   `rownames<-`(productions_by_customer$prod_season_no) %>%
  #   `colnames<-`(productions_by_customer$prod_season_no) %>% reshape2::melt(value.name="dist")
  #dist_time_corr = merge(production_distance2_corr,production_time_distance2,by=c("Var1","Var2"))

  sample_frac(dist_time_corr,.1) %>% ggplot() + geom_density_2d_filled(aes(sqrt(time),boot::logit(dist)),bins=100) +
    theme(legend.position="none")

  production_distance_corr = dist_time_corr[,.(Var1,Var2,dist)] %>% dcast(Var1~Var2,value.var="dist") %>%
    tibble::column_to_rownames("Var1") %>%
    as.matrix %>% as.dist

  cluster.ward = hclust(production_distance_corr,method="ward.D2")
  #cluster.diana = cluster::diana(production_distance_corr)
  #cluster.agnes = cluster::agnes(production_distance_corr)
  #table(cutree(cluster.ward %>% as.dendrogram,12),productions$season)

  production_groups <- data.frame(group = cutree(cluster.ward, 8)) %>%
    tibble::rownames_to_column("prod_season_no") %>%
    mutate(prod_season_no = as.integer(prod_season_no)) %>%
    left_join(prodData,by="prod_season_no")

  production_groups %>% split(.$group) %>% purrr::imap(\(data,i) {
    paste(c(deparse1(data$perf_desc),
            deparse1(data$prod_season_no)),
          collapse = "\n")
  }) %>% paste(collapse = "\n\n") %>%
    stringr::str_replace_all("(\\d+)L(,|\\))","\\1\\2") %>%
    stringr::str_replace_all("c\\(","(") %>%
    write(filename_txt)

  cluster.ward$labels = prodData[match(cluster.ward$labels,prod_season_no),perf_desc]

  prodMap <- cmdscale(production_distance_corr) %>% as.data.frame() %>% tibble::rownames_to_column("prod_season_no") %>%
    mutate(prod_season_no=as.factor(prod_season_no)) %>%
    merge(prodData,on="prod_season_no") %>%
    merge(seasonData,on="prod_season_no") %>%
    setDT

  customerMap <- prodMap[tickets,,on=c("prod_season_no")][!is.na(V1)] %>%
    .[,`:=`(mean_V1_0 = mean(V1),
            mean_V2_0 = mean(V2),
            error_0 = sum((mean(V1)-V1)^2 + (mean(V2)-V2)^2)/.N)] %>%
    .[,`:=`(max_season_fyear=max(season_fyear),
            mean_V1 = mean(V1),
            mean_V2 = mean(V2),
            error = (sum((mean(V1)-V1)^2 + (mean(V2)-V2)^2)+error_0)/(.N+1)),
      by="group_customer_no"] %>%
    .[,`:=`(N=.N),by="prod_season_no"]
}

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
}

