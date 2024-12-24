#' crossovers_prod
#'
#' Simple analysis of crossover audiences for particular production(s), returns a [data.table] of productions,
#' ranked from highest to lowest % crossover.
#'
#' @param prod_season_no [integer] vector of production season numbers to search for crossovers against
#' @param min_customers [integer](1) a performance must have at least this number of customers to qualify as
#' a potential crossover
#' @importFrom checkmate assert_integerish
#' @importFrom tessilake read_tessi
#' @importFrom dplyr filter select collect mutate group_by summarize n_distinct
crossovers_prod <- function(prod_season_no, min_customers = 10) {

  assert_integerish(prod_season_no)
  assert_integerish(min_customers,len = 1)

  t <- read_tessi("tickets", select = c("customer_no", "prod_season_no", "prod_season_desc")) %>%
    collect %>% setDT %>%
    .[,n_customers := n_distinct(group_customer_no), by = "prod_season_no"]

  .prod_season_no <- prod_season_no
  buyers <- t[prod_season_no %in% .prod_season_no, group_customer_no]

  crossovers <- t[group_customer_no %in% buyers & n_customers >= min_customers] %>%
    .[,.(rate = n_distinct(group_customer_no)/max(n_customers),
         n_customers = max(n_customers)),
                  by = c("prod_season_no","prod_season_desc")] %>%
    setorder(-rate)

}

crossovers_for_tessi <- function(crossovers) {
  cat(deparse(as.numeric(crossovers$prod_season_no)))
  cat(deparse(crossovers$prod_season_desc))
}
