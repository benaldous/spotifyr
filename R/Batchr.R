#' Batch a tibble column
#'
#' @param x A tbl
#' @param col Column to batch
#' @param n Rows in each group
#' 
#' @import dplyr
#' @importFrom rlang quo_name
#' 
#' @export

group_n = function(x,col,n) {
  group_by(x,Group = floor((row_number() - 1)/n)) %>%
    summarize_at(vars(as_name(enquo(col))),list) %>%
    pull()
}