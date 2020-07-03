#' @importFrom data.table rbindlist
#' @importFrom dplyr %>% arrange filter group_by slice ungroup mutate as_tibble select mutate_each desc funs n bind_rows
#' @importFrom ggplot2 ggplot geom_ribbon aes geom_line geom_vline xlim annotate geom_point ylim scale_fill_manual geom_point scale_color_manual
#' @importFrom rstan sampling stan_model extract
#' @importFrom tidyr gather
#' @importFrom rvest html_nodes html_text
#' @importFrom stats model.matrix acf pacf
#' @importFrom XML readHTMLTable
#' @importFrom xml2 read_html xml_attr 
NULL

globalVariables(".")
