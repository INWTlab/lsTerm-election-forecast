#' @importFrom cmdstanr cmdstan_model
#' @importFrom data.table rbindlist
#' @importFrom dplyr %>% arrange all_of across everything contains filter group_by
#' slice ungroup mutate as_tibble select mutate_each desc funs n bind_rows left_join
#' case_when pull reframe rename_at row_number starts_with vars rename summarise
#' @importFrom ggplot2 ggplot geom_ribbon aes geom_line geom_vline xlim annotate
#' geom_point ylim scale_fill_manual geom_point scale_color_manual scale_y_continuous
#' @importFrom httr GET
#' @importFrom rjson fromJSON
#' @importFrom jsonlite toJSON
#' @importFrom tidyr expand gather pivot_longer
#' @importFrom rvest html_nodes html_text html_attr html_table html_node
#' @importFrom stats model.matrix acf pacf na.omit quantile rgamma time
#' @importFrom qdapRegex ex_between
#' @importFrom utils combn read.csv read.csv2
#' @importFrom XML readHTMLTable
#' @importFrom xml2 read_html xml_attr
#' @importFrom utils read.delim
NULL

globalVariables(c(".",
                "governing_coaltion",
                "possible_coalition",
                "land",
                "year",
                "election",
                "possible_coalition_ordered",
                "prob",
                "date_forecast",
                "coalition_id",
                "possible_coalition",
                "governing_coalition",
                "governing_coalition_yesno",
                "latest_election",
                "firstPollDate",
                "latestPoll",
                "Datum",
                "Institut",
                "CDU/CSU",
                "CDU.CSU",
                "SPD",
                "GR\\u00dcNE",
                "FDP",
                "LINKE",
                "AfD",
                "BSW",
                "Sonstige",
                "PDS",
                "REP",
                "Linke.PDS",
                "PIRATEN",
                "Rechte",
                "FW",
                "REP/DVU",
                "Befragte",
                "Institut",
                "Datum",
                "party",
                "seat_share",
                "upper",
                "value",
                "Beteiligte.Parteien",
                "Bildung",
                "Election",
                "Land",
                "estimate",
                "lower",
                "party_id",
                "parties"))
