#' Queries election results from url
#'
#' @param urls url where the election results can be found
#'
#' @return dataframe with election results
get_election_results_from_url <- function(urls) {
  lapply(urls, function(url) {
    bundesland_name <- rename_bundesland(url)
    
    print(sprintf("Querying election results for: %s", bundesland_name))
    # GET ELECTION TABLE
    election_table <-
      url %>%
      read_html() %>%
      html_node("table.border") %>%
      html_table(header = FALSE)
    
    # KEEP ONLY COLUMNS WITH PERCENTAGE AND PARTY NAME
    columns_with_percentage <- as.vector(election_table[2, ] == "%")
    election_table <-
      cbind(party = election_table$X1, election_table[columns_with_percentage])
    
    # KEEP ONLY FIRST FOUR DIGITS OF YEAR (REMOVE UNWANTED * OR ¹)
    colnames(election_table)[2:ncol(election_table)] <-
      substr(election_table[1, 2:ncol(election_table)], 1, 4)
    
    # REMOVE COLUMN WITH DUPLICATED NAME
    # SPECIAL CASE WHERE TWO ELECTIONS PER YEAR TOOK PLACE
    election_table <-
      election_table[, !duplicated(colnames(election_table))]
    
    # REMOVE ROWS THAT CONTAIN OLD COLUMN HEADERS AND REMOVE WAHLBETEILIGUNG
    election_table <-
      election_table %>%
      filter(!grepl("Wahlb", party)) %>%
      filter(!grepl("^$", party))
    
    # CONVERT PERCENTAGES TO NUMERIC
    election_table <-
      election_table %>%
      mutate(across(.cols = everything(), .fns = ~ gsub(",", ".", .))) %>%
      mutate(across(
        .cols = !contains("party"),
        .fns = ~ as.numeric(.) / 100
      ))
    
    # KEEP ONLY LATEST YEAR AND RENAME COLUMN TO BUNDESLAND
    latest_election_years <-
      as.character(-sort(-as.numeric(colnames(election_table)))[1:2])
    election_table <-
      election_table %>%
      mutate(land = bundesland_name) %>%
      select(party, land, all_of(latest_election_years)) %>%
      pivot_longer(cols = all_of(latest_election_years),
                          names_to = "year")
    
    # REMOVE NA
    election_table <-
      election_table %>%
      na.omit()
    
    # RENAME PARTIES
    election_table <-
      election_table %>%
      mutate(
        party = case_when(
          grepl(paste(c("CDU", "CSU"), collapse = "|"), party, ignore.case = TRUE) ~ "CDU/CSU",
          grepl(paste(c(
            "GR\u00dcNE", "GRUENE"
          ), collapse = "|"), party, ignore.case = TRUE) ~ "GR\u00dcNE",
          grepl("SPD", party, ignore.case = TRUE) ~ "SPD",
          grepl("FDP", party, ignore.case = TRUE) ~ "FDP",
          grepl("BSW", party, ignore.case = TRUE) ~ "BSW",
          grepl("LINKE", party, ignore.case = TRUE) ~ "LINKE",
          grepl("AFD", party, ignore.case = TRUE) ~ "AfD",
          grepl("SONST", party, ignore.case = TRUE) ~ "Sonstige",
          TRUE ~ party
        )
      )
    
    # CALCULATE SHARE OF SEATS
    election_table <-
      election_table %>%
      group_by(year) %>%
      filter(value >= 0.05 & party != "Sonstige") %>%
      mutate(seat_share = round(value / sum(value), 4))
    
    # FILTER PARTIES
    election_table <-
      election_table %>%
      filter(party %in% parties()) %>%
      arrange(year, seat_share)
  })
}

#' Splits election results by year
#'
#' @param election_results list containing election results, each element is a
#' dataframe containing a column 'year'
#'
#' @return list with dataframes
split_election_results_by_year <- function(election_results) {
  unlist(lapply(election_results, function(dataframe) {
    split_dataframes <- split(dataframe, dataframe$year)
    list_of_dataframes <- lapply(split_dataframes, as.data.frame)
  }), recursive = FALSE)
}

get_election_result_urls <- function() {
  mainPage <- "https://www.wahlrecht.de/ergebnisse/"
  
  links <-
    mainPage %>%
    read_html() %>%
    html_nodes(., "td.l a") %>%
    html_attr("href") %>%
    grep(
      pattern = "..",
      x = .,
      value = TRUE,
      invert = TRUE,
      fixed = TRUE
    ) %>%
    grep(
      pattern = "europa",
      x = .,
      value = TRUE,
      invert = TRUE,
      fixed = TRUE
    )
  
  urls <- paste0(mainPage, links)
  
  if (length(urls) != 17)
    stop("incorrect number of urls")
  
  urls
}
