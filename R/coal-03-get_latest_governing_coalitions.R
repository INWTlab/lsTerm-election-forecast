#' Load new coalition data from wikipedia
#'
#'@description
#' Loads the parties of the latest governing coalition for the federal states of
#' Germany from wikipedia.
#'
#'@return dataframe with variables land, governing_coalition, year
#'
#'@export
get_latest_gov_coalitions <- function() {
  governing_coalition_link <-
    "https://de.wikipedia.org/wiki/Landesregierung_(Deutschland)"
  
  governing_coalition_df <-
    governing_coalition_link %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[1]') %>%
    html_table(header = TRUE) %>%
    data.frame() %>%
    select(land = Land,
           governing_coalition = Beteiligte.Parteien,
           year = Bildung) %>%
    mutate(land = rename_bundesland(land),
           year = substr(year, nchar(year) - 3, nchar(year)))
  
  # RENAME PARTIES
  governing_coalition_df <-
    governing_coalition_df %>%
    mutate(governing_coalition = gsub(
      paste(c("CDU", "CSU"), collapse = "|"),
      "CDU/CSU",
      governing_coalition,
      ignore.case = TRUE
    )) %>%
    mutate(
      governing_coalition = gsub(
        paste(c("GR\u00dcNE", "GRUENE"), collapse = "|"),
        "GR\u00dcNE",
        governing_coalition,
        ignore.case = TRUE
      )
    ) %>%
    mutate(governing_coalition = gsub("SPD", "SPD", governing_coalition,
                                      ignore.case = TRUE)) %>%
    mutate(governing_coalition = gsub("FDP", "FDP", governing_coalition,
                                      ignore.case = TRUE)) %>%
    mutate(governing_coalition = gsub("LINKE", "LINKE", governing_coalition,
                                      ignore.case = TRUE)) %>%
    mutate(governing_coalition = gsub("AFD", "AfD", governing_coalition,
                                      ignore.case = TRUE))
}
