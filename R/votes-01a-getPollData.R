### -------- GET DATA -------- ###
##
#
getPollData <- function(dateMax = Sys.Date()) {
  mainPage <- "http://www.wahlrecht.de/umfragen/"
  
  # get name of each institute from the table at the mainpage
  institutes <- mainPage %>%
    read_html %>%
    html_nodes(., "th.in a") %>%
    html_text
  
  # get link to each institute's page from the table at the mainpage
  links <- mainPage %>%
    read_html %>%
    html_nodes(., "th.in a") %>%
    xml_attr("href")
  
  # combine 'mainPage' and 'links' to valid urls
  urls <- paste0(mainPage, links)
  completeUrlList <-
    urls %>% lapply(generateTableUrls, mainPage = mainPage)
  names(completeUrlList) <- institutes
  
  # read all tables into list 'crawled_tables_list'
  # crawled_tables_list is then a list containing for each institute another list
  # with a data.frame for each time period
  # due to website structure every second element in the second level list is NULL
  crawled_tables_list <- completeUrlList %>%
    sapply(function(institute_entry)
      sapply(institute_entry, function(url)
        readHTMLTable(rawToChar(GET(
          url
        )$content))))
  
  ### -------- PREPARE FOR THE MERGE -------- ###
  ##
  # Crawled colnames of table
  # http://www.wahlrecht.de/umfragen/politbarometer/politbarometer-1998.htm (Forsch’gr.Wahlen)
  # are V1, V2, V3... causing problems
  # So we set them manually, taking some of them from a year for which we have
  # meaningful names in the data
  forschGrWahlenIdx <-
    which(grepl("Forsch", names(crawled_tables_list)))
  colnames(crawled_tables_list[[forschGrWahlenIdx]][[4]]) <-
    c('Datum',
      colnames(crawled_tables_list[[forschGrWahlenIdx]][[2]])[2:6],
      'PDS',
      'Sonstige')
  
  # every second list is empty, filter them
  data <- vector('list', length(crawled_tables_list))
  
  for (i in seq_along(crawled_tables_list)) {
    data[[i]] <-
      crawled_tables_list[[i]][sapply(crawled_tables_list[i], function(x)
        lapply(x, length)) > 0]
  }
  
  # in some tables the 'Datum' column has no column name
  # this causes problems merging the tables
  # here every 'Datum' column is named 'Datum' to avoid this problem
  for (institute_idx in 1:length(data)) {
    for (year_idx in 1:length(data[[institute_idx]]))
      colnames(data[[institute_idx]][[year_idx]])[1] <- 'Datum'
  }
  
  # create one data.frame for each institute
  # warnings occur because factors are coerced to characters
  # result is a list with 7 data.frames
  # each data.frame contains all data from an institute
  data <- data %>%
    lapply(rbindlist, fill = T)
  
  # provide every data.frame with a column that refers to the institute
  # so the polls can be distinguished in the final data set
  for (institute_idx in 1:length(data)) {
    data[[institute_idx]] <-
      cbind(data[[institute_idx]], Institut = c(institutes[institute_idx]))
  }
  
  # now combine the elements of 'data' to one data.frame called 'wahlProg'
  wahlProg <- rbindlist(data, fill = TRUE)
  
  ### -------- CLEANING -------- ###
  ##
  # solve encoding issue in header
  Encoding(names(wahlProg)) <- 'UTF-8'
  
  # remove row(s) where 'Bundestagswahl' occurs
  omit <- c(
    which(wahlProg == 'Bundestagswahl', arr.ind = TRUE)[, "row"],
    which(wahlProg == 'Wahl 1998', arr.ind = TRUE)[, "row"]
  )

  wahlProg <- wahlProg[-omit, ]
  wahlProg <-
    as.data.frame(wahlProg)[, which(!duplicated(colnames(wahlProg)))]
  
  # BSW sometimes doesn't have a separate column but is in the "Sonstige" column
  posBswBroken <- grepl("BSW", wahlProg$Sonstige)
  replacement <- ex_between(wahlProg[posBswBroken, "Sonstige"], "BSW ", " %")
  wahlProg[posBswBroken, "BSW"] <- unlist(replacement)
  
  # replace entries in "Sonstige" that start with letters by NAs; set each
  # party's column to numeric and divide by 100
  wahlProg <- wahlProg %>%
    mutate(
      Sonstige = ifelse(grepl("^[A-Za-z]", Sonstige), NA, Sonstige),
      across(.cols = everything(), .fns = ~ gsub("%", "", .)),
      across(.cols = everything(), .fns = ~ gsub(",", ".", .)),
      across(.cols = everything(), .fns = ~ gsub("\u2013", NA, .)),
      across(.cols = everything(), .fns = ~ gsub("\\?", NA, .))
    ) %>%
    # remove entries that contain a range instead of a specific percentage
    # It suffices to use the CDU/CSU column for this filter
    filter(
      !grepl("-", `CDU/CSU`), !grepl("\u2013", `CDU/CSU`),
      # Remove a row that contains text
      !grepl("Zur telefonischen Umfrage", `CDU/CSU`)
    ) %>%
    mutate(
      `CDU/CSU` = asNumericWithNA(`CDU/CSU`) / 100,
      SPD = asNumericWithNA(SPD) / 100,
      'GR\u00dcNE' = asNumericWithNA(!!as.symbol("GR\u00dcNE")) / 100,
      FDP = asNumericWithNA(FDP) / 100,
      LINKE = asNumericWithNA(LINKE) / 100,
      BSW = asNumericWithNA(BSW) / 100,
      AfD = asNumericWithNA(AfD) / 100,
      Sonstige = asNumericWithNA(Sonstige) / 100,
      PDS = asNumericWithNA(PDS) / 100,
      REP = asNumericWithNA(REP) / 100,
      Linke.PDS = asNumericWithNA(Linke.PDS) / 100,
      PIRATEN = asNumericWithNA(PIRATEN) / 100,
      Rechte = asNumericWithNA(Rechte) / 100,
      FW = asNumericWithNA(FW) / 100,
      `REP/DVU` = asNumericWithNA(`REP/DVU`) / 100
    )
  
  # the column 'Befragte' includes lots of non-numeric chars like '~', '?', '.', ...
  wahlProg <- wahlProg %>%
    mutate(Befragte = as.numeric(gsub("\\D", "", Befragte)))
  
  # Datum
  wahlProg$Datum <- gsub('[*]', '', wahlProg$Datum)
  wahlProg$Datum <-
    wahlProg$Datum %>% as.Date(wahlProg$Datum, format = "%d.%m.%Y")
  
  # add PDS to Linke and sum irrelevant parties up to 'Sonstige'
  wahlProg <- wahlProg %>%
    mutate(
      LINKE = pmax(LINKE, Linke.PDS, PDS, na.rm = TRUE),
      Linke.PDS = 0,
      PDS = 0
    )
  
  # select relevant variables
  wahlProg <- wahlProg %>%
    select(Institut,
           Datum,
           `CDU/CSU`,
           SPD,
           "GR\u00dcNE",
           FDP,
           LINKE,
           AfD,
           BSW,
           Befragte)
  wahlProg <-
    wahlProg %>% filter(!is.na(Datum), !is.na(Institut), !is.na(SPD))
  
  wahlProg$Sonstige <-
    1 - rowSums(wahlProg[, which(names(wahlProg) %in% parties())],
                na.rm = TRUE)
  
  return(wahlProg[wahlProg$Datum <= dateMax, ])
}

generateTableUrls <- function(institute_url, mainPage) {
  # there are different tables (and urls) for different time periods
  # task: get urls for each time period available from each institute
  # current data can be found in the table retrieved from 'urls', i.e.
  # http://www.wahlrecht.de/umfragen/allensbach.htm
  #
  # non-current tables contain the (latest) year in their url, i.e.
  # http://www.wahlrecht.de/umfragen/allensbach/2002.htm
  allUrls <- institute_url %>%
    read_html %>%
    html_nodes(., "p.navi a") %>%
    xml_attr("href")
  if (length(allUrls) > 0) {
    allUrls <- c(institute_url, paste0(mainPage, allUrls))
  } else {
    allUrls <- institute_url
  }
  # exclude urls refering to tables we do not need
  allUrlsStripped <-
    grep(paste(c("west", "ost", "stimmung"), collapse = "|"), allUrls)
  if (length(allUrlsStripped) > 0) {
    allUrls <- allUrls[-c(allUrlsStripped)]
  }
  return(allUrls)
}

asNumericWithNA <- function(vec) {
  # Transform a vector containing to numeric that contains
  # - numbers stored as character
  # - NAs
  
  # Create a results vector of the correct length, still containing only NAs
  result_vec <- rep(NA_real_, length(vec))
  # Replace all entries of the result vector that have values in the original vector with numbers
  naPositions <- is.na(vec)
  result_vec[!naPositions] <- as.numeric(vec[!naPositions])
  result_vec
}


#' Print date of latest poll per pollster
#'
#' @param fact_survey data.frame with columns "Institut" and "Datum"
#' 
#' @export
printLatestPollPerPollster <- function(fact_survey) {
  cat("Date of latest poll per pollster:\n")
  fact_survey %>%
    group_by(Institut) %>%
    summarise(latestPoll = max(Datum)) %>%
    arrange(desc(latestPoll)) %>%
    print()
}
