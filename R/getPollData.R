###------------------------------------------ GET DATA ------------------------------------------###
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
  
  # there are different tables (and urls) for different time periods
  # task: get urls for each time period available from each institute
  # current data can be found in the table retrieved from 'urls', i.e.http://www.wahlrecht.de/umfragen/allensbach.htm
  # non-current tables contain the (latest) year in their url, i.e. http://www.wahlrecht.de/umfragen/allensbach/2002.htm
  generateTableUrls <- function(x) {
    allUrls <- x %>%
      read_html %>%
      html_nodes(., "p.navi a") %>%
      xml_attr("href") %>%
      # control for the case where there is only one table for an institute (i.e. insa)
      {
        ifelse(length(.) > 0, return(c(x, paste0(mainPage, .))), return(x))
      }
    
    # exclude urls refering to tables we do not need
    paste(c("west", "ost", "stimmung"), collapse = "|") %>%
      grep(., allUrls) %>%
      {
        ifelse(length(.) > 0, return(allUrls[-c(.)]), return(allUrls))
      }
    
  }
  
  completeUrlList <- urls %>% lapply(generateTableUrls)
  names(completeUrlList) <- institutes
  
  # read all tables into list 'raw'
  # raw is then a list containing for each institute another list
  # with a data.frame for each time period
  # due to website structure every second element in the second level list is NULL
  raw <- completeUrlList %>%
    sapply(function(x)
      sapply(x, readHTMLTable))
  
  
  ###------------------------------------------ PREPARE FOR THE MERGE ------------------------------------------###
  ##
  # colnames of table http://www.wahlrecht.de/umfragen/politbarometer/politbarometer-1998.htm
  # are V1, V2, V3... causing problems
  colnames(raw[[4]][[4]]) <-
    c(
      'Datum',
      colnames(raw[[4]][[2]])[2],
      'CDU/CSU',
      'SPD',
      colnames(raw[[4]][[2]])[5],
      'FDP',
      'PDS',
      'Sonstige'
    )
  
  # every second list is empty, filter them
  data <- vector('list', length(raw))
  
  for (i in seq_along(raw)) {
    data[[i]] <-
      raw[[i]][sapply(raw[i], function(x)
        lapply(x, length)) > 0]
  }
  
  
  # in some tables the 'Datum' column has no column name
  # this causes problems merging the tables
  # here every 'Datum' column is named 'Datum'to avoid this problem
  for (i in 1:length(data)) {
    for (j in 1:length(data[[i]]))
      colnames(data[[i]][[j]])[1] <- 'Datum'
  }
  
  
  # create one data.frame for each institue
  # warnings occur because factors are coerced to characters
  # result is a list with 7 data.frames
  # each data.frame contains all data from an institute
  data <- data %>%
    lapply(data.table::rbindlist, fill = T)
  
  # provide every data.frame with a column that refers to the institute
  # so the polls can be distinguished in the final data set
  for (i in 1:length(data)) {
    data[[i]] <- cbind(data[[i]], Institut = c(institutes[i]))
  }
  
  # now combine the elements of 'data' to one data.frame called 'wahlProg'
  wahlProg <- data %>% bind_rows
  
  
  ###------------------------------------------ CLEANING ------------------------------------------###
  ##
  # solve encoding issue in header
  Encoding(names(wahlProg)) <- 'UTF-8'
  
  # remove row(s) where 'Bundestagswahl' occurs
  omit <- c(
    which(wahlProg == 'Bundestagswahl', arr.ind = TRUE)[, 1],
    which(wahlProg == 'Wahl 1998', arr.ind = TRUE)[, 1]
  )
  wahlProg %<>%
    filter(!row_number() %in% omit)
  
  # set each party's column to numeric and divide by 100
  wahlProg %<>%
    mutate_each(funs(gsub(" %", "", .))) %>%
    mutate_each(funs(gsub(",", ".", .))) %>%
    mutate(`CDU/CSU` = as.numeric(`CDU/CSU`) / 100) %>%
    mutate(SPD = as.numeric(SPD) / 100) %>%
    mutate(GRÜNE = as.numeric(GRÜNE) / 100) %>%
    mutate(FDP = as.numeric(FDP) / 100) %>%
    mutate(LINKE = as.numeric(LINKE) / 100) %>%
    mutate(AfD = as.numeric(AfD) / 100) %>%
    mutate(Sonstige = as.numeric(Sonstige) / 100) %>%
    mutate(PDS = as.numeric(PDS) / 100) %>%
    mutate(REP = as.numeric(REP) / 100) %>%
    mutate(Linke.PDS = as.numeric(Linke.PDS) / 100) %>%
    mutate(PIRATEN = as.numeric(PIRATEN) / 100) %>%
    mutate(Rechte = as.numeric(Rechte) / 100) %>%
    mutate(`Nichtwähler/Unentschl.` = as.numeric(`Nichtwähler/Unentschl.`) /
             100) %>%
    mutate(FW = as.numeric(FW) / 100) %>%
    mutate(`REP/DVU` = as.numeric(`REP/DVU`) / 100)
  
  # the column 'Befragte' includes lots of non-numeric chars like '~', '?', '.', ...
  wahlProg %<>%
    mutate(Befragte = as.numeric(gsub("\\D", "", Befragte)))
  
  
  
  # Datum
  wahlProg$Datum <- gsub('[*]', '', wahlProg$Datum)
  wahlProg$Datum %<>% as.Date(wahlProg$Datum, format = "%d.%m.%Y")
  FW <- wahlProg$Institut %>% table %>% .[4] %>% names
  
  wahlProg[is.na(wahlProg$Datum) == TRUE &
             wahlProg$Institut == FW, ]$Datum <-
    rev(c(
      seq(as.Date("1994/11/1"), as.Date("1995/07/1"), by = "month"),
      seq(as.Date("1995/09/1"), as.Date("1996/06/1"), by = "month"),
      seq(as.Date("1996/08/1"), as.Date("1997/06/1"), by = "month"),
      seq(as.Date("1997/08/1"), as.Date("1997/12/1"), by = "month")
    ))
  
  wahlProg[is.na(wahlProg$Datum) == TRUE &
             wahlProg$Institut == 'GMS', ]$Datum <- as.Date('2002-12-01')
  wahlProg[is.na(wahlProg$Datum) == TRUE &
             wahlProg$Institut == 'Emnid' &
             wahlProg$`CDU/CSU` == 0.35 &
             wahlProg$SPD == 0.32 , ]$Datum <- as.Date('2005-12-01')
  
  # add pds to linke and sum irrelevant parties up to 'Sonstige'
  wahlProg %<>%
    mutate(
      LINKE = pmax(LINKE, Linke.PDS, PDS, na.rm = TRUE),
      Linke.PDS = 0,
      PDS = 0,
      Sonstige = rowSums(.[c(12:15, 17, 21:22)], na.rm = T)
    )
  
  # select relevant variables
  wahlProg %<>%
    dplyr::select(Institut,
                  Datum,
                  `CDU/CSU`,
                  SPD,
                  GRÜNE,
                  FDP,
                  LINKE,
                  AfD,
                  Sonstige,
                  Befragte)
  
  return(wahlProg[wahlProg$Datum <= dateMax,])
}
