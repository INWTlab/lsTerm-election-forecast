library(dbtools)
library(lsTermElectionForecast)
library(cmdstanr)
library(lubridate)
library(dplyr)

dev <- FALSE
predDate <- Sys.Date()


# Print session information ----

cat("System information:\n")
for (i in seq_along(sysinfo <- Sys.info()))
  cat(" ", names(sysinfo)[i], ":", sysinfo[i], "\n")
sessionInfo()


# Prepare credentials for database access ----

# Create creds.json locally by writing username and password from bitwarden into
# creds_template.json.
creds <- rjson::fromJSON(file = "creds.json")
cred <- Credentials(
  drv = MySQL,
  username = creds$username,
  password = creds$password,
  dbname = "election_forecast",
  host = creds$host,
  port = 3306,
  client.flag = CLIENT_SSL
)


# Get and prepare data ----

dataDE <- getDataDE(predDate)
fact_survey <- getFactSurvey(dataDE$pollData)

latestPolls <- latestPollPerPollster(fact_survey)
latestPollsDb <- getLatestPollsDb(cred)
stopIfNewPollster(latestPolls$Institut, latestPollsDb$Institut)

cat("Date of latest poll per pollster:\n")
print(latestPolls)

newPolls <- checkForNewPolls(latestPolls, latestPollsDb)
checkIfPollDataChanged(fact_survey, cred)

if (!newPolls) {
  
  cat("No new polls available on www.wahlrecht.de. Model won't be retrained.")
  
} else {
  
  dataPrep <- preparePollData(dataDE$pollData,
                              dataDE$Elections,
                              predDate,
                              minDate = predDate - 365 * 20.1)
  
  
  # Estimate models ----
  
  modelResults <- compileRunModel(data = dataPrep$modelData, dev = dev)
  plotForecast <- plotElectionData(modelResults, dataPrep, predDate,
                                   dataDE$pollData, start = "2020-01-01")
  print(plotForecast$plot)
  fact_forecast <- prepareForecastTable(modelResults, dataPrep, predDate)
  fact_event_prob <- eventsDE(modelResults, dataPrep, predDate)
  fact_coalition_prob <- calculate_coalition_probs(modelResults, dataPrep, predDate)
  fact_part_of_government <- partOfGovernmentDE(fact_coalition_prob, predDate)
  fact_forecast_all <- getForecastAllTable(plotForecast$plotData)
  fact_coalition_prob$possible_coalition <- NULL
  
  
  # Write results to database ----
  
  if (!dev) {
    sendData(db = cred, data = fact_coalition_prob, mode = "update")
    sendData(db = cred, data = fact_part_of_government, mode = "update")
    sendData(db = cred, data = fact_event_prob, mode = "update")
    sendData(db = cred, data = fact_forecast, mode = "update")
    # There is a bug ignoring the mode in some cases, so we truncate
    # fact_forecast_all manually first:
    sendQuery(db = cred, query = "truncate table fact_forecast_all;")
    sendData(db = cred, data = fact_forecast_all, mode = "insert")
    sendData(db = cred, data = fact_survey, mode = "update")
  }
}
