library("dbtools")
library(lsTermElectionForecast)

# Create creds.json locally by writing username and password from bitwarden into
# creds_template.json:
creds <- rjson::fromJSON(file = "creds.json")
cred <- dbtools::Credentials(
  drv = dbtools::MySQL,
  username = creds$username,
  password = creds$password,
  dbname = "election_forecast",
  host = creds$host,
  port = 3306
)

df <- dbtools::sendQuery(
  db = cred,
  "
select date_forecast, event_id, estimate, id, descr_german, descr_english
from fact_event_prob
inner join dim_event on fact_event_prob.event_id = dim_event.id
where id in (3, 5, 6, 7, 8, 10, 17, 20, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32)
and date_forecast >= '2024-04-25';"
)

df <- fillDateGapsPerEvent(df)

json <-
  rjson::toJSON(unname(split(df, seq_len(nrow(
    df
  )))), indent = 2)
write(json, "public/fact_event_prob.json")


df <- dbtools::sendQuery(
  db = cred,
  "
select
  *
from
  fact_coalition_prob
inner join dim_coalition on dim_coalition.id = fact_coalition_prob.coalition_id
  where date_forecast >= '2024-04-25';"
)

json <-
  rjson::toJSON(unname(split(df, seq_len(nrow(
    df
  )))), indent = 2)
write(json, "public/fact_coalition_prob.json")

df <- dbtools::sendQuery(
  db = cred,
  "
select
  *
from
  fact_part_of_government
inner join dim_party on dim_party.id = fact_part_of_government.party_id;"
)
# No date filter because the app shows only the latest prediction anyway

json <-
  rjson::toJSON(unname(split(df, seq_len(nrow(
    df
  )))), indent = 2)
write(json, "public/fact_part_of_government.json")

df <- dbtools::sendQuery(
  db = cred,
  "
select
  *
from
  fact_forecast
inner join dim_party on dim_party.id = fact_forecast.party_id
  where date_forecast >= '2024-04-25';"
)

json <-
  rjson::toJSON(unname(split(df, seq_len(nrow(
    df
  )))), indent = 2)
write(json, "public/fact_forecast.json")

df <- dbtools::sendQuery(
  db = cred,
  "
select
  chancellor,
  date_forecast,
  sum(estimate) as estimate
from
  election_forecast.fact_coalition_prob
inner join
  election_forecast.dim_coalition on dim_coalition.id = fact_coalition_prob.coalition_id
where date_forecast >= '2024-04-25'
group by
  chancellor, date_forecast;"
)
df <- subset(df, chancellor != "tba")
df$estimate <- pmin(df$estimate, 1)

json <-
  rjson::toJSON(unname(split(df, seq_len(nrow(
    df
  )))), indent = 2)
write(json, "public/fact_chancellor_prob.json")

df <- dbtools::sendQuery(db = cred,
                         "select * from fact_forecast_all;")

json <-
  rjson::toJSON(unname(split(df, seq_len(nrow(
    df
  )))), indent = 2)
write(json, "public/fact_forecast_all.json")

df <- dbtools::sendQuery(
  db = cred,
  "select Datum as date, Institut as institute, party_id, estimate from fact_survey
  where Datum >= '2019-01-01'
  and Institut is not null
  and Institut != '';"
)

json <-
  rjson::toJSON(unname(split(df, seq_len(nrow(
    df
  )))), indent = 2)
write(json, "public/fact_survey.json")


# write data from db into csvs for publication

df <- dbtools::sendQuery(
  db = cred,
  "SELECT ff.date_forecast, ff.type, ff.estimate, ff.lower_bound, ff.upper_bound,
  ff.date_last_update, dp.descr_english, dp.descr_german
    FROM election_forecast.fact_forecast ff
    LEFT JOIN election_forecast.dim_party dp ON ff.party_id = dp.id
    WHERE ff.date_forecast >= '2024-04-25';"
)

write.csv(df, "public/forecast.csv", row.names = FALSE)

df <- dbtools::sendQuery(
  db = cred,
  "SELECT fep.date_forecast, fep.estimate, de.descr_english, de.descr_german
    FROM election_forecast.fact_event_prob fep
    LEFT JOIN election_forecast.dim_event de ON fep.event_id = de.id
    WHERE fep.date_forecast >= '2024-04-25'
    AND id in (2, 3, 5, 6, 7, 8, 10, 13, 17, 19, 20, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32);"
)

write.csv(df, "public/event_prob.csv", row.names = FALSE)

df <- dbtools::sendQuery(
  db = cred,
  "SELECT fcp.date_forecast, fcp.estimate, dc.descr_english, dc.descr_german, dc.chancellor
    FROM election_forecast.fact_coalition_prob fcp
    LEFT JOIN election_forecast.dim_coalition dc ON fcp.coalition_id = dc.id
    WHERE fcp.date_forecast >= '2024-04-25';"
)

write.csv(df, "public/coalition_prob.csv", row.names = FALSE)

df <- dbtools::sendQuery(
  db = cred,
  "
select
  chancellor,
  date_forecast,
  round(sum(estimate), 4) as estimate
from
  election_forecast.fact_coalition_prob
inner join
  election_forecast.dim_coalition on dim_coalition.id = fact_coalition_prob.coalition_id
where date_forecast >= '2024-04-25'
group by
  chancellor, date_forecast;"
)

df <- subset(df, chancellor != "tba")
df$estimate <- pmin(df$estimate, 1)

write.csv(df, "public/chancellor_prob.csv", row.names = FALSE)

df <- dbtools::sendQuery(
  db = cred,
  "SELECT fpog.date_forecast, fpog.estimate, dp.descr_english, dp.descr_german
    FROM election_forecast.fact_part_of_government fpog
    LEFT JOIN election_forecast.dim_party dp ON fpog.party_id = dp.id
    WHERE fpog.date_forecast >= '2024-04-25';"
)

write.csv(df, "public/part_of_government.csv", row.names = FALSE)

data <- list(time = toString(Sys.time()))

json <- rjson::toJSON(data)
write(json, "public/lastUpdate.json")
