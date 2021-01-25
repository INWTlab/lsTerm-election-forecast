2021 German Federal Election Forecasting Model
===============================================

 With the German federal election (_Bundestagswahl_) approaching in September 2021, [INWT-Statistics](https://www.inwt-statistics.com/home.html) aims to build a comprehensive model that forecasts the _Bundestagswahl_ outcomes. 
 
 This repository provides the code of our election forecast predictions published on **[www.wer-gewinnt-die-wahl.de](https://www.wer-gewinnt-die-wahl.de)** using a dynamic multilevel Bayesian model written in _R_ and _Stan_.

### Model

Improving on our [2017 Bundestagswahl forecasting](https://www.inwt-statistics.com/read-blog/forecast-2017-bundestag-election.html), our model this year uses a _Long-short term memory state-space model for election forecasting_ ([Groß, 2019](https://zenodo.org/record/3697270)). The model simulates election outcomes by using poll, election and government data, applies a long-short-term voter memory state-space model and accounts for different sources of uncertainties. 

For press coverage of our 2017 forecast, please refer to these articles on [_Die Welt_](https://www.welt.de/politik/deutschland/article163306730/Diese-Spezial-Analyse-sieht-einen-klaren-Wahlsieger-in-Deutschland.html), the [_New York Times_](https://www.nytimes.com/2017/09/18/world/europe/germany-election-martin-schulz.html), and [Sueddeutsche](https://www.sueddeutsche.de/digital/wahlprognosen-der-naechste-bundeskanzler-wird-1.3584122)_._ 

State-space models are common choices for modelling voting intentions using poll data. For the 2021 Bundestagswahl, we are going beyond the random-walk approaches by introducing the _long-short term event memory effect method_. Because vote shares tend to reverse back to the party’s long-term trend after larger short-term movements, we hypothesize that events influencing the vote share can be decomposed into:

* a short-term effect due to media information spreading,
* a smaller remaining long-term effect e.g. new events and ‘forgetfulness’.

We also categorized sources of uncertainty in forecasting vote share into two types:

* uncertainty about the future events, i.e. shocks to vote share
* uncertainty in polling: 
    * common bias of all pollsters for a specific party
    * house bias of a specific pollster for a specific party 
    * polling uncertainty of a specific pollster

Our model

- outperforms poll averages by around 15 %,
- delivers an accurate statement about the uncertainty of our forecasts,
- extends forecasting to events that describe the likelihood of government coalitions or government participation of individual parties.

For a more detailed overview on model specifiations and performance take a look at [this](https://github.com/INWTlab/lsTerm-election-forecast/blob/master/Poster/190820_Poster_StanCon_2019a.pdf) Stan conference poster. 

### Data

Our model uses three different types of input data:

1. **Polling data:** the data amounts to more than 4,000 polls from eight different pollsters between November 1st, 1994, through the current date for the German federal election (&quot;Bundestagswahl&quot;). We scrape this data from [www.wahlrecht.de](https://www.wahlrecht.de/umfragen/index.htm), which collects all available polling data and is frequently updated.
![](RackMultipart20210122-4-evjy0c_html_32fd1d9d6396a093.png)

2. **Election outcome data**: the model considers data [election results](https://github.com/INWTlab/lsTerm-election-forecast/blob/master/data/Elections.csv) and [government / opposition status](https://github.com/INWTlab/lsTerm-election-forecast/blob/master/data/Government_read.csv) of the six large parties for all German federal elections since 1998. For a given election result usually multiple coalitions are conceivable and the government is formed independent from the voters.
![](RackMultipart20210122-4-evjy0c_html_6414da996d33e1aa.png)

3. **Expert interviews**: the model also makes use of expert interviews, that reflect expert opinions regarding coalition preferences of the parties. Experts are defined as political scientists or people working for a party / a party affiliated foundation. A list of potential coalitions was given to the experts, then the experts ranked these coalitions under the premise of independence from potential election results or actual polling. Up to date, the model considers [16 interview responses with 12 rankings](https://github.com/INWTlab/lsTerm-election-forecast/blob/master/data/Koalitionen_read.csv). This data also provides critical priors for our Bayesian workflow. For more detailed methodology, please read the [methods](https://github.com/INWTlab/lsTerm-election-forecast/blob/master/Notebook/notebook.pdf) section.


### How to run the model

`dataDE <- loadDataDE(predDate)` 
> `returns` list of poll data, elections data, and coalition data 
- `predDate`:: the date of running the model e.g. as.Date(&quot;yyyy-mm-dd&quot;)

`dataPrep <- preparePollData(dataDE$pollData, dataDE$Elections, predDate)`
> `returns` a list of cleaned data sets ready for modeling
- `dataDE$pollData` :: contains the imported polls data from the directory
- `dataDE$Elections` :: contains the historical German elections results data
- `predDate` :: date of running the model

`modelResults <- compileRunModel(dataPrep$modelData)`
> `returns` a list of stan models after sampling
- `dataPrep$modelData` :: formatted cleaned data input resulted from previous `preparePollData` function

`plotForecast <- plotElectionData(modelResults, dataPrep, predDate, dataDE$pollData, start = "2016-01-01")`
> `Returns` list of _ggplot_ graph, and JSON file output 
- `modelResults` :: a list of model output of the function _compileRunModel()_
- `dataPrep` :: output of the function _preparePollData()_
- `predDate` :: date of running the model
- `dataDE$pollData` :: contains the imported polls data from the directory
- `start` :: date format &quot;yyyy-mm-dd&quot;

`fact_forecast <- getForecastTable(modelResults, dataPrep, predDate)`
> `Returns` a table that contains each political party prediction forecasts
- `modelResults`: a list of model output of the function _compileRunModel()_
- `dataPrep`:: list of cleaned data sets ready for modeling from _preparePollData()_
- `predDate`:: date of running the model, as.Date(&quot;yyyy-mm-dd&quot;)

`fact_event_prob <- eventsDE(modelResults, dataPrep, predDate)`
> `Returns` a dataframe of events taking in consideration election forecasts
- `modelResults`: a list of model output of the function _compileRunModel()_
- `dataPrep`:: list of cleaned data sets ready for modeling from _preparePollData()_
- `predDate`:: date of running the model, as.Date(&quot;yyyy-mm-dd&quot;)

fact_coalition_prob <- koalitionDE(dataDE$Koalitionen, modelResults, dataPrep, predDate)
> `Returns` dataframe of political party coalitions possible estimates 
- `dataDE$Koalitionen`:: dataframe from function _loadDataDE(predDate)_
- `modelResults`:: a list of model output of the function _compileRunModel()_
- `dataPrep`:: list of cleaned data sets ready for modeling from _preparePollData()_
- `predDate`:: date of running the model, as.Date(&quot;yyyy-mm-dd&quot;)

fact_part_of_government <- partOfGovernmentDE(fact_coalition_prob, predDate)
> `Returns` dataframe of sum estimates of the different possibilities of coalitions
- `fact_coalition_prob` :: dataframe that contains party coalition estimates
- `predDate` :: date of running the model, as.Date(&quot;yyyy-mm-dd&quot;)

A more detailed tutorial on how to run the model can be found [here](https://github.com/INWTlab/lsTerm-election-forecast/blob/master/Notebook/notebook.pdf).

# Licence 

[CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/legalcode)
