2021 German Federal Election Forecasting Model
===============================================

 With the German federal election (_Bundestagswahl_) approaching in September 2025, [INWT-Statistics](https://www.inwt-statistics.com) aims to build a comprehensive model that forecasts the _Bundestagswahl_ results. 
 
 This repository provides the code of our election forecast predictions published on **[www.wer-gewinnt-die-wahl.de](https://www.wer-gewinnt-die-wahl.de)** using a dynamic multilevel Bayesian model written in _R_ and _Stan_.

# Model

Improving on our [2017 and 2021 Bundestagswahl forecasting](https://www.inwt-statistics.com/read-blog/forecast-2017-bundestag-election.html), our model this year uses a _Long-short term memory state-space model for election forecasting_ ([Groß, 2019](https://zenodo.org/record/3697270)). The model simulates election outcomes by using poll, election and government data, applies a long-short-term voter memory state-space model and accounts for different sources of uncertainties. 

For press coverage of our 2017 forecast, please refer to these articles on [_Die Welt_](https://www.welt.de/politik/deutschland/article163306730/Diese-Spezial-Analyse-sieht-einen-klaren-Wahlsieger-in-Deutschland.html), the [_New York Times_](https://www.nytimes.com/2017/09/18/world/europe/germany-election-martin-schulz.html), and [Sueddeutsche](https://www.sueddeutsche.de/digital/wahlprognosen-der-naechste-bundeskanzler-wird-1.3584122)_._ 

State-space models are common choices for modelling voting intentions using poll data. For the 2025 Bundestagswahl, we are going beyond the random-walk approaches by introducing a _long-short term event memory effect approach_. Because vote shares tend to reverse back to the party’s long-term trend after larger short-term movements, we hypothesize that events influencing the vote share can be decomposed into:

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

A detailed description of model specifications and performance can be found [here](https://github.com/INWTlab/lsTerm-election-forecast/blob/master/inst/Notebook/notebook.pdf), a presentation held at the BerlinBayes Meetup can be found [here](https://github.com/INWTlab/lsTerm-election-forecast/blob/master/inst/210701_BerlinBayes.pdf). For quick reference and overview, check out [this](https://github.com/INWTlab/lsTerm-election-forecast/blob/master/inst/Poster/190820_Poster_StanCon_2019a.pdf) poster presented at the Stan 2019 conference, where we first introduced our model. Please note that the poster refers to an older version of the model. Since then, we have added a few more features.

# Data

Our model uses three different types of input data:

1. **Polling data:** the data amounts to more than 4,000 polls from eight different pollsters between November 1st, 1994, through the current date for the German federal election (&quot;Bundestagswahl&quot;). We scrape this data from [www.wahlrecht.de](https://www.wahlrecht.de/umfragen/index.htm), which collects all available polling data and is frequently updated.
![](RackMultipart20210122-4-evjy0c_html_32fd1d9d6396a093.png)

2. **Election outcome data**: the model considers data [election results](https://github.com/INWTlab/lsTerm-election-forecast/blob/master/inst/prepared_data/Elections.csv) and [government / opposition status](https://github.com/INWTlab/lsTerm-election-forecast/blob/master/inst/prepared_data/governing_coalitions.csv) of the seven largest parties for all German federal elections since 1998. For a given election result usually multiple coalitions are conceivable and the government is formed independent from the voters.


# How to run the model

Install the package and all dependencies and run the [CMDStan installation](https://github.com/INWTlab/lsTerm-election-forecast/blob/master/inst/R_Scripts/install_CmdStan.R).

To train the model and make predictions, run [scriptMulti.R](https://github.com/INWTlab/lsTerm-election-forecast/blob/master/inst/R_Scripts/scriptMulti.R).

# Licence 

[CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/legalcode)
