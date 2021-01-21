# 2021 German Federal Election Forecasting Model 

With the Bundestagswahl approaching in September 2021, INWT-Statistics Lab aims to build a comprehensive model that forecasts the German federal election (Bundestagswahl) outcomes. Here we provide the code for our dynamic multilevel Bayesian model for our predictions written in R and Stan. We believe that our model brings these advantages:
 - Our model could be better than depending solely on polls in predicting the outcome of the election.
 - Make an accurate statement about the uncertainty of our forecasts. 
 - Optimize prediction through expert opinions, the model can provide information about the likelihood of government coalitions or government participation of individual parties. 



# How does the model work?

### The Bayesian State-Space-Modell provides forecast with uncertainties through these steps: 
1. Simulating a large number of possible outcomes of the Bundestag election
2. Sources of uncertainty that are taken into account in the model:
 >>- Future events 
 >>- Sampling errors due to the limited number of people surveyed per survey 
 >>- Considers the distortion of election polls due to inaccurate mapping of the electoral population (representativeness), non-response or incorrect information from survey participants
3. Maintain predictive accuracy by:
  >> - Surveying results are adjusted for “house effects” of the surveying institutes
  >> - Modeling a long and short-term memory of the electorate potentials
  >> - Considering ‘opposition’ or government status of the parties (opposition parties tend to gain, government parties tend to lose)


# Defining uncertainty: categorized sources of uncertainty in forecasting vote share into two types:
### 1. Uncertainty about the future events, i.e. shocks to vote share
### 2. Uncertainty in polling: 
>> - common bias of all pollsters for a specific party
>> - house bias of a specific pollster for a specific party
>> - polling uncertainty of a specific pollster


# 2017 Model Description: 
### Marcus Groß’s in 2017 built a state-space model for the federal Bundestag election, which is a common choice for modelling voting intentions using poll data. We foresee these advantages are what make our model unique:

>> - The model is different from conventional polls: not only a forecast for a hypothetical election next Sunday, but also for the election itself.
>> - Through multiple simulations of the Bundestag election results, realistic uncertainties for our forecasts can be quantified.
>> - We can provide more realistic forecasts for certain events relating to the outcome of the election, such as the participation of individual parliamentary groups in government.
>> - Model Performance: the forecasting quality of our model is about 10-15% better than a simple averaging of the surveys of the past week (checked by backtesting at the last elections)


## For more detailed coverage of our 2017 forecast, please refer to these articles on the Die Welt, the New York Times, and Sueddeutsche. 

# 2021 Model Description: 
### Improving on our 2017 Bundestagswahl forecasting, our updated new model this year uses a Long-short term memory state-space model for election forecasting (Groß, 2019). 
>> - Instead of several individual models for different aspects, this year we have a holistic, self-contained model
>> - Faster response to short-term trends in voter sentiment
>> - Improved modeling of medium and long-term trends rendering now more meaningful interpretation
>> - Consideration of government status (opposition / government) as an influencing factor in future events
More precise modeling of house effects of the institutes and interactions between parties


# DATA

## Our model uses three different types of input data

### 1) Polling data: the data amounts to more than 4,000 polls from eight different pollsters between November 1st, 1994, through the current date for the German federal election ("Bundestagswahl"). We scrape this data from www.wahlrecht.de, which collects all available polling data and is frequently updated.

### 2) Election outcome data: the model uses the respective parties forming the government and opposition are available for all elections since 1998. For a given election result usually multiple coalitions are conceivable and the government is formed independent from the voters. To answer questions like “How large is the probability that the government consists of a coalition of parties A and B”, “What is the probability that person X is going to be chancellor / prime minister” or “How likely is it that party C is part of the government”, the incorporation of an expert poll is proposed. Data is available for the six large parties: CDU/CSU, SPD, Die Grünen, FDP, Die Linke and AfD.

### 3) Expert interviews: the model combines the results with expert interviews, gathering expert opinion regarding coalition preferences.  Experts are defined as people associated and familiar with politics, e.g. active in politics, working for a party or a politics-related institution or having an academic degree in political science. A list of potential coalitions was given to the experts, and the experts are given the task to rank these coalitions under the premise to do this independently to the potential election result. Up to date [21.01.2010], the model considers 16 interview responses with 12 rankings, see list here. The interview process provides critical priors for our Bayesian workflow. For more detailed methodology, please read the methods section.

# Model Performance: 
A forecast can never – and should never – claim to be prophetic. Forecasts make statements about future conditions, and are therefore invariably coupled with uncertainty. 


# lsTerm-election-forecast
A long-short term event memory state-space model for multi-party elections

![Poster](Poster/190820_Poster_StanCon_2019.jpg)
