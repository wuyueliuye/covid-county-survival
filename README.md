# covid-county-survival
County level survival analysis on covid-19 rate

##### Study Design

Covid-19 has been lasted for more than 1 years, which has been impacting our society in many perspectives. As time goes by, the case rate has been rising up quite fast. There're a lot of factors associated with it, like population, unemployment rate, elderly people rate and mask use rate. In this project, I'm interested in exploring the correlation between these factors and the covid case rate in each county in the US, in terms of event and time using survival methods.

The study designations are as follows:

subject: each county in the US

event: reach a 10% the case rate

start: the 1st case confirmed date in each county

end: either the event happen date or the last day from the dataset (02-01-2021) for each county

time: date difference between end and start for each county

status: either event happened (1) or still censoring (0)

covariates: interest factors like population, unemployment, elderly people rate,education, mask use

##### Variables

case rate: the confirmed covid cases comparing with the population in each county

fips: unique code for each county in the US

time: number of days since the 1st case confirmed date for each county

status: either event happened or not accomplishing with the time

65+ rate: the rate of people aged 65 or older 

bachelor+ rate: the rate of people with bachelor or higher degrees

unemployment rate: the percentage of people unemployed

mask rate: the rate of people frequently or always wearing masks in public places

population: population in each county by thousand

#### Methodology

##### Exploratory Analysis

- Summary Statistics: 5 statistics of case rate at 02-01-2021
- Histograms and Boxplots: histogram boxplots visualizing the distributions of covariates
- Pairwise Scatterplot: comparisons between covariates
- Multicollinearity Checking: VIFs for the covariates

##### Statistical Analysis

Survival Analysis 

- Cox Proportional Hazard Model: protective effects in terms of the factors considered 
- Model Diagnosis: proportional hazard assumption, outlier checking, nonlinearity checking

![image](https://user-images.githubusercontent.com/47954276/119771047-ae0c4e00-be82-11eb-8f2c-7d0bd43be242.png)
Wearing masks have protective effect on serious covid case rate

#### References

[https://stats.idre.ucla.edu/sas/seminars/sas-survival/](https://nam11.safelinks.protection.outlook.com/?url=https%3A%2F%2Fstats.idre.ucla.edu%2Fsas%2Fseminars%2Fsas-survival%2F&data=04|01|Z1835441%40students.niu.edu|22d3109e516c47a92e3008d8d1f3957d|ea8733908c1c4231a7996b5a0235b2e6|0|0|637490191392757349|Unknown|TWFpbGZsb3d8eyJWIjoiMC4wLjAwMDAiLCJQIjoiV2luMzIiLCJBTiI6Ik1haWwiLCJXVCI6Mn0%3D|1000&sdata=ZutHxOWN0fhEU1TyZQfOEAiu2yI0vHKXOyGEbnuX%2F6s%3D&reserved=0)

https://rviews.rstudio.com/2017/09/25/survival-analysis-with-r/

http://www.sthda.com/english/wiki/cox-model-assumptions

https://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/BS704_Survival/BS704_Survival6.html
