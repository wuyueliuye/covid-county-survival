library(reshape2)
library(survival)
library(survminer)
library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggplot2)
library(ggfortify)
library(gridExtra)
library(grid)
library(packHV)

df = read.csv('us_counties_covid-19.csv')
df1 <- read.csv('us_counties_covariates.csv')
df2 <- read.csv('mask-use-by-county.csv')

df$fips <- as.factor(df$fips)
df$date <- as.Date(df$date)

sum(is.na(df$fips))
df <- na.omit(df)

## case by fips & date
df_temp1 <- df%>%group_by(fips, date)%>%summarise(cases=cases, deaths=deaths)


df_mask <- df2
df_mask$mask_rate <- df_mask$FREQUENTLY+df_mask$ALWAYS
df_mask_temp <- df_mask[,c('COUNTYFP', 'mask_rate')]
df_pop_temp <- df1[,c('FIPS', 'POP_ESTIMATE_2018')]

df_case_pop <- merge(df, df_pop_temp, by.x = 'fips', by.y = 'FIPS')
# sum(is.na(df_case_pop$fips))  # check for missing


########### preliminary EDA ####
# ## case rate at 02-01-2021
# df_case_rate <- df_case_pop[df_case_pop$date==as.Date('2021-02-01'),c('fips', 'cases', 'POP_ESTIMATE_2018')]
# df_case_rate$rate <- df_case_rate$cases / df_case_rate$POP_ESTIMATE_2018
# head(df_case_rate)
# summary(df_case_rate$rate) # the case rate 5 numbers of statistics up to Feb 1st

# 
# # df_case_rate2 <- df_case_pop[df_case_pop$date==as.Date('2020-11-01'),c('fips', 'cases', 'POP_ESTIMATE_2018')]
# # df_case_rate2$rate <- df_case_rate2$cases / df_case_rate2$POP_ESTIMATE_2018
# # head(df_case_rate2)
# # summary(df_case_rate2$rate) # the case rate 5 numbers of statistics up to Nov 1st,2020
# # 
# # number of counties with status=1 up to 2020-02-01, 797 vs. 2338
# length(unique(df_case_pop$fips[df_case_pop$cases>0.1*df_case_pop$POP_ESTIMATE_2018]))
# length(unique(df_case_pop$fips))-length(unique(df_case_pop$fips[df_case_pop$cases>0.1*df_case_pop$POP_ESTIMATE_2018]))

########################################## original tracked data ########

df_event_track <- df_case_pop%>%group_by(fips)%>%
  summarise(t=min(date[cases>POP_ESTIMATE_2018*0.1]),t0=min(date), 
            t1=as.Date('2020-08-01'), t2=as.Date('2020-11-01'), t3=as.Date('2020-12-01'),t4=as.Date('2021-01-01'),t5=max(date))
df_event_track$e1 <-  ifelse(!is.finite(df_event_track$t), 0, 
                             ifelse(df_event_track$t1<df_event_track$t, 0, 1))
df_event_track$e2 <-  ifelse(!is.finite(df_event_track$t), 0, 
                             ifelse(df_event_track$t2<df_event_track$t, 0, 1))
df_event_track$e3 <-  ifelse(!is.finite(df_event_track$t), 0, 
                             ifelse(df_event_track$t3<df_event_track$t, 0, 1))
df_event_track$e4 <-  ifelse(!is.finite(df_event_track$t), 0, 
                             ifelse(df_event_track$t4<df_event_track$t, 0, 1))
df_event_track$e5 <-  ifelse(!is.finite(df_event_track$t), 0, 
                             ifelse(df_event_track$t5<df_event_track$t, 0, 1))
df_event_track$e5 <- ifelse(df_event_track$e4==1, NA, df_event_track$e5)
df_event_track$e4 <- ifelse(df_event_track$e3==1, NA, df_event_track$e4)
df_event_track$e3 <- ifelse(df_event_track$e2==1, NA, df_event_track$e3)
df_event_track$e2 <- ifelse(df_event_track$e1==1, NA, df_event_track$e2)


df_event_m1 <- melt(df_event_track, c('fips', 't', 't0', 't1', 't2', 't3', 't4', 't5'), 
                    variable.name = 'followup', value.name = 'event')
# sum(is.na(df_event_m1$event))
df_event_m1 <- df_event_m1[!is.na(df_event_m1$event),]
df_event_m1$days <- ifelse(df_event_m1$followup=='e1', df_event_m1$t1-df_event_m1$t0, 
                           ifelse(df_event_m1$followup=='e2', df_event_m1$t2-df_event_m1$t0,
                                  ifelse(df_event_m1$followup=='e3', df_event_m1$t3-df_event_m1$t0,
                                         ifelse(df_event_m1$followup=='e4', df_event_m1$t4-df_event_m1$t0,
                                                df_event_m1$t5-df_event_m1$t0))))
df_event_temp <- df_event_m1%>%group_by(fips)%>%summarise(time=days, status=event)
df_event_temp <- df_event_temp[df_event_temp$time>0, ]
# sum(is.finite(df_event_track$t))
# sum(df_event_temp$status==1) #check if equal
head(df_event_temp)
write.csv(df_event_temp, 'df_time_status.csv')

##################################################################### Merge the covariates
# df1$ICU.Beds

keys_covarites <- c('FIPS', 'POP_ESTIMATE_2018', 'Total_age65plus', 
                    'Unemployment_rate_2018', 'Percent.of.adults.with.a.bachelor.s.degree.or.higher.2014.18')
df_cov_temp <- df1[,keys_covarites]
names(df_cov_temp)[2:5] <- c('population', 'age65+', 'unemployment_rate', 'bachelor+_rate')
head(df_cov_temp)
df_cov_temp$`age65+_rate` <- df_cov_temp$`age65+`/df_cov_temp$population

df_cov_temp1 <- merge(df_cov_temp, df_mask_temp, by.x='FIPS', by.y = 'COUNTYFP')
head(df_cov_temp1)

df_event_cov_temp <- merge(df_event_temp, df_cov_temp1, by.x = 'fips', by.y = 'FIPS')
# df_event_cov_temp <- merge(df_event_cov_temp, df_mask_temp, by.x = 'fips', by.y = 'COUNTYFP')
View(df_event_cov_temp)

write.csv(df_event_cov_temp, 'covid_clean.csv', row.names = F)

pairs(df_cov_temp1[,-c(1, 3)]) # pairwise scatter plot
length(unique(df_event_cov_temp$fips)) #number of subjects



#######################################################################

#### --------------- revised event censoring data -------------- ####

df_event_censoring <- df_case_pop%>%group_by(fips)%>%
  summarise(t=min(date[cases>POP_ESTIMATE_2018*0.1]),t0=min(date),t1=max(date))
# head(df_event_censoring)
# sum(!is.finite(df_event_censoring$t))
df_event_censoring$time <- ifelse(is.finite(df_event_censoring$t),df_event_censoring$t-df_event_censoring$t0, df_event_censoring$t1-df_event_censoring$t0 )
df_event_censoring$status <- ifelse(is.finite(df_event_censoring$t), 1, 0)

keys_covarites <- c('FIPS', 'POP_ESTIMATE_2018', 'Total_age65plus', 
                    'Unemployment_rate_2018', 'Percent.of.adults.with.a.bachelor.s.degree.or.higher.2014.18')
df_cov_temp <- df1[,keys_covarites]
names(df_cov_temp)[2:5] <- c('population', 'age65+', 'unemployment_rate', 'bachelor+_rate')
# head(df_cov_temp)
df_cov_temp$`age65+_rate` <- df_cov_temp$`age65+`/df_cov_temp$population

df_cov_temp1 <- merge(df_cov_temp, df_mask_temp, by.x='FIPS', by.y = 'COUNTYFP')
# head(df_cov_temp1)

df_event_cov_temp <- merge(df_event_censoring, df_cov_temp1, by.x = 'fips', by.y = 'FIPS')
# head(df_event_cov_temp)
df_event_cov_temp <- df_event_cov_temp[,-c(2,3,4)]
df_event_cov_temp$population1 <- df_event_cov_temp$population/1000
df_event_cov <- df_event_cov_temp[,-c(4,5)]
head(df_event_cov)
write.csv(df_event_cov, 'covid_clean_revised.csv', row.names = F)

# pairs(df_cov_temp1[,-c(1, 3)]) # pairwise scatter plot
# length(unique(df_event_cov_temp$fips)) #number of subjects

#### ------------- EDA ---------------- ####
## Data summary
df_case_rate <- df_case_pop[df_case_pop$date==as.Date('2021-02-01'),c('fips', 'cases', 'POP_ESTIMATE_2018')]
df_case_rate$rate <- df_case_rate$cases / df_case_rate$POP_ESTIMATE_2018
# head(df_case_rate)
# hist(df_case_rate$rate)
packHV::hist_boxplot(df_case_rate$rate, col='lightblue', 
                     main = 'Histogram & Boxplot of Case Rate in 02/01/2021',
                     xlab = 'case rate')
summary(df_case_rate$rate) # the case rate 5 numbers of statistics up to Feb 1st

# #####
# h_r <- ggplot(aes(x=rate), data=df_case_rate)+
#   geom_histogram(col='red', fill='pink', alpha=0.2, xlab=NULL)
# 
# b_r <- ggplot(aes(x=rate), data=df_case_rate)+
#   stat_boxplot()+
#   geom_boxplot(outlier.color = 'red', col='red', fill='pink', alpha=0.2)
# # grid.draw(rbind(ggplotGrob(h_r), ggplotGrob(b_r), size='first'))
# ggarrange(h_r, b_r, heights = c(2, 0.5), ncol = 1, nrow = 2)

## histogram
# hist_boxplots for covariates, df_cov_temp1
# hist_boxplot(df_cov_temp1$population, col='lightblue',
#              main='hist_boxplot of population',
#              xlab = 'population')


## case rate at 02-01-2021
df_case_rate <- df_case_pop[df_case_pop$date==as.Date('2021-02-01'),c('fips', 'cases', 'POP_ESTIMATE_2018')]
df_case_rate$rate <- df_case_rate$cases / df_case_rate$POP_ESTIMATE_2018
head(df_case_rate)
hist_boxplot(df_case_rate$rate, col='lightblue', 
             main = 'hist_boxplot of case rate at 02-01-2021',
             xlab = 'case rate')
summary(df_case_rate$rate) # the case rate 5 numbers of statistics up to Feb 1st

hist_boxplot(covid_clean$population1, col='lightblue',
             main='hist_boxplot of population',
             xlab = 'population (k)')

hist_boxplot(df_cov_temp1$unemployment_rate, col='lightblue', 
             main='hist_boxplot of unemployment rate',
             xlab = 'unemployment %')

hist_boxplot(df_cov_temp1$`bachelor+_rate`, col='lightblue', 
             main='hist_boxplot of bachelor or higher degree rate',
             xlab = 'bachelor+ %')

hist_boxplot(df_cov_temp1$`age65+_rate`, col='lightblue', 
             main='hist_boxplot of age65+ rate',
             xlab = 'age65+ rate')

hist_boxplot(df_cov_temp1$mask_rate, col='lightblue', 
             main='hist_boxplot of mask use rate',
             xlab = 'mask use rate')

## pairwise scatter plot
# pairs(df_cov_temp1[,-c(1, 3)])

### reload data
covid_clean <- read.csv('covid_clean_revised.csv')
head(covid_clean)
covid_clean$age65._rate <- covid_clean$age65._rate*100
covid_clean$mask_rate <- covid_clean$mask_rate*100
write.csv(covid_clean, 'covid-clean-1.csv', row.names = F)
## hist-box
hist_boxplot(covid_clean$`age65._rate`, col='lightblue', 
             main='hist_boxplot of age65+ rate',
             xlab = 'age65+ rate')

hist_boxplot(covid_clean$mask_rate, col='lightblue', 
             main='hist_boxplot of mask use rate',
             xlab = 'mask use rate')

# pairwise
pairs(covid_clean[,-c(1,2,3)])
## multicollinearity 
car::vif(coxph(Surv(time, status)~.-fips, data=covid_clean))

#### ------------- Statistical Analysis -------------- ####
## Cox model
cox_model1 <- coxph(Surv(time, status)~.-fips, data = covid_clean)
summary(cox_model1)
cox_fit1 <- survfit(cox_model1)
autoplot(cox_fit1)

summary(covid_clean$unemployment_rate)
summary(covid_clean$bachelor._rate)
summary(covid_clean$age65._rate)
summary(covid_clean$mask_rate)

##### other models ####
## Aalen's Additive regression model
# The Aalen model assumes that the cumulative hazard H(t) for a subject 
# can be expressed as a(t) + X B(t), where a(t) is a time-dependent intercept term,
# X is the vector of covariates for the subject (possibly time-dependent), 
# and B(t) is a time-dependent matrix of coefficients."
aa_fit1 <- aareg(Surv(time, status)~.-fips-`age65.`, data = covid_clean)
aa_fit1
autoplot(aa_fit1)

## rf
r_fit1 <- ranger::ranger(Surv(time, status)~.-fips-`age65.`, 
                         data=covid_clean[complete.cases(covid_clean),],
                         mtry=3,  importance = "permutation",
                         splitrule = "extratrees",
                         verbose = TRUE)

death_times1 <- r_fit1$unique.death.times 
surv_prob1 <- data.frame(r_fit1$survival)
avg_prob1 <- sapply(surv_prob1,mean)

vi1 <- data.frame(sort(round(r_fit1$variable.importance, 4), decreasing = TRUE))
names(vi1) <- "importance"
head(vi1)

#### other-models ####

####### without mask_rate
cox_model2 <- coxph(Surv(time, status)~.-fips-mask_rate, data=covid_clean)
summary(cox_model2)
cox_fit2 <- survfit(cox_model2)
autoplot(cox_fit2)

#### other models ####
aa_fit2 <- aareg(Surv(time, status)~.-fips-`age65.`-mask_rate, data = covid_clean)
aa_fit2
autoplot(aa_fit2)


r_fit2 <- ranger::ranger(Surv(time, status)~.-fips-`age65.`-mask_rate, 
                         data=covid_clean[complete.cases(covid_clean),],
                         mtry=2,  importance = "permutation",
                         splitrule = "extratrees",
                         verbose = TRUE)


vi2 <- data.frame(sort(round(r_fit2$variable.importance, 4), decreasing = TRUE))
names(vi2) <- "importance"
head(vi2)
#### other models ####
#### ------------- Model Diagnosis ------------- ####
## cox_model1
#test for the proportional-hazards (PH) assumption
cox_ph1 <- cox.zph(cox_model1)
cox_ph1
ggcoxzph(cox_ph1)

# influential observations
# Specifying the argument type = "dfbeta", plots the estimated changes in the regression coefficients upon deleting each observation in turn; likewise, type="dfbetas" produces the estimated changes in the coefficients divided by their standard errors.
ggcoxdiagnostics(cox_model1, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())

# outliers, need symmetric, kinda positively skewed, lots of too long
ggcoxdiagnostics(cox_model1, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())

# nonlinearity, martingale, (-infin, 1), from dfbeta residuals, check for the largest residual covariate, here's age65_rate
# ggcoxdiagnostics(cox_model1, type = 'martingale', 
#                  linear.predictions = F, ggtheme = theme_bw())
#ggcoxfunctional(Surv(time, status)~x+log(x)+sqrt(x), data = covid_clean), using plot to decide the transformation
ggcoxdiagnostics(cox_model1)
ggcoxfunctional(Surv(time, status)~`age65._rate`+log(`age65._rate`)+sqrt(`age65._rate`), data = covid_clean)
# slightly nonlinearity

## cox_model2
#test for the proportional-hazards (PH) assumption
cox_ph2 <- cox.zph(cox_model2)
cox_ph2
ggcoxzph(cox_ph2)

# influential observations, age65_rate largest residual deviance
ggcoxdiagnostics(cox_model2, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())

# outliers, need  symmetric, kinda positively skewed
ggcoxdiagnostics(cox_model2, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())

# nonlinearity, same as model1
ggcoxfunctional(Surv(time, status)~`age65._rate`+log(`age65._rate`)+sqrt(`age65._rate`), data = covid_clean)

#### ------------- Results & Conclusions ------------- ####
lmtest::lrtest(cox_model2, cox_model1)
