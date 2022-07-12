library(tidyverse)
library(countrycode)
library(lubridate)
library(lme4)
library(lmerTest)
library(gee)
library(geepack)
library(ggplot2)

##################### DATA CLEANING AND SETUP ############################

## IMPORT GALLUP DATA and standardize dates and country codes
assasraw <- read_csv("Raw Data/gallupAssas.csv")%>%
  rename(country=COUNTRYNEW, country_code_c=COUNTRY_ISO3, survey_date=FIELD_DATE,
         year=YEAR_WAVE)%>%
  mutate(country_code = countrycode(country, origin="country.name", 
                                    destination="iso3n", warn=TRUE, nomatch=NULL,
                                    custom_match=c("Azerbaijan"="031", "Kosovo"="926")))%>%
  mutate(survey_date = mdy(survey_date))

## CREATE DATAFRAME WITH JUST SURVEY DATES for comparing with event dates for data selection
survey_dates <- assasraw %>%
  group_by(country_code, year) %>%
  summarize(survey_date = first(survey_date))

## IMPORT HANDCODED COUNTRY DATA, standardizing dates and country codes
events <- read_csv("Raw Data/Country Coding.csv")%>%
  select(country=name, year, event_date=date, gallup_availability=`Gallup Coverage`)%>%
  drop_na()%>%
  filter(gallup_availability=="yes" & !(country=="US" & year==2011))%>%     ##REMOVE US 2011 events manually, this is just a stop-gap measure to make coding simpler, can be brought back later
  mutate(country_code_c = countrycode(country, origin="country.name", 
                                    destination="iso3c", warn=TRUE, nomatch=NULL,
                                    custom_match=c("Azerbaijan"="AZE", "Kosovo"="XKX")))%>%
  mutate(country_code = countrycode(country, origin="country.name", 
                                      destination="iso3n", warn=TRUE, nomatch=NULL,
                                    custom_match=c("Azerbaijan"="031", "Kosovo"="926")))%>%
  mutate(event_date=ymd(event_date)) %>%
  select(-gallup_availability) %>%
  left_join(survey_dates, by=c("country_code", "year")) %>%  #combine survey and event dates
  mutate(before_survey = survey_date>event_date)%>%  #check if survey occurred before event to determine when to begin the year selection
  mutate(start_year = if_else(before_survey, year-3, year-2))%>% # determine the first of the six years to extract from gallup data
  filter(country_code != 275) %>% filter(country_code != 608) %>% #Remove Palestine and Philippines (more than 1 assassination in a year)
  mutate(event_id=factor(row_number())) # creates an id for each event, which will be useful in the case of multiple events in the same country
  
# IN ORDER TO SELECT THE REQUIRED YEARS of REQUIRED COUNTRIES from GALLUP and CODE THEM WITH EVENT IDENTIFIERS
# I make a new dataframe based on the data in 'events' which includes a row for each country and year combination needed
# in the six year study intervals. 
event_dataframes <- list() # create a list to fill with dataframes for each country and year

# Make a dataframe for each row in events with a row for each year in the study interval. Here we create the variables needed
# interrupted time series analysis: a binary 'after' variable identifying whether the event has taken place, a 'year_number' 
# variable which counts which year of the six we are in, and 'year_after' which counts how many years have passed since the event.
for(i in 1:nrow(events)){
  row <- events[i,]
  event_dataframes[[i]] <- tibble(event_id=rep(row$event_id, 6),
                                country = rep(row$country, 6),
                                country_code = rep(row$country_code, 6),
                                country_code_c = rep(row$country_code_c, 6),
                                event_date = rep(row$event_date, 6),
                                event_year = rep(row$year, 6),
                                start_year = rep(row$start_year,6),
                                year = row$start_year:(row$start_year+5),
                                after = c(0,0,0,1,1,1))
}

event_dataframe <- do.call(rbind,event_dataframes) # now bind all dataframes into a single dataframe


## MERGE 'event_dataframe' and GALLUP 'assasraw' to select only the years and countries required and fill each row with the 
# interrupted time series variables required

assas <- left_join(event_dataframe, assasraw, by=c("country_code", "year"))%>%
  select(-country.y, -country_code_c.y, -YEAR_CALENDAR, -`...1`) %>%
  rename(country=country.x, country_code_c=country_code_c.x, weight=WGT) %>%
  mutate(year_number = as.numeric(interval(event_date, survey_date), 'years')) %>%
  mutate(year_after = if_else(after==1,as.numeric(interval(event_date, survey_date), 'years'), 0))


### PAST HERE IS ***UNDER DEVELOPMENT*** 
### it shows conceptual ideas, but they haven't been implemented, tested, or the value names matched yet


################ ANALYSIS #################


# 1. Life Satisfaction Yearly Model

ls.model <- lmer(ls ~ year_number + after + year_after + (after|event_id) + (year_after|event_id), assas, weight=weight)
summary(ls.model)

# 2. Life Satisfaction Moderated by Survey Delay
# 'delay' is a variable that counts the days between the event and the survey
# if it has a non-zero effect on the effect of after or year_after,
# then this is evidence of a short-term effect

# Specifically a negative effect for delay suggests a short term

# We may wish to try a log-linear effect of 'delay', as we might expect that only low
# values of prox have a noticeable effect where after a certain point the effect
# quickly falls off as we pass the horizon

ls.delay.model <- lmer(lifesat ~ year_number*event_id + (after|event_id) + (delay|event_id) + (year_after|event_id), assas, weight=wgt)
  
# 3. Life Satisfaction Daily Model
# Here, in order to account for the effect of delay between the event and measurement
# in a more complete way, instead of considering the life satisfaction measurements
# by year, we consider them by day. Individual surveys by Gallup are taken on different
# days and these days are recorded in the database. We introduce parallel measures to the 
# years model aboove: day_number (days since the beginning of the third calendar year
# before the event), day_after (days since the event). Like the previous model,
# a log-linear relationship may be best.

# We introduce individual controls for this model, as we theorize that demographic
# factors may influence the difficulty Gallup would have in contacting their household,
# or the order in which households are approached. This makes demographic and regional
# factors controls in the Pearl-ian sense: they are causal of 'day_after' and 'life satisfaction'.

ls.daily.model <- lmer(lifesat ~ day_number*event_id + (after|event_id) + (day_after|event_id) + gender + age + employment + marriage + region, assas, weight=wgt)


############################ GEE Analysis by FC #################################
#Given the non-convergence, I decided to try a GEE to account for the longitudinal nature of the data.

ls.gee <- geeglm(ls ~ year_number + after + year_after,
              data = assas, 
              id = event_id,
              corstr = "exchangeable")
summary(ls.gee)


########################### Plot ################################################
#Create an aggregated-level dataset for plotting
agg <- assas %>% 
  group_by(country, year_number) %>% 
  summarise(n = n(),
            ls = weighted.mean(ls, weight, na.rm = T),
            hope = weighted.mean(hope, weight, na.rm = T),
            pa = weighted.mean(pa, weight, na.rm = T),
            na = weighted.mean(na, weight, na.rm = T))

#Plot ls over time for each country
ls.plot <- agg %>% 
  ggplot(aes(x = year_number, y = ls)) +
  geom_point() +
  geom_line() + 
  geom_vline(xintercept = 0, linetype="dotted", color = "blue") +
  facet_wrap(~country, ncol = 3) +
  theme_classic() +
  theme(text = element_text(size=16),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = , b = 0, l = 0))) +
  xlab("Year to Event") +
  ylab("Life Satisfaction")

ggsave("ls.png", width = 15, height = 30, units = "cm")

#Plot hope over time for each country
hope.plot <- agg %>% 
  ggplot(aes(x = year_number, y = hope)) +
  geom_point() +
  geom_line() + 
  geom_vline(xintercept = 0, linetype="dotted", color = "blue") +
  facet_wrap(~country, ncol = 3) +
  theme_classic() +
  theme(text = element_text(size=16),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = , b = 0, l = 0))) +
  xlab("Year to Event") +
  ylab("Hope")

ggsave("hope.png", width = 15, height = 30, units = "cm")

#Plot pa over time for each country
pa.plot <- agg[!is.na(agg$pa),] %>% 
  ggplot(aes(x = year_number, y = pa)) +
  geom_point() +
  geom_line() + 
  geom_vline(xintercept = 0, linetype="dotted", color = "blue") +
  facet_wrap(~country, ncol = 3) +
  theme_classic() +
  theme(text = element_text(size=16),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = , b = 0, l = 0))) +
  xlab("Year to Event") +
  ylab("Positive Affect")

ggsave("pa.png", width = 15, height = 30, units = "cm")

#Plot na over time for each country
na.plot <- agg[!is.na(agg$na),] %>% 
  ggplot(aes(x = year_number, y = na)) +
  geom_point() +
  geom_line() + 
  geom_vline(xintercept = 0, linetype="dotted", color = "blue") +
  facet_wrap(~country, ncol = 3) +
  theme_classic() +
  theme(text = element_text(size=16),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = , b = 0, l = 0))) +
  xlab("Year to Event") +
  ylab("Negative Affect")

ggsave("na.png", width = 15, height = 30, units = "cm")

