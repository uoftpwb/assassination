library(tidyverse)
library(countrycode)
library(lubridate)
library(lme4)
library(lmerTest)
library(gee)
library(geepack)
library(ggplot2)
library(rio)
library(texreg)

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
  mutate(event_date=mdy(event_date)) %>%
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
  select(-country.y, -country_code_c.y, -YEAR_CALENDAR) %>%
  rename(country=country.x, country_code_c=country_code_c.x, weight=WGT) %>%
  mutate(year_number = as.numeric(interval(event_date, survey_date), 'years')) %>%
  mutate(year_after = if_else(after==1,as.numeric(interval(event_date, survey_date), 'years'), 0))

## IMPORT CNTS DATA
cnts <- import("Raw Data/2021 Edition CNTSDATA.xlsx", skip = 1)

#extracting the domestic conflict variable
#getting country codes to facilitate merging

cntsa <- cnts %>% select(year, country, domestic9) %>% 
  mutate(country_code = countrycode(country, origin="country.name", 
                                    destination="iso3n", warn=TRUE, nomatch=NULL,
                                    custom_match=c("Azerbaijan"="031", "Kosovo"="926"))) %>% 
  select(-country)

#getting pre-event conflict data
merged <- assas %>% left_join(., cntsa, by = c("country_code", "year"))
merged_before <- merged %>% filter(after == 0) %>% 
  group_by(country_code) %>% 
  summarize(conflict = mean(domestic9, na.rm=T))

#MERGE CNTS pre-event conflict data back to the assas object
assas <- assas %>% left_join(., merged_before)

##IMPORT GDP and V-DEM data
country <- import("Raw Data/GDPandLDI.csv") %>% 
  select(country = "Country Name", year = Year, gdp = GDPperCapPPP, dem = v2x_libdem) %>% 
  mutate(country_code = countrycode(country, origin="country.name", 
                                    destination="iso3n", warn=TRUE, nomatch=NULL,
                                    custom_match=c("Azerbaijan"="031", "Kosovo"="926","Turkiye" = "792"))) %>% 
  select(-country)

#MERGE country-level covariates to assas data
assas <- assas %>% left_join(., country, by = c("country_code","year"))

#DATA PROCESSING creating standardized country-level covariates
#GDP is log2 transformed to account for diminishing return
assas <- assas %>% ungroup() %>% 
  mutate(loggdp_z = scale(log2(gdp))[,1],
         conflict_z = scale(conflict)[,1])

#Create a new file without CNTS data
des <- assas %>% select(-conflict, -conflict_z)
#save(des, file = "descriptives")

des2 <- des
des2 <- des2[rowSums(is.na(des[,c("ls", "hope", "pa", "na")]))!=4,]
cat("Number of rows in ORIGINAL data frame:", nrow(des), "\n")
cat("Number of rows in NEW data frame:", nrow(des2), "\n")
cat("Number of dropped rows:", nrow(des) - nrow(des2))

table(des2$country)

################ ANALYSIS #################
# 1. Life Satisfaction Yearly Model

ls.model <- lmer(ls ~ year_number + after + year_after + loggdp_z + conflict_z + 
                   (year_number + after + year_after|event_id), assas, weight=weight,
                 control=lmerControl(optimizer="bobyqa",
                                      optCtrl=list(maxfun=2e5)))
summary(ls.model)

hope.model <- lmer(hope ~ year_number + after + year_after + loggdp_z + conflict_z + 
                   (year_number + after + year_after|event_id), assas, weight=weight,
                 control=lmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=2e5)))
summary(hope.model)

pa.model <- lmer(pa ~ year_number + after + year_after + loggdp_z + conflict_z + 
                   (year_number + after + year_after|event_id), assas, weight=weight,
                 control=lmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=2e5)))
summary(pa.model)

na.model <- lmer(na ~ year_number + after + year_after + loggdp_z + conflict_z + 
                   (year_number + after + year_after|event_id), assas, weight=weight,
                 control=lmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=2e5)))
summary(na.model)

# Creating Table 1
htmlreg(list(ls.model, hope.model, pa.model, na.model), file = "results 3rs.doc", 
        inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
        head.tag = TRUE, body.tag = TRUE,
        digits = 3, stars = 0.05,
        custom.model.names = c('Life Satisfaction','Hope','Positive Affect', 'Negative Affect'),
        custom.coef.names = c('Intercept', 'Time', 'Assassination', 'Time * Assassination', "GDP (log)", "Pre-assassination Conflict"))

# 2. Testing the significance of random slopes
# I reestimated the models without random slopes.
# Then used likelihood ratio tests to test the significance of the random slopes
ls.model0 <- lmer(ls ~ year_number + after + year_after + loggdp_z + conflict_z + 
                   (1|event_id), assas, weight=weight,
                 control=lmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=2e5)))
hope.model0 <- lmer(hope ~ year_number + after + year_after + loggdp_z + conflict_z + 
                     (1|event_id), assas, weight=weight,
                   control=lmerControl(optimizer="bobyqa",
                                       optCtrl=list(maxfun=2e5)))
pa.model0 <- lmer(pa ~ year_number + after + year_after + loggdp_z + conflict_z + 
                   (1|event_id), assas, weight=weight,
                 control=lmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=2e5)))
na.model0 <- lmer(na ~ year_number + after + year_after + loggdp_z + conflict_z + 
                   (1|event_id), assas, weight=weight,
                 control=lmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=2e5)))
anova(ls.model0, ls.model)
#npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
#ls.model0    8 540408 540486 -270196   540392                         
#ls.model    17 539353 539518 -269660   539319 1072.9  9  < 2.2e-16 ***
anova(hope.model0, hope.model)
#npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
#hope.model0    8 524172 524249 -262078   524156                         
#hope.model    17 523051 523215 -261509   523017 1138.9  9  < 2.2e-16 ***
anova(pa.model0, pa.model)
#npar    AIC    BIC logLik deviance  Chisq Df Pr(>Chisq)    
#pa.model0    8 122333 122410 -61158   122317                         
#pa.model    17 121712 121875 -60839   121678 639.24  9  < 2.2e-16 ***
anova(na.model0, na.model)
#npar   AIC   BIC logLik deviance Chisq Df Pr(>Chisq)    
#na.model0    8 88308 88385 -44146    88292                        
#na.model    17 87712 87876 -43839    87678 613.4  9  < 2.2e-16 ***

# 3. Get the random slopes for each country
# LS
ls.fix <- as.data.frame(t(fixef(ls.model))) %>% #get fixed effect
  rename(intercept = 1) %>% #rename the intercept to remove the parenthesis
  mutate(event_id = "19", country = "all", event_date = NA) #add additional columns for merging with the random slopes

ls.ran <- as.data.frame(coef(ls.model)[1]) %>% #save the country-specific slopes as a data frame
  mutate(event_id = as.factor(row_number())) %>% #create an event_id variable 
  rename(intercept = 1, year_number = 2, after = 3, year_after = 4, loggdp_z = 5, conflict_z = 6) %>% # rename columns
  left_join(., events %>% select(event_id, country, event_date), by = "event_id") %>%  #merge the country name to aid interpretation  rbind(., as.data.frame(coef(ls.model)[[1]]))
  rbind(., ls.fix)
# Hope
hope.fix <- as.data.frame(t(fixef(hope.model))) %>% #get fixed effect
  rename(intercept = 1) %>% #rename the intercept to remove the parenthesis
  mutate(event_id = "19", country = "all", event_date = NA) #add additional columns for merging with the random slopes

hope.ran <- as.data.frame(coef(hope.model)[1]) %>% #save the country-specific slopes as a data frame
  mutate(event_id = as.factor(row_number())) %>% #create an event_id variable 
  rename(intercept = 1, year_number = 2, after = 3, year_after = 4, loggdp_z = 5, conflict_z = 6) %>% # rename columns
  left_join(., events %>% select(event_id, country, event_date), by = "event_id") %>% #merge the country name to aid interpretation
  rbind(., hope.fix)
# Positive Affect
pa.fix <- as.data.frame(t(fixef(pa.model))) %>% #get fixed effect
  rename(intercept = 1) %>% #rename the intercept to remove the parenthesis
  mutate(event_id = "19", country = "all", event_date = NA) #add additional columns for merging with the random slopes

pa.ran <- as.data.frame(coef(pa.model)[1]) %>% #save the country-specific slopes as a data frame
  mutate(event_id = as.factor(row_number())) %>% #create an event_id variable 
  rename(intercept = 1, year_number = 2, after = 3, year_after = 4, loggdp_z = 5, conflict_z = 6) %>% # rename columns
  left_join(., events %>% select(event_id, country, event_date), by = "event_id") %>% #merge the country name to aid interpretation
  rbind(., pa.fix)
# Negative Affect
na.fix <- as.data.frame(t(fixef(na.model))) %>% #get fixed effect
  rename(intercept = 1) %>% #rename the intercept to remove the parenthesis
  mutate(event_id = "19", country = "all", event_date = NA) #add additional columns for merging with the random slopes

na.ran <- as.data.frame(coef(na.model)[1]) %>% #save the country-specific slopes as a data frame
  mutate(event_id = as.factor(row_number())) %>% #create an event_id variable 
  rename(intercept = 1, year_number = 2, after = 3, year_after = 4, loggdp_z = 5, conflict_z = 6) %>% # rename columns
  left_join(., events %>% select(event_id, country, event_date), by = "event_id") %>%  #merge the country name to aid interpretation
  rbind(., na.fix)

write.csv(ls.ran, file = "ls ran.csv")
write.csv(hope.ran, file = "hope ran.csv")
write.csv(pa.ran, file = "pa ran.csv")
write.csv(na.ran, file = "na ran.csv")

# Figure 1
## Life Satisfaction
## Create predicted values based on coefficients from the MLM
### Predicted values pre-event
fig1.ls.data.pre <- ls.ran %>% 
  slice(rep(1:n(), each=4)) %>%  # Repeat each row 4 times (to simulate how well-being changes in the 3 years before assassination)
  group_by(event_id) %>% 
  mutate(pre = 1,
         year = seq(-3, 0, 1),
         after_pred = 0,
         yr_after_pred = 0,
         pred = intercept + (year_number * year) + (after * after_pred) + (year_after * yr_after_pred))     
### Predicted values post-event
fig1.ls.data.post <- ls.ran %>% 
  slice(rep(1:n(), each=4)) %>%  # Repeat each row 4 times (to simulate how well-being changes in the 3 years after assassination)
  group_by(event_id) %>% 
  mutate(pre = 0,
         year = seq(0, 3, 1),
         after_pred = 1,
         yr_after_pred = year,
         pred = intercept + (year_number * year) + (after * after_pred) + (year_after * yr_after_pred))     
### Merge predicted values
fig1.ls.data <- rbind(fig1.ls.data.pre, fig1.ls.data.post)

## Create separate datasets for plotting
### The average trend is based on the fixed effect 
fig1.ls.fixed <- fig1.ls.data %>% filter(event_id == 19)
### Use Kenya and Uganda as examples
fig1.ls.kenya <- fig1.ls.data %>% filter(event_id == 10)
fig1.ls.uganda <- fig1.ls.data %>% filter(event_id == 15)
### A dataset with the rest of the countries
fig1.ls.rest <- fig1.ls.data %>% filter(event_id !=19) %>% 
  filter(event_id !=10) %>% 
  filter(event_id !=15) 

## GGplot for LS
fig1.ls <- fig1.ls.rest %>% 
  ggplot(aes(x = year, y = pred, group = event_id)) +
#  geom_point() +
  geom_line(data=fig1.ls.rest %>% filter(pre==1),alpha=.1, position=position_jitter(w=0.02, h=0)) +
  geom_line(data=fig1.ls.rest %>% filter(pre==0),alpha=.1, position=position_jitter(w=0.02, h=0)) +
  geom_line(data=fig1.ls.fixed %>% filter(pre==1), aes(x = year, y = pred), color = "navyblue", size = 1.5, position=position_jitter(w=0.02, h=0)) +
  geom_line(data=fig1.ls.fixed %>% filter(pre==0), aes(x = year, y = pred), color = "navyblue", size = 1.5, position=position_jitter(w=0.02, h=0)) +
  geom_line(data=fig1.ls.kenya %>% filter(pre==1), aes(x = year, y = pred), color = "darkgreen", position=position_jitter(w=0.02, h=0)) +
  geom_line(data=fig1.ls.kenya %>% filter(pre==0), aes(x = year, y = pred), color = "darkgreen", position=position_jitter(w=0.02, h=0)) +
  geom_line(data=fig1.ls.uganda %>% filter(pre==1), aes(x = year, y = pred), color = "red", position=position_jitter(w=0.02, h=0)) +
  geom_line(data=fig1.ls.uganda %>% filter(pre==0), aes(x = year, y = pred), color = "red", position=position_jitter(w=0.02, h=0)) +
  geom_vline(xintercept = 0, linetype="dotted", color = "blue") +
  geom_text(x=2.8,y=4.9,label="Kenya",size=5, color="darkgreen") +
  geom_text(x=2.8,y=5.9,label="Uganda",size=5, color="red") +
  geom_text(x=2.8,y=5.2,label="Average",size=5, color="navyblue") +
  geom_text(x=0.7,y=6.8,label="Assassination",size=5, color="black") +
  theme_classic() +
  theme(text = element_text(size=16),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = , b = 0, l = 0))) +
  xlab("Year to Event") +
  ylab("Life Satisfaction")

########################### Additional Plots ################################################
#Not included in PNAS submission
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
