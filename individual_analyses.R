library(tidyverse)
library(sjPlot)

assas <- read.csv("assas.csv")

countries <- unique(assas$country)
i<- 1
summaries <- list()
models <- list()

for(countryname in countries) {
  country <- filter(assas, country==countryname)
  ls.model <- lm(ls ~ year_number + after + year_after + loggdp_z + conflict_z, country, weight=weight)
  summaries[[i]] <- summary(ls.model)
  models[[i]] <- ls.model
  i <- i+1
}

tab_model(models, dv.labels = countries, file="individual_country_output.html")


### POSITIVE AFFECT ###
i<- 1
summaries <- list()
models <- list()

for(countryname in countries) {
  country <- filter(assas, country==countryname)
  pa.model <- lm(pa ~ year_number + after + year_after + loggdp_z + conflict_z, country, weight=weight)
  summaries[[i]] <- summary(pa.model)
  models[[i]] <- pa.model
  i <- i+1
}

tab_model(models, dv.labels = countries, file="individual_country_output_pa.html")


### NEGATIVE AFFECT ###
i<- 1
summaries <- list()
models <- list()

for(countryname in countries) {
  country <- filter(assas, country==countryname)
  na.model <- lm(na ~ year_number + after + year_after + loggdp_z + conflict_z, country, weight=weight)
  summaries[[i]] <- summary(na.model)
  models[[i]] <- na.model
  i <- i+1
}

tab_model(models, dv.labels = countries, file="individual_country_output_na.html")


### HOPE ###
i<- 1
summaries <- list()
models <- list()

for(countryname in countries) {
  country <- filter(assas, country==countryname)
  hope.model <- lm(hope ~ year_number + after + year_after + loggdp_z + conflict_z, country, weight=weight)
  summaries[[i]] <- summary(hope.model)
  models[[i]] <- hope.model
  i <- i+1
}

tab_model(models, dv.labels = countries, file="individual_country_output_hope.html")