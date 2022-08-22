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
models[1]
dv.labels = c("Armenia", "OtherCountry")