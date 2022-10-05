library(brms)
library(tidyverse)

model <- readRDS("../BayesOutput/ModeratorAnalysis/model-ls-completed.rds")

postsample <- posterior_samples(model, c("after", "year_after"))

ggplot(as_draws(model), aes(x=`b_after:completed`, y=`b_year_after:completed`)) + 
  geom_hex()
