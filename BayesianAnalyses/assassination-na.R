required_packages = c("brms") # include list of packages needed here, e.g. required_packages = c("stats", "tidyverse", "lme4")

# this for-loop checks each package is installed and then installs them if not
for (package in required_packages) {
  if(package %in% rownames(installed.packages()) == FALSE) {install.packages(package, repos = "http://cran.us.r-project.org", dependencies=TRUE, INSTALL_opts = "--no-lock")}
}

# include library(REQUIRED_PACKAGE) statement for each required package here
library(brms)

dataframe <- read.csv("assas.csv") # load your data from csv

model <- brm(na ~ year_number + after + year_after + loggdp_z + conflict_z + 
                   (year_number + after + year_after|event_id), dataframe)  # run your model
saveRDS(model, "model-na.rds")       # save the model object for later reference
summary(model)                    # output the summary of a model to the output file specified in your ".sh" file