library(sjPlot)
dataframe <- read.csv("assas.csv")


model1 <- readRDS("../BayesOutput/model.rds")
tab_model(model1, file="../BayesOutput/ls_results_table.html")

model2 <- readRDS("../BayesOutput/model-hope.rds")
tab_model(model2, file="../BayesOutput/hope_results_table.html")

model3 <- readRDS("../BayesOutput/model-na.rds")
tab_model(model3, file="../BayesOutput/na_results_table.html")

model4 <- readRDS("../BayesOutput/model-pa.rds")
tab_model(model4, file="../BayesOutput/pa_results_table.html")

tab_model(model1, model2, model3, model4)


