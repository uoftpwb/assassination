library(brms)
library(Matrix)
library(tidyverse)
library(egg)
library(sjPlot)
library(bayestestR)

theme_anthony <- function(){
  theme_minimal() %+replace%
    theme(
      panel.grid.minor=element_blank(),
      panel.grid.major.x=element_blank(),
      axis.ticks.x = element_line(size=0.5),
      axis.line.x.bottom = element_line(size=0.5, color="grey"),
      
      axis.text = element_text(
      )
    )
}

##### CREATE FUNCTION TO PLOT A PAGE OF RESULTS #####

plot_page <- function(ivs, iv_realnames) {
  ## Declare Dependent Variables and Names that are constant across all pages
  dvs = c("ls", "hope", "pa", "na")
  dv_realnames = c("Life Satisfaction", "Hope", "Positive Affect", "Negative Affect")
  
  plots <- list()
  i = 0
  for(dv in dvs){
    for(iv in ivs){
      if (file.exists(paste0("../BayesOutput/ModeratorAnalysis/model-", dv, "-", iv, ".rds"))){
        model <- readRDS(paste0("../BayesOutput/ModeratorAnalysis/model-", dv, "-", iv, ".rds"))
        iv <- gsub("_", ":", iv)
        iv <- gsub("conflict:z", "conflict_z", iv)
        if(iv == ""){
          postsample <- as.data.frame(model, variable=c(paste0("b_after"), paste0("b_year_after")))
          p <- ggplot(postsample, aes(x=!!sym(paste0("b_after")), y=!!sym(paste0("b_year_after")))) + 
            geom_hex() + 
            geom_hline(yintercept=0, color="maroon") +
            geom_vline(xintercept=0, color="maroon") + 
            theme_anthony() + 
            theme(legend.position="none",
                  axis.line.y.left = element_line(size=0.5, color="grey"),
                  panel.grid.major = element_blank(),
                  axis.ticks.y = element_line(size=0.5)
            )
        } else {
          postsample <- as.data.frame(model, variable=c(paste0("b_after:", iv), paste0("b_year_after:", iv)))
          p <- ggplot(postsample, aes(x=!!sym(paste0("b_after:", iv)), y=!!sym(paste0("b_year_after:", iv)))) + 
            geom_hex() + 
            geom_hline(yintercept=0, color="maroon") +
            geom_vline(xintercept=0, color="maroon") + 
            theme_anthony() + 
            theme(legend.position="none",
                  axis.line.y.left = element_line(size=0.5, color="grey"),
                  panel.grid.major = element_blank(),
                  axis.ticks.y = element_line(size=0.5)
            )
        }
        
      } else {
        p <- ggplot() + 
          geom_hline(yintercept=0, color="maroon") +
          geom_vline(xintercept=0, color="maroon") + 
          theme_anthony() + 
          theme(legend.position="none",
                axis.line.y.left = element_line(size=0.5, color="grey"),
                panel.grid.major = element_blank(),
                axis.ticks.y = element_line(size=0.5)
          )
        
      }
      i = i+1
      plots[[i]] <- p
    }
  }
  
  ## Add Row and Column Names
  plots[c(1,4,7,10)] = lapply(c(1,4,7,10), function(i){
    arrangeGrob(plots[[i]], left=dv_realnames[[((i-1)/3)+1]])
  })
  
  plots[1:3] = lapply(1:3, function(i){
    arrangeGrob(plots[[i]], top=iv_realnames[[i]])
  })
  
  ## Plot the page
  grid.arrange(grobs=plots)
}

##### RUN THE FUNCTION FOR EACH PAGE #####

ivs1 = c("","completed", "rulingParty")
iv_realnames1 = c("Main Analysis","Target dies", "Target in ruling party", "Ruling party changes")

plot_page(ivs1, iv_realnames1)


ivs3 = c("dem_rulingParty", "completed_rulingParty", "govapproval_rulingParty")
iv_realnames3 = c("Democracy when Target in Gov.", "Target Dies & Target in Gov.", "Approve Gov. & Target in Gov.")

plot_page(ivs3, iv_realnames3)

ivs2 = c("male", "socialsupport", "age")
iv_realnames2 = c("Being Male", "Having Social Support", "Age")

plot_page(ivs2, iv_realnames2)

ivs4 = c("rulPartyChange", "conflict_z", " ")
iv_realnames4 = c("Ruling Party Changes", "Pre-event Conflict", " ")

plot_page(ivs4, iv_realnames4)


##### REGRESSION TABLES #####
for(iv in ivscomplete){
  models <- list()
  i = 0
  for(dv in dvs){
    if (file.exists(paste0("../BayesOutput/ModeratorAnalysis/model-", dv, "-", iv, ".rds"))){
      i=i+1
      models[[i]] = readRDS(paste0("../BayesOutput/ModeratorAnalysis/model-", dv, "-", iv, ".rds"))
    }
  }
  if (i==4) {
    print(tab_model(models[[1]], models[[2]], models[[3]], models[[4]], file=paste0("../BayesOutput/ModeratorAnalysis/results_table_", iv, ".html"), dv.labels=c("Life Satisfaction", "Hope", "Positive Affect", "Negative Affect")))
  } else if(i==3){
    print(tab_model(models[[1]], models[[2]], models[[3]], file=paste0("../BayesOutput/ModeratorAnalysis/results_table_", iv, ".html")))
  } else if(i==2){
    print(tab_model(models[[1]], models[[2]], file=paste0("../BayesOutput/ModeratorAnalysis/results_table_", iv, ".html")))
  } else if(i==1){
    print(tab_model(models[[1]], file=paste0("../BayesOutput/ModeratorAnalysis/results_table_", iv, ".html")))
  }
}

library(sjPlot)
dataframe <- read.csv("assas.csv")
ivscomplete <- c("completed", "rulingParty", "rulPartyChange", "male", "socialsupport", "govapproval_rulingParty", "dem_rulingParty", "completed_rulingParty", "conflict", "age")
dvs = c("ls", "hope", "pa", "na")


  

modelhope <- readRDS("../BayesOutput/model-hope.rds")
tab_model(model2, file="../BayesOutput/hope_results_table.html")

modelna <- readRDS("../BayesOutput/model-na.rds")
tab_model(model3, file="../BayesOutput/na_results_table.html")

modelpa <- readRDS("../BayesOutput/model-pa.rds")
tab_model(model4, file="../BayesOutput/pa_results_table.html")

tab_model(model1, model2, model3, model4, dv.labels=c("Life Satisfaction", "Hope", "Negative Affect", "Positive Affect"))

##### ROPE TEST #####

## FUNCTION FOR GETTING ROPE VALUES
rope_dataframe <- function(ivs) {
  ropes <- tibble(variables=c("ls", "hope", "pa", "na"))
  ## Declare Dependent Variables and Names that are constant across all pages
  dvs = c("ls", "hope", "pa", "na")
  dv_realnames = c("Life Satisfaction", "Hope", "Positive Affect", "Negative Affect")
  

  for(iv in ivs){
    after<-c()
    year_after<-c()
    for(dv in dvs){
      if (file.exists(paste0("../BayesOutput/ModeratorAnalysis/model-", dv, "-", iv, ".rds"))){
        model <- readRDS(paste0("../BayesOutput/ModeratorAnalysis/model-", dv, "-", iv, ".rds"))
        iv1 <- gsub("_", ":", iv)
        iv1 <- gsub("conflict:z", "conflict_z", iv1)
        if(iv1 == ""){
          rope <- rope(model, range=c(as.numeric(sds[sds$variable==dv, "sd"]*0.1),as.numeric(sds[sds$variable==dv, "sd"]*-0.1)), ci=1)
          
          after <- append(after, rope$ROPE_Percentage[rope$Parameter==paste0("b_after")])
          year_after <- append(year_after, rope$ROPE_Percentage[rope$Parameter==paste0("b_year_after")])
          
        } else {
          
          rope <- rope(model, range=c(as.numeric(sds[sds$variable==dv, "sd"]*0.1),as.numeric(sds[sds$variable==dv, "sd"]*-0.1)), ci=1)
          
          after <- append(after, rope$ROPE_Percentage[rope$Parameter==paste0("b_after:", iv1)])
          year_after <- append(year_after, rope$ROPE_Percentage[rope$Parameter==paste0("b_year_after:", iv1)])
        }
        
      } else {
        after <- append(after, 0)
        year_after <- append(year_after, 0)
        
      }
    }
    ropes[paste0(iv, "_after")] <- after
    ropes[paste0(iv, "_year_after")] <- year_after
    print(after)
    print(year_after)
  }
  return(ropes)
}

sds <- read_csv("standarddeviations.csv")

ropesOutput <- rope_dataframe(c(ivs1, ivs3, ivs2, ivs4))

tab_df(ropesOutput, file="ropes.html")

pd_dataframe <- function(ivs) {
  pds <- tibble(variables=c("ls", "hope", "pa", "na"))
  ## Declare Dependent Variables and Names that are constant across all pages
  dvs = c("ls", "hope", "pa", "na")
  dv_realnames = c("Life Satisfaction", "Hope", "Positive Affect", "Negative Affect")
  
  iv = "dem_rulingParty"
  dv = "na"
  
  for(iv in ivs){
    after<-c()
    year_after<-c()
    for(dv in dvs){
      if (file.exists(paste0("../BayesOutput/ModeratorAnalysis/model-", dv, "-", iv, ".rds"))){
        model <- readRDS(paste0("../BayesOutput/ModeratorAnalysis/model-", dv, "-", iv, ".rds"))
        iv1 <- gsub("_", ":", iv)
        iv1 <- gsub("conflict:z", "conflict_z", iv1)
        if(iv1 == ""){
          pd <- pd(model)
          
          
          after <- append(after, pd$pd[pd$Parameter==paste0("b_after")])
          year_after <- append(year_after, pd$pd[pd$Parameter==paste0("b_year_after")])
          
        } else {
          
          pd <- pd(model)
          
          after <- append(after, pd$pd[pd$Parameter==paste0("b_after:", iv1)])
          year_after <- append(year_after, pd$pd[pd$Parameter==paste0("b_year_after:", iv1)])
        }
        
      } else {
        after <- append(after, 0)
        year_after <- append(year_after, 0)
        
      }
    }
    pds[paste0(iv, "_after")] <- after
    pds[paste0(iv, "_year_after")] <- year_after
    print(after)
    print(year_after)
  }
  return(pds)
}

pdsOutput <- pd_dataframe(c(ivs1, ivs3, ivs2, ivs4))

tab_df(pdsOutput, file="pds.html")


## Testing PD for one model:
model <- readRDS(paste0("../BayesOutput/ModeratorAnalysis/model-na-dem_rulingParty.rds"))

p_d <- pd(model)
summary(p_d)
plot(p_d)

p_d$pd[p_d$Parameter==paste0("b_after:", "dem:rulingParty")]
p_d$pd[p_d$Parameter==paste0("b_year_after:", "dem:rulingParty")]

