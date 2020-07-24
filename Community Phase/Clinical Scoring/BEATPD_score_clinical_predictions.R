
library(synapser)
library(synapserutils)
library(tidyverse)
library(metap)
library(lme4)
library(lmerTest)

synLogin()

setwd("/Users/ssieberts/Documents/PDDB Challenge 2/Community Phase/Clinical Scoring/")

load("Clinical_scores_CIS_REAL.Rdat")

cisinds<-c(1004, 1006, 1007, 1019, 1020, 1023, 1032, 1034, 1038, 1039, 1043, 1044, 1046, 
           1048, 1049, 1051)

cis_tasks<-cis_tasks[cis_tasks$subject_id%in%cisinds,]


# Get predictions

syncFromSynapse("syn22239683", path="./")

# CIS Tasks

files<-dir("CIS-PD Clinic Tasks/", full.names = TRUE)
files<-files[-grep("SYNAPSE", files)]

cistasklist<-lapply(files, function(x, truth){
  dat<-read.csv(x, header=T, as.is=T)
  print(names(dat))
  names(dat)<-paste(names(dat), "pred", sep=".")
  truth<-merge.data.frame(truth, dat, by.x="task_id", by.y="measurement_id.pred", all.x=T, sort=F)
  return(truth)
}, truth=cis_tasks)

files<-gsub("CIS-PD Clinic Tasks//", "", files)
teams<-unlist(lapply(strsplit(files, split = "_"), function(x){ return(x[1])}))

names(cistasklist)<-teams

#mod1 = lmer(tremor.pred ~ tremor + (1|subject_id), data = tmp, na.action = na.exclude)
#mod2 = lmer(tremor.pred ~ tremor + (tremor|subject_id), data = tmp, na.action = na.exclude)
#mod3 = lmer(tremor.pred ~ tremor + (tremor||subject_id), data = tmp, na.action = na.exclude)

# Compute correlation within subject_id
corfunc<-function(x1,x2, alternative="greater"){
  if(sum(!is.na(x1))<3 | sum(!is.na(x2))<3){
    res<-c(estimate=NA, p.value=NA)
  } else {
    tmp2<-cor.test(x1,x2, alternative=alternative)
    res<-c(estimate=tmp2$estimate, p.value=tmp2$p.value)
  }
  return(res)
}


cistaskcor<-lapply(cistasklist, function(tmp){
  corres<-tmp %>% group_by(subject_id) %>% 
    summarize(tremor.est=corfunc(tremor, tremor.pred)[1], tremor.pval=corfunc(tremor, tremor.pred)[2], 
            dyskinesia.est=corfunc(dyskinesia, dyskinesia.pred)[1], dyskinesia.pval=corfunc(dyskinesia, dyskinesia.pred)[2],
            on_off.est=corfunc(overall, on_off.pred)[1], on_off.pval=corfunc(overall, on_off.pred)[2])
})

#Perform meta-analysis across subjects
cistaskmeta<-




