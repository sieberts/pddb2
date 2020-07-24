
library(synapser)
library(gdata)
library(tidyverse)

synLogin()

tasks_query<-synTableQuery("select * from syn19288412")
cis_tasks<- tasks_query$asDataFrame()
cis_tasks<-cis_tasks[,-c(1:2)]

cis_tasks$tremor[cis_tasks$side=="Left"]<-cis_tasks$tremor_left[cis_tasks$side=="Left"]
cis_tasks$tremor[cis_tasks$side=="Right"]<-cis_tasks$tremor_right[cis_tasks$side=="Right"]

cis_tasks$dyskinesia[cis_tasks$side=="Left"]<-cis_tasks$dyskinesia_left[cis_tasks$side=="Left"]
cis_tasks$dyskinesia[cis_tasks$side=="Right"]<-cis_tasks$dyskinesia_right[cis_tasks$side=="Right"]

cis_side<-table(cis_tasks$subject_id, cis_tasks$side)
cis_side<-data.frame(subject_id=row.names(cis_side), side=colnames(cis_side)[apply(cis_side, 1, which.max)], stringsAsFactors = FALSE)


updrs_query <- synTableQuery("select * from syn18435297")
cis_updrs_score <- updrs_query$asDataFrame()
cis_updrs_score<-cis_updrs_score[,-c(1:2)]
#cis_updrs_score<-cis_updrs_score[!is.na(cis_updrs_score$ParticipantState),]

updrs_query <- synTableQuery("select * from syn22213711")
cis_updrs_device <- updrs_query$asDataFrame()
cis_updrs_device<-cis_updrs_device[,-c(1:2)]

cis_updrs<-cbind(cis_updrs_device, cis_updrs_score[match(paste(cis_updrs_device$subject_id, cis_updrs_device$visit), paste(cis_updrs_score$`Subject ID`, cis_updrs_score$Visit)),])
cis_updrs$side<-cis_side$side[match(cis_updrs$subject_id, cis_side$subject_id)]

cis_updrs$postural_tremor[cis_updrs$side=="Right"]<-cis_updrs$`3.15 Right Hand`[cis_updrs$side=="Right"]
cis_updrs$postural_tremor[cis_updrs$side=="Left"]<-cis_updrs$`3.15 Left Hand`[cis_updrs$side=="Left"]

cis_updrs$kinetic_tremor[cis_updrs$side=="Right"]<-cis_updrs$`3.16 Right Hand`[cis_updrs$side=="Right"]
cis_updrs$kinetic_tremor[cis_updrs$side=="Left"]<-cis_updrs$`3.16 Left Hand`[cis_updrs$side=="Left"]

cis_updrs$rest_tremor[cis_updrs$side=="Right"]<-cis_updrs$`3.17 Right Upper Extremity`[cis_updrs$side=="Right"]
cis_updrs$rest_tremor[cis_updrs$side=="Left"]<-cis_updrs$`3.17 Left Upper Extremity`[cis_updrs$side=="Left"]

cis_updrs$dyskinesia<-as.numeric(cis_updrs$`3.19A`=="Yes")






#######
## REAL-PD
#######
clin<-read.delim("~/Documents/PDDB Challenge 2/REAL-PD/Other/Home-based_validation_csv_export_20181129160617/Home-based_validation_export_20181129.csv", sep=";", header=T, as.is=T)
names(clin)[1]<-"subject_id"
names(clin)[names(clin)=="Interal_1_assessor"]<-"Interval_1_assessor"

# Hauser Diaries
real_hauser<-clin[, c(1,grep("nterval", names(clin)))]

names(real_hauser)<-gsub("Interval_", "", names(real_hauser))
names(real_hauser)<-gsub("_interval", "", names(real_hauser))
names(real_hauser)[grep("assessor", names(real_hauser))]<-paste("assessor_", gsub("_assessor", "", names(real_hauser)[grep("assessor", names(real_hauser))]), sep="")
names(real_hauser)[grep("patient", names(real_hauser))]<-paste("patient_", gsub("_patient", "", names(real_hauser)[grep("patient", names(real_hauser))]), sep="")

real_hauser<-real_hauser %>%
  pivot_longer(-subject_id,
               names_to = c(".value", "interval"),
               names_pattern = "([:alnum:]+)_([:alnum:]+)"
  )
real_hauser[real_hauser==-97]<-NA

hauser_query <- synTableQuery("select * from syn22210734")
real_hauser_device <- hauser_query$asDataFrame()
real_hauser_device<-real_hauser_device[,-c(1:2)]

real_hauser<-cbind(real_hauser_device, real_hauser[match(paste(real_hauser_device$subject_id, real_hauser_device$time_interval), paste(real_hauser$subject_id, real_hauser$interval)),-c(1:2)])

real_hauser$on_off_assessor[!is.na(real_hauser$assessor)]<-0
real_hauser$on_off_assessor[!is.na(real_hauser$assessor)&real_hauser$assessor==1]<-1
real_hauser$dyskinesia_assessor[!is.na(real_hauser$assessor)]<-0
real_hauser$dyskinesia_assessor[!is.na(real_hauser$assessor)&real_hauser$assessor==3]<-1
real_hauser$dyskinesia_assessor[!is.na(real_hauser$assessor)&real_hauser$assessor==4]<-2

real_hauser$on_off_patient[!is.na(real_hauser$patient)]<-0
real_hauser$on_off_patient[!is.na(real_hauser$patient)&real_hauser$patient==1]<-1
real_hauser$dyskinesia_patient[!is.na(real_hauser$patient)]<-0
real_hauser$dyskinesia_patient[!is.na(real_hauser$patient)&real_hauser$patient==3]<-1
real_hauser$dyskinesia_patient[!is.na(real_hauser$patient)&real_hauser$patient==4]<-2




# UPDRS

temp<-clin[,c(1,grep("Motorola_side|UPDRS|DyskAanwezig|HenY", names(clin)))]
temp<-temp[,c(1,2,grep("ON|OFF", names(temp)))]

temp$Motorola_side[temp$Motorola_side==1]<-"Left"
temp$Motorola_side[temp$Motorola_side==2]<-"Right"
temp[temp==-99]<-NA
temp[temp=="-99"]<-NA
temp[temp==-96]<-NA
temp[temp=="-96"]<-NA
temp[temp==""]<-NA
temp[temp=="-"]<-NA

names(temp)[names(temp)=="ON_UPDRS_3_22"]<-"ON_UPDRS_3_18"
names(temp)[names(temp)=="ON_UPDRS_3_22_video"]<-"ON_UPDRS_3_18_video"

real_updrs<-temp %>%
  pivot_longer(-c(subject_id, Motorola_side),
               names_to = c("interval", ".value"),
               names_pattern = "([:alnum:]+)_(.*)"
  )

updrs_query <- synTableQuery("select * from syn22210733")
real_updrs_device <- updrs_query$asDataFrame()
real_updrs_device<-real_updrs_device[,-c(1:2)]

temp2<-cbind(real_updrs_device, real_updrs[match(paste(real_updrs_device$subject_id, real_updrs_device$state), paste(real_updrs$subject_id, tolower(real_updrs$interval))),-c(1,3)])

videonames<-names(temp2)[grep("video", names(temp2))]
for(nm in videonames){
  nm1<-gsub("_video", "", nm)
  temp2[,nm1]<-apply(temp2[,c(nm, nm1)],1, mean, na.rm=TRUE)
  temp2<-temp2[,!names(temp2)%in%nm]
}

real_updrs<-temp2

real_updrs$postural_tremor[real_updrs$Motorola_side=="Right"]<-real_updrs$UPDRS_3_15a[real_updrs$Motorola_side=="Right"]
real_updrs$postural_tremor[real_updrs$Motorola_side=="Left"]<-real_updrs$UPDRS_3_15b[real_updrs$Motorola_side=="Left"]

real_updrs$kinetic_tremor[real_updrs$Motorola_side=="Right"]<-real_updrs$UPDRS_3_16a[real_updrs$Motorola_side=="Right"]
real_updrs$kinetic_tremor[real_updrs$Motorola_side=="Left"]<-real_updrs$UPDRS_3_16b[real_updrs$Motorola_side=="Left"]

real_updrs$rest_tremor[real_updrs$Motorola_side=="Right"]<-real_updrs$UPDRS_3_17a[real_updrs$Motorola_side=="Right"]
real_updrs$rest_tremor[real_updrs$Motorola_side=="Left"]<-real_updrs$UPDRS_3_17b[real_updrs$Motorola_side=="Left"]

real_updrs$dyskinesia<-real_updrs$DyskAanwezig



save(cis_tasks, cis_updrs, real_hauser, real_updrs, file = "/Users/ssieberts/Documents/PDDB Challenge 2/Community Phase/Clinical Scoring/Clinical_scores_CIS_REAL.Rdat")
