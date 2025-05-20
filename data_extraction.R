library(tidyverse)
library(here)
library(redcapAPI)
library(lubridate)
library(RMySQL)
library(DBI)

#Internet location of the database online 
jpi_token <- "42602AC27FE5F8B79D6C293342105A43"
kemri_url <- "https://redcap.kemri-wellcome.org/redcap/redcap_v14.3.3/API/"


# Redcap connection
jpi_con <- redcapConnection(url = kemri_url, token = jpi_token)

# Getting data from redCAP
data_df <- exportRecordsTyped(
  rcon = jpi_con,
  records = NULL,# Do not filter any record - EXTRACT all records
  forms = NULL,# Do not filter by FORMS - EXTRACT data from all forms 
  #labels = FALSE,# this means do not extract questions associated with variables
  #dates = TRUE,# Get me the date as per REDCap date settings, not as a STRING, where it will use my computer time stamp
  survey = FALSE,# Forms allow Participant to answer directly- qual data 
  factors = FALSE,#Related to dates, retains structure of REDCap e.g. M/F as 1,2 - respects class orders
  dag = T,# Return data by access group, e.g. Dhaka, etc. Works when CRF does not have "site"
  #  fields = c("record_id", "redcap_event_name"),  # Include key identifiers
  checkboxLabels = TRUE # Return labels with options e.g. gender_MALE, gender_FEMALE, gender_OTHER
  
)

#reviewInvalidRecords(data_df)

# Replace "Unchecked" with NA in all factor columns
data_df[sapply(data_df, is.factor)] <- lapply(data_df[sapply(data_df, is.factor)], function(x) {
  x[x == "Unchecked"] <- NA
  return(x)
})

hospital_df <- data_df %>% filter(redcap_event_name == "admission (Arm 1: Hospitalized)")
hospital_df <- hospital_df[, colSums(is.na(hospital_df)) < nrow(hospital_df)]

community_wet_df <- data_df %>% filter(redcap_event_name == "Wet (Arm 2: Community)" & is.na(redcap_repeat_instrument))
community_wet_df <- community_wet_df[, colSums(is.na(community_wet_df)) < nrow(community_wet_df)]

community_stool_df <- data_df %>% filter(redcap_event_name == "Wet (Arm 2: Community)" & redcap_repeat_instrument == "Stool sample collection")
community_stool_df <- community_stool_df[, colSums(is.na(community_stool_df)) < nrow(community_stool_df)]

community_care_seeking_df <- data_df %>% filter(redcap_event_name == "Wet (Arm 2: Community)" & redcap_repeat_instrument == "Care Seeking Episodes")
community_care_seeking_df <- community_care_seeking_df[, colSums(is.na(community_care_seeking_df)) < nrow(community_care_seeking_df)]

exit_df <- data_df %>% filter(redcap_event_name == "exit (Arm 3: Exit Interviews)" )
exit_df <- exit_df[, colSums(is.na(exit_df)) < nrow(exit_df)]

pps_ward_df <- data_df %>% filter(redcap_event_name == "PPS Ward (Arm 4: PPS Ward)" )
pps_ward_df <- pps_ward_df[, colSums(is.na(pps_ward_df)) < nrow(pps_ward_df)]
pps_ward_df <- pps_ward_df %>% 
dplyr::filter(is.na(redcap_repeat_instance))

pps_patient_df <- data_df %>% filter(redcap_repeat_instrument == "PPS patient Form" )
pps_patient_df <- pps_patient_df[, colSums(is.na(pps_patient_df)) < nrow(pps_patient_df)]


saveRDS(hospital_df, here("data","hospital_df.rds"))
saveRDS(community_wet_df, here("data","community_wet_df.rds"))
saveRDS(community_stool_df, here("data","community_stool_df.rds"))
saveRDS(community_care_seeking_df, here("data","community_care_seeking_df.rds"))
saveRDS(exit_df, here("data","exit_df.rds"))
saveRDS(pps_ward_df, here("data","pps_ward_df.rds"))
saveRDS(pps_patient_df, here("data","pps_patient_df.rds"))


#### KIDMS Data extraction 
kidms_user <- Sys.getenv("kidms_user_name")
kidms_pass <- Sys.getenv("kidms_user_passowrd")
dbname <- Sys.getenv("kidms_dbname")
host <- Sys.getenv("kidms_host")
mydb <- dbConnect(MySQL(),
                  user =  kidms_user, 
                  password =  kidms_pass, 
                  
                  dbname = dbname, 
                  host = host)

### All samples 
all_samples <- dbGetQuery(
  mydb,
  "SELECT lsr.id, lsr.fk_study,specimen_source,serial_study_id,vol_brought,vol_brought_unit,
  date_brought,time_brought,lsr.date_received,lsr.time_received,lab_tech_ini,fw_ini,date_collect,time_collect,lsr.time_point,pk_specimen_id,weight_before,
  weight_after,date_processed,time_processed,type_name
FROM lab_sample_receive lsr    
JOIN lab_specimen ls ON ls.fk_receive_specimen = lsr.id
JOIN lab_specimen_type spty ON spty.id = lsr.fk_specimen_type
WHERE lsr.fk_study = 337
ORDER BY lsr.`date_collect` DESC;
")
saveRDS(all_samples, here("data","all_samples_df.rds"))

table(all_samples$type_name, useNA = "always")

### Rectal swabs kidms 
rectal_swabs <- all_samples %>% 
  dplyr::filter(type_name == "RECTAL SWAB")

saveRDS(rectal_swabs, here("data","samples","rectal_swabs_df.rds"))

#### Stool KIDMS 
stool_kidms <- all_samples %>% 
  dplyr::filter(type_name == "STOOL")

saveRDS(stool_kidms, here("data","samples","stool_kidms_df.rds"))


#pps_ward_df_test <- pps_ward_df %>% 
  #left_join(pps_patient_df, by = "record_id")
