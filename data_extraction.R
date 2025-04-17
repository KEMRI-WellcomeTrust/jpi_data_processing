library(tidyverse)
library(here)
library(redcapAPI)
library(lubridate)

#Internet location of the database online 
jpi_token <- "AFC4872B0BBB5F2ECA3354BE8D426ECD"
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

# Replace "Unchecked" with NA in all factor columns
data_df[sapply(data_df, is.factor)] <- lapply(data_df[sapply(data_df, is.factor)], function(x) {
  x[x == "Unchecked"] <- NA
  return(x)
})

community_wet_df <- data_df %>% filter(redcap_event_name == "Wet (Arm 2: Community)" & is.na(redcap_repeat_instrument))
community_wet_df <- community_wet_df[, colSums(is.na(community_wet_df)) < nrow(community_wet_df)]

community_stool_df <- data_df %>% filter(redcap_event_name == "Wet (Arm 2: Community)" & redcap_repeat_instrument == "Stool sample collection")
community_stool_df <- community_stool_df[, colSums(is.na(community_stool_df)) < nrow(community_stool_df)]

community_care_seeking_df <- data_df %>% filter(redcap_event_name == "Wet (Arm 2: Community)" & redcap_repeat_instrument == "Care Seeking Episodes")
community_care_seeking_df <- community_care_seeking_df[, colSums(is.na(community_care_seeking_df)) < nrow(community_care_seeking_df)]

exit_df <- data_df %>% filter(redcap_event_name == "exit (Arm 3: Exit Interviews)" )
exit_df <- exit_df[, colSums(is.na(exit_df)) < nrow(exit_df)]


hospital_df <- data_df %>% filter(redcap_event_name == "admission (Arm 1: Hospitalized)")
hospital_df <- hospital_df[, colSums(is.na(hospital_df)) < nrow(hospital_df)]


saveRDS(community_wet_df, here("data","community_wet_df.rds"))
saveRDS(community_stool_df, here("data","community_stool_df.rds"))
saveRDS(community_care_seeking_df, here("data","community_care_seeking_df.rds"))
saveRDS(exit_df, here("data","exit_df.rds"))
saveRDS(hospital_df, here("data","hospital_df.rds"))









