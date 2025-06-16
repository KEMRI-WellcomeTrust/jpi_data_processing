library(tidyverse)
library(lubridate)

 # Convertir les dates en format `Date`
ALARUMTest_DATA_1 <- as.data.frame(ALARUMTest_DATA_1)
str(ALARUMTest_DATA_1)

ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  mutate(across(contains("date_"), ~ as.Date(.x, format="%Y-%m-%d")))
ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  mutate(dob_adm = as.Date(dob_adm, format="%Y-%m-%d"))


 # Convertir les heures 
ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  mutate(across(contains("time_"), 
                ~ if (is.numeric(.x)) {
                  format(as.POSIXct(.x * 86400, origin = "1899-12-30", tz = "UTC"), "%H:%M:%S")
                } else {
                  format(as.POSIXct(.x, format="%H:%M:%S"), "%H:%M:%S")
                }))
 # Vérifier le bon format des variable 
str(ALARUMTest_DATA_1)

cols_numeric <- c("axilla_temp_adm", "weight_adm", "length_adm") 
ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  mutate(across(all_of(cols_numeric), as.numeric))
str(ALARUMTest_DATA_1)

 # Vérifier "" = NA

ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  mutate(across(where(is.character), ~ na_if(.x, "")))
sum(ALARUMTest_DATA_1 == "", na.rm = TRUE)

##1. Labelliser les variables en les transfo en facteurs 

ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  mutate(site_adm = factor(site_adm, levels = c(1, 2), labels = c("Kilifi", "Nanoro")))

ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  mutate(age_years_adm = factor(age_years_adm, levels = c(1, 0), labels = c("Oui", "Non")))

ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  mutate(acute_illness_adm = factor(acute_illness_adm, levels = c(1, 0), labels = c("Oui", "Non")))

ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  mutate(hdss_res_adm = factor(hdss_res_adm, levels = c(1, 0), labels = c("Oui", "Non")))

ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  mutate(ref_from_another_hosp_adm = factor(ref_from_another_hosp_adm, levels = c(1, 0), labels = c("Oui", "Non")))

ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  mutate(trauma_adm = factor(trauma_adm, levels = c(1, 0), labels = c("Oui", "Non")))

ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  mutate(born_hosp_adm = factor(born_hosp_adm, levels = c(1, 0), labels = c("Oui", "Non")))

ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  mutate(refuse_consent_adm = factor(refuse_consent_adm, levels = c(1, 0), labels = c("Oui", "Non")))

ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  mutate(dob_type_adm = factor(dob_type_adm, levels = c(1, 0), labels = c("Connue", "Estimée")))

ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  mutate(sex_adm = factor(sex_adm, levels = c(1, 0), labels = c("Masculin", "Féminin")))

ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  mutate(prev_adm_hosp_adm = factor(prev_adm_hosp_adm, levels = c(0, 1, 2, 3), labels = c("Non", "< 2 semaines", "entre 2 semaines - 1 mois", ">1 mois")))

ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  mutate(other_diag_adm = factor(other_diag_adm, levels = c(1, 2, 3), labels = c("Autre", "Inconnu","Echec au test d'appétit uniquement")))

ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  mutate(malaria_rdt_adm = factor(malaria_rdt_adm, levels = c(1, 2, 0), labels = c("Positif", "Négatif","Non réalisé")))

ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  mutate(hiv_known_positive_adm = factor(hiv_known_positive_adm, levels = c(1, 2, 0, 999), labels = c("Oui, PCR positive connue", "Oui, anticorps positifs, statut PCR inconnu maladie", "Non", "Inconnu")))

ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  mutate(rectswab_adm = factor(rectswab_adm, levels = c(1, 2, 0), labels = c("Oui, avant ATB", "Non, après ATB", "Non réalisé")))

ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  mutate(stoolsample_adm = factor(stoolsample_adm, levels = c(0, 1, 2), labels = c("Non", "Oui, avant ATB", "Non, après ATB")))

ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  mutate(bld_culture_adm = factor(bld_culture_adm, levels = c(0, 1, 2), labels = c("Non", "Oui, avant ATB", "Non, après ATB")))

ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  mutate(csf_adm = factor(csf_adm, levels = c(1, 0), labels = c("Oui", "Non")))

ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  mutate(livestock_ownership_adm = factor(livestock_ownership_adm, levels = c(1, 2, 999), labels = c("Oui", "Non", "Inconnu")))

ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  mutate(toilet_type_adm = factor(toilet_type_adm, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 999), labels = c("Chasse d'eau ou chasse d'eau vers égout canalisé", "Chasse d'eau vers fosse septique", "Chasse d'eau vers latrine à fosse", "Chasse d'eau vers autre endroit", "Latrine améliorée ventilée", "Fosse ouverte / Latrine à fosse sans dalle", "Latrine à fosse avec dalle", "Toilette à compost", "Seau", "Buisson / Champ / Plein air", "Inconnu")))

ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  mutate(toilet_share_adm = factor(toilet_share_adm, levels = c(1, 2, 3, 99), labels = c("Partagé avec des ménages connus", "Partagé avec le grand public", "Privé", "Ne sait pas")))

ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  mutate(enrlement_crf_complete = factor(enrlement_crf_complete, levels = c(0, 1, 2), labels = c("Incomplete", "Unverified","Complete")))

 # Vérification
str(ALARUMTest_DATA_1$site_adm)
str(ALARUMTest_DATA_1$toilet_share_adm)
str(ALARUMTest_DATA_1$stoolsample_adm)

##2 Réorganiser les variables catégorielles
###2.1 Médicaments

labels_medicament <- c(
  "med_last7days_adm___1" = 1,  "med_last7days_adm___2" = 2, "med_last7days_adm___3" = 3,
  "med_last7days_adm___88" = 88)

Excel_test_medicament <- ALARUMTest_DATA_1 %>%
  pivot_longer(cols = starts_with("med_last7days_adm___"), 
               names_to = "medicament_code", 
               values_to = "presence") %>%
  filter(presence == 1) %>%  
  mutate(med_last7days_adm = labels_medicament[medicament_code]) %>%  
  select(record_id, med_last7days_adm)
view(Excel_test_medicament)

 # Convertir avec labels
Excel_test_medicament <- Excel_test_medicament %>%
  mutate(med_last7days_adm = factor(med_last7days_adm, levels = c(1, 2, 3, 88), 
                                    labels = c("Pas de médicament", "Antibiotique", 
                                               "Antipaludique", "Autre")))

 # Fusionner plusieurs médicaments en une seule cellule (si plusieurs sont sélectionnés)
Excel_test_medicament <- Excel_test_medicament %>%
  group_by(record_id) %>%
  summarise(med_last7days_adm = paste(unique(na.omit(med_last7days_adm)), collapse = ", "))  

 # Ajouter cette nouvelle variable propre à la base principale
ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  select(-starts_with("med_last7days_adm___")) %>%  
  left_join(Excel_test_medicament, by = "record_id")

rm(Excel_test_medicament)

###2.2 diagnostic respiratoire 

labels_diagno_respi <- c(
  "diag_resp_adm___1" = 1,  "diag_resp_adm___2" = 2, "diag_resp_adm___3" = 3, "diag_resp_adm___4" = 4, "diag_resp_adm___5" = 5, "diag_resp_adm___6" = 6)

Excel_test_diagno_respi <- ALARUMTest_DATA_1 %>%
  pivot_longer(cols = starts_with("diag_resp_adm___"), 
               names_to = "diagno_repsi_code", 
               values_to = "presence") %>%
  filter(presence == 1) %>%  
  mutate(diag_resp_adm = labels_diagno_respi[diagno_repsi_code]) %>%  
  select(record_id, diag_resp_adm)
view(Excel_test_diagno_respi)

 # Ajouter les potentiel NA
individus_sans_diag <- ALARUMTest_DATA_1 %>%
  filter(rowSums(!is.na(select(., starts_with("diag_resp_adm___")))) == 0) %>%
  select(record_id) %>%
  mutate(diag_resp_adm = NA)

Excel_test_diagno_respi <- bind_rows(Excel_test_diagno_respi, individus_sans_diag)

 # Convertir avec labels
Excel_test_diagno_respi <- Excel_test_diagno_respi %>%
  mutate(diag_resp_adm = factor(diag_resp_adm, levels = c(1, 2, 3, 4, 5, 6), 
                                labels = c("IVRI/pneumonie", "Asthme", "Bronchiolite", 
                                           "IVRS (infection des voies respiratoires supérieure)", 
                                           "TB Pulmonaire", "Otite moyenne")))

 # Fusionner plusieurs diagno en une seule cellule (si plusieurs sont sélectionnées)
Excel_test_diagno_respi <- Excel_test_diagno_respi %>%
  group_by(record_id) %>%
  summarise(diag_resp_adm = paste(unique(na.omit(diag_resp_adm)), collapse = ", "))  

 # Ajouter cette nouvelle variable propre à la base principale
ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  select(-starts_with("diag_resp_adm___")) %>%  
  left_join(Excel_test_diagno_respi, by = "record_id")

rm(individus_sans_diag)
rm(Excel_test_diagno_respi)

###2.3 diagnostic general 

labels_diagno_gen <- c(
  "diag_gen_adm___1" = 1,  "diag_gen_adm___2" = 2, "diag_gen_adm___3" = 3,
  "diag_gen_adm___4" = 4, "diag_gen_adm___5" = 5, "diag_gen_adm___6" = 6,
  "diag_gen_adm___7" = 7,  "diag_gen_adm___8" = 8)

Excel_test_diagno_gen <- ALARUMTest_DATA_1 %>%
  pivot_longer(cols = starts_with("diag_gen_adm___"), 
               names_to = "diagno_gen_code", 
               values_to = "presence") %>%
  filter(presence == 1) %>%  
  mutate(diag_gen_adm= labels_diagno_gen[diagno_gen_code]) %>%  
  select(record_id, diag_gen_adm)
view(Excel_test_diagno_gen)

 # Ajouter les potentiel NA
individus_sans_diag_gen <- ALARUMTest_DATA_1 %>%
  filter(rowSums(!is.na(select(., starts_with("diag_gen_adm___")))) == 0) %>%
  select(record_id) %>%
  mutate(diag_gen_adm = NA)

Excel_test_diagno_gen <- bind_rows(Excel_test_diagno_gen, individus_sans_diag_gen)

 # Convertir avec labels
Excel_test_diagno_gen <- Excel_test_diagno_gen %>%
  mutate(diag_gen_adm = factor(diag_gen_adm, levels = c(1, 2, 3, 4, 5, 6, 7, 8), 
                               labels = c("Anémie", "Drépanocytose", "Thalassémie", "Iléus", 
                                          "Syndrome Néphrotique", "Néphrite", "Dysfonctionement hépatique", 
                                          "Maladie cardiaque congénitale")))

 # Fusionner plusieurs diagnostic en une seule cellule (si plusieurs sont sélectionnés)
Excel_test_diagno_gen <- Excel_test_diagno_gen %>%
  group_by(record_id) %>%
  summarise(diag_gen_adm = paste(unique(na.omit(diag_gen_adm)), collapse = ", "))  

 # Ajouter cette nouvelle variable propre à la base principale
ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  select(-starts_with("diag_gen_adm___")) %>%  
  left_join(Excel_test_diagno_gen, by = "record_id")

rm(individus_sans_diag_gen)
rm(Excel_test_diagno_gen)

###2.4 diagnostic infectieux 

labels_diagno_infect <- c(
  "diag_infec_adm___1" = 1,  "diag_infec_adm___2" = 2, "diag_infec_adm___3" = 3,
  "diag_infec_adm___4" = 4, "diag_infec_adm___5" = 5, "diag_infec_adm___6" = 6,
  "diag_infec_adm___7" = 7,  "diag_infec_adm___8" = 8, "diag_infec_adm___9" = 9,
  "diag_infec_adm___10" = 10, "diag_infec_adm___11" = 11, "diag_infec_adm___12" = 12 )

Excel_test_diagno_inf <- ALARUMTest_DATA_1 %>%
  pivot_longer(cols = starts_with("diag_infec_adm___"), 
               names_to = "diagno_inf_code", 
               values_to = "presence") %>%
  filter(presence == 1) %>%  
  mutate(diag_infec_adm= labels_diagno_infect[diagno_inf_code]) %>%  
  select(record_id, diag_infec_adm)
view(Excel_test_diagno_inf)

 # Ajouter les potentiel NA
individus_sans_diag_inf <- ALARUMTest_DATA_1 %>%
  filter(rowSums(!is.na(select(., starts_with("diag_infec_adm___")))) == 0) %>%
  select(record_id) %>%
  mutate(diag_infec_adm = NA)

Excel_test_diagno_inf <- bind_rows(Excel_test_diagno_inf, individus_sans_diag_inf)

 # Convertir avec labels
Excel_test_diagno_inf <- Excel_test_diagno_inf %>%
  mutate(diag_infec_adm = factor(diag_infec_adm, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
                                 labels = c("Gastro-entérite", "Septicémie", "Paludisme", 
                                            "TB Extra-Pulmonaire", "Infections des tissus mous", 
                                            "ITU (infection du tractus urinaire)", "Maladie liée au VIH", 
                                            "Rougeole", "Varicelle", "Ostéomyélite", "Maladie fébrile", "Fièvre entérique")))

 # Fusionner plusieurs diagnostics en une seule cellule (si plusieurs sont sélectionnés)
Excel_test_diagno_inf <- Excel_test_diagno_inf %>%
  group_by(record_id) %>%
  summarise(diag_infec_adm = paste(unique(na.omit(diag_infec_adm)), collapse = ", "))  

 # Ajouter cette nouvelle variable propre à la base principale
ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  select(-starts_with("diag_infec_adm___")) %>%  
  left_join(Excel_test_diagno_inf, by = "record_id")

rm(individus_sans_diag_inf)
rm(Excel_test_diagno_inf)

###2.5 diagnostic CNS 

labels_diagno_cns <- c(
  "diag_cns_adm___1" = 1,  "diag_cns_adm___2" = 2, "diag_cns_adm___3" = 3,
  "diag_cns_adm___4" = 4, "diag_cns_adm___5" = 5, "diag_cns_adm___6" = 6,
  "diag_cns_adm___7" = 7)

Excel_test_diagno_cns <- ALARUMTest_DATA_1 %>%
  pivot_longer(cols = starts_with("diag_cns_adm___"), 
               names_to = "diagno_cns_code", 
               values_to = "presence") %>%
  filter(presence == 1) %>%  
  mutate(diag_cns_adm= labels_diagno_cns[diagno_cns_code]) %>%  
  select(record_id, diag_cns_adm)
view(Excel_test_diagno_cns)

 # Ajouter les potentiel NA
individus_sans_diag_cns <- ALARUMTest_DATA_1 %>%
  filter(rowSums(!is.na(select(., starts_with("diag_cns_adm___")))) == 0) %>%
  select(record_id) %>%
  mutate(diag_cns_adm = NA)

Excel_test_diagno_cns <- bind_rows(Excel_test_diagno_cns, individus_sans_diag_cns)

 # Convertir avec labels
Excel_test_diagno_cns <- Excel_test_diagno_cns %>%
  mutate(diag_cns_adm = factor(diag_cns_adm, levels = c(1, 2, 3, 4, 5, 6, 7), 
                               labels = c("Convulsion fébriles", "Epilepsie", "Méningite probable", 
                                          "Autres encéphalopathies", "Hydrocéphalie", "Paralysie cérébrale", 
                                          "Retard de développement")))

 # Fusionner plusieurs diagnostics en une seule cellule (si plusieurs sont sélectionnés)
Excel_test_diagno_cns <- Excel_test_diagno_cns %>%
  group_by(record_id) %>%
  summarise(diag_cns_adm = paste(unique(na.omit(diag_cns_adm)), collapse = ", "))  

 # Ajouter cette nouvelle variable propre à la base principale
ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  select(-starts_with("diag_cns_adm___")) %>%  
  left_join(Excel_test_diagno_cns, by = "record_id")

rm(Excel_test_diagno_cns)
rm(individus_sans_diag_cns)

###2.6 Animaux du ménage

labels_animaux <- c(
  "livestock_owned_adm___1" = 1,  "livestock_owned_adm___2" = 2, "livestock_owned_adm___3" = 3, 
  "livestock_owned_adm___4" = 4, "livestock_owned_adm___5" = 5, "livestock_owned_adm___6" = 6, 
  "livestock_owned_adm___7" = 7, "livestock_owned_adm___8" = 8, "livestock_owned_adm___9" = 9, "livestock_owned_adm___88" = 88)

Excel_test_animaux <- ALARUMTest_DATA_1 %>%
  pivot_longer(cols = starts_with("livestock_owned_adm___"), 
               names_to = "animaux_code", 
               values_to = "presence") %>%
  filter(!is.na(presence) & presence == 1) %>%  
  mutate(livestock_owned_adm = labels_animaux[animaux_code]) %>%  
  select(record_id, livestock_owned_adm)
view(Excel_test_animaux)

 # Convertir avec labels
Excel_test_animaux <- Excel_test_animaux %>%
  mutate(livestock_owned_adm = factor(livestock_owned_adm, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9,88), 
                                      labels = c("Poulet", "Canard", "Porc", "Bovins", "Chèvre", "Moutons", 
                                                 "Cheval, âne ou mule", "Chien", "Chat", "Autre")))

 # Fusionner plusieurs animaux en une seule cellule (si plusieurs sont sélectionnés)
Excel_test_animaux <- Excel_test_animaux %>%
  group_by(record_id) %>%
  summarise(livestock_owned_adm = paste(unique(na.omit(livestock_owned_adm)), collapse = ", "))  

 # Ajouter cette nouvelle variable propre à la base principale
ALARUMTest_DATA_1 <- ALARUMTest_DATA_1 %>%
  select(-starts_with("livestock_owned_adm___")) %>%  
  left_join(Excel_test_animaux, by = "record_id")

rm(Excel_test_animaux)
