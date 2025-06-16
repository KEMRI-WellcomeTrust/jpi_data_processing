################SCRIPT QUALITY DATA, PART 1: ENROLEMENT CRF####################
### 0. Préliminaire: Vérifier les double ID

double_id <- ALARUMTest_DATA_1 %>%
  count(record_id) %>%
  filter(n > 1)
print(double_id)

### 1. Vérifier Critères d'inclusion/exclusion
#1.1 Vérifier l'âge

individus_5_18_ans <- ALARUMTest_DATA_1 %>%
  filter(ageyrs_adm > 5 & ageyrs_adm < 18) %>%
  select(record_id, ageyrs_adm )
print(individus_5_18_ans)

#1.2 Vérifier que les personnes ont un diagnostic 

individus_sans_diagnostic <- ALARUMTest_DATA_1 %>%
  filter(if_all(c(diag_resp_adm, diag_infec_adm, diag_gen_adm, diag_cns_adm, other_diag_adm), is.na)) %>%
  select(record_id)
print(individus_sans_diagnostic)

individus_diagnostic_inconnu <- ALARUMTest_DATA_1 %>%
  filter(other_diag_adm == 2) %>%
  select(record_id)
print(individus_diagnostic_inconnu)

#1.3 Vérifier que les personnes résident dans un HDSS

individus_hors_HDSS <- ALARUMTest_DATA_1 %>%
  filter(!(dss_no_adm %in% ALARUM_reference_HDSS$HDSS)) %>%
  select(record_id, dss_no_adm)
print(individus_hors_HDSS)

#1.4 Identifier les participants qui ont été transféré d'un autre hopital

individus_transfert <- ALARUMTest_DATA_1 %>%
  filter(ref_from_another_hosp_adm == 1) %>%
  select(record_id)
print(individus_transfert)

#1.5 Identifier les personnes admises pour traumatisme/chirurgie elective

individus_trauma <- ALARUMTest_DATA_1 %>%
  filter(trauma_adm == 1) %>%
  select(record_id)
print(individus_trauma)

#1.6 Identifier les personnes restées à l'hopital depuis la naissance

individus_naiss_hospi <- ALARUMTest_DATA_1 %>%
  filter(born_hosp_adm == 1) %>%
  select(record_id)
print(individus_naiss_hospi)

#1.7 Vérifier que 2 echantillons rectal ont été pris 

individus_1_echantillons <- ALARUMTest_DATA_1 %>%
  filter(rectswab_num_adm == 1) %>%
  select(record_id,rectswab_num_adm)
print(individus_1_echantillons)

#1.8 Vérifier qu'il y a max 48h entre l'entrée et la prise d'échantillon

individus_ecart_48h <- ALARUMTest_DATA_1 %>%
  mutate(ecart_heures = as.numeric(difftime(ymd(date_rectswab_adm), ymd(date_arrive_hosp_adm), units = "hours"))) %>%
  filter(ecart_heures > 48) %>%
  select(record_id, date_arrive_hosp_adm, date_rectswab_adm, ecart_heures)
print(individus_ecart_48h)

### 2. Vérifier concordance des dates 

individus_dates_differentes <- ALARUMTest_DATA_1 %>%
  filter(!(date_arrive_hosp_adm == date_consent_adm & date_consent_adm == date_adm)) %>%
  select(record_id, date_arrive_hosp_adm, date_consent_adm, date_adm)

### 3. Vérifier les valeurs abérrantes potentielles

axilla_temp_nopossible <- ALARUMTest_DATA_1 %>%
  filter(axilla_temp_adm > 43) %>%
  select(record_id, axilla_temp_adm)
print(axilla_temp_nopossible)

taille_nopossible <- ALARUMTest_DATA_1 %>%
  filter(length_adm > 210) %>%
  select(record_id, length_adm)
print(taille_nopossible)

poids_nopossible <- ALARUMTest_DATA_1 %>%
  filter(weight_adm > 150) %>%
  select(record_id, weight_adm)
print(poids_nopossible )

table_taille_age <- ALARUMTest_DATA_1 %>%
  select(record_id, ageyrs_adm, length_adm) %>%
  arrange(ageyrs_adm)
print(table_taille_age)

### 4. Facteurs pouvant influencer

individus_hospitalisé <- ALARUMTest_DATA_1 %>%
  filter(prev_adm_hosp_adm == 1) %>%
  select(record_id)
print(individus_diagnostic_inconnu)

