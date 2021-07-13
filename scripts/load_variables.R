library(tidyverse)


# Inspections -------------------------------------------------------------
inspect_n <- df_inspect %>% nrow()
inspect_proc_n <- nrow(df_324)
inspect_earliest <- min(df_324$date, na.rm = TRUE)
inspect_latest <- max(df_324$date, na.rm = TRUE)

facility_n <- df_inspect %>% 
  distinct(facility) %>% arrange(facility) %>% nrow()

# Standards ---------------------------------------------------------------
df_standards <- df_324 %>%
  select(facility, standards) %>%
  group_by(standards) %>% 
  summarise(count_by_standard = length(standards),
            percent_count = round((count_by_standard/inspect_proc_n)*100, digits = 2)
  ) %>% 
  arrange(desc(count_by_standard)) %>% 
  ungroup()


nds <- pull(df_standards[2,2] + df_standards[3,2] + df_standards[5,2])
nds_perc <- pull(df_standards[2,3] + df_standards[3,3] + df_standards[5,3])
pbnds <- pull(df_standards[1,2] + df_standards[4,2])
pbnds_perc <- pull(df_standards[1,3] + df_standards[4,3])

# Rating ------------------------------------------------------------------
df_rating <- df_324 %>% 
  select(facility, recommended_rating) %>% 
  group_by(recommended_rating) %>% 
  summarise(count_by_rating = length(recommended_rating),
            percent_count = round((count_by_rating/inspect_proc_n)*100, digits = 2)
  ) %>% 
  arrange(desc(count_by_rating)) %>% 
  ungroup()


df_rating <- df_324 %>% 
  select(facility, recommended_rating) %>% 
  group_by(recommended_rating) %>% 
  summarise(count_by_rating = length(recommended_rating),
            percent_count = round((count_by_rating/inspect_proc_n)*100, digits = 2)
  ) %>% 
  arrange(desc(count_by_rating)) %>% 
  ungroup()

pass <- pull(df_rating[1,2]+df_rating[2,2])
pass_perc <- pull(df_rating[1,3]+df_rating[2,3])

no_pass <- pull(df_rating[4,2]+df_rating[5,2])
no_pass_perc <- pull(df_rating[4,3]+df_rating[5,3])

# Assaults ----------------------------------------------------------------

assault_sum <- aggregate(assault_count~assault_type, df_assaults, sum)[5,2]


# Disciplinary ------------------------------------------------------------
disciplinary_sum <- aggregate(disciplinary_count~disciplinary_type, df_discipline, sum)[6,2]
disciplinary_guilty_sum <- aggregate(disciplinary_count~disciplinary_type, df_discipline, sum)[2,2]
disciplinary_guilty_perc <- round(disciplinary_guilty_sum/disciplinary_sum*100, digits = 2)

disciplinary_appeal <- aggregate(disciplinary_count~disciplinary_type, df_discipline, sum)[3,2]
disciplinary_in_favor_detained_sum <- aggregate(disciplinary_count~disciplinary_type, df_discipline, sum)[4,2]
disciplinary_60_days <- aggregate(disciplinary_count~disciplinary_type, df_discipline, sum)[5,2]
# Solitary ----------------------------------------------------------------
segregation_sum <- aggregate(segregation_count~segregation_type, df_solitary, sum)[5,2]

segregation_admin_sum <- aggregate(segregation_count~segregation_type, df_solitary, sum)[1,2]
segregation_admin_perc <- round(segregation_admin_sum/segregation_sum*100, digits = 2)

segregation_disciplinary_sum <- aggregate(segregation_count~segregation_type, df_solitary, sum)[2,2]
segregation_disciplinary_perc <- round(segregation_disciplinary_sum/segregation_sum*100, digits = 2)

segregation_medical_sum <- aggregate(segregation_count~segregation_type, df_solitary, sum)[3,2]
segregation_medical_perc <- round(segregation_medical_sum/segregation_sum*100, digits = 2)

segregation_mental_health_sum <- aggregate(segregation_count~segregation_type, df_solitary, sum)[4,2]
segregation_mental_health_perc <- round(segregation_mental_health_sum/segregation_sum*100, digits = 2)


# Use of Force ------------------------------------------------------------
use_force_sum <- aggregate(use_of_force_count~use_of_force_type, df_force, sum)[8,2]

immediate_use_force_sum <- aggregate(use_of_force_count~use_of_force_type, df_force, sum)[1,2]
immediate_use_force_perc <- round(immediate_use_force_sum/use_force_sum*100, digits = 2)

calculated_use_force_sum <- aggregate(use_of_force_count~use_of_force_type, df_force, sum)[2,2]
calculated_use_force_perc <- round(calculated_use_force_sum/use_force_sum*100, digits = 2)

use_force_chemical_agents_sum <- aggregate(use_of_force_count~use_of_force_type, df_force, sum)[3,2]
use_force_chemical_agents_perc <- round(use_force_chemical_agents_sum/use_force_sum*100, digits = 2)

use_force_non_lethal_weapons_sum <- aggregate(use_of_force_count~use_of_force_type, df_force, sum)[4,2]
use_force_non_lethal_weapons_perc <- round(use_force_non_lethal_weapons_sum/use_force_sum*100, digits = 2)

use_force_4_5_point_sum <- aggregate(use_of_force_count~use_of_force_type, df_force, sum)[5,2]
use_force_4_5_point_perc <- round(use_force_4_5_point_sum/use_force_sum*100, digits = 2)

use_force_serious_injury_sum <- aggregate(use_of_force_count~use_of_force_type, df_force, sum)[6,2]
use_force_serious_injury_perc <- round(use_force_serious_injury_sum/use_force_sum*100, digits = 2)

use_force_strip_search_sum <- aggregate(use_of_force_count~use_of_force_type, df_force, sum)[7,2]
use_force_strip_search_perc <- round(use_force_strip_search_sum/use_force_sum*100, digits = 2)

# Sexual Abuse and Assault ------------------------------------------------
sex_abuse_alleg_sum <- aggregate(sexual_abuse_allegations_count~sexual_abuse_allegations_type, df_sex_alleg, sum)[6,2]
sex_abuse_sub_sum <- aggregate(sexual_abuse_substantiated_count~sexual_abuse_substantiated_type, df_sex_alleg_sub, sum)
infectious_disease_conf_sum <- aggregate(infectious_disease_count~infectious_disease_type, df_infectious, sum)[2,2]
# Medical -----------------------------------------------------------------

# Medical Observation
medical_mental_observation_sum <- aggregate(medical_mental_observation_count~medical_mental_observation_type, df_observation, sum)[3,2]
medical_observ_sum <- aggregate(medical_mental_observation_count~medical_mental_observation_type, df_observation, sum)[1,2]
medical_mental_observation_sum <- aggregate(medical_mental_observation_count~medical_mental_observation_type, df_observation, sum)[2,2]

# Referrals
medical_referral_sum <- aggregate(referral_count~referral_type, df_referrals, sum)[5,2]
medical_emergency_hospital_transfer_sum <- aggregate(referral_count~referral_type, df_referrals, sum)[2,2]
medical_off_site_mental_health_admission_sum <- aggregate(referral_count~referral_type, df_referrals, sum)[4,2]

# Sick call
sick_call_requests_sum <- aggregate(sick_call_count~sick_call_type, df_sick_call, sum)[1,2]
sick_call_encounters_sum <- aggregate(sick_call_count~sick_call_type, df_sick_call, sum)[2,2]

# Suicide
suicide_self_harm_attempts_watch_sum <- aggregate(suicide_count~suicide_type, df_suicide, sum)[3,2]
suicide_self_harm_sum <- aggregate(suicide_count~suicide_type, df_suicide, sum)[1,2]

suicide_watch_mental_health_obser_sum <- aggregate(suicide_count~suicide_type, df_suicide, sum)[2,2]

# n facilities w hunger strikes
hunger_strike_facility_n <- df_hunger_strike %>% 
  filter(hunger_strikes >0) %>% 
  distinct(facility) %>% 
  nrow()

# Sum of hunger strikes
hunger_strike_sum <- sum(df_324_inc$hunger_strikes, na.rm = TRUE)

# Cause of Death ----------------------------------------------------------



