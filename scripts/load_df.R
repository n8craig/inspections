library(tidyverse)


# Standards DF ------------------------------------------------------------
# See load_variables.R


# Rating DF ---------------------------------------------------------------
# See load_variables.R


# Inspection Count by Facility --------------------------------------------

df_facility_n_inspect <- df_324 %>% 
  group_by(facility, fac_operator) %>% 
  summarise(n_inspections = n()) %>% 
  ungroup() 

# Assault DF --------------------------------------------------------------
df_assaults <- df_324_inc %>%
  # Subset the df to only the used cols
  select(id, facility, date,
         detainee_physical_assault_on_staff_with_serious_injury:
           detainee_on_detainee_physical_assault_fight_with_no_serious_injury
  ) %>% 
  # Need the rowwise function to compute a row-at-a-time
  # in the following mutate function
  rowwise(id) %>% 
  # Create a new total column
  mutate(total_assaults = sum(c_across(
    detainee_physical_assault_on_staff_with_serious_injury:
      detainee_on_detainee_physical_assault_fight_with_no_serious_injury
  ), na.rm = TRUE)) %>% 
  # Call a range of table columns and pivot long
  pivot_longer(.,
               cols=  detainee_physical_assault_on_staff_with_serious_injury:total_assaults,
               names_to = "assault_type",
               values_to = "assault_count") %>% 
  # Remove NA Values
  drop_na() %>% 
  # Explicitly define factor levels
  mutate(assault_type = factor(assault_type, levels = c(
    "detainee_physical_assault_on_staff_with_serious_injury",
    "detainee_physical_assault_on_staff_with_no_serious_injury",
    "detainee_on_detainee_physical_assault_fight_with_serious_injury",
    "detainee_on_detainee_physical_assault_fight_with_no_serious_injury",
    "total_assaults"
  )))


# Disciplinary DF ---------------------------------------------------------
df_discipline <- df_324_inc %>%
  # Subset the df to only the used cols
  select(id, facility, date,
         disciplinary_infractions:
           sanctions_over_60_days
  ) %>% 
  # Need the rowwise function to compute a row-at-a-time
  # in the following mutate function
  rowwise(id) %>% 
  # Create a new total column
  mutate(total_disciplinary = sum(c_across(
    disciplinary_infractions:
      sanctions_over_60_days
  ), na.rm = TRUE)) %>% 
  # Call a range of table columns and pivot long
  pivot_longer(.,
               cols=  disciplinary_infractions:total_disciplinary,
               names_to = "disciplinary_type",
               values_to = "disciplinary_count") %>%
  # Remove NA values
  drop_na() %>% 
  # Explicitly set factor levels
  mutate(disciplinary_type = factor(disciplinary_type, levels =c(
    "disciplinary_infractions",
    "disciplinary_infractions_guilty",
    "disciplinary_appeals",
    "disciplinary_appeals_found_in_favor_of_detainee",
    "sanctions_over_60_days",
    "total_disciplinary"
  )))


# Solitary DF -------------------------------------------------------------

df_solitary <- df_324_inc %>%
  
  # Select a subset of columns to work with
  select(id,
         facility,
         date,
         detainees_placed_in_administrative_segregation:
           detainees_placed_in_segregation_for_mental_health_reasons) %>% 
  
  # Need the rowwise function to compute a row-at-a-time
  # in the following mutate function
  rowwise(id) %>% 
  
  # Create new total column
  mutate(total_segregation = sum(c_across(detainees_placed_in_administrative_segregation:
                                            detainees_placed_in_segregation_for_mental_health_reasons), na.rm = TRUE)) %>% 
  
  # Tidy
  pivot_longer(.,
               cols= detainees_placed_in_administrative_segregation:
                 total_segregation,
               names_to = "segregation_type",
               values_to = "segregation_count") %>% 
  
  # Remove NA
  drop_na() %>% 
  
  # Explicitly set factor levels
  mutate(segregation_type = factor(segregation_type, levels = c(
    "detainees_placed_in_administrative_segregation",
    "detainees_placed_in_disciplinary_segregation",
    "detainees_placed_in_segregation_for_medical_reasons",
    "detainees_placed_in_segregation_for_mental_health_reasons",
    "total_segregation"
  )))


# Use of Force DF ---------------------------------------------------------
df_force <- df_324_inc %>%
  
  # Subset the df to only the used cols
  select(id, facility, inspection_date, date,
         immediate_use_of_force_incidents:
           strip_searches
  ) %>% 
  
  # Need the rowwise function to compute a row-at-a-time
  # in the following mutate function
  rowwise(id) %>% 
  
  # Create a new total column
  mutate(total_use_of_force = sum(c_across(
    immediate_use_of_force_incidents:
      strip_searches
  ), na.rm = TRUE)) %>% 
  
  # Call a range of table columns and pivot long
  pivot_longer(.,
               cols=  immediate_use_of_force_incidents:total_use_of_force,
               names_to = "use_of_force_type",
               values_to = "use_of_force_count") %>%
  
  # Remove NA values
  drop_na() %>% 
  
  # Explicitly define factors
  mutate(use_of_force_type = factor(use_of_force_type, levels = c(
    "immediate_use_of_force_incidents",
    "calculated_use_of_force_incidents",
    "uses_of_force_with_chemical_agents",
    "incidents_where_non_lethal_weapons_were_used",
    "number_of_times_4_5_point_restraints_were_used",
    "use_of_force_with_serious_injury",
    "strip_searches",
    "total_use_of_force"
  )))


# Sexual Abuse and Assault Allegations DF ---------------------------------

df_sex_alleg <- df_324_inc %>%
  
  # Subset the df to only the used cols
  select(id, facility, date,
         sexual_abuse_allegations_detainee_on_detainee:
           sexual_abuse_allegations_detainee_on_staff_contractor_volunteer
  ) %>% 
  
  # Need the rowwise function to compute a row-at-a-time
  # in the following mutate function
  rowwise(id) %>% 
  
  # Create a new total column
  mutate(total_sexual_abuse_allegations = sum(c_across(
    sexual_abuse_allegations_detainee_on_detainee:
      sexual_abuse_allegations_detainee_on_staff_contractor_volunteer
  ), na.rm = TRUE)) %>% 
  
  # Call a range of table columns and pivot long
  pivot_longer(.,
               cols=  sexual_abuse_allegations_detainee_on_detainee:total_sexual_abuse_allegations,
               names_to = "sexual_abuse_allegations_type",
               values_to = "sexual_abuse_allegations_count") %>%
  
  # Remove NA
  drop_na() %>% 
  
  # Explicitly define factor levels
  mutate(sexual_abuse_allegations_type = factor(sexual_abuse_allegations_type, levels = c(
    "sexual_abuse_allegations_detainee_on_detainee",
    "sexual_abuse_allegations_inmate_on_detainee",
    "sexual_abuse_allegations_detainee_on_inmate",
    "sexual_abuse_allegations_staff_contractor_volunteer_on_detainee",
    "sexual_abuse_allegations_detainee_on_staff_contractor_volunteer",
    "total_sexual_abuse_allegations"
  )))


# Sexual Abuse and Assault Substantiated DF--------------------------------

df_sex_alleg_sub <- df_324_inc %>%
  
  # Subset the df to only the used cols
  select(id, facility, date,
         sexual_abuse_allegations_substantiated_detainee_on_detainee:
           sexual_abuse_allegations_substantiated_detainee_on_staff_contractor_volunteer
  ) %>% 
  
  # Need the rowwise function to compute a row-at-a-time
  # in the following mutate function
  rowwise(id) %>% 
  
  # Create a new total column
  mutate(total_sexual_abuse_allegations_substantiated = sum(c_across(
    sexual_abuse_allegations_substantiated_detainee_on_detainee:
      sexual_abuse_allegations_substantiated_detainee_on_staff_contractor_volunteer
  ), na.rm = TRUE)) %>% 
  
  # Call a range of table columns and pivot long
  pivot_longer(.,
               cols=  sexual_abuse_allegations_substantiated_detainee_on_detainee:total_sexual_abuse_allegations_substantiated,
               names_to = "sexual_abuse_substantiated_type",
               values_to = "sexual_abuse_substantiated_count") %>%
  
  # Remove NA values
  drop_na() %>% 
  
  # Explicitly set factor levels
  mutate(sexual_abuse_substantiated_type= factor(sexual_abuse_substantiated_type, levels =c(
    "sexual_abuse_allegations_substantiated_detainee_on_detainee",
    "sexual_abuse_allegations_substantiated_inmate_on_detainee",
    "sexual_abuse_allegations_substantiated_detainee_on_inmate",
    "sexual_abuse_allegations_substantiated_staff_contractor_volunteer_on_detainee",
    "sexual_abuse_allegations_substantiated_detainee_on_staff_contractor_volunteer",
    "total_sexual_abuse_allegations_substantiated"
  )))


# Medical Observation DF --------------------------------------------------

df_observation <- df_324_inc %>%
  
  # Subset the df to only the used cols
  select(id, facility, date,
         detainees_in_medical_observation:
           detainees_in_mental_health_observation
  ) %>% 
  
  # Need the rowwise function to compute a row-at-a-time
  # in the following mutate function
  rowwise(id) %>% 
  
  # Create a new total column
  mutate(total_mental_medical_observation = sum(c_across(
    detainees_in_medical_observation:
      detainees_in_mental_health_observation
  ), na.rm = TRUE)) %>% 
  
  # Call a range of table columns and pivot long
  pivot_longer(.,
               cols=  detainees_in_medical_observation:total_mental_medical_observation,
               names_to = "medical_mental_observation_type",
               values_to = "medical_mental_observation_count") %>% 
  # Remove NA values
  drop_na() %>% 
  
  # Explicitly set factor levels
  mutate(medical_mental_observation_type = factor(medical_mental_observation_type, levels = c(
    "detainees_in_medical_observation",
    "detainees_in_mental_health_observation",
    "total_mental_medical_observation"
  )))


# Medical Infectious Disease DF -------------------------------------------

df_infectious <-  df_324_inc %>% 
  
  # Subset the df to only the used cols
  select(id, facility, date,
         infectious_disease_reported:
           infections_disease_confirmed
  ) %>% 
  
  # Need the rowwise function to compute a row-at-a-time
  # in the following mutate function
  rowwise(id) %>% 
  
  # Call a range of table columns and pivot long
  pivot_longer(.,
               cols=  infectious_disease_reported:infections_disease_confirmed,
               names_to = "infectious_disease_type",
               values_to = "infectious_disease_count") %>%
  
  # Remove NA values
  drop_na() %>% 
  
  # Explicitly define factor levels
  mutate(infectious_disease_type = factor(infectious_disease_type, levels = c(
    "infectious_disease_reported",
    "infections_disease_confirmed"
  )))


# Medical Referrals DF ----------------------------------------------------

df_referrals <- df_324_inc %>%
  
  # Subset the df to only the used cols
  select(id, facility, date,
         outside_medical_referrals:
           admissions_to_off_site_hospitals_for_mental_health_reasons
  ) %>% 
  
  # Need the rowwise function to compute a row-at-a-time
  # in the following mutate function
  rowwise(id) %>% 
  
  # Create a new total column
  mutate(total_referrals = sum(c_across(
    outside_medical_referrals:
      admissions_to_off_site_hospitals_for_mental_health_reasons
  ), na.rm = TRUE)) %>% 
  
  # Call a range of table columns and pivot long
  pivot_longer(.,
               cols=  outside_medical_referrals:total_referrals,
               names_to = "referral_type",
               values_to = "referral_count") %>%
  
  # Remove NA values
  drop_na() %>% 
  
  # Explicitly define factor levels
  mutate(referral_type= factor(referral_type, levels = c(
    "outside_medical_referrals",
    "detainees_transported_to_off_site_hospitals_for_emergency_care",
    "admissions_to_off_site_hospitals_for_medical_reasons",
    "admissions_to_off_site_hospitals_for_mental_health_reasons",
    "total_referrals"
  )))


# Medical Sick Calls DF ---------------------------------------------------

df_sick_call <- df_324_inc %>%
  
  # Select a subset of columns to work with
  select(id,
         facility,
         date,
         sick_call_requests,
         sick_call_encounters) %>% 
  
  # Need the rowwise function to compute a row-at-a-time
  # in the following mutate function
  rowwise(id) %>% 
  
  # Create new total column
  mutate(total_sick_call = sick_call_requests +
         sick_call_encounters) %>%
  
  # Tidy
  pivot_longer(.,
               cols= sick_call_requests:
                 total_sick_call,
               names_to = "sick_call_type",
               values_to = "sick_call_count") %>% 
  
  # Remove NA
  drop_na() %>% 
  
  # Explicitly set factor levels
  mutate(sick_call_type = factor(sick_call_type, levels = c(
    "sick_call_requests",
    "sick_call_encounters",
    "total_sick_call"
  )))


# Medical Suicide DF ------------------------------------------------------

df_suicide <- df_324_inc %>%
  
  # Select a subset of columns to work with
  select(id,
         facility,
         date,
         suicide_attempts_or_self_harm,
         suicide_watches_constant_watch_mental_health_observation) %>% 
  
  # Need the rowwise function to compute a row-at-a-time
  # in the following mutate function
  rowwise(id) %>% 
  
  # Create new total column
  mutate(total = suicide_attempts_or_self_harm +
           suicide_watches_constant_watch_mental_health_observation) %>% 
  
  # Tidy
  pivot_longer(.,
               cols= suicide_attempts_or_self_harm:
                 total,
               names_to = "suicide_type",
               values_to = "suicide_count") %>% 
  
  # Remove NA
  drop_na() %>% 
  
  # Explicitly set factor levels
  mutate(segregation_type = factor(suicide_type, levels = c(
    "suicide_attempts_or_self_harm",
    "suicide_watches_constant_watch_mental_health_observation",
    "total"
  )))



# Hunger Strike DF --------------------------------------------------------
df_hunger_strike <- df_324_inc %>%
  select(id, facility, date, hunger_strikes) %>% 
  drop_na()

# Suicide DF --------------------------------------------------------------
df_suicide <- df_324_inc %>%
  
  # Select a subset of columns to work with
  select(id,
         facility,
         date,
         suicide_attempts_or_self_harm,
         suicide_watches_constant_watch_mental_health_observation) %>%
  
  # Need the rowwise function to compute a row-at-a-time
  # in the following mutate function
  rowwise(id) %>%
  
  # Create new total column
  mutate(total = suicide_attempts_or_self_harm +
           suicide_watches_constant_watch_mental_health_observation) %>%
  
  # Tidy
  pivot_longer(.,
               cols= suicide_attempts_or_self_harm:
                 total,
               names_to = "suicide_type",
               values_to = "suicide_count") %>%
  
  # Remove NA
  drop_na() %>%
  
  # Explicitly set factor levels
  mutate(suicide_type = factor(suicide_type, levels = c(
    "suicide_attempts_or_self_harm",
    "suicide_watches_constant_watch_mental_health_observation",
    "total"
  )))
