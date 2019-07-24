# Get some statistics about asthma patient drug exposures

library(tidyverse)

# Constants and configuration
Sys.setenv(R_CONFIG_FILE=here::here("population_report/config.yml"))
database_file <- config::get("database")
#aux_path <- config::get("aux data")
#ics_files <- config::get("tables of ICS drugs")
query_limit <- config::get("query_limit")
known_ics_ehr_file <- here::here(config::get("table of ICS drugs in EHR"))
patient_ages_file <- here::here(config::get("patient ages at first diagnosis"))
ranked_drugs_file <- here::here(config::get("ranked drug list"))
min_age_incl <- 0
max_age_excl <- 18

# Connect to database
con <- DBI::dbConnect(RSQLite::SQLite(), database_file, flags=RSQLite::SQLITE_RO)

# Get drug exposures
drug_exposure <- tbl(con, 'drug_exposure')

# Get known drugs
known_drugs <-read_tsv(known_ics_ehr_file)

# Asthma patients of interest
asthma_patients <- read_csv(patient_ages_file) %>%
  filter(age >= min_age_incl, age < max_age_excl)

# Filter exposures to patients of interest
patient_list <- asthma_patients %>% distinct(patient_id) %>% pull(patient_id)
exposures_of_interest <- drug_exposure %>%
  filter(patient_id %in% patient_list) %>%
  group_by(drug_concept_id) %>%
  summarize(count=n()) %>%
  ungroup() %>%
  collect()

annotated_exposures <- left_join(exposures_of_interest, known_drugs, by=c('drug_concept_id'='Id')) %>%
  arrange(count)

write_csv(annotated_exposures, ranked_drugs_file)
