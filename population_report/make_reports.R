# Get some statistics about our patients

library(tidyverse)

# Constants (TODO: Replace with reading from config)
hegiht_concept_id <- 3036277
weight_concept_id <- 3025315
database_file <- NA
asthma_conditions_file <- 'aux_data/asthma_codes.tsv'

# Load files
asthma_codes <- read_tsv(asthma_conditions_file)

# Helper table
asthma_concept_names <- asthma_codes %>% select(concept_id=Id, name=Name)

# Connect to database
con <- DBI::dbConnect(RSQLite::SQLite(), path=database_file, flags=RSQLite::SQLITE_RO)

condition_occurrence <- tbl(con, 'condition_occurence')
observation <- tbl(con, 'observation')

# Separate asthma codes into conditions and observations
asthma_conditions <- asthma_codes %>% filter(Domain == "Condition") %>% pull(Id)
asthma_observations <- asthma_codes %>% filter(Domain == "Observation") %>% pull(Id)

# Find the asthma codes in the EHR
ehr_asthma_conditions <- condition_occurrence %>% filter(condition_concept_id %in% asthma_conditions)
ehr_asthma_observations <- observation %>% filter(observation %in% asthma_observations)

# Merge into one list
asthma_records <- union_all(
  ehr_asthma_conditions %>% select(person_id, concept_id=condition_concept_id, date=condition_start_date),
  ehr_asthma_observations %>% select(person_id, concept_id=observation_concept_id, date=observation_date)
)

# Count unique concepts
asthma_concept_counts <- asthma_records %>% group_by(concept_id) %>% summarize(count=n()) %>% ungroup()
write_csv(inner_join(asthma_concept_counts, asthma_concept_names, by='concept_id'), file='population_report/concept_counts.csv')

# Find the first occurrence for each observation type for each patient