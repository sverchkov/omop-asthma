# Get some statistics about our patients

library(tidyverse)

# Constants and configuration
Sys.setenv(R_CONFIG_FILE="population_report/config.yml")
database_file <- config::get("database")
asthma_conditions_file <- config::get("asthma_conditions_file")
query_limit <- config::get("query_limit")

# Load files
asthma_codes <- read_tsv(asthma_conditions_file)

# Helper table
asthma_concept_names <- asthma_codes %>% select(concept_id=Id, name=Name)

# Connect to database
con <- DBI::dbConnect(RSQLite::SQLite(), database_file, flags=RSQLite::SQLITE_RO)

condition_occurrence <- tbl(con, 'condition_occurrence')
observation <- tbl(con, 'observation')

# Enforce limits
if( !is.null(query_limit) && query_limit > 0 ){
  condition_occurrence <- condition_occurrence %>% head(query_limit)
  observation <- observation %>% head(query_limit)
}

# Separate asthma codes into conditions and observations
asthma_conditions <- asthma_codes %>% filter(Domain == "Condition") %>% pull(Id)
asthma_observations <- asthma_codes %>% filter(Domain == "Observation") %>% pull(Id)

# Find the asthma codes in the EHR
ehr_asthma_conditions <- condition_occurrence %>% filter(condition_concept_id %in% asthma_conditions)
ehr_asthma_observations <- observation %>% filter(observation_concept_id %in% asthma_observations)

# Merge into one list
asthma_records <- union_all(
  ehr_asthma_conditions %>% select(person_id, concept_id=condition_concept_id, the_date=condition_start_date),
  ehr_asthma_observations %>% select(person_id, concept_id=observation_concept_id, the_date=observation_date)
)

# Count unique concepts
asthma_concept_counts <- asthma_records %>% group_by(concept_id) %>% summarize(count=n()) %>% ungroup() %>% collect()
write_csv(inner_join(asthma_concept_counts, asthma_concept_names, by='concept_id'), path='population_report/concept_counts.csv')

# Find the first occurrence for each observation type for each patient