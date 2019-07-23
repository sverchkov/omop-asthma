# Get some statistics about our patients

library(tidyverse)

# Constants and configuration
Sys.setenv(R_CONFIG_FILE="population_report/config.yml")
database_file <- config::get("database")
asthma_conditions_file <- config::get("asthma_conditions_file")
query_limit <- config::get("query_limit")
asthma_concept_counts <- config::get("counts of asthma concepts in EHR")
asthma_first_concept_counts <- conig::get("counts of first occurrences of asthma concepts in EHR")

# Validate required configuration parameters
if (is.null(database_file)) stop("Did you specify a database in the config file?")
if (is.null(asthma_conditions_file)) stop("Did you specify an asthma conditions file in the config file?")
if (is.null(asthma_concept_counts)) stop("Did you specify a concept counts output file in the config file?")
if (is.null(asthma_first_concept_counts)) stop("Did you specify a first concept counts output file in the config file?")

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
) %>% collect()

# Count unique concepts
asthma_concept_counts <- asthma_records %>% group_by(concept_id) %>% summarize(count=n()) %>% ungroup()
sorted_named_concept_counts <- inner_join(asthma_concept_counts, asthma_concept_names, by='concept_id') %>%
  arrange( desc(count) )
write_csv(sorted_named_concept_counts, path=asthma_concept_counts)

# Find and count the first occurrence for each observation type for each patient
first_asthma_records <- asthma_records %>% group_by(person_id) %>% first(the_date)
sorted_named_first_concept_counts <- inner_join(first_asthma_counts, asthma_concept_names, by='concept_id') %>%
  arrange( desc(count) )
write_csv(sorted_named_first_concept_counts, path=asthma_first_concept_counts)