# Get some statistics about our patients

library(tidyverse)
library(lubridate)
library(ggplot2)

# Constants and configuration
Sys.setenv(R_CONFIG_FILE="population_report/config.yml")
database_file <- config::get("database")
asthma_conditions_file <- config::get("asthma_conditions_file")
query_limit <- config::get("query_limit")
concept_counts_file <- config::get("counts of asthma concepts in EHR")
first_concept_counts_file <- config::get("counts of first occurrences of asthma concepts in EHR")
asthma_patient_ages_file <- config::get("patient ages at first diagnosis")

# Validate required configuration parameters
if (is.null(database_file)) stop("Did you specify a database in the config file?")
if (is.null(asthma_conditions_file)) stop("Did you specify an asthma conditions file in the config file?")
if (is.null(concept_counts_file)) stop("Did you specify a concept counts output file in the config file?")
if (is.null(first_concept_counts_file)) stop("Did you specify a first concept counts output file in the config file?")

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
) %>%
  collect() %>%
  mutate(the_date=as.Date(the_date, format='%Y-%m-%d'))

# Count unique concepts
asthma_concept_counts <- asthma_records %>% group_by(concept_id) %>% summarize(count=n()) %>% ungroup()
sorted_named_concept_counts <- inner_join(asthma_concept_counts, asthma_concept_names, by='concept_id') %>%
  arrange( desc(count) )
write_csv(sorted_named_concept_counts, path=concept_counts_file)

# Find and count the first occurrence for each observation type for each patient
first_asthma_records <- asthma_records %>% group_by(person_id) %>% top_n(-1, the_date) %>% ungroup()
first_asthma_counts <- first_asthma_records %>% group_by(concept_id) %>% summarize(count=n()) %>% ungroup()
sorted_named_first_concept_counts <- inner_join(first_asthma_counts, asthma_concept_names, by='concept_id') %>%
  arrange( desc(count) )
write_csv(sorted_named_first_concept_counts, path=first_concept_counts_file)

# Figure out patient ages at first diagnosis
asthma_patient_list <- first_asthma_records %>% distinct(person_id) %>% pull(person_id) # Get subset of patients
person <- tbl(con, 'person') # We need the person table for this
birthdates <- person %>% filter(person_id %in% asthma_patient_list) %>% collect() %>%
  transmute(person_id, birthdate=as.Date(ISOdate(year=year_of_birth, month=month_of_birth, day=day_of_birth)))
first_asthma_records_w_ages <-
  inner_join(first_asthma_records, birthdates, by='person_id') %>%
  left_join(asthma_concept_names, by='concept_id') %>%
  mutate(age=time_length(the_date-birthdate, "years"))

write_csv(first_asthma_records_w_ages, asthma_patient_ages_file)
