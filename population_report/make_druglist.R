# Get some statistics about the drugs in our records

library(tidyverse)

# Constants and configuration
Sys.setenv(R_CONFIG_FILE="population_report/config.yml")
database_file <- config::get("database")
aux_path <- config::get("aux data")
ics_files <- config::get("tables of ICS drugs")
query_limit <- config::get("query_limit")
output <- config::get("table of ICS drugs in EHR")

# Connect to database
con <- DBI::dbConnect(RSQLite::SQLite(), database_file, flags=RSQLite::SQLITE_RO)

# Get list of unique drugs in DB
ehr_drugs <- tbl(con, 'drug_exposure') %>% distinct(drug_concept_id)

# Enforce limits and load into memory
ehr_drugs <-
  if (!is.null(query_limit) && query_limit > 0){
    ehr_drugs %>% head(query_limit)
  } else {
    ehr_drugs
  } %>% collect()

# Make unified list of drugs to search for
ics_drugs <- imap_dfr(
  ics_files,
  function (filename, drug) {
    read_tsv(file.path(aux_path, filename)) %>% mutate(Drug = drug)
  }
)

# Find drugs
ehr_ics <- inner_join(ics_drugs, ehr_drugs, by=c("Id"="drug_concept_id"))

# Save result
write_tsv(ehr_ics, output)