# Match drugs to their names for readability

Sys.setenv(R_CONFIG_FILE=here::here("population_report/config.yml"))
database_file <- config::get("vocab db")
ranked_drugs_file <- here::here(config::get("ranked drug list"))
ranked_named_drugs_file <- here::here(config::get("ranked named drug list"))

# Load drug list
ranked_drugs <- read_csv(ranked_drugs_file)

# Open DB and get names
con <- DBI::dbConnect(RSQLite::SQLite(), database_file, flags=RSQLite::SQLITE_RO)

concept <- tbl(con, 'concept') %>% select(concept_id, concept_name)

# Join with names
ranked_named_drugs <- left_join(ranked_drugs, concept, by=c('drug_concept_id'='concept_id'), copy=TRUE)

# Save
write_csv(ranked_named_drugs, ranked_named_drugs_file)

