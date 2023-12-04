
rm(list=ls())  # This clears anything currently in the environment
setwd("~/Documents/GitHub/GJ_systematic_review/Global/Data/Rabv_seq")

# packages
library(dplyr)
library(tidyr)
library(ggplot2)

## data 

lac=read.csv("/Users/criseldabautista/Documents/GitHub/GJ_systematic_review/Global/Data/Rabv_seq/LAC_Af_As.csv")

#### summarise the no of sequences per clade per country 
# Group by country and alignment name, then count the number of sequences
clade_summary <- lac %>%
  group_by(sequence.m49_country.display_name, alignment.name) %>%
  summarise(Number_of_Sequences = n()) %>%
  ungroup()

# Pivot the table to have clade names as columns
summary_pivot <- clade_summary %>%
  pivot_wider(names_from = alignment.name, values_from = Number_of_Sequences, values_fill = 0)


### Then separately summarise the total number of seq and unique papers per country

# Replace "-" and blank PubMed IDs with NA
lac$sequence.gb_pubmed_id[lac$sequence.gb_pubmed_id %in% c("-", "")] <- NA

# Group by country and PubMed ID, then count the number of unique PubMed IDs and total number of sequences
summary_df <- lac %>%
  group_by(sequence.m49_country.display_name, sequence.gb_pubmed_id) %>%
  summarise(Number_of_Sequences = n_distinct(sequence.sequenceID)) %>%
  ungroup()

# Filter out rows with NA PubMed IDs
summary_df <- summary_df[complete.cases(summary_df$sequence.gb_pubmed_id),]

# Summarize total number of sequences and unique PubMed IDs per country
summary_country <- summary_df %>%
  group_by(sequence.m49_country.display_name) %>%
  summarise(Number_of_Publication = n(),
            Total_Number_of_Sequences = sum(Number_of_Sequences))

### Then merge the two datasets to get format for mapping
result <- left_join(summary_country, summary_pivot, by = c("sequence.m49_country.display_name" = "sequence.m49_country.display_name"))

print(result)

#Save the result!
write.csv(result, file = "result.csv", row.names = FALSE)

