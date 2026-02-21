#####################################################################
# Preprocess the Tyler et al. dataset

library(tidyverse)
library(data.table)
library(readxl)
library(parallel)

df <- read_xlsx("/scratch/project_2003061/trait_datasets/Tyler/1-s2.0-S1470160X20308621-mmc1.xlsx") %>% as.data.table() %>% 
  select(-`...35`)

df <- df %>% select(`Scientific name`,`Dyntaxa ID number`,`Nectar production`:`38 Subalpine Betula forest`)

df <- df  %>% 
  mutate(across(`Nectar production`:`38 Subalpine Betula forest`, as.character)) %>% 
  mutate(`Scientific name` = gsub(" sect. ", " ", `Scientific name`)) %>% 
  pivot_longer(cols = `Nectar production`:`38 Subalpine Betula forest`, names_to = "trait", values_to = "value") %>% 
  filter(!is.na(value) & value != "") %>% as.data.table() %>% 
  rename(OriginalName = `Scientific name`,
         dyntaxa_id = `Dyntaxa ID number`)

library(googlesheets4)
fn <- read_sheet("https://docs.google.com/spreadsheets/d/1qgBOjM_941h4zm0foy83PyT6Og0ocWlDOUHXWmdlWIA/edit?gid=211021139#gid=211021139", col_types = "c") %>% 
  mutate(OriginalName = str_squish(paste(fn$taxon, ifelse(is.na(fn$author), "", fn$author)))) %>% 
  select(OriginalName, WFO_species)

df2 <- inner_join(df, fn %>% distinct()) %>% 
  filter(!is.na(WFO_species)) %>% 
  relocate(WFO_species)

df2 %>% select(-OriginalName, -dyntaxa_id) %>% write_csv("/scratch/project_2003061/trait_datasets/Tyler/traits_prepared.csv")
