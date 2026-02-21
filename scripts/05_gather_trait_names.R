#####################################################################
# Extract all trait names across datasets

library(tidyverse)
library(data.table)

f <- list.files("/scratch/project_2003061/trait_datasets", pattern = "traits_prepared.csv", 
                full.names = TRUE, recursive = TRUE)

# TRY
db <- "TRY"
d <- fread(f[grepl(db,f)])
m <- fread(gsub("traits_prepared.csv","measurement_information.csv",f[grepl(db,f)]))
d <- left_join(d, m %>% select(TraitID, TraitName) %>% distinct()) %>% 
  rename(trait_name = TraitName) %>% 
  filter(!is.na(StdValue))

d1 <- d %>% 
  group_by(WFO_species, trait_name) %>% 
  count() %>% 
  group_by(trait_name) %>% 
  summarise(n_species = n(),
            n_obs = sum(n)) %>% 
  mutate(database = str_split_i(f[grepl(db,f)], "/", 5))

# BIEN
db <- "BIEN"
d <- fread(f[grepl(db,f)])

d2 <- d %>% 
  group_by(WFO_species, trait_name) %>% 
  count() %>% 
  group_by(trait_name) %>% 
  summarise(n_species = n(),
            n_obs = sum(n)) %>% 
  mutate(database = str_split_i(f[grepl(db,f)], "/", 5))

# FRED
db <- "FRED"
d <- fread(f[grepl(db,f)]) %>% 
  rename(trait_name = Trait)

d3 <- d %>% 
  group_by(WFO_species, trait_name) %>% 
  count() %>% 
  group_by(trait_name) %>% 
  summarise(n_species = n(),
            n_obs = sum(n)) %>% 
  mutate(database = str_split_i(f[grepl(db,f)], "/", 5))

# GIFT
db <- "GIFT"
d <- fread(f[grepl(db,f)]) %>% 
  rename(trait_name = Trait2)

d4 <- d %>% 
  group_by(WFO_species, trait_name) %>% 
  count() %>% 
  group_by(trait_name) %>% 
  summarise(n_species = n(),
            n_obs = sum(n)) %>% 
  mutate(database = str_split_i(f[grepl(db,f)], "/", 5))

# GRoot
db <- "GRoot"
d <- fread(f[grepl(db,f)]) %>% 
  rename(trait_name = Trait)

d5 <- d %>% 
  group_by(WFO_species, trait_name) %>% 
  count() %>% 
  group_by(trait_name) %>% 
  summarise(n_species = n(),
            n_obs = sum(n)) %>% 
  mutate(database = str_split_i(f[grepl(db,f)], "/", 5))

# LEDA
db <- "LEDA"
d <- fread(f[grepl(db,f)]) %>% 
  rename(trait_name = Trait)

d6 <- d %>% 
  group_by(WFO_species, trait_name) %>% 
  count() %>% 
  group_by(trait_name) %>% 
  summarise(n_species = n(),
            n_obs = sum(n)) %>% 
  mutate(database = str_split_i(f[grepl(db,f)], "/", 5))

# Niittynen
db <- "Niittynen"
d <- fread(f[grepl(db,f)]) %>% 
  rename(trait_name = Trait)

d7 <- d %>% 
  group_by(WFO_species, trait_name) %>% 
  count() %>% 
  group_by(trait_name) %>% 
  summarise(n_species = n(),
            n_obs = sum(n)) %>% 
  mutate(database = str_split_i(f[grepl(db,f)], "/", 5))

# TR8
db <- "TR8"
d <- fread(f[grepl(db,f)]) %>% 
  rename(trait_name = name)

d8 <- d %>% 
  group_by(WFO_species, trait_name) %>% 
  count() %>% 
  group_by(trait_name) %>% 
  summarise(n_species = n(),
            n_obs = sum(n)) %>% 
  mutate(database = str_split_i(f[grepl(db,f)], "/", 5))

# TTT
db <- "TTT"
d <- fread(f[grepl(db,f)]) %>% 
  rename(trait_name = Trait)

d9 <- d %>% 
  group_by(WFO_species, trait_name) %>% 
  count() %>% 
  group_by(trait_name) %>% 
  summarise(n_species = n(),
            n_obs = sum(n)) %>% 
  mutate(database = str_split_i(f[grepl(db,f)], "/", 5))

# Tyler
db <- "Tyler"
d <- fread(f[grepl(db,f)]) %>% 
  rename(trait_name = trait)

d10 <- d %>% 
  group_by(WFO_species, trait_name) %>% 
  count() %>% 
  group_by(trait_name) %>% 
  summarise(n_species = n(),
            n_obs = sum(n)) %>% 
  mutate(database = str_split_i(f[grepl(db,f)], "/", 5))

# The rest
d <- bind_rows(fread(f[grepl("Baruah",f)]) %>% 
                 select(WFO_species, Trait) %>% 
                 mutate(database = "Baruah"),
               fread(f[grepl("Majekova",f)]) %>% 
                 select(WFO_species, Trait) %>% 
                 mutate(database = "Majekova"),
               fread(f[grepl("Mudrak",f)]) %>% 
                 select(WFO_species, Trait) %>% 
                 mutate(database = "Mudrak"),
               fread(f[grepl("Tichy",f)]) %>% 
                 pivot_longer(cols = ELLENBERG_LIGHT:ELLENBERG_SALINITY, names_to = "Trait", values_to = "Value") %>% 
                 select(WFO_species, Trait) %>% 
                 mutate(database = "Tichy"),
               fread(f[grepl("Zanne",f)]) %>% 
                 select(WFO_species, Trait) %>% 
                 mutate(database = "Zanne"),
               fread(f[grepl("Zuijlen",f)]) %>% 
                 pivot_longer(cols = leaf_N:LDMC, names_to = "Trait", values_to = "Value") %>% 
                 select(WFO_species, Trait) %>% 
                 mutate(database = "Zuijlen"),
               fread(f[grepl("Drevojan",f)]) %>% 
                 pivot_longer(cols = Phanerophyte:`Herbaceous liana`, names_to = "Trait", values_to = "Value") %>% 
                 select(WFO_species, Trait) %>% 
                 mutate(database = "Drevojan"),
               fread(f[grepl("Lososova",f)]) %>% 
                 mutate(across(height:dispersal_anthropogenic, as.character)) %>% 
                 pivot_longer(cols = height:dispersal_anthropogenic, names_to = "Trait", values_to = "Value") %>% 
                 select(WFO_species, Trait) %>% 
                 mutate(database = "Lososova"),
               fread(f[grepl("Midolo",f)]) %>% 
                 mutate(across(N_EUNIS_habitats:soil_disturbance_indicator, as.character)) %>% 
                 pivot_longer(cols = N_EUNIS_habitats:soil_disturbance_indicator, names_to = "Trait", values_to = "Value") %>% 
                 select(WFO_species, Trait) %>% 
                 mutate(database = "Midolo"),
               fread(f[grepl("Tesitel",f)]) %>% 
                 select(WFO_species, Trait) %>% 
                 mutate(database = "Tesitel")) %>% 
  rename(trait_name = Trait)

d11 <- d %>% 
  group_by(database, WFO_species, trait_name) %>% 
  count() %>% 
  group_by(database, trait_name) %>% 
  summarise(n_species = n(),
            n_obs = sum(n))

# Combine
all <- bind_rows(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11) %>% 
  relocate(database)

all %>% writexl::write_xlsx("output/trait_names.xlsx")
