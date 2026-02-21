#####################################################################
# Preprocess all other trait datasets

library(tidyverse)
library(data.table)
library(readxl)

########################################################################################
# Niittynen

df <- fread("/scratch/project_2003061/trait_datasets/Niittynen/All_vascular_2016-2024_wide_LCVP.csv") %>% 
  filter(!setting %in% c("KIL_SEA")) %>% 
  mutate(species_LCVP = ifelse(species_LCVP == "Arctostaphylos alpinus", "Arctostaphylos alpina", species_LCVP)) %>% 
  mutate(species_LCVP = ifelse(species_LCVP == "Calamagrostis neglecta", "Calamagrostis stricta", species_LCVP)) %>% 
  mutate(species_LCVP = ifelse(species_LCVP == "Cherleria biflora", "Minuartia biflora", species_LCVP)) %>% 
  mutate(species_LCVP = ifelse(species_LCVP == "Oxygraphis glacialis", "Ranunculus glacialis", species_LCVP)) %>% 
  mutate(species_LCVP = ifelse(species_LCVP == "Sabulina rubella", "Minuartia rubella", species_LCVP)) %>% 
  mutate(species_LCVP = ifelse(species_LCVP == "Athyrium alpestre", "Athyrium distentifolium", species_LCVP)) %>% 
  mutate(species_LCVP = ifelse(species_LCVP == "Erigeron acer", "Erigeron acris", species_LCVP))


df <- df %>% 
  select(species_LCVP, area, Lat, Lon, median_height, max_height, d_weight, leaf_area,
         SLA, LDMC) %>% 
  rename(AccSpeciesName = species_LCVP, 
         Latitude = Lat,
         Longitude = Lon,
         dataset = area,
         mean_height = median_height,
         leaf_mass = d_weight) %>% 
  pivot_longer(cols = mean_height:LDMC, names_to = "Trait", values_to = "Value") %>% 
  drop_na()

# Names
library(U.Taxonstand, lib.loc = "/projappl/project_2003061/Rpackages")

all_species <- nameSplit(unique(df$AccSpeciesName)) %>% 
  filter(Rank > 1) %>% 
  select(Submitted_Name_Author, Name, Author) %>% 
  setNames(c("AccSpeciesName","Name","Author")) %>% 
  tibble()%>% 
  filter(!grepl("×",Name),
         !grepl(" X ",Name),
         !grepl(" x ",Name)) %>% 
  distinct() %>% 
  mutate(Name = str_squish(Name),
         Author = str_squish(Author)) %>% 
  mutate(Author = ifelse(Author == "", NA, Author)) %>% 
  distinct()

# the World Flora Online
load("/scratch/project_2003061/trait_datasets/Plants_WFO.rdata")
all <- nameMatch(spList = all_species %>% select(Name,Author), 
                 spSource = database, max.distance = 2,
                 author = TRUE, matchFirst = TRUE)

all %>% 
  filter(is.na(Accepted_SPNAME))

df2 <- left_join(df,
                 bind_cols(all_species %>% select(AccSpeciesName), all) %>% 
                   mutate(Accepted_SPNAME = ifelse(is.na(Accepted_SPNAME), Name_spLev, Accepted_SPNAME)) %>% 
                   select(AccSpeciesName, Submitted_Author, Accepted_SPNAME) %>% distinct() %>% 
                   select(AccSpeciesName, Accepted_SPNAME) %>% 
                   rename(WFO_species = Accepted_SPNAME) %>% 
                   distinct() %>% 
                   drop_na())

df2 %>% 
  filter(is.na(WFO_species)) %>% pull(AccSpeciesName) %>% unique()

df2 <- df2 %>% 
  filter(!is.na(WFO_species)) %>% 
  relocate(WFO_species)

library(googlesheets4)
fn <- read_sheet("https://docs.google.com/spreadsheets/d/1qgBOjM_941h4zm0foy83PyT6Og0ocWlDOUHXWmdlWIA/edit?gid=211021139#gid=211021139", col_types = "c")

edited <- inner_join(df2,
                     fn %>% 
                       filter(!is.na(Manually_edited)) %>% 
                       rename(AccSpeciesName = taxon, 
                              scrubbed_author = author), by = join_by(AccSpeciesName)) %>% 
  select(AccSpeciesName, WFO_species.x, WFO_species.y, Note) %>% 
  unique() %>% 
  filter(WFO_species.x != WFO_species.y)

df2 <- bind_rows(anti_join(df2, edited) %>% 
                   select(AccSpeciesName:WFO_species),
                 inner_join(df2, edited) %>% 
                   mutate(WFO_species = WFO_species.y) %>% 
                   select(AccSpeciesName:WFO_species))

df2 <- df2 %>% 
  mutate(WFO_species = ifelse(WFO_species == "Carex caespitosa", "Carex cespitosa", WFO_species)) %>% 
  filter(WFO_species %in% fn$WFO_species) %>% 
  relocate(WFO_species)

df2 %>% select(-AccSpeciesName) %>% write_csv("/scratch/project_2003061/trait_datasets/Niittynen/traits_prepared.csv")

########################################################################################
# Tichy

df <- read_xlsx("/scratch/project_2003061/trait_datasets/Tichy/europen_indicator_values.xlsx") %>% 
  rename(AccSpeciesName = Taxon) %>% 
  mutate(AccSpeciesName = gsub(" sect. ", " ", AccSpeciesName)) %>% 
  mutate(AccSpeciesName = gsub(" aggr.", "", AccSpeciesName))
names(df)[3:8] <- paste0("ELLENBERG_", names(df)[3:8])

# Names
library(U.Taxonstand, lib.loc = "/projappl/project_2003061/Rpackages")

all_species <- nameSplit(unique(df$AccSpeciesName)) %>% 
  filter(Rank > 1) %>% 
  select(Submitted_Name_Author, Name, Author) %>% 
  setNames(c("AccSpeciesName","Name","Author")) %>% 
  tibble()%>% 
  filter(!grepl("×",Name),
         !grepl(" X ",Name),
         !grepl(" x ",Name)) %>% 
  distinct() %>% 
  mutate(Name = str_squish(Name),
         Author = str_squish(Author)) %>% 
  mutate(Author = ifelse(Author == "", NA, Author)) %>% 
  distinct()

# the World Flora Online
load("/scratch/project_2003061/trait_datasets/Plants_WFO.rdata")
all <- nameMatch(spList = all_species %>% select(Name,Author), 
                 spSource = database, max.distance = 2,
                 author = TRUE, matchFirst = TRUE)

all %>% 
  filter(is.na(Accepted_SPNAME))

df2 <- left_join(df,
                 bind_cols(all_species %>% select(AccSpeciesName), all) %>% 
                   mutate(Accepted_SPNAME = ifelse(is.na(Accepted_SPNAME), Name_spLev, Accepted_SPNAME)) %>% 
                   select(AccSpeciesName, Submitted_Author, Accepted_SPNAME) %>% distinct() %>% 
                   select(AccSpeciesName, Accepted_SPNAME) %>% 
                   rename(WFO_species = Accepted_SPNAME) %>% 
                   distinct() %>% 
                   drop_na())

library(googlesheets4)
fn <- read_sheet("https://docs.google.com/spreadsheets/d/1qgBOjM_941h4zm0foy83PyT6Og0ocWlDOUHXWmdlWIA/edit?gid=211021139#gid=211021139", col_types = "c")

edited <- inner_join(df2,
                     fn %>% 
                       filter(!is.na(Manually_edited)) %>% 
                       rename(AccSpeciesName = taxon, 
                              scrubbed_author = author), by = join_by(AccSpeciesName)) %>% 
  select(AccSpeciesName, WFO_species.x, WFO_species.y, Note) %>% 
  unique() %>% 
  filter(WFO_species.x != WFO_species.y)

df2 <- bind_rows(anti_join(df2, edited) %>% 
                   select(AccSpeciesName:WFO_species),
                 inner_join(df2, edited) %>% 
                   mutate(WFO_species = WFO_species.y) %>% 
                   select(AccSpeciesName:WFO_species))

df2 <- df2 %>% 
  mutate(WFO_species = ifelse(WFO_species == "Carex caespitosa", "Carex cespitosa", WFO_species)) %>% 
  filter(WFO_species %in% fn$WFO_species) %>% 
  relocate(WFO_species)

df2 %>% select(-AccSpeciesName) %>% write_csv("/scratch/project_2003061/trait_datasets/Tichy/traits_prepared.csv")

########################################################################################
# Baruah

df <- bind_rows(read_xls("/scratch/project_2003061/trait_datasets/Baruah/41598_2017_2595_MOESM1_ESM.xls") %>% 
                  rename(Value = Height,
                         Plot = Plot_no) %>% 
                  mutate(Trait = "Height"),
                read_xls("/scratch/project_2003061/trait_datasets/Baruah/41598_2017_2595_MOESM2_ESM.xls") %>% 
                  pivot_longer(cols = Leaf_length:Leaf_width, names_to = "Trait", values_to = "Value")) %>% 
  rename(AccSpeciesName = Species) %>% 
  mutate(AccSpeciesName = gsub(" sect. ", " ", AccSpeciesName)) %>% 
  mutate(AccSpeciesName = gsub(" aggr.", "", AccSpeciesName)) %>% 
  relocate(AccSpeciesName)

# Names
library(U.Taxonstand, lib.loc = "/projappl/project_2003061/Rpackages")

all_species <- nameSplit(unique(df$AccSpeciesName)) %>% 
  filter(Rank > 1) %>% 
  select(Submitted_Name_Author, Name, Author) %>% 
  setNames(c("AccSpeciesName","Name","Author")) %>% 
  tibble()%>% 
  filter(!grepl("×",Name),
         !grepl(" X ",Name),
         !grepl(" x ",Name)) %>% 
  distinct() %>% 
  mutate(Name = str_squish(Name),
         Author = str_squish(Author)) %>% 
  mutate(Author = ifelse(Author == "", NA, Author)) %>% 
  distinct()

# the World Flora Online
load("/scratch/project_2003061/trait_datasets/Plants_WFO.rdata")
all <- nameMatch(spList = all_species %>% select(Name,Author), 
                 spSource = database, max.distance = 2,
                 author = TRUE, matchFirst = TRUE)

all %>% 
  filter(is.na(Accepted_SPNAME))

df2 <- left_join(df,
                 bind_cols(all_species %>% select(AccSpeciesName), all) %>% 
                   mutate(Accepted_SPNAME = ifelse(is.na(Accepted_SPNAME), Name_spLev, Accepted_SPNAME)) %>% 
                   select(AccSpeciesName, Submitted_Author, Accepted_SPNAME) %>% distinct() %>% 
                   select(AccSpeciesName, Accepted_SPNAME) %>% 
                   rename(WFO_species = Accepted_SPNAME) %>% 
                   distinct() %>% 
                   drop_na())

library(googlesheets4)
fn <- read_sheet("https://docs.google.com/spreadsheets/d/1qgBOjM_941h4zm0foy83PyT6Og0ocWlDOUHXWmdlWIA/edit?gid=211021139#gid=211021139", col_types = "c")

edited <- inner_join(df2,
                     fn %>% 
                       filter(!is.na(Manually_edited)) %>% 
                       rename(AccSpeciesName = taxon, 
                              scrubbed_author = author), by = join_by(AccSpeciesName)) %>% 
  select(AccSpeciesName, WFO_species.x, WFO_species.y, Note) %>% 
  unique() %>% 
  filter(WFO_species.x != WFO_species.y)

df2 <- bind_rows(anti_join(df2, edited) %>% 
                   select(AccSpeciesName:WFO_species),
                 inner_join(df2, edited) %>% 
                   mutate(WFO_species = WFO_species.y) %>% 
                   select(AccSpeciesName:WFO_species))

df2 <- df2 %>% 
  mutate(WFO_species = ifelse(WFO_species == "Carex caespitosa", "Carex cespitosa", WFO_species)) %>% 
  filter(WFO_species %in% fn$WFO_species) %>% 
  relocate(WFO_species)

df2 %>% select(WFO_species, Treatment, Plot, Trait, Value) %>% write_csv("/scratch/project_2003061/trait_datasets/Baruah/traits_prepared.csv")

########################################################################################
# Majekova

df <- fread("/scratch/project_2003061/trait_datasets/Majekova/Majekova_et_al_2021_Functional_Ecology_data_dryad.csv") %>% 
  rename(AccSpeciesName = Species) %>% 
  mutate(AccSpeciesName = gsub(" sect. ", " ", AccSpeciesName)) %>% 
  mutate(AccSpeciesName = gsub(" aggr.", "", AccSpeciesName)) %>% 
  relocate(AccSpeciesName) %>% 
  rename(leaf_area = LA,
         leaf_thickness = LT) %>% 
  select(-TLP, -GrowthForm)

# Names
library(U.Taxonstand, lib.loc = "/projappl/project_2003061/Rpackages")

all_species <- nameSplit(unique(df$AccSpeciesName)) %>% 
  filter(Rank > 1) %>% 
  select(Submitted_Name_Author, Name, Author) %>% 
  setNames(c("AccSpeciesName","Name","Author")) %>% 
  tibble()%>% 
  filter(!grepl("×",Name),
         !grepl(" X ",Name),
         !grepl(" x ",Name)) %>% 
  distinct() %>% 
  mutate(Name = str_squish(Name),
         Author = str_squish(Author)) %>% 
  mutate(Author = ifelse(Author == "", NA, Author)) %>% 
  distinct()

# the World Flora Online
load("/scratch/project_2003061/trait_datasets/Plants_WFO.rdata")
all <- nameMatch(spList = all_species %>% select(Name,Author), 
                 spSource = database, max.distance = 2,
                 author = TRUE, matchFirst = TRUE)

all %>% 
  filter(is.na(Accepted_SPNAME))

df2 <- left_join(df,
                 bind_cols(all_species %>% select(AccSpeciesName), all) %>% 
                   mutate(Accepted_SPNAME = ifelse(is.na(Accepted_SPNAME), Name_spLev, Accepted_SPNAME)) %>% 
                   select(AccSpeciesName, Submitted_Author, Accepted_SPNAME) %>% distinct() %>% 
                   select(AccSpeciesName, Accepted_SPNAME) %>% 
                   rename(WFO_species = Accepted_SPNAME) %>% 
                   distinct() %>% 
                   drop_na())

library(googlesheets4)
fn <- read_sheet("https://docs.google.com/spreadsheets/d/1qgBOjM_941h4zm0foy83PyT6Og0ocWlDOUHXWmdlWIA/edit?gid=211021139#gid=211021139", col_types = "c")

df2 <- df2 %>% 
  mutate(WFO_species = ifelse(WFO_species == "Carex caespitosa", "Carex cespitosa", WFO_species)) %>% 
  filter(WFO_species %in% fn$WFO_species) %>% 
  relocate(WFO_species)

df2 %>% 
  select(-AccSpeciesName) %>% 
  pivot_longer(cols = Height:d13C, names_to = "Trait", values_to = "Value") %>% 
  drop_na() %>% 
  write_csv("/scratch/project_2003061/trait_datasets/Majekova/traits_prepared.csv")

########################################################################################
# Mudrak

df <- read_xlsx("/scratch/project_2003061/trait_datasets/Mudrak/Ohrazeni_Var_Traits_data.xlsx") %>% 
  select(-ID) %>% 
  rename(AccSpeciesName = species) %>% 
  mutate(AccSpeciesName = gsub("\\.", " ", AccSpeciesName)) %>% 
  mutate(AccSpeciesName = gsub(" sect. ", " ", AccSpeciesName)) %>% 
  mutate(AccSpeciesName = gsub(" aggr.", "", AccSpeciesName)) %>% 
  relocate(AccSpeciesName)

# Names
library(U.Taxonstand, lib.loc = "/projappl/project_2003061/Rpackages")

all_species <- nameSplit(unique(df$AccSpeciesName)) %>% 
  filter(Rank > 1) %>% 
  select(Submitted_Name_Author, Name, Author) %>% 
  setNames(c("AccSpeciesName","Name","Author")) %>% 
  tibble()%>% 
  filter(!grepl("×",Name),
         !grepl(" X ",Name),
         !grepl(" x ",Name)) %>% 
  distinct() %>% 
  mutate(Name = str_squish(Name),
         Author = str_squish(Author)) %>% 
  mutate(Author = ifelse(Author == "", NA, Author)) %>% 
  distinct()

# the World Flora Online
load("/scratch/project_2003061/trait_datasets/Plants_WFO.rdata")
all <- nameMatch(spList = all_species %>% select(Name,Author), 
                 spSource = database, max.distance = 2,
                 author = TRUE, matchFirst = TRUE)

all %>% 
  filter(is.na(Accepted_SPNAME))

df2 <- left_join(df,
                 bind_cols(all_species %>% select(AccSpeciesName), all) %>% 
                   mutate(Accepted_SPNAME = ifelse(is.na(Accepted_SPNAME), Name_spLev, Accepted_SPNAME)) %>% 
                   select(AccSpeciesName, Submitted_Author, Accepted_SPNAME) %>% distinct() %>% 
                   select(AccSpeciesName, Accepted_SPNAME) %>% 
                   rename(WFO_species = Accepted_SPNAME) %>% 
                   distinct() %>% 
                   drop_na())

library(googlesheets4)
fn <- read_sheet("https://docs.google.com/spreadsheets/d/1qgBOjM_941h4zm0foy83PyT6Og0ocWlDOUHXWmdlWIA/edit?gid=211021139#gid=211021139", col_types = "c")

df2 <- df2 %>% 
  mutate(WFO_species = ifelse(WFO_species == "Carex caespitosa", "Carex cespitosa", WFO_species)) %>% 
  filter(WFO_species %in% fn$WFO_species) %>% 
  relocate(WFO_species)

df2  %>% 
  select(-AccSpeciesName) %>% 
  pivot_longer(cols = Height:`N%`, names_to = "Trait", values_to = "Value") %>% 
  drop_na() %>% 
  write_csv("/scratch/project_2003061/trait_datasets/Mudrak/traits_prepared.csv")

########################################################################################
# Zanne

df <- bind_rows(fread("/scratch/project_2003061/trait_datasets/Zanne/GlobalWoodinessDatabase.csv") %>% 
                  select(-woodiness.count) %>% 
                  mutate(Trait = "woodiness") %>% 
                  rename(AccSpeciesName = gs,
                         Value = woodiness),
                fread("/scratch/project_2003061/trait_datasets/Zanne/GlobalLeafPhenologyDatabase.csv") %>% 
                  mutate(Trait = "phenology") %>% 
                  rename(AccSpeciesName = Binomial,
                         Value = Phenology)) %>% 
  mutate(AccSpeciesName = gsub("\\.", " ", AccSpeciesName)) %>% 
  mutate(AccSpeciesName = gsub(" sect. ", " ", AccSpeciesName)) %>% 
  mutate(AccSpeciesName = gsub(" aggr.", "", AccSpeciesName)) %>% 
  relocate(AccSpeciesName)

# Names
library(U.Taxonstand, lib.loc = "/projappl/project_2003061/Rpackages")

all_species <- nameSplit(unique(df$AccSpeciesName)) %>% 
  filter(Rank > 1) %>% 
  select(Submitted_Name_Author, Name, Author) %>% 
  setNames(c("AccSpeciesName","Name","Author")) %>% 
  tibble()%>% 
  filter(!grepl("×",Name),
         !grepl(" X ",Name),
         !grepl(" x ",Name)) %>% 
  distinct() %>% 
  mutate(Name = str_squish(Name),
         Author = str_squish(Author)) %>% 
  mutate(Author = ifelse(Author == "", NA, Author)) %>% 
  distinct()

# the World Flora Online
load("/scratch/project_2003061/trait_datasets/Plants_WFO.rdata")
all <- nameMatch(spList = all_species %>% select(Name,Author), 
                 spSource = database, max.distance = 2,
                 author = TRUE, matchFirst = TRUE)

all %>% 
  filter(is.na(Accepted_SPNAME))

df2 <- left_join(df,
                 bind_cols(all_species %>% select(AccSpeciesName), all) %>% 
                   mutate(Accepted_SPNAME = ifelse(is.na(Accepted_SPNAME), Name_spLev, Accepted_SPNAME)) %>% 
                   select(AccSpeciesName, Submitted_Author, Accepted_SPNAME) %>% distinct() %>% 
                   select(AccSpeciesName, Accepted_SPNAME) %>% 
                   rename(WFO_species = Accepted_SPNAME) %>% 
                   distinct() %>% 
                   drop_na())

library(googlesheets4)
fn <- read_sheet("https://docs.google.com/spreadsheets/d/1qgBOjM_941h4zm0foy83PyT6Og0ocWlDOUHXWmdlWIA/edit?gid=211021139#gid=211021139", col_types = "c")

edited <- inner_join(df2,
                     fn %>% 
                       filter(!is.na(Manually_edited)) %>% 
                       rename(AccSpeciesName = taxon, 
                              scrubbed_author = author), by = join_by(AccSpeciesName)) %>% 
  select(AccSpeciesName, WFO_species.x, WFO_species.y, Note) %>% 
  unique() %>% 
  filter(WFO_species.x != WFO_species.y)

df2 <- bind_rows(anti_join(df2, edited) %>% 
                   select(AccSpeciesName:WFO_species),
                 inner_join(df2, edited) %>% 
                   mutate(WFO_species = WFO_species.y) %>% 
                   select(AccSpeciesName:WFO_species))

df2 <- df2 %>% 
  mutate(WFO_species = ifelse(WFO_species == "Carex caespitosa", "Carex cespitosa", WFO_species)) %>% 
  filter(WFO_species %in% fn$WFO_species) %>% 
  relocate(WFO_species)

df2 %>% select(-AccSpeciesName) %>% write_csv("/scratch/project_2003061/trait_datasets/Zanne/traits_prepared.csv")

########################################################################################
# Zuijlen

df <- left_join(fread("/scratch/project_2003061/trait_datasets/Zuijlen/01_Vasc_traits.txt") %>% 
                  rename(code = species),
                fread("/scratch/project_2003061/trait_datasets/Zuijlen/05_Species_names.txt")) %>% 
  select(-code, -cover.original, -rel.cover, -group) %>% 
  rename(AccSpeciesName = full_name) %>% 
  mutate(AccSpeciesName = gsub(" sect. ", " ", AccSpeciesName)) %>% 
  mutate(AccSpeciesName = gsub(" aggr.", "", AccSpeciesName)) %>% 
  relocate(AccSpeciesName) %>% 
  rename(leaf_N = N,
         leaf_C = C,
         leaf_CN_ratio = CN)

# Names
library(U.Taxonstand, lib.loc = "/projappl/project_2003061/Rpackages")

all_species <- nameSplit(unique(df$AccSpeciesName)) %>% 
  filter(Rank > 1) %>% 
  select(Submitted_Name_Author, Name, Author) %>% 
  setNames(c("AccSpeciesName","Name","Author")) %>% 
  tibble()%>% 
  filter(!grepl("×",Name),
         !grepl(" X ",Name),
         !grepl(" x ",Name)) %>% 
  distinct() %>% 
  mutate(Name = str_squish(Name),
         Author = str_squish(Author)) %>% 
  mutate(Author = ifelse(Author == "", NA, Author)) %>% 
  distinct()

# the World Flora Online
load("/scratch/project_2003061/trait_datasets/Plants_WFO.rdata")
all <- nameMatch(spList = all_species %>% select(Name,Author), 
                 spSource = database, max.distance = 2,
                 author = TRUE, matchFirst = TRUE)

all %>% 
  filter(is.na(Accepted_SPNAME))

df2 <- left_join(df,
                 bind_cols(all_species %>% select(AccSpeciesName), all) %>% 
                   mutate(Accepted_SPNAME = ifelse(is.na(Accepted_SPNAME), Name_spLev, Accepted_SPNAME)) %>% 
                   select(AccSpeciesName, Submitted_Author, Accepted_SPNAME) %>% distinct() %>% 
                   select(AccSpeciesName, Accepted_SPNAME) %>% 
                   rename(WFO_species = Accepted_SPNAME) %>% 
                   distinct() %>% 
                   drop_na())

library(googlesheets4)
fn <- read_sheet("https://docs.google.com/spreadsheets/d/1qgBOjM_941h4zm0foy83PyT6Og0ocWlDOUHXWmdlWIA/edit?gid=211021139#gid=211021139", col_types = "c")

df2 <- df2 %>% 
  mutate(WFO_species = ifelse(WFO_species == "Carex caespitosa", "Carex cespitosa", WFO_species)) %>% 
  filter(WFO_species %in% fn$WFO_species) %>% 
  relocate(WFO_species)

df2 %>% select(-AccSpeciesName) %>% write_csv("/scratch/project_2003061/trait_datasets/Zuijlen/traits_prepared.csv")



########################################################################################
# Drevojan

df <- read_xlsx("/scratch/project_2003061/trait_datasets/Drevojan/Life_form.xlsx") %>% 
  select(-SeqID) %>% 
  rename(AccSpeciesName = FloraVeg.Taxon) %>% 
  mutate(AccSpeciesName = gsub(" sect. ", " ", AccSpeciesName)) %>% 
  mutate(AccSpeciesName = gsub(" aggr.", "", AccSpeciesName))

# Names
library(U.Taxonstand, lib.loc = "/projappl/project_2003061/Rpackages")

all_species <- nameSplit(unique(df$AccSpeciesName)) %>% 
  filter(Rank > 1) %>% 
  select(Submitted_Name_Author, Name, Author) %>% 
  setNames(c("AccSpeciesName","Name","Author")) %>% 
  tibble()%>% 
  filter(!grepl("×",Name),
         !grepl(" X ",Name),
         !grepl(" x ",Name)) %>% 
  distinct() %>% 
  mutate(Name = str_squish(Name),
         Author = str_squish(Author)) %>% 
  mutate(Author = ifelse(Author == "", NA, Author)) %>% 
  distinct()

# the World Flora Online
load("/scratch/project_2003061/trait_datasets/Plants_WFO.rdata")
all <- nameMatch(spList = all_species %>% select(Name,Author), 
                 spSource = database, max.distance = 2,
                 author = TRUE, matchFirst = TRUE)

all %>% 
  filter(is.na(Accepted_SPNAME))

df2 <- left_join(df,
                 bind_cols(all_species %>% select(AccSpeciesName), all) %>% 
                   mutate(Accepted_SPNAME = ifelse(is.na(Accepted_SPNAME), Name_spLev, Accepted_SPNAME)) %>% 
                   select(AccSpeciesName, Submitted_Author, Accepted_SPNAME) %>% distinct() %>% 
                   select(AccSpeciesName, Accepted_SPNAME) %>% 
                   rename(WFO_species = Accepted_SPNAME) %>% 
                   distinct() %>% 
                   drop_na())

library(googlesheets4)
fn <- read_sheet("https://docs.google.com/spreadsheets/d/1qgBOjM_941h4zm0foy83PyT6Og0ocWlDOUHXWmdlWIA/edit?gid=211021139#gid=211021139", col_types = "c")

edited <- inner_join(df2,
                     fn %>% 
                       filter(!is.na(Manually_edited)) %>% 
                       rename(AccSpeciesName = taxon, 
                              scrubbed_author = author), by = join_by(AccSpeciesName)) %>% 
  select(AccSpeciesName, WFO_species.x, WFO_species.y, Note) %>% 
  unique() %>% 
  filter(WFO_species.x != WFO_species.y)

df2 <- bind_rows(anti_join(df2, edited) %>% 
                   select(AccSpeciesName:WFO_species),
                 inner_join(df2, edited) %>% 
                   mutate(WFO_species = WFO_species.y) %>% 
                   select(AccSpeciesName:WFO_species))

df2 <- df2 %>% 
  mutate(WFO_species = ifelse(WFO_species == "Carex caespitosa", "Carex cespitosa", WFO_species)) %>% 
  filter(WFO_species %in% fn$WFO_species) %>% 
  relocate(WFO_species)

df2 <- df2 %>% 
  mutate(across(Phanerophyte:`Herbaceous liana`, ~ifelse(.x == 1, "yes", "no")))

df2 %>% select(-AccSpeciesName) %>% write_csv("/scratch/project_2003061/trait_datasets/Drevojan/traits_prepared.csv")

########################################################################################
# Lososova

df <- read_xlsx("/scratch/project_2003061/trait_datasets/Lososova/Lososova_et_al_2023_Dispersal_version2_2024-06-14.xlsx") %>% 
  select(Taxon, `Plant height (m)`, `Seed mass (mg)`, `Efficient dispersal mode - common`,
         `Dispersal distance class (1-6)`, `Efficient dispersal mode - anthropogenic`) %>% 
  rename(AccSpeciesName = Taxon,
         height = `Plant height (m)`,
         seed_mass = `Seed mass (mg)`,
         dispersal_mode = `Efficient dispersal mode - common`,
         dispersal_distance_class = `Dispersal distance class (1-6)`,
         dispersal_anthropogenic = `Efficient dispersal mode - anthropogenic`) %>% 
  mutate(dispersal_anthropogenic = ifelse(is.na(dispersal_anthropogenic), "no", "yes")) %>% 
  mutate(AccSpeciesName = gsub(" sect. ", " ", AccSpeciesName)) %>% 
  mutate(AccSpeciesName = gsub(" aggr.", "", AccSpeciesName)) %>% 
  relocate(AccSpeciesName)

# Names
library(U.Taxonstand, lib.loc = "/projappl/project_2003061/Rpackages")

all_species <- nameSplit(unique(df$AccSpeciesName)) %>% 
  filter(Rank > 1) %>% 
  select(Submitted_Name_Author, Name, Author) %>% 
  setNames(c("AccSpeciesName","Name","Author")) %>% 
  tibble()%>% 
  filter(!grepl("×",Name),
         !grepl(" X ",Name),
         !grepl(" x ",Name)) %>% 
  distinct() %>% 
  mutate(Name = str_squish(Name),
         Author = str_squish(Author)) %>% 
  mutate(Author = ifelse(Author == "", NA, Author)) %>% 
  distinct()

# the World Flora Online
load("/scratch/project_2003061/trait_datasets/Plants_WFO.rdata")
all <- nameMatch(spList = all_species %>% select(Name,Author), 
                 spSource = database, max.distance = 2,
                 author = TRUE, matchFirst = TRUE)

all %>% 
  filter(is.na(Accepted_SPNAME))

df2 <- left_join(df,
                 bind_cols(all_species %>% select(AccSpeciesName), all) %>% 
                   mutate(Accepted_SPNAME = ifelse(is.na(Accepted_SPNAME), Name_spLev, Accepted_SPNAME)) %>% 
                   select(AccSpeciesName, Submitted_Author, Accepted_SPNAME) %>% distinct() %>% 
                   select(AccSpeciesName, Accepted_SPNAME) %>% 
                   rename(WFO_species = Accepted_SPNAME) %>% 
                   distinct() %>% 
                   drop_na())

library(googlesheets4)
fn <- read_sheet("https://docs.google.com/spreadsheets/d/1qgBOjM_941h4zm0foy83PyT6Og0ocWlDOUHXWmdlWIA/edit?gid=211021139#gid=211021139", col_types = "c")

edited <- inner_join(df2,
                     fn %>% 
                       filter(!is.na(Manually_edited)) %>% 
                       rename(AccSpeciesName = taxon, 
                              scrubbed_author = author), by = join_by(AccSpeciesName)) %>% 
  select(AccSpeciesName, WFO_species.x, WFO_species.y, Note) %>% 
  unique() %>% 
  filter(WFO_species.x != WFO_species.y)

df2 <- bind_rows(anti_join(df2, edited) %>% 
                   select(AccSpeciesName:WFO_species),
                 inner_join(df2, edited) %>% 
                   mutate(WFO_species = WFO_species.y) %>% 
                   select(AccSpeciesName:WFO_species))

df2 <- df2 %>% 
  mutate(WFO_species = ifelse(WFO_species == "Carex caespitosa", "Carex cespitosa", WFO_species)) %>% 
  filter(WFO_species %in% fn$WFO_species) %>% 
  relocate(WFO_species)

df2 <- df2 %>% 
  group_by(WFO_species) %>% 
  slice_head(n = 1)

df2 %>% select(-AccSpeciesName) %>% write_csv("/scratch/project_2003061/trait_datasets/Lososova/traits_prepared.csv")

########################################################################################
# Midolo

df <- read_xlsx("/scratch/project_2003061/trait_datasets/Midolo/disturbance_indicator_values.xlsx") %>% 
  select(species, n.EUNIS.habitats, Disturbance.Severity, Disturbance.Frequency, Mowing.Frequency, Grazing.Pressure, Soil.Disturbance) %>% 
  mutate(across(Disturbance.Severity:Soil.Disturbance, as.numeric)) %>% 
  rename(AccSpeciesName = species,
         N_EUNIS_habitats = n.EUNIS.habitats,
         disturbance_severity_indicator = Disturbance.Severity,
         disturbance_frequency_indicator = Disturbance.Frequency,
         mowing_frequency_indicator = Mowing.Frequency,
         grazing_pressure_indicator = Grazing.Pressure,
         soil_disturbance_indicator = Soil.Disturbance) %>% 
  mutate(AccSpeciesName = gsub(" sect. ", " ", AccSpeciesName)) %>% 
  mutate(AccSpeciesName = gsub(" aggr.", "", AccSpeciesName)) %>% 
  relocate(AccSpeciesName)

# Names
library(U.Taxonstand, lib.loc = "/projappl/project_2003061/Rpackages")

all_species <- nameSplit(unique(df$AccSpeciesName)) %>% 
  filter(Rank > 1) %>% 
  select(Submitted_Name_Author, Name, Author) %>% 
  setNames(c("AccSpeciesName","Name","Author")) %>% 
  tibble()%>% 
  filter(!grepl("×",Name),
         !grepl(" X ",Name),
         !grepl(" x ",Name)) %>% 
  distinct() %>% 
  mutate(Name = str_squish(Name),
         Author = str_squish(Author)) %>% 
  mutate(Author = ifelse(Author == "", NA, Author)) %>% 
  distinct()

# the World Flora Online
load("/scratch/project_2003061/trait_datasets/Plants_WFO.rdata")
all <- nameMatch(spList = all_species %>% select(Name,Author), 
                 spSource = database, max.distance = 2,
                 author = TRUE, matchFirst = TRUE)

all %>% 
  filter(is.na(Accepted_SPNAME))

df2 <- left_join(df,
                 bind_cols(all_species %>% select(AccSpeciesName), all) %>% 
                   mutate(Accepted_SPNAME = ifelse(is.na(Accepted_SPNAME), Name_spLev, Accepted_SPNAME)) %>% 
                   select(AccSpeciesName, Submitted_Author, Accepted_SPNAME) %>% distinct() %>% 
                   select(AccSpeciesName, Accepted_SPNAME) %>% 
                   rename(WFO_species = Accepted_SPNAME) %>% 
                   distinct() %>% 
                   drop_na())

library(googlesheets4)
fn <- read_sheet("https://docs.google.com/spreadsheets/d/1qgBOjM_941h4zm0foy83PyT6Og0ocWlDOUHXWmdlWIA/edit?gid=211021139#gid=211021139", col_types = "c")

edited <- inner_join(df2,
                     fn %>% 
                       filter(!is.na(Manually_edited)) %>% 
                       rename(AccSpeciesName = taxon, 
                              scrubbed_author = author), by = join_by(AccSpeciesName)) %>% 
  select(AccSpeciesName, WFO_species.x, WFO_species.y, Note) %>% 
  unique() %>% 
  filter(WFO_species.x != WFO_species.y)

df2 <- bind_rows(anti_join(df2, edited) %>% 
                   select(AccSpeciesName:WFO_species),
                 inner_join(df2, edited) %>% 
                   mutate(WFO_species = WFO_species.y) %>% 
                   select(AccSpeciesName:WFO_species))

df2 <- df2 %>% 
  mutate(WFO_species = ifelse(WFO_species == "Carex caespitosa", "Carex cespitosa", WFO_species)) %>% 
  filter(WFO_species %in% fn$WFO_species) %>% 
  relocate(WFO_species)

df2 <- df2 %>% 
  group_by(WFO_species) %>% 
  slice_head(n = 1)

df2 %>% select(-AccSpeciesName) %>% write_csv("/scratch/project_2003061/trait_datasets/Midolo/traits_prepared.csv")

########################################################################################
# Tesitel

df <- read_xlsx("/scratch/project_2003061/trait_datasets/Tesitel/Tesitel-et-al-Parasitism-mycotrophy.xlsx") %>% 
  rename(AccSpeciesName = TaxonFloraVeg,
         Value = `Parasitism and mycoheterotrophy`) %>% 
  mutate(Trait = "parasitism_mycoheterotrophy") %>% 
  mutate(AccSpeciesName = gsub(" sect. ", " ", AccSpeciesName)) %>% 
  mutate(AccSpeciesName = gsub(" aggr.", "", AccSpeciesName)) %>% 
  relocate(AccSpeciesName)

# Names
library(U.Taxonstand, lib.loc = "/projappl/project_2003061/Rpackages")

all_species <- nameSplit(unique(df$AccSpeciesName)) %>% 
  filter(Rank > 1) %>% 
  select(Submitted_Name_Author, Name, Author) %>% 
  setNames(c("AccSpeciesName","Name","Author")) %>% 
  tibble()%>% 
  filter(!grepl("×",Name),
         !grepl(" X ",Name),
         !grepl(" x ",Name)) %>% 
  distinct() %>% 
  mutate(Name = str_squish(Name),
         Author = str_squish(Author)) %>% 
  mutate(Author = ifelse(Author == "", NA, Author)) %>% 
  distinct()

# the World Flora Online
load("/scratch/project_2003061/trait_datasets/Plants_WFO.rdata")
all <- nameMatch(spList = all_species %>% select(Name,Author), 
                 spSource = database, max.distance = 2,
                 author = TRUE, matchFirst = TRUE)

all %>% 
  filter(is.na(Accepted_SPNAME))

df2 <- left_join(df,
                 bind_cols(all_species %>% select(AccSpeciesName), all) %>% 
                   mutate(Accepted_SPNAME = ifelse(is.na(Accepted_SPNAME), Name_spLev, Accepted_SPNAME)) %>% 
                   select(AccSpeciesName, Submitted_Author, Accepted_SPNAME) %>% distinct() %>% 
                   select(AccSpeciesName, Accepted_SPNAME) %>% 
                   rename(WFO_species = Accepted_SPNAME) %>% 
                   distinct() %>% 
                   drop_na())

library(googlesheets4)
fn <- read_sheet("https://docs.google.com/spreadsheets/d/1qgBOjM_941h4zm0foy83PyT6Og0ocWlDOUHXWmdlWIA/edit?gid=211021139#gid=211021139", col_types = "c")

edited <- inner_join(df2,
                     fn %>% 
                       filter(!is.na(Manually_edited)) %>% 
                       rename(AccSpeciesName = taxon, 
                              scrubbed_author = author), by = join_by(AccSpeciesName)) %>% 
  select(AccSpeciesName, WFO_species.x, WFO_species.y, Note) %>% 
  unique() %>% 
  filter(WFO_species.x != WFO_species.y)

df2 <- bind_rows(anti_join(df2, edited) %>% 
                   select(AccSpeciesName:WFO_species),
                 inner_join(df2, edited) %>% 
                   mutate(WFO_species = WFO_species.y) %>% 
                   select(AccSpeciesName:WFO_species))

df2 <- df2 %>% 
  mutate(WFO_species = ifelse(WFO_species == "Carex caespitosa", "Carex cespitosa", WFO_species)) %>% 
  filter(WFO_species %in% fn$WFO_species) %>% 
  relocate(WFO_species)

df2 <- df2 %>% 
  group_by(WFO_species) %>% 
  slice_head(n = 1)

df2 %>% select(-AccSpeciesName) %>% write_csv("/scratch/project_2003061/trait_datasets/Tesitel/traits_prepared.csv")
