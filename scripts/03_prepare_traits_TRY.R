#####################################################################
# Preprocess the TRY datasets

library(tidyverse)
library(data.table)
library(readr)

f <- list.files("/scratch/project_2003061/trait_datasets/TRY/", pattern = ".txt$", full.names = T)

all <- tibble()
for(i in f){
  print(i)
  
  try <- read_tsv(i, col_names = T, locale = locale(encoding = "latin1")) %>% 
    mutate(Replicates = as.numeric(Replicates))
  gc()
  # try %>% filter(TraitName == "Dispersal unit floating capacity" & ObsDataID == 16661979) %>% as.data.table()
  # try %>% filter(!is.na(TraitID)) %>% select(-Reference) %>% sample_n(size = 10) %>% as.data.table() %>% 
  #   select(TraitName, OrigValueStr, OrigUnitStr, StdValue, UnitName)
  
  try %>% dplyr::select(-LastName, -FirstName, -OrigUncertaintyStr, -`...28`, -Comment,
                        -ValueKindName, -UncertaintyName, -OrigObsDataID, -RelUncertaintyPercent) -> try
  
  coords <- try %>% filter(DataID %in% c(59,60)) %>% 
    dplyr::select(ObservationID, DataID, StdValue) %>% 
    mutate(coords = ifelse(DataID == 59, "Latitude", "Longitude")) %>% 
    dplyr::select(-DataID) %>% pivot_wider(id_cols = ObservationID,
                                           names_from = coords,
                                           values_from = StdValue, values_fn = mean)
  
  expo <- try %>% filter(DataID  == 327) %>% 
    dplyr::select(ObservationID, DataID, DataName, OriglName, OrigValueStr, StdValue) %>%
    rename(Exposition = OrigValueStr) %>% 
    select(ObservationID, Exposition)
  
  treat <- try %>% filter(DataID  == 308) %>% 
    dplyr::select(ObservationID, DataID, DataName, OriglName, OrigValueStr, StdValue) %>%
    rename(Treatment = OrigValueStr) %>% 
    select(ObservationID, Treatment)
  
  nrep <- try %>% filter(DataID  == 213) %>% 
    dplyr::select(ObservationID, DataID, DataName, OriglName, OrigValueStr, StdValue) %>%
    rename(n_repl = OrigValueStr) %>% 
    select(ObservationID, n_repl)
  
  npla <- try %>% filter(DataID  == 358) %>% 
    dplyr::select(ObservationID, DataID, DataName, OriglName, OrigValueStr, StdValue) %>%
    rename(n_plants = OrigValueStr) %>% 
    select(ObservationID, n_plants)
  
  pstatus <- try %>% filter(DataID  == 413) %>% 
    dplyr::select(ObservationID, DataID, DataName, OriglName, OrigValueStr, StdValue) %>%
    rename(plant_status = OrigValueStr) %>% 
    select(ObservationID, plant_status)
  
  phealth <- try %>% filter(DataID  == 1961) %>% 
    dplyr::select(ObservationID, DataID, DataName, OriglName, OrigValueStr, StdValue) %>%
    rename(plant_health = OrigValueStr) %>% 
    select(ObservationID, plant_health)
  
  try <- try %>% filter(!is.na(TraitID))
  
  try <- full_join(try, coords) %>% 
    full_join(., expo) %>% 
    full_join(., treat) %>% 
    full_join(., nrep) %>% 
    full_join(., npla) %>% 
    full_join(., pstatus) %>% 
    full_join(., phealth)
  
  all <- bind_rows(all, try)
  gc()
}

table(all$Dataset) %>% sort

all <- all %>% 
  filter(!Dataset %in% c("The LEDA Traitbase",
                         "FRED - Fine Root Ecology Database"))

# Names
library(U.Taxonstand, lib.loc = "/projappl/project_2003061/Rpackages")

all_species <- nameSplit(unique(all$AccSpeciesName)) %>% 
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
alln <- nameMatch(spList = all_species %>% select(Name,Author), 
                 spSource = database, max.distance = 2,
                 author = TRUE, matchFirst = TRUE)

alln %>% 
  filter(is.na(Accepted_SPNAME))

df2 <- left_join(all,
                 bind_cols(all_species %>% select(AccSpeciesName), alln) %>% 
                   mutate(Accepted_SPNAME = ifelse(is.na(Accepted_SPNAME), Name_spLev, Accepted_SPNAME)) %>% 
                   select(AccSpeciesName, Submitted_Author, Accepted_SPNAME) %>% distinct() %>% 
                   select(AccSpeciesName, Accepted_SPNAME) %>% 
                   rename(WFO_species = Accepted_SPNAME) %>% 
                   distinct() %>% 
                   drop_na())

df2 %>% 
  filter(is.na(WFO_species)) %>% pull(AccSpeciesName) %>% unique()

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
                   select(DatasetID:WFO_species),
                 inner_join(df2, edited) %>% 
                   mutate(WFO_species = WFO_species.y) %>% 
                   select(DatasetID:WFO_species))

df2 <- df2 %>% 
  mutate(WFO_species = ifelse(WFO_species == "Carex caespitosa", "Carex cespitosa", WFO_species)) %>% 
  filter(WFO_species %in% fn$WFO_species) %>% 
  relocate(WFO_species)

datasets <- df2 %>% select(DatasetID, Dataset) %>% 
  group_by(DatasetID, Dataset) %>% count()

traits <- df2 %>% select(TraitID, TraitName, DataID, DataName) %>% 
  group_by(TraitID, TraitName, DataID, DataName) %>% count()

refs <- df2 %>% select(Reference) %>% 
  group_by(Reference) %>% count() %>% 
  ungroup() %>% 
  mutate(TRY_source = paste0("TRY_", rownames(.)))

df2 <- df2 %>% full_join(., refs %>% select(-n))

unique(df2$TraitName)
df2 %>% as.data.table()
df2 <- df2 %>% select(-AccSpeciesName,-ObsDataID,-TraitName,-DataName,
                      -Reference,-Exposition,-Treatment,-ErrorRisk,
                      -Replicates, -Dataset,-SpeciesName,-AccSpeciesID)
df2 %>% sample_n(size = 10) %>% as.data.table()

df2 <- df2 %>% 
  mutate(StdValue = ifelse(is.na(StdValue), OrigValueStr, StdValue),
         UnitName = ifelse(is.na(UnitName), OrigUnitStr, UnitName)) %>% 
  filter(!is.na(StdValue))

df2 %>% select(WFO_species, DatasetID, TraitID, OriglName, StdValue, UnitName, TRY_source, Latitude, Longitude, n_repl, n_plants, plant_status, plant_health) %>% 
  write_csv("/scratch/project_2003061/trait_datasets/TRY/traits_prepared.csv")
datasets %>% write_csv("/scratch/project_2003061/trait_datasets/TRY/datasets.csv")
traits %>% write_csv("/scratch/project_2003061/trait_datasets/TRY/measurement_information.csv")
refs %>% write_csv("/scratch/project_2003061/trait_datasets/TRY/citations.csv")
