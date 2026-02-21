#####################################################################
# Preprocess the GRoot datasets

library(tidyverse)
library(data.table)

df <- read_csv("/scratch/project_2003061/trait_datasets/GRoot/GRooTFullVersion.csv") %>% as.data.table()

df <- df %>% select(genusTNRS, speciesTNRS, infraspecificTNRS,
                    decimalLatitude, decimalLongitud, traitName, traitValue, referencesAbbreviated) %>% 
  mutate(speciesTNRS = ifelse(is.na(speciesTNRS), "", speciesTNRS)) %>% 
  mutate(infraspecificTNRS = ifelse(is.na(infraspecificTNRS), "", paste0("subsp. ", infraspecificTNRS))) %>% 
  mutate(AccSpeciesName = str_squish(paste(genusTNRS, speciesTNRS, infraspecificTNRS, sep = " "))) %>% 
  select(-c(genusTNRS, speciesTNRS, infraspecificTNRS)) %>% 
  relocate(AccSpeciesName) %>% 
  rename(Latitude = decimalLatitude,
         Longitude = decimalLongitud,
         Trait = traitName,
         Value = traitValue)

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

table(df2$referencesAbbreviated)

df2 %>% select(-referencesAbbreviated) %>% write_csv("/scratch/project_2003061/trait_datasets/GRoot/traits_prepared.csv")
