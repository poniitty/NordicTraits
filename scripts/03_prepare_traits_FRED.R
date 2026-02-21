#####################################################################
# Preprocess the FRED datasets

library(tidyverse)
library(data.table)

df <- fread("/scratch/project_2003061/trait_datasets/FRED/FRED3_Entire_Database_2021.csv") %>% as.data.table()

colnam <- fread("/scratch/project_2003061/trait_datasets/FRED/FRED3_Entire_Database_2021.csv",nrows = 2, header = F) %>% slice(2) %>% as.character()
colunits <- fread("/scratch/project_2003061/trait_datasets/FRED/FRED3_Entire_Database_2021.csv",nrows = 3, header = F) %>% slice(3) %>% as.character()

df <- fread("/scratch/project_2003061/trait_datasets/FRED/FRED3_Entire_Database_2021.csv", skip = 13)
names(df) <- colnam

df <- df %>% select(-starts_with("n_"), -starts_with("SE_"), -starts_with("SE "), -starts_with("Max_"), -starts_with("Min_"), -starts_with("SD_"), -starts_with("Median_"), -starts_with("Upper quartile_"),
                    -starts_with("Lower quartile_"), -starts_with("5th percentile_"), -starts_with("95th percentile"), -starts_with("Notes_")) %>% 
  select(Name,`Abbreviated article citation`,`Plant taxonomy_Accepted group_TPL`:`Plant taxonomy_Species name unresolved`,
         `Belowground part`:`Root volume per ground area`,
         "Latitude_Main":"Longitude_Estimated") %>% 
  mutate(across(`Root diameter class_Lower bound`:`Root volume per ground area`, as.character)) %>% 
  pivot_longer(cols = `Root diameter class_Lower bound`:`Root volume per ground area`, names_to = "trait", values_to = "value") %>% 
  filter(!is.na(value) & value != "") %>% as.data.table()

combine_names <- function(x){
  str_squish(paste(x["Plant taxonomy_Genus_Data Source"],
                   x["Plant taxonomy_Species_Data source"],
                   ifelse(is.na(x["Plant taxonomy_Subspecies_Data source"]) | x["Plant taxonomy_Subspecies_Data source"] == "", "", paste("subsp. ", x["Plant taxonomy_Subspecies_Data source"])),
                   sep = " "))
}

df$AccSpeciesName <- apply(df,1,function(x) combine_names(x))

df <- df %>% 
  mutate(Latitude = ifelse(is.na(Latitude), Latitude_Estimated, Latitude),
         Longitude = ifelse(is.na(Longitude), Longitude_Estimated, Longitude)) %>% 
  mutate(Latitude = ifelse(is.na(Latitude), Latitude_Main, Latitude),
         Longitude = ifelse(is.na(Longitude), Longitude_Main, Longitude)) %>% 
  select(-Latitude_Estimated, -Longitude_Estimated, -Latitude_Main, -Longitude_Main) %>% 
  rename(Trait = trait,
         Value = value) %>% 
  select(AccSpeciesName, 
         Latitude, Longitude,
         Trait, Value,
        `Abbreviated article citation`)

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


table(df2$Trait)

df2 %>% select(-`Abbreviated article citation`) %>% write_csv("/scratch/project_2003061/trait_datasets/FRED/traits_prepared.csv")
