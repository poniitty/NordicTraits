#####################################################################
# Download the GIFT datasets

library(tidyverse)
library(data.table)
library(GIFT)
library(parallel)
library(googlesheets4)

options(timeout = max(1000, getOption("timeout")))

# READ ALL DATA
fn <- read_sheet("https://docs.google.com/spreadsheets/d/1qgBOjM_941h4zm0foy83PyT6Og0ocWlDOUHXWmdlWIA/edit?gid=211021139#gid=211021139", col_types = "c")

all_species <- unique(c(fn$taxon,
                        fn$WFO_species,
                        fn$TPL_species,
                        fn$LCVP_species)) %>% 
  str_squish()

all_species <- all_species[!(grepl("×", all_species) | grepl(" x ", all_species) | grepl(" X ", all_species))]
all_species <- all_species[unlist(lapply(all_species, function(x) length(strsplit(x, " ")[[1]]) > 1))]
all_species <- unique(c(all_species, paste(str_split_i(all_species, " ", 1), str_split_i(all_species, " ", 2), sep = " ")))
all_species <- all_species %>% sort

all_species <- tibble(all_species = all_species,
                      id = 1:length(all_species)) %>% 
  arrange(all_species)

trait_meta <- GIFT_traits_meta()

df <- mclapply(trait_meta$Lvl3, function(i){
  # i <- "1.10.2"
  print(i)
  trait_raw <- try({
    trait_raw <- GIFT_traits_raw(trait_IDs = i)
  
  trait_raw <- trait_raw %>% 
    select(trait_derived_ID, trait_ID, trait_value,
           work_genus, work_species, work_author, ref_ID) %>% 
    mutate(species = paste(work_species, work_author, sep = " ")) %>% 
    relocate(species) %>% 
    select(-c(work_genus, work_species, work_author)) %>% 
    mutate(trait_value = as.character(trait_value))
  })
  
  if(class(trait_raw)[[1]] == "try-error"){
    return(NULL)
  } else {
    return(trait_raw)
  }
  
}, mc.cores = future::availableCores()) %>% 
  bind_rows()

miss <- c("3.11.3","3.19.1","3.19.2","3.19.3","4.1.1","4.1.2","4.3.1","4.3.2","4.9.1","4.9.2","4.9.3")

trait_meta %>% filter(Lvl3 %in% miss)

df <- left_join(df,
          trait_meta %>% select(Trait1,Lvl3,Trait2) %>% rename(trait_ID = Lvl3))

# Names
library(U.Taxonstand, lib.loc = "/projappl/project_2003061/Rpackages")

all_species <- df %>% 
  select(species) %>% 
  tibble() %>% 
  distinct() %>% 
  filter(!grepl("×",species),
         !grepl(" X ",species),
         !grepl(" x ",species))

all_species <- nameSplit(all_species$species) %>% 
  select(Submitted_Name_Author, Name, Author) %>% 
  mutate(Name = str_squish(Name),
         Author = str_squish(Author)) %>% 
  mutate(Author = ifelse(Author == "", NA, Author))

# the World Flora Online
load("/scratch/project_2003061/trait_datasets/Plants_WFO.rdata")
all <- nameMatch(spList = all_species %>% select(Name,Author), 
                 spSource = database, max.distance = 2,
                 author = TRUE, matchFirst = TRUE)

all %>% 
  filter(is.na(Accepted_SPNAME) & is.na(Name_spLev))

all2 <- nameMatch(spList = tibble(Name = c("Stellaria holostea", "Chamorchis alpina")), 
                  spSource = database, max.distance = 2,
                  matchFirst = TRUE)

all <- bind_rows(all %>% 
                   filter(!(is.na(Accepted_SPNAME) & is.na(Name_spLev))),
                 all2 %>% 
                   mutate(Submitted_Name = c("Rabelera holostea","Chamaeorchis alpinus"),
                          Submitted_Author = c("(L.) M.T.Sharples & E.A.Tripp",NA)))

df2 <- left_join(df,
                 all %>% 
                   select(Submitted_Name, Submitted_Author, Accepted_SPNAME) %>% distinct() %>% 
                   mutate(species = paste(Submitted_Name, Submitted_Author, sep = " ")) %>% 
                   select(species, Accepted_SPNAME) %>% 
                   rename(WFO_species = Accepted_SPNAME) %>% 
                   distinct() %>% 
                   drop_na())

library(googlesheets4)
fn <- read_sheet("https://docs.google.com/spreadsheets/d/1qgBOjM_941h4zm0foy83PyT6Og0ocWlDOUHXWmdlWIA/edit?gid=211021139#gid=211021139", col_types = "c")

edited <- inner_join(df2,
                     fn %>% 
                       filter(!is.na(Manually_edited)) %>% 
                       mutate(species = paste(taxon, author)), 
                     by = join_by(species)) %>% 
  select(species, WFO_species.x, WFO_species.y, Note) %>% 
  unique() %>% 
  filter(WFO_species.x != WFO_species.y)

df2 <- bind_rows(anti_join(df2, edited) %>% 
                   select(species:WFO_species),
                 inner_join(df2, edited) %>% 
                   mutate(WFO_species = WFO_species.y) %>% 
                   select(species:WFO_species))

df2 <- df2 %>% 
  mutate(WFO_species = ifelse(WFO_species == "Carex caespitosa", "Carex cespitosa", WFO_species)) %>% 
  filter(WFO_species %in% fn$WFO_species) %>% 
  relocate(WFO_species)

df2 %>% select(-species) %>% write_csv("/scratch/project_2003061/trait_datasets/GIFT/traits_prepared.csv")
trait_meta %>% write_csv("/scratch/project_2003061/trait_datasets/GIFT/measurement_information.csv")

