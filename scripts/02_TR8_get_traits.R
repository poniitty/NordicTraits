#####################################################################
# Download the TR8 datasets


library(tidyverse)
library(data.table)
library(TR8)
library(parallel)
library(googlesheets4)

available_tr8 <- available_traits() %>% filter(db != "AMF")

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
# all_species <- tibble(all_species = all_species,
#                       id = 1:length(all_species))

res <- mclapply(all_species, function(x) {
  # x <- "Antennaria dioica"
  e <- try({
    temp <- tr8(x, download_list = available_tr8$short_code,
                catminat_alternatives=F, allow_persistent=T)
    
    temp <- temp@results %>% as.data.frame() %>% 
      mutate(species = x) %>% 
      mutate(across(h_max:Veneer.Product, as.character)) %>% 
      pivot_longer(cols = h_max:Veneer.Product, values_drop_na = T)
    
    if(nrow(temp) > 0){
      return(temp)
    }}, silent = TRUE)
  if(class(e) == "try-error"){
    return(NULL)
  }
}, mc.cores = future::availableCores())

res <- rbindlist(res)

# Names
library(U.Taxonstand, lib.loc = "/projappl/project_2003061/Rpackages")

all_species <- res %>% 
  select(species) %>% 
  tibble() %>% 
  distinct() %>% 
  filter(!grepl("×",species),
         !grepl(" X ",species),
         !grepl(" x ",species)) %>% 
  rename(Name = species)

# the World Flora Online
load("/scratch/project_2003061/trait_datasets/Plants_WFO.rdata")
all <- nameMatch(spList = all_species, 
                 spSource = database, max.distance = 2,
                 matchFirst = TRUE)

all %>% 
  filter(is.na(Accepted_SPNAME) & is.na(Name_spLev))

df2 <- left_join(res,
                 all %>% 
                   mutate(Submitted_Name = gsub(" ssp", " subsp", Submitted_Name)) %>% 
                   mutate(Accepted_SPNAME = ifelse(is.na(Accepted_SPNAME), Name_spLev, Accepted_SPNAME)) %>% 
                   select(Submitted_Name, Accepted_SPNAME) %>% distinct() %>% 
                   rename(species = Submitted_Name) %>% 
                   rename(WFO_species = Accepted_SPNAME) %>% 
                   distinct() %>% 
                   drop_na())

df2 %>% 
  filter(species != "NA NA") %>% 
  filter(is.na(WFO_species)) %>% 
  pull(species) %>% unique()

library(googlesheets4)
fn <- read_sheet("https://docs.google.com/spreadsheets/d/1qgBOjM_941h4zm0foy83PyT6Og0ocWlDOUHXWmdlWIA/edit?gid=211021139#gid=211021139", col_types = "c")

edited <- inner_join(df2,
                     fn %>% 
                       filter(!is.na(Manually_edited)) %>% 
                       rename(species = taxon, 
                              scrubbed_author = author), by = join_by(species)) %>% 
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

df2 %>% write_csv("/scratch/project_2003061/trait_datasets/TR8/traits_prepared.csv")
