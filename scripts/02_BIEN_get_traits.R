#####################################################################
# Download the BIEN datasets

library(tidyverse)
library(data.table)
library(BIEN)
library(parallel)
library(googlesheets4)


# READ ALL DATA
fn <- read_sheet("https://docs.google.com/spreadsheets/d/1qgBOjM_941h4zm0foy83PyT6Og0ocWlDOUHXWmdlWIA/edit?gid=211021139#gid=211021139", col_types = "c")

all_species <- unique(c(fn$taxon,
                        str_squish(paste(fn$taxon, fn$author, sep = " ")),
                        fn$WFO_species,
                        str_squish(paste(fn$WFO_species, fn$WFO_author, sep = " ")),
                        fn$TPL_species,
                        str_squish(paste(fn$TPL_species, fn$TPL_author, sep = " ")),
                        fn$LCVP_species,
                        str_squish(paste(fn$LCVP_species, fn$LCVP_author, sep = " "))))

all_species <- all_species[!(grepl("×", all_species) | grepl(" x ", all_species) | grepl(" X ", all_species))]
all_species <- all_species[unlist(lapply(all_species, function(x) length(strsplit(x, " ")[[1]]) > 1))]
all_species <- unique(c(all_species, paste(str_split_i(all_species, " ", 1), str_split_i(all_species, " ", 2), sep = " ")))

all_species <- tibble(all_species = all_species,
                      id = 1:length(all_species)) %>% 
  arrange(all_species)

out <- mclapply(seq(1, nrow(all_species), by = 50), function(i){
  # i <- 1
  if(!file.exists(paste0("/scratch/project_2003061/temp/BIEN_traits_",i,".csv"))){
    print(i)
    try({
      name <- all_species$all_species[i:(i+49)]
      name <- gsub("'"," ",name)
      
      temp <- BIEN_trait_species(name, all.taxonomy = T, political.boundaries = F, source.citation = T) %>% 
        distinct(id, .keep_all = TRUE)
      
      temp <- temp %>% 
        select(scrubbed_species_binomial, scrubbed_author, trait_name, trait_value, unit, method, latitude, longitude, 
               url_source, source_citation, id)
      
      if(nrow(temp) > 0){
        write_csv(temp, paste0("/scratch/project_2003061/temp/BIEN_traits_",i,".csv"))
        return(TRUE)
      } else {
        return(FALSE)
      }
    })
  }
}, mc.cores = future::availableCores())


f <- list.files("/scratch/project_2003061/temp", pattern = "BIEN_traits_", full.names = T)
mylist <- lapply(f, function(x) fread(x))
df <- rbindlist(mylist)
rm(mylist)

df <- df %>% distinct()

# Names
library(U.Taxonstand, lib.loc = "/projappl/project_2003061/Rpackages")

all_species <- df %>% 
  select(scrubbed_species_binomial, scrubbed_author) %>% 
  distinct() %>% 
  setNames(c("Name","Author")) %>% 
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

fn %>% 
  filter(!is.na(Manually_edited))

df2 <- left_join(df %>% 
                   mutate(scrubbed_species_binomial = str_squish(scrubbed_species_binomial),
                          scrubbed_author = str_squish(scrubbed_author)) %>% 
                   mutate(scrubbed_author = ifelse(is.na(scrubbed_author), "", scrubbed_author)),
                 bind_cols(all_species %>% setNames(c("scrubbed_species_binomial", "scrubbed_author")), 
                           all) %>% 
                   mutate(Accepted_SPNAME = ifelse(is.na(Accepted_SPNAME), Name_spLev, Accepted_SPNAME)) %>% 
                   select(scrubbed_species_binomial, scrubbed_author, Accepted_SPNAME) %>% distinct() %>% 
                   rename(WFO_species = Accepted_SPNAME) %>% 
                   distinct() %>% 
                   mutate(scrubbed_author = ifelse(is.na(scrubbed_author), "", scrubbed_author)))

df2 %>% 
  filter(is.na(WFO_species))

edited <- inner_join(df2,
                     fn %>% 
                       filter(!is.na(Manually_edited)) %>% 
                       rename(scrubbed_species_binomial = taxon, 
                              scrubbed_author = author), by = join_by(scrubbed_species_binomial, scrubbed_author)) %>% 
  select(scrubbed_species_binomial, scrubbed_author, WFO_species.x, WFO_species.y, Note) %>% 
  unique() %>% 
  filter(WFO_species.x != WFO_species.y)

df2 <- bind_rows(anti_join(df2, edited) %>% 
                   select(scrubbed_species_binomial:WFO_species),
                 inner_join(df2, edited) %>% 
                   mutate(WFO_species = WFO_species.y) %>% 
                   select(scrubbed_species_binomial:WFO_species))

df2 <- df2 %>% 
  mutate(WFO_species = ifelse(WFO_species == "Carex caespitosa", "Carex cespitosa", WFO_species)) %>% 
  filter(WFO_species %in% fn$WFO_species) %>% 
  relocate(WFO_species)

ci <- df2 %>% 
  select(url_source, source_citation) %>% 
  distinct() %>% 
  filter(!(is.na(url_source) & is.na(source_citation))) %>% 
  mutate(BIEN_source = paste0("BIEN_", rownames(.)))

meta <- df2 %>% 
  select(trait_name, unit, method) %>% 
  distinct()

df2 <- left_join(df2, ci) %>% 
  select(-url_source, -source_citation, -method)

df2 %>% write_csv("/scratch/project_2003061/trait_datasets/BIEN/traits_prepared.csv")
ci %>% write_csv("/scratch/project_2003061/trait_datasets/BIEN/citations.csv")
meta %>% write_csv("/scratch/project_2003061/trait_datasets/BIEN/measurement_information.csv")

