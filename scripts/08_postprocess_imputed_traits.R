#####################################################################
# Postprocess the imputed trait dataset

library(tidyverse)
library(data.table)
library(googlesheets4)

d <- fread("output/imputation_traits_300.csv")

d <- d %>% 
  mutate(height = exp(height-1),
         height_reproductive = exp(height_reproductive-1),
         leaf_area = exp(leaf_area-1),
         leaf_area_leaflet = exp(leaf_area_leaflet-1),
         seed_mass = exp(seed_mass-1),
         leaf_mass = exp(leaf_mass-1)) %>% 
  mutate(LEDA_seed_number_flower = exp(LEDA_seed_number_flower-1),
         LEDA_seed_number_plant = exp(LEDA_seed_number_plant-1),
         leaf_width = exp(leaf_width-1),
         leaf_length = exp(leaf_length-1),
         leaf_thickness = exp(leaf_thickness-1),
         SRL = exp(SRL-1),
         root_depth = exp(root_depth-1),
         root_P = exp(root_P-1),
         SLA = exp(SLA-1))

tr <- read_sheet("https://docs.google.com/spreadsheets/d/18K7Ff3yPcQ1ungiWKLiBc9FXZspxtp6LamimXXS3scE/edit?gid=832928905#gid=832928905", col_types = "c")
sp <- read_sheet("https://docs.google.com/spreadsheets/d/1qgBOjM_941h4zm0foy83PyT6Og0ocWlDOUHXWmdlWIA/edit?gid=211021139#gid=211021139", col_types = "c")

d <- d %>% 
  select(WFO_species, 
         all_of(tr %>% filter(to_final_dataset == 1) %>% arrange(category, trait) %>% pull(trait)))

tr %>% filter(integer == 1) %>% pull(trait)

d <- right_join(sp %>% select(WFO_species, WFO_family, WFO_author) %>% 
             mutate(WFO_genus = str_split_i(WFO_species, " ", 1)) %>% 
             relocate(WFO_author, WFO_genus, .after = WFO_species) %>% 
               distinct(WFO_species, .keep_all = TRUE),
           d) %>% 
  mutate(across(c(Lososova_dispersal_distance_class,palatability,leaf_C,leaf_CN_ratio,leaf_N,leaf_NP_ratio,
                  leaf_P,Tyler_longevity,Tyler_nectar_production,Tyler_seed_bank,Tyler_seed_dormancy,
                  mycorrhiza_colonization_intensity,root_N,height,leaf_area, leaf_length,leaf_mass), ~as.numeric(round(.x, 1))),
         across(all_of(tr %>% filter(integer == 1) %>% pull(trait)),
                ~as.numeric(round(.x, 0))),
         across(c(LEDA_terminal_velocity,buoyancy,SLA,leaf_chlorophyll,LEDA_seed_longevity_index,seed_mass,
                  SRA,SRL,root_P,root_depth), ~as.numeric(round(.x, 2))),
         across(c(LDMC,RDMC,leaf_thickness), ~as.numeric(round(.x, 3))))

d %>% select(where(is.numeric)) %>% 
  summary()

summary(d)
glimpse(d)

dl <- d %>% 
  select(-WFO_author, -WFO_genus, -WFO_family) %>% 
  mutate(across(LEDA_terminal_velocity:last_col(), as.character)) %>% 
  pivot_longer(cols = LEDA_terminal_velocity:last_col(), names_to = "trait_name", values_to = "value")

do <- fread("output/traits_missing_finalized.csv") %>% 
  select(WFO_species, all_of(unique(dl$trait_name))) %>% 
  mutate(across(everything(), as.character)) %>% 
  pivot_longer(cols = LEDA_terminal_velocity:last_col(), names_to = "trait_name", values_to = "value") %>% 
  drop_na() %>% 
  mutate(imputed = FALSE) %>% 
  select(-value)

dl <- left_join(dl, do) %>% 
  mutate(imputed = ifelse(is.na(imputed), TRUE, FALSE))


dl <- left_join(dl,
          tr %>% 
            select(trait, category, final_trait_name, unit) %>% 
            rename(trait_name = trait)) %>% 
  select(WFO_species, final_trait_name, value, unit, imputed, category) %>% 
  rename(species = WFO_species,
         trait_name = final_trait_name) %>% 
  arrange(species, category, trait_name)

dl %>% write_csv("output/Nordic_imputed_traits_long.csv")

dl %>% 
  pivot_wider(id_cols = species, names_from = trait_name, values_from = value) %>% 
  write_csv("output/Nordic_imputed_traits_wide.csv")

dl %>% filter(trait_name == "longevity", value == 0)

dl %>% filter(trait_name == "dispersal_distance_class") %>% pull(value) %>% table
dl %>% filter(trait_name == "dispersal_distance_class", imputed == FALSE) %>% pull(value) %>% table

res <- lapply(unique(dl$trait_name), function(x){
  # x <- "dispersal_vector"
  vals <- dl %>% filter(trait_name == x) %>% pull(value)
  if(tr %>% filter(final_trait_name == x) %>% pull(type) == "numerical"){
    vals <- as.numeric(vals)
    res <- tibble(trait = x,
                  values = paste0("min = ", min(vals), "; max = ", max(vals)))
  } else {
    res <- tibble(trait = x,
                  values = paste(sort(unique(vals)), collapse = "; "))
  }
  return(res)
}) %>% 
  bind_rows()

tr2 <- readxl::read_xlsx("output/Final_trait_table.xlsx")

tr2 <- left_join(tr2, res)

tr2 %>% writexl::write_xlsx("output/Final_trait_table2.xlsx")
