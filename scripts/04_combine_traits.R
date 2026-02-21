#####################################################################
# Combine & harmonize all preprocessed trait datasets

library(tidyverse)
library(data.table)
library(googlesheets4)
library(scales)

get_mode <- function(x) {
  freq_table <- table(x)
  max_freq <- max(freq_table)
  modes <- as.vector(names(freq_table[freq_table == max_freq]))
  if (length(modes) > 1) {
    sample(modes, 1)
  } else {
    modes
  }
}
calculate_cell_mean <- function(x) {
  values <- as.numeric(str_split(x, ";")[[1]])
  as.character(mean(values))
}

tr <- read_sheet("https://docs.google.com/spreadsheets/d/1-iuDW_8hcFxWyX4h19wWoj3APBd0fvZpqHcA31cOFrA/edit?gid=1056781006#gid=1056781006", col_types = "c")

f <- list.files("/scratch/project_2003061/trait_datasets", pattern = "traits_prepared.csv", 
                full.names = TRUE, recursive = TRUE)

################ Read the trait data in ############
# TRY
db <- "TRY"
d1 <- fread(f[grepl(db,f)])
m <- fread(gsub("traits_prepared.csv","measurement_information.csv",f[grepl(db,f)]))
d1 <- left_join(d1, m %>% select(TraitID, TraitName) %>% distinct()) %>% 
  rename(trait_name = TraitName) %>% 
  filter(!is.na(StdValue))

d1 <- full_join(d1, tr %>% filter(database == db)) %>% 
  select(WFO_species, final_trait_name, StdValue, UnitName,
         combine, priority, TRY_source, database, OriglName) %>% 
  rename(trait = final_trait_name,
         value = StdValue,
         unit = UnitName,
         dataset = TRY_source) %>% 
  mutate(dataset = ifelse(is.na(dataset), db, dataset),
         value = as.character(value)) %>% 
  filter(!is.na(trait))

# BIEN
db <- "BIEN"
d2 <- fread(f[grepl(db,f)])

d2 <- full_join(d2, tr %>% filter(database == db)) %>% 
  select(WFO_species, final_trait_name, trait_value, unit,
         combine, priority, BIEN_source, database) %>% 
  rename(trait = final_trait_name,
         value = trait_value,
         unit = unit,
         dataset = BIEN_source) %>% 
  mutate(dataset = ifelse(is.na(dataset), db, dataset),
         value = as.character(value))

# FRED
db <- "FRED"
d3 <- fread(f[grepl(db,f)]) %>% 
  rename(trait_name = Trait)

d3 <- full_join(d3, tr %>% filter(database == db)) %>% 
  select(WFO_species, final_trait_name, Value,
         combine, priority, database) %>% 
  rename(trait = final_trait_name,
         value = Value) %>% 
  mutate(dataset = db,
         value = as.character(value))


# GIFT
db <- "GIFT"
d4 <- fread(f[grepl(db,f)]) %>% 
  rename(trait_name = Trait2)

d4 <- full_join(d4, tr %>% filter(database == db)) %>% 
  select(WFO_species, final_trait_name, trait_value,
         combine, priority, database) %>% 
  rename(trait = final_trait_name,
         value = trait_value) %>% 
  mutate(dataset = db,
         value = as.character(value))

# GRoot
db <- "GRoot"
d5 <- fread(f[grepl(db,f)]) %>% 
  rename(trait_name = Trait)

d5 <- full_join(d5, tr %>% filter(database == db)) %>% 
  select(WFO_species, final_trait_name, Value,
         combine, priority, database) %>% 
  rename(trait = final_trait_name,
         value = Value) %>% 
  mutate(dataset = db,
         value = as.character(value))

# LEDA
db <- "LEDA"
d6 <- fread(f[grepl(db,f)]) %>% 
  rename(trait_name = Trait)

d6 <- full_join(d6, tr %>% filter(database == db)) %>% 
  select(WFO_species, final_trait_name, Value, Unit,
         combine, priority, database) %>% 
  rename(trait = final_trait_name,
         value = Value,
         unit = Unit) %>% 
  mutate(dataset = db,
         value = as.character(value))

# Niittynen
db <- "Niittynen"
d7 <- fread(f[grepl(db,f)]) %>% 
  rename(trait_name = Trait)

d7 <- full_join(d7, tr %>% filter(database == db)) %>% 
  select(WFO_species, final_trait_name, Value,
         dataset, combine, priority, database) %>% 
  rename(trait = final_trait_name,
         value = Value) %>% 
  mutate(dataset = paste(database, dataset, sep = "_"),
         value = as.character(value))

# TR8
db <- "TR8"
d8 <- fread(f[grepl(db,f)]) %>% 
  rename(trait_name = name)

d8 <- full_join(d8, tr %>% filter(database == db)) %>% 
  select(WFO_species, final_trait_name, value,
         combine, priority, database) %>% 
  rename(trait = final_trait_name,
         value = value) %>% 
  mutate(dataset = db,
         value = as.character(value))

# TTT
db <- "TTT"
d9 <- fread(f[grepl(db,f)]) %>% 
  rename(trait_name = Trait)

d9 <- full_join(d9, tr %>% filter(database == db)) %>% 
  select(WFO_species, final_trait_name, Value, Units,
         combine, priority, TTT_source, database) %>% 
  rename(trait = final_trait_name,
         value = Value,
         unit = Units,
         dataset = TTT_source) %>% 
  mutate(dataset = ifelse(is.na(dataset), db, dataset),
         value = as.character(value))

# Tyler
db <- "Tyler"
d10 <- fread(f[grepl(db,f)]) %>% 
  rename(trait_name = trait)

d10 <- full_join(d10, tr %>% filter(database == db)) %>% 
  select(WFO_species, final_trait_name, value,
         combine, priority, database) %>% 
  rename(trait = final_trait_name,
         value = value) %>% 
  mutate(dataset = db,
         value = as.character(value))

# The rest
d11 <- bind_rows(fread(f[grepl("Baruah",f)]) %>% 
                   select(WFO_species, Trait, Value) %>% 
                   mutate(database = "Baruah") %>% 
                   mutate(Value = as.character(Value)),
                 fread(f[grepl("Majekova",f)]) %>% 
                   select(WFO_species, Trait, Value) %>% 
                   mutate(database = "Majekova") %>% 
                   mutate(Value = as.character(Value)),
                 fread(f[grepl("Mudrak",f)]) %>% 
                   select(WFO_species, Trait, Value) %>% 
                   mutate(database = "Mudrak") %>% 
                   mutate(Value = as.character(Value)),
                 fread(f[grepl("Tichy",f)]) %>% 
                   pivot_longer(cols = ELLENBERG_LIGHT:ELLENBERG_SALINITY, names_to = "Trait", values_to = "Value") %>% 
                   mutate(database = "Tichy") %>% 
                   mutate(Value = as.character(Value)),
                 fread(f[grepl("Zanne",f)]) %>% 
                   select(WFO_species, Trait, Value) %>% 
                   mutate(database = "Zanne") %>% 
                   mutate(Value = as.character(Value)),
                 fread(f[grepl("Zuijlen",f)]) %>% 
                   pivot_longer(cols = leaf_N:LDMC, names_to = "Trait", values_to = "Value") %>% 
                   select(WFO_species, Trait, Value) %>% 
                   mutate(database = "Zuijlen") %>% 
                   mutate(Value = as.character(Value)),
                 fread(f[grepl("Drevojan",f)]) %>% 
                   pivot_longer(cols = Phanerophyte:`Herbaceous liana`, names_to = "Trait", values_to = "Value") %>% 
                   select(WFO_species, Trait, Value) %>% 
                   mutate(database = "Drevojan") %>% 
                   mutate(Value = as.character(Value)),
                 fread(f[grepl("Lososova",f)]) %>% 
                   mutate(across(height:dispersal_anthropogenic, as.character)) %>% 
                   pivot_longer(cols = height:dispersal_anthropogenic, names_to = "Trait", values_to = "Value") %>% 
                   select(WFO_species, Trait, Value) %>% 
                   mutate(database = "Lososova") %>% 
                   mutate(Value = as.character(Value)),
                 fread(f[grepl("Midolo",f)]) %>% 
                   mutate(across(N_EUNIS_habitats:soil_disturbance_indicator, as.character)) %>% 
                   pivot_longer(cols = N_EUNIS_habitats:soil_disturbance_indicator, names_to = "Trait", values_to = "Value") %>% 
                   select(WFO_species, Trait, Value) %>% 
                   mutate(database = "Midolo") %>% 
                   mutate(Value = as.character(Value)),
                 fread(f[grepl("Tesitel",f)]) %>% 
                   select(WFO_species, Trait, Value) %>% 
                   mutate(database = "Tesitel") %>% 
                   mutate(Value = as.character(Value))) %>% 
  rename(trait_name = Trait,
         value = Value) %>% 
  mutate(dataset = database)

d11 <- left_join(d11, tr) %>% 
  select(WFO_species, final_trait_name, value,
         combine, priority, database, dataset) %>% 
  rename(trait = final_trait_name,
         value = value) %>% 
  mutate(value = as.character(value))

################ Combine ############################

all <- bind_rows(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11) %>% 
  relocate(database) %>% 
  drop_na(trait)

all <- all %>% 
  mutate(trait2 = ifelse(is.na(combine), paste(database, trait, sep = "_"), trait))

full_join(all %>% group_by(database) %>% summarise(n_obs = n()),
          all %>% group_by(database, trait) %>% count() %>% group_by(database) %>% summarise(n_traits = n())) %>% 
  full_join(.,
            all %>% group_by(database, WFO_species) %>% count() %>% group_by(database) %>% summarise(n_species = n())) %>% 
  writexl::write_xlsx("output/Final_source_table.xlsx")

traits_names <- unique(all$trait)

traits <- tibble()
################ SLA ##################################

all %>% 
  filter(trait == "SLA") %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(value = ifelse(database == "GIFT", value/10, value)) %>% 
  mutate(value = ifelse(dataset %in% c("BIEN_240","BIEN_57"), value/10, value)) %>% 
  mutate(value = ifelse(dataset %in% c("BIEN_9"), value*100, value)) %>% 
  mutate(value = ifelse(dataset %in% c("BIEN_10","BIEN_13","BIEN_105","BIEN_241","BIEN_243","BIEN_355","BIEN_83"), value*1000000, value)) %>% 
  group_by(dataset) %>% 
  summarise(value = mean(as.numeric(value), na.rm = TRUE)) %>% mutate(value = round(value, 3)) # %>% view

traits <- all %>% 
  filter(trait == "SLA") %>% 
  filter(!dataset %in% c("BIEN_58")) %>% 
  mutate(value = as.numeric(value)) %>% 
  filter(!is.na(value)) %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(value = ifelse(database == "GIFT", value/10, value)) %>% 
  mutate(value = ifelse(dataset %in% c("BIEN_240","BIEN_57"), value/10, value)) %>% 
  mutate(value = ifelse(dataset %in% c("BIEN_9"), value*100, value)) %>% 
  mutate(value = ifelse(dataset %in% c("BIEN_10","BIEN_13","BIEN_105","BIEN_241","BIEN_243","BIEN_355","BIEN_83"), value*1000000, value)) %>% 
  filter(value > 1) %>% 
  group_by(WFO_species, dataset) %>% 
  summarise(value = median(value, na.rm = TRUE)) %>% 
  group_by(WFO_species) %>% 
  summarise(SLA = mean(value, na.rm = TRUE)) %>% 
  drop_na()

################ LDMC #########################################

all %>% 
  filter(trait == "LDMC") %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(value = ifelse(database %in% c("BIEN","LEDA","Majekova","Mudrak"), value/1000, value)) %>% 
  mutate(value = ifelse(dataset %in% c("BIEN_355","BIEN_82"), value*100, value)) %>% 
  group_by(dataset) %>% 
  summarise(value = median(as.numeric(value), na.rm = TRUE))# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "LDMC") %>% 
                      mutate(value = as.numeric(value)) %>% 
                      mutate(value = ifelse(database %in% c("BIEN","LEDA"), value/1000, value)) %>% 
                      mutate(value = ifelse(dataset %in% c("BIEN_355","BIEN_82"), value*100, value)) %>% 
                      filter(value < 1) %>% 
                      group_by(WFO_species, dataset) %>% 
                      summarise(value = median(value, na.rm = TRUE)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(LDMC = mean(value, na.rm = TRUE)) %>% 
                      drop_na())

################ Seed mass ######################################

bind_rows(all %>% 
            filter(trait == "seed_mass",
                   database != "TR8"),
          all %>% 
            filter(trait == "seed_mass",
                   database == "TR8") %>% 
            mutate(value = sapply(value, calculate_cell_mean))) %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(value = ifelse(database %in% c("GIFT"), value*1000, value)) %>% 
  mutate(value = ifelse(dataset %in% c("TRY_196","BIEN_72"), value/10000, value)) %>%
  filter(value > 0) %>% 
  mutate(value = log(as.numeric(value)+1)) %>% 
  group_by(dataset) %>% 
  summarise(value = median(as.numeric(value), na.rm = TRUE))# %>% view

traits <- full_join(traits,
                    bind_rows(all %>% 
                                filter(trait == "seed_mass",
                                       database != "TR8"),
                              all %>% 
                                filter(trait == "seed_mass",
                                       database == "TR8") %>% 
                                mutate(value = sapply(value, calculate_cell_mean))) %>% 
                      mutate(value = as.numeric(value)) %>% 
                      mutate(value = ifelse(database %in% c("GIFT"), value*1000, value)) %>% 
                      mutate(value = ifelse(dataset %in% c("TRY_196","BIEN_72"), value/10000, value)) %>%
                      filter(value > 0) %>% 
                      mutate(value = log(as.numeric(value)+1)) %>% 
                      group_by(WFO_species, dataset) %>% 
                      summarise(value = median(value, na.rm = TRUE)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(seed_mass = mean(value, na.rm = TRUE)) %>% 
                      drop_na())

################ Leaf area #################################################

all %>% 
  filter(trait == "leaf_area",
         database != "TR8") %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(value = ifelse(database %in% c("GIFT","Majekova","Niittynen"), value*100, value)) %>% 
  mutate(value = ifelse(dataset %in% c("BIEN_120"), value/100, value)) %>%
  filter(value > 0) %>% 
  mutate(value = log(as.numeric(value)+1)) %>% 
  group_by(dataset) %>% 
  summarise(value = median(as.numeric(value), na.rm = TRUE))# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "leaf_area",
                             database != "TR8") %>% 
                      mutate(value = as.numeric(value)) %>% 
                      mutate(value = ifelse(database %in% c("GIFT","Majekova","Niittynen"), value*100, value)) %>% 
                      mutate(value = ifelse(dataset %in% c("BIEN_120"), value/100, value)) %>%
                      filter(value > 0) %>% 
                      mutate(value = log(as.numeric(value)+1)) %>% 
                      group_by(WFO_species, dataset) %>% 
                      summarise(value = median(value, na.rm = TRUE)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(leaf_area = mean(value, na.rm = TRUE)) %>% 
                      drop_na())

################ Height ########################################

bind_rows(all %>% 
            filter(trait == "height",
                   database != "TR8"),
          all %>% 
            filter(trait == "height",
                   database == "TR8") %>% 
            mutate(value = sapply(value, calculate_cell_mean))) %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(value = ifelse(database %in% c("BIEN","GIFT","LEDA","Lososova","TRY","TTT"), value*100, value)) %>%
  mutate(value = ifelse(dataset %in% c("Baruah"), value/10, value)) %>%
  filter(value > 0) %>% 
  mutate(value = log(as.numeric(value)+1)) %>% 
  group_by(dataset) %>% 
  summarise(value = median(as.numeric(value), na.rm = TRUE))# %>% view

traits <- full_join(traits,
                    bind_rows(all %>% 
                                filter(trait == "height",
                                       database != "TR8"),
                              all %>% 
                                filter(trait == "height",
                                       database == "TR8") %>% 
                                mutate(value = sapply(value, calculate_cell_mean))) %>% 
                      mutate(value = as.numeric(value)) %>% 
                      mutate(value = ifelse(database %in% c("BIEN","GIFT","LEDA","Lososova","TRY","TTT"), value*100, value)) %>%
                      mutate(value = ifelse(dataset %in% c("Baruah"), value/10, value)) %>%
                      filter(value > 0) %>% 
                      mutate(value = log(as.numeric(value)+1)) %>% 
                      group_by(WFO_species, dataset) %>% 
                      summarise(value = median(value, na.rm = TRUE)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(height = mean(value, na.rm = TRUE)) %>% 
                      drop_na())

################ Height generative #########################################

all %>% 
  filter(trait == "height_reproductive") %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(value = ifelse(database %in% c("BIEN","GIFT","LEDA","Lososova","TRY","TTT"), value*100, value)) %>%
  mutate(value = ifelse(dataset %in% c("Baruah"), value/10, value)) %>%
  filter(value > 0) %>% 
  mutate(value = log(as.numeric(value)+1)) %>% 
  group_by(dataset) %>% 
  summarise(value = median(as.numeric(value), na.rm = TRUE))# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "height_reproductive") %>% 
                      mutate(value = as.numeric(value)) %>% 
                      mutate(value = ifelse(database %in% c("BIEN","GIFT","LEDA","Lososova","TRY","TTT"), value*100, value)) %>%
                      mutate(value = ifelse(dataset %in% c("Baruah"), value/10, value)) %>%
                      filter(value > 0) %>% 
                      mutate(value = log(as.numeric(value)+1)) %>% 
                      group_by(WFO_species, dataset) %>% 
                      summarise(value = median(value, na.rm = TRUE)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(height_reproductive = mean(value, na.rm = TRUE)) %>% 
                      drop_na())

################ leaf_13C################################

all %>% 
  filter(trait == "leaf_13C") %>% 
  mutate(value = as.numeric(value)) %>% 
  filter(!(dataset == "TRY_48" & OriglName != "little.d13.org")) %>%
  group_by(dataset) %>% 
  summarise(value = median(as.numeric(value), na.rm = TRUE))# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "leaf_13C") %>% 
                      mutate(value = as.numeric(value)) %>% 
                      filter(!(dataset == "TRY_48" & OriglName != "little.d13.org")) %>%
                      group_by(WFO_species, dataset) %>% 
                      summarise(value = median(value, na.rm = TRUE)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(leaf_13C = mean(value, na.rm = TRUE)) %>% 
                      drop_na())

################ leaf_15N ###############################

all %>% 
  filter(trait == "leaf_15N") %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(dataset) %>% 
  summarise(value = median(as.numeric(value), na.rm = TRUE))# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "leaf_15N") %>% 
                      mutate(value = as.numeric(value)) %>% 
                      group_by(WFO_species, dataset) %>% 
                      summarise(value = median(value, na.rm = TRUE)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(leaf_15N = mean(value, na.rm = TRUE)) %>% 
                      drop_na())

################ leaf_area_leaflet #########################

all %>% 
  filter(trait == "leaf_area_leaflet") %>% 
  mutate(value = as.numeric(value)) %>%
  filter(value > 0) %>% 
  mutate(value = log(as.numeric(value)+1)) %>% 
  group_by(dataset) %>% 
  summarise(value = median(as.numeric(value), na.rm = TRUE))# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "leaf_area_leaflet") %>% 
                      mutate(value = as.numeric(value)) %>%
                      filter(value > 0) %>% 
                      mutate(value = log(as.numeric(value)+1)) %>% 
                      group_by(WFO_species, dataset) %>% 
                      summarise(value = median(value, na.rm = TRUE)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(leaf_area_leaflet = mean(value, na.rm = TRUE)) %>% 
                      drop_na())

################ leaf_C ###################################

all %>% 
  filter(trait == "leaf_C") %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(value = ifelse(dataset %in% c("Mudrak","Zuijlen"), value*10, value)) %>% 
  group_by(dataset) %>% 
  summarise(value = median(as.numeric(value), na.rm = TRUE))# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "leaf_C") %>% 
                      mutate(value = as.numeric(value)) %>% 
                      mutate(value = ifelse(dataset %in% c("Mudrak","Zuijlen"), value*10, value)) %>% 
                      group_by(WFO_species, dataset) %>% 
                      summarise(value = median(value, na.rm = TRUE)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(leaf_C = mean(value, na.rm = TRUE)) %>% 
                      drop_na())

################ leaf_CN_ratio #################################################

all %>% 
  filter(trait == "leaf_CN_ratio",
         dataset != "TR8",
         dataset != "TRY_136") %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(dataset) %>% 
  summarise(value = median(as.numeric(value), na.rm = TRUE))# %>% view

all %>% 
  filter(trait == "leaf_CN_ratio",
         dataset == "TRY_136") %>% 
  mutate(value = as.numeric(value)) %>% pull(WFO_species)

all %>% 
  filter(trait == "leaf_CN_ratio",
         WFO_species == "Verbascum thapsus") %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(dataset) %>% 
  summarise(seed_mass = mean(value, na.rm = TRUE))

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "leaf_CN_ratio",
                             dataset != "TR8",
                             dataset != "TRY_136") %>% 
                      mutate(value = as.numeric(value)) %>% 
                      group_by(WFO_species, dataset) %>% 
                      summarise(value = median(value, na.rm = TRUE)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(leaf_CN_ratio = mean(value, na.rm = TRUE)) %>% 
                      drop_na())

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "leaf_CN_ratio",
                             dataset == "TR8") %>% 
                      filter(value != "") %>% 
                      select(WFO_species, value) %>% 
                      rename(leaf_CN_ratio_class = value))

################ leaf_length ##################

all %>% 
  filter(trait == "leaf_length") %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(value = ifelse(dataset %in% c("GIFT","TRY_222"), value*10, value)) %>%
  group_by(dataset) %>% 
  summarise(value = median(as.numeric(value), na.rm = TRUE))# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "leaf_length") %>% 
                      mutate(value = as.numeric(value)) %>% 
                      mutate(value = ifelse(dataset %in% c("GIFT","TRY_222"), value*10, value)) %>% 
                      group_by(WFO_species, dataset) %>% 
                      summarise(value = median(value, na.rm = TRUE)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(leaf_length = mean(value, na.rm = TRUE)) %>% 
                      drop_na())

################ leaf_mass ##################

all %>% 
  filter(trait == "leaf_mass") %>% 
  filter(dataset != "BIEN_240") %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(value = ifelse(database %in% c("Niittynen","BIEN"), value*1000, value)) %>%
  mutate(value = ifelse(dataset %in% c("TTT_47"), value*1000, value)) %>%
  mutate(value = log(as.numeric(value)+1)) %>% 
  group_by(dataset) %>% 
  summarise(value = median(as.numeric(value), na.rm = TRUE))# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "leaf_mass") %>% 
                      filter(dataset != "BIEN_240") %>% 
                      mutate(value = as.numeric(value)) %>% 
                      mutate(value = ifelse(database %in% c("Niittynen","BIEN"), value*1000, value)) %>%
                      mutate(value = ifelse(dataset %in% c("TTT_47"), value*1000, value)) %>%
                      mutate(value = log(as.numeric(value)+1)) %>% 
                      group_by(WFO_species, dataset) %>% 
                      summarise(value = median(value, na.rm = TRUE)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(leaf_mass = mean(value, na.rm = TRUE)) %>% 
                      drop_na())

################ leaf_N ##################

all %>% 
  filter(trait == "leaf_N") %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(value = ifelse(dataset %in% c("Zuijlen","Mudrak"), value*10, value)) %>%
  group_by(dataset) %>% 
  summarise(value = median(as.numeric(value), na.rm = TRUE))# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "leaf_N") %>% 
                      mutate(value = as.numeric(value)) %>% 
                      mutate(value = ifelse(dataset %in% c("Zuijlen","Mudrak"), value*10, value)) %>%
                      group_by(WFO_species, dataset) %>% 
                      summarise(value = median(value, na.rm = TRUE)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(leaf_N = mean(value, na.rm = TRUE)) %>% 
                      drop_na())

################ leaf_N_area ##################

all %>% 
  filter(trait == "leaf_N_area") %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(value = ifelse(database %in% c("BIEN"), value*1000, value)) %>%
  group_by(dataset) %>% 
  summarise(value = median(as.numeric(value), na.rm = TRUE))# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "leaf_N_area") %>% 
                      mutate(value = as.numeric(value)) %>% 
                      mutate(value = ifelse(database %in% c("BIEN"), value*1000, value)) %>%
                      group_by(WFO_species, dataset) %>% 
                      summarise(value = median(value, na.rm = TRUE)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(leaf_N_area = mean(value, na.rm = TRUE)) %>% 
                      drop_na())

################ leaf_NP_ratio ##################

all %>% 
  filter(trait == "leaf_NP_ratio") %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(dataset) %>% 
  summarise(value = median(as.numeric(value), na.rm = TRUE))# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "leaf_NP_ratio") %>% 
                      mutate(value = as.numeric(value)) %>% 
                      group_by(WFO_species, dataset) %>% 
                      summarise(value = median(value, na.rm = TRUE)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(leaf_NP_ratio = mean(value, na.rm = TRUE)) %>% 
                      drop_na())

################ leaf_P ##################

all %>% 
  filter(trait == "leaf_P") %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(dataset) %>% 
  summarise(value = median(as.numeric(value), na.rm = TRUE))# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "leaf_P") %>% 
                      mutate(value = as.numeric(value)) %>% 
                      group_by(WFO_species, dataset) %>% 
                      summarise(value = median(value, na.rm = TRUE)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(leaf_P = mean(value, na.rm = TRUE)) %>% 
                      drop_na())

################ leaf_P_area ##################

all %>% 
  filter(trait == "leaf_P_area") %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(dataset) %>% 
  summarise(value = median(as.numeric(value), na.rm = TRUE))# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "leaf_P_area") %>% 
                      mutate(value = as.numeric(value)) %>% 
                      group_by(WFO_species, dataset) %>% 
                      summarise(value = median(value, na.rm = TRUE)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(leaf_P_area = mean(value, na.rm = TRUE)) %>% 
                      drop_na())

################ leaf_thickness ##################

all %>% 
  filter(trait == "leaf_thickness") %>% 
  filter(dataset != "TRY_223") %>%
  mutate(value = as.numeric(value)) %>% 
  mutate(value = ifelse(dataset %in% c("Majekova"), value/1000, value)) %>%
  group_by(dataset) %>% 
  summarise(value = median(as.numeric(value), na.rm = TRUE))# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "leaf_thickness") %>% 
                      filter(dataset != "TRY_223") %>%
                      mutate(value = as.numeric(value)) %>% 
                      mutate(value = ifelse(dataset %in% c("Majekova"), value/1000, value)) %>%
                      group_by(WFO_species, dataset) %>% 
                      summarise(value = median(value, na.rm = TRUE)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(leaf_thickness = mean(value, na.rm = TRUE)) %>% 
                      drop_na())

################ leaf_width ##################

all %>% 
  filter(trait == "leaf_width") %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(value = ifelse(dataset %in% c("Baruah"), value/10, value)) %>%
  group_by(dataset) %>% 
  summarise(value = median(as.numeric(value), na.rm = TRUE))# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "leaf_width") %>% 
                      mutate(value = as.numeric(value)) %>% 
                      mutate(value = ifelse(dataset %in% c("Baruah"), value/10, value)) %>%
                      group_by(WFO_species, dataset) %>% 
                      summarise(value = median(value, na.rm = TRUE)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(leaf_width = mean(value, na.rm = TRUE)) %>% 
                      drop_na())

################ leaf_width ##################

all %>% 
  filter(trait == "leaf_width") %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(value = ifelse(dataset %in% c("Baruah"), value/10, value)) %>%
  group_by(dataset) %>% 
  summarise(value = median(as.numeric(value), na.rm = TRUE))# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "leaf_width") %>% 
                      mutate(value = as.numeric(value)) %>% 
                      mutate(value = ifelse(dataset %in% c("Baruah"), value/10, value)) %>%
                      group_by(WFO_species, dataset) %>% 
                      summarise(value = median(value, na.rm = TRUE)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(leaf_width = mean(value, na.rm = TRUE)) %>% 
                      drop_na())

################ RDMC ##################

all %>% 
  filter(trait == "RDMC") %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(dataset) %>% 
  summarise(value = median(as.numeric(value), na.rm = TRUE))# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "RDMC") %>% 
                      mutate(value = as.numeric(value)) %>% 
                      group_by(WFO_species, dataset) %>% 
                      summarise(value = median(value, na.rm = TRUE)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(RDMC = mean(value, na.rm = TRUE)) %>% 
                      drop_na())

################ root_N ##################

all %>% 
  filter(trait == "root_N") %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(dataset) %>% 
  summarise(value = median(as.numeric(value), na.rm = TRUE))# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "root_N") %>% 
                      mutate(value = as.numeric(value)) %>% 
                      group_by(WFO_species, dataset) %>% 
                      summarise(value = median(value, na.rm = TRUE)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(root_N = mean(value, na.rm = TRUE)) %>% 
                      drop_na())

################ root_P ##################

all %>% 
  filter(trait == "root_P") %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(dataset) %>% 
  summarise(value = median(as.numeric(value), na.rm = TRUE))# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "root_P") %>% 
                      mutate(value = as.numeric(value)) %>% 
                      group_by(WFO_species, dataset) %>% 
                      summarise(value = median(value, na.rm = TRUE)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(root_P = mean(value, na.rm = TRUE)) %>% 
                      drop_na())

################ SRA ##################

all %>% 
  filter(trait == "SRA") %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(dataset) %>% 
  summarise(value = median(as.numeric(value), na.rm = TRUE))# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "SRA") %>% 
                      mutate(value = as.numeric(value)) %>% 
                      group_by(WFO_species, dataset) %>% 
                      summarise(value = median(value, na.rm = TRUE)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(SRA = mean(value, na.rm = TRUE)) %>% 
                      drop_na())

################ SRL ##################

all %>% 
  filter(trait == "SRL") %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(dataset) %>% 
  summarise(value = median(as.numeric(value), na.rm = TRUE))# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "SRL") %>% 
                      mutate(value = as.numeric(value)) %>% 
                      group_by(WFO_species, dataset) %>% 
                      summarise(value = median(value, na.rm = TRUE)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(SRL = mean(value, na.rm = TRUE)) %>% 
                      drop_na())

################ age_first_flowering ##################

all %>% 
  filter(trait == "age_first_flowering") %>% 
  mutate(value = str_split_i(value, " \\+ ", 1)) %>% 
  group_by(dataset, value) %>% 
  count# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "age_first_flowering") %>% 
                      mutate(value = str_split_i(value, " \\+ ", 1)) %>% 
                      group_by(trait, WFO_species) %>% 
                      summarise(age_first_flowering = get_mode(value)) %>% 
                      ungroup() %>% 
                      select(-trait))

################ aquatic ##################

all %>% 
  filter(trait == "aquatic") %>% 
  mutate(value = ifelse(value == "aquatic/semiaquatic", "semiaquatic", value)) %>% 
  # mutate(value = str_split_i(value, " \\+ ", 1)) %>% 
  group_by(dataset, value) %>% 
  count# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "aquatic") %>% 
                      mutate(value = ifelse(value == "aquatic/semiaquatic", "semiaquatic", value)) %>% 
                      group_by(trait, WFO_species) %>% 
                      summarise(aquatic = get_mode(value)) %>% 
                      ungroup() %>% 
                      select(-trait))

################ buoyancy ##################

all %>% 
  filter(trait == "buoyancy") %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(WFO_species) %>% 
  summarise(value = mean(as.numeric(value), na.rm = TRUE))# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "buoyancy") %>% 
                      mutate(value = as.numeric(value)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(buoyancy = mean(as.numeric(value), na.rm = TRUE)))

################ carnivory ##################

all %>% 
  filter(trait == "carnivory") %>% 
  group_by(dataset, value) %>% 
  count# %>% view

all %>% 
  filter(trait == "carnivory") %>% 
  mutate(value = ifelse(value %in% c("carnivorous","1"), "yes", "no")) %>% 
  group_by(WFO_species) %>% 
  summarise(carnivory = get_mode(value)) %>% pull(carnivory) %>% table


traits <- full_join(traits,
                    all %>% 
                      filter(trait == "carnivory") %>% 
                      mutate(value = ifelse(value %in% c("carnivorous","1"), "yes", "no")) %>% 
                      group_by(WFO_species) %>% 
                      summarise(carnivory = get_mode(value)))

################ climber ##################

all %>% 
  filter(startsWith(trait, "climber")) %>% 
  mutate(value = ifelse(value == "liana/vine", "liana", value)) %>% 
  group_by(trait, value) %>% 
  count# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(startsWith(trait, "climber")) %>% 
                      mutate(value = ifelse(value == "liana/vine", "liana", value)) %>% 
                      group_by(trait, WFO_species) %>% 
                      summarise(value = get_mode(value)) %>% 
                      pivot_wider(id_cols = WFO_species, names_from = trait, values_from = value))

################ Tyler traits ##############################################

traits <- full_join(traits,
                    all %>% 
                      filter(startsWith(trait2, "Tyler_")) %>% 
                      filter(trait != "carnivory") %>% 
                      select(WFO_species, trait2, value) %>% 
                      group_by(WFO_species, trait2) %>% 
                      slice_head(n = 1) %>% 
                      drop_na() %>% 
                      distinct() %>% 
                      pivot_wider(id_cols = WFO_species, names_from = trait2, values_from = value) %>% 
                      mutate(Tyler_photosynthetic_pathway = toupper(Tyler_photosynthetic_pathway)))

################ LEDA traits #####################

traits <- full_join(traits,
                    all %>% 
                      filter(startsWith(trait2, "LEDA_")) %>% 
                      filter(!trait %in% c("age_first_flowering","buoyancy")) %>% 
                      select(WFO_species, trait2, value) %>% 
                      group_by(WFO_species, trait2) %>% 
                      slice_head(n = 1) %>% 
                      drop_na() %>% 
                      distinct() %>% 
                      pivot_wider(id_cols = WFO_species, names_from = trait2, values_from = value))

################ deciduousness_1 ##################

all %>% 
  filter(trait == "deciduousness_1") %>% 
  group_by(trait, value) %>% 
  count# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "deciduousness_1") %>% 
                      group_by(trait2, WFO_species) %>% 
                      summarise(value = get_mode(value)) %>% 
                      rename(GIFT_deciduousness_1 = value) %>% 
                      ungroup() %>% 
                      select(-trait2))

################ Lososova traits ##################

traits <- full_join(traits,
                    all %>% 
                      filter(startsWith(trait2, "Lososova")) %>% 
                      select(WFO_species, trait2, value) %>% 
                      drop_na() %>% 
                      group_by(WFO_species, trait2) %>% 
                      summarise(value = get_mode(value)) %>% 
                      drop_na() %>% 
                      distinct() %>% 
                      pivot_wider(id_cols = WFO_species, names_from = trait2, values_from = value))

################ dispersal_syndrome ##################

all %>% 
  filter(startsWith(trait, "dispersal_syndrome")) %>% 
  group_by(dataset, trait, value) %>% 
  count# %>% view

traits <- full_join(traits,
                    full_join(all %>% 
                                filter(startsWith(trait, "dispersal_syndrome"),
                                       database == "GIFT") %>% 
                                select(WFO_species, value) %>% 
                                distinct() %>% 
                                mutate(value = tolower(value)) %>% 
                                mutate(GIFT_dispersal_anemochor = grepl("anemochor",value),
                                       GIFT_dispersal_zoochor = grepl("zoochor",value),
                                       GIFT_dispersal_autochor = grepl("autochor",value),
                                       GIFT_dispersal_hydrochor = grepl("hydrochor",value),
                                       GIFT_dispersal_anthropochor = grepl("anthropochor",value),
                                       GIFT_dispersal_myrmecochor = grepl("myrmecochor",value),
                                       GIFT_dispersal_unspecialized = grepl("unspecialized",value)) %>% 
                                group_by(WFO_species) %>% 
                                summarise(across(GIFT_dispersal_anemochor:GIFT_dispersal_unspecialized, max)),
                              all %>% 
                                filter(startsWith(trait, "dispersal_syndrome"),
                                       database == "TR8") %>% 
                                select(WFO_species, value) %>% 
                                distinct() %>% 
                                mutate(value = tolower(value)) %>% 
                                mutate(TR8_dispersal_agochor = grepl("agochor",value),
                                       TR8_dispersal_autochor = grepl("autochor",value),
                                       TR8_dispersal_ballochor = grepl("ballochor",value),
                                       TR8_dispersal_blastochor = grepl("blastochor",value),
                                       TR8_dispersal_boleochor = grepl("boleochor",value),
                                       TR8_dispersal_chamaechor = grepl("chamaechor",value),
                                       TR8_dispersal_dysochor = grepl("dysochor",value),
                                       TR8_dispersal_endozoochor = grepl("endozoochor",value),
                                       TR8_dispersal_epizoochor = grepl("epizoochor",value),
                                       TR8_dispersal_hemerochor = grepl("hemerochor",value),
                                       TR8_dispersal_meteorochor = grepl("meteorochor",value),
                                       TR8_dispersal_nautochor = grepl("nautochor",value),
                                       TR8_dispersal_ombrochor = grepl("ombrochor",value),
                                       TR8_dispersal_speirochor = grepl("speirochor",value)) %>% 
                                group_by(WFO_species) %>% 
                                summarise(across(TR8_dispersal_agochor:TR8_dispersal_speirochor, max))))

################ Midolo traits ##################

traits <- full_join(traits,
                    all %>% 
                      filter(startsWith(trait2, "Midolo")) %>% 
                      select(WFO_species, trait2, value) %>% 
                      drop_na() %>% 
                      group_by(WFO_species, trait2) %>% 
                      summarise(value = get_mode(value)) %>% 
                      drop_na() %>% 
                      distinct() %>% 
                      pivot_wider(id_cols = WFO_species, names_from = trait2, values_from = value))

################ Tichy Ellenbergs ##################

traits <- full_join(traits,
                    all %>% 
                      filter(database == "Tichy") %>% 
                      select(WFO_species, trait2, value) %>% 
                      mutate(value = as.numeric(value)) %>% 
                      drop_na() %>% 
                      group_by(WFO_species, trait2) %>% 
                      summarise(value = mean(value)) %>% 
                      drop_na() %>% 
                      distinct() %>% 
                      pivot_wider(id_cols = WFO_species, names_from = trait2, values_from = value))

################ Grime_strategy ##################

all %>% 
  filter(trait == "Grime_strategy") %>%
  filter(dataset != "TRY_126") %>% 
  mutate(value = toupper(value)) %>% 
  group_by(dataset, value) %>% 
  count# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "Grime_strategy") %>%
                      filter(dataset != "TRY_126") %>% 
                      mutate(value = toupper(value)) %>% 
                      mutate(Grime_C = grepl("C",value),
                             Grime_S = grepl("S",value),
                             Grime_R = grepl("R",value)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(across(Grime_C:Grime_R, max)) %>% 
                      mutate(GRIME_C2 = ifelse(Grime_C == 1, "C", ""),
                             GRIME_S2 = ifelse(Grime_S == 1, "S", ""),
                             GRIME_R2 = ifelse(Grime_R == 1, "R", "")) %>% 
                      mutate(Grime_strategy = paste0(GRIME_C2, GRIME_S2, GRIME_R2)) %>% 
                      select(WFO_species, starts_with("Grime", ignore.case = FALSE)))

################ growth_form_ ##################

all %>% 
  filter(startsWith(trait, "growth_form")) %>% 
  group_by(dataset, trait, value) %>% 
  count# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(database == "GIFT",
                             trait == "growth_form_1") %>% 
                      filter(value != "other") %>% 
                      mutate(value = ifelse(value == "herb/shrub", "semishrub", value),
                             value = ifelse(value == "shrub/tree", "semitree", value)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(GIFT_growth_form_1 = get_mode(value)))

################ Drevojan traits ##################

traits <- full_join(traits,
                    all %>% 
                      filter(startsWith(trait2, "Drevojan")) %>% 
                      select(WFO_species, trait2, value) %>% 
                      drop_na() %>% 
                      group_by(WFO_species, trait2) %>% 
                      summarise(value = get_mode(value)) %>% 
                      drop_na() %>% 
                      distinct() %>% 
                      pivot_wider(id_cols = WFO_species, names_from = trait2, values_from = value))

################ leaf_phenology_type ##################

all %>% 
  filter(startsWith(trait, "leaf_phenology_type")) %>% 
  group_by(dataset, OriglName, trait, value) %>% 
  count# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(startsWith(trait, "leaf_phenology_type")) %>% 
                      mutate(value = tolower(value)) %>% 
                      filter(value != "winter deciduous",
                             value != "deciduous/evergreen") %>% 
                      mutate(value = ifelse(value == "nonevergreen", "deciduous", value),
                             value = ifelse(value == "d", "deciduous", value),
                             value = ifelse(value == "e", "evergreen", value)) %>% 
                      mutate(leaf_phenology = ifelse(grepl("deciduous",value), "deciduous", NA),
                             leaf_phenology = ifelse(grepl("evergreen",value), "evergreen", leaf_phenology)) %>% 
                      drop_na(leaf_phenology) %>% 
                      group_by(WFO_species) %>% 
                      summarise(TRY_leaf_phenology = get_mode(leaf_phenology)))

################ life_form_ ##################

all %>% 
  filter(startsWith(trait, "life_form_"),
         database == "GIFT") %>% 
  group_by(trait, value) %>% 
  count# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(startsWith(trait, "life_form_1"),
                             database == "GIFT") %>% 
                      select(WFO_species, value) %>% 
                      distinct() %>% 
                      mutate(value = tolower(value)) %>% 
                      mutate(GIFT_lifeform1_chamaephyte = grepl("chamaephyte",value),
                             GIFT_lifeform1_cryptophyte = grepl("cryptophyte",value),
                             GIFT_lifeform1_hemicryptophyte = grepl("hemicryptophyte",value),
                             GIFT_lifeform1_phanerophyte = grepl("phanerophyte",value),
                             GIFT_lifeform1_therophyte = grepl("therophyte",value)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(across(GIFT_lifeform1_chamaephyte:GIFT_lifeform1_therophyte, max)))

all %>% 
  filter(startsWith(trait, "life_form_2"),
         database == "GIFT") %>% 
  pull(value) %>% unique() %>% 
  str_split(., "\\/") %>% unlist %>% unique %>% sort

traits <- full_join(traits,
                    all %>% 
                      filter(startsWith(trait, "life_form_1"),
                             database == "GIFT") %>% 
                      select(WFO_species, value) %>% 
                      distinct() %>% 
                      mutate(value = tolower(value)) %>% 
                      mutate(GIFT_lifeform2_chamaephyte = grepl("chamaephyte",value),
                             GIFT_lifeform2_geophyte = grepl("geophyte",value),
                             GIFT_lifeform2_helophyte = grepl("helophyte",value),
                             GIFT_lifeform2_hemicryptophyte = grepl("hemicryptophyte",value),
                             GIFT_lifeform2_hydrophyte = grepl("hydrophyte",value),
                             GIFT_lifeform2_lithophyte = grepl("lithophyte",value),
                             GIFT_lifeform2_nanophanerophyte = grepl("nanophanerophyte",value),
                             GIFT_lifeform2_phanerophyte = grepl("phanerophyte",value),
                             GIFT_lifeform2_therophyte = grepl("therophyte",value),) %>% 
                      group_by(WFO_species) %>% 
                      summarise(across(GIFT_lifeform2_chamaephyte:GIFT_lifeform2_therophyte, max)))

################ mycorrhiza_colonization_intensity ##################

all %>% 
  filter(trait %in% c("mycorrhiza_colonization_intensity", "mycorrhizal_infection_intensity")) %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(dataset) %>% 
  summarise(value = median(as.numeric(value), na.rm = TRUE))# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait %in% c("mycorrhiza_colonization_intensity", "mycorrhizal_infection_intensity")) %>% 
                      mutate(value = as.numeric(value)) %>% 
                      group_by(WFO_species, dataset) %>% 
                      summarise(value = median(value, na.rm = TRUE)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(mycorrhiza_colonization_intensity = mean(value, na.rm = TRUE)) %>% 
                      drop_na())

################ nitrogen_fixation ##################

all %>% 
  filter(trait == "nitrogen_fixation") %>% 
  mutate(value = tolower(value)) %>% 
  mutate(value = ifelse(value %in% c("1","2","high","medium","n fixer","n-fixer","n2 fixing","y"),"yes",value),
         value = ifelse(value %in% c("0","n"),"no",value)) %>% 
  mutate(value2 = ifelse(grepl("yes", value), "yes", NA),
         value2 = ifelse(grepl("no", value), "no", value2)) %>% 
  drop_na(value2) %>% 
  group_by(value2) %>% 
  count# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "nitrogen_fixation") %>% 
                      mutate(value = tolower(value)) %>% 
                      mutate(value = ifelse(value %in% c("1","2","high","medium","n fixer","n-fixer","n2 fixing","y"),"yes",value),
                             value = ifelse(value %in% c("0","n"),"no",value)) %>% 
                      mutate(value2 = ifelse(grepl("yes", value), "yes", NA),
                             value2 = ifelse(grepl("no", value), "no", value2)) %>% 
                      drop_na(value2) %>% 
                      group_by(WFO_species) %>% 
                      summarise(nitrogen_fixation = get_mode(value2)))

################ palatability ##################

all %>% 
  filter(trait == "palatability") %>% 
  mutate(value = tolower(value)) %>%
  group_by(dataset, value) %>% 
  count# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "palatability",
                             dataset %in% c("TRY_103", "TRY_234")) %>% 
                      mutate(value = as.numeric(value)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(palatability = mean(value)))


all %>% 
  filter(trait == "palatability",
         !dataset %in% c("TRY_103", "TRY_234")) %>% 
  mutate(value = tolower(value)) %>% 
  mutate(value = ifelse(value %in% c("no"),"none",value),
         value = ifelse(value %in% c("slight"),"low",value),
         value = ifelse(value %in% c("variable (e.g. young plants palatable but adult plant not)","moderate","yes"),"medium",value),
         value = ifelse(value %in% c("severe"),"high",value),) %>% 
  drop_na(value) %>% 
  group_by(value) %>% 
  count# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "palatability",
                             !dataset %in% c("TRY_103", "TRY_234")) %>% 
                      mutate(value = tolower(value)) %>% 
                      mutate(value = ifelse(value %in% c("no"),"none",value),
                             value = ifelse(value %in% c("slight"),"low",value),
                             value = ifelse(value %in% c("variable (e.g. young plants palatable but adult plant not)","moderate","yes"),"medium",value),
                             value = ifelse(value %in% c("severe"),"high",value),) %>% 
                      drop_na(value) %>% 
                      group_by(value) %>% 
                      group_by(WFO_species) %>% 
                      summarise(palatability_class = get_mode(value)))

################ photosynthetic_pathway ##################

all %>% 
  filter(trait == "photosynthetic_pathway") %>% 
  mutate(value = tolower(value)) %>% 
  mutate(value = gsub("\\?","",value)) %>% 
  mutate(value = str_split_i(value, "\\;", 1)) %>% 
  mutate(value = str_split_i(value, "\\/", 1)) %>% 
  mutate(value = ifelse(value == "3", "C3", value)) %>% 
  filter(value != "unknown") %>% 
  drop_na(value) %>% 
  group_by(database, value) %>% 
  count# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "photosynthetic_pathway") %>% 
                      mutate(value = tolower(value)) %>% 
                      mutate(value = gsub("\\?","",value)) %>% 
                      mutate(value = str_split_i(value, "\\;", 1)) %>% 
                      mutate(value = str_split_i(value, "\\/", 1)) %>% 
                      mutate(value = ifelse(value == "3", "C3", value)) %>% 
                      filter(value != "unknown") %>% 
                      drop_na(value) %>% 
                      group_by(WFO_species) %>% 
                      summarise(photosynthetic_pathway = get_mode(value)) %>% 
                      mutate(photosynthetic_pathway = toupper(photosynthetic_pathway)))

################ pollination_syndrome ##################

all %>% 
  filter(startsWith(trait, "pollination_syndrome")) %>% 
  group_by(dataset, trait, value) %>% 
  count# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "pollination_syndrome_1") %>% 
                      group_by(WFO_species) %>% 
                      summarise(GIFT_pollination_syndrome = get_mode(value)))

all %>% 
  filter(trait == "pollination_syndrome_2",
         database == "GIFT") %>% 
  pull(value) %>% unique() %>% 
  str_split(., "\\/") %>% unlist %>% unique %>% sort

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "pollination_syndrome_2",
                             database == "GIFT") %>% 
                      select(WFO_species, value) %>% 
                      distinct() %>% 
                      mutate(value = tolower(value)) %>% 
                      mutate(GIFT_pollination_syndrome_bird = grepl("bird",value),
                             GIFT_pollination_syndrome_insect = grepl("insect",value),
                             GIFT_pollination_syndrome_other = grepl("other",value),
                             GIFT_pollination_syndrome_water = grepl("water",value),
                             GIFT_pollination_syndrome_wind = grepl("wind",value)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(across(GIFT_pollination_syndrome_bird:GIFT_pollination_syndrome_wind, max)))

all %>% 
  filter(trait == "pollination_syndrome_3",
         database == "GIFT") %>% 
  pull(value) %>% unique() %>% 
  str_split(., "\\/") %>% unlist %>% unique %>% sort


traits <- full_join(traits,
                    all %>% 
                      filter(trait == "pollination_syndrome_3",
                             database == "GIFT") %>% 
                      select(WFO_species, value) %>% 
                      distinct() %>% 
                      mutate(value = tolower(value)) %>% 
                      mutate(GIFT_pollination_animal_bee = grepl("bee\\/",value)) %>% 
                      mutate(GIFT_pollination_animal_bee = ifelse(grepl("bee$",value), TRUE, GIFT_pollination_animal_bee),
                             GIFT_pollination_animal_beetle = grepl("beetle",value),
                             GIFT_pollination_animal_bird = grepl("bird",value),
                             GIFT_pollination_animal_butterfly = grepl("butterfly",value),
                             GIFT_pollination_animal_fly = grepl("fly",value),
                             GIFT_pollination_animal_moth = grepl("moth",value)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(across(GIFT_pollination_animal_bee:GIFT_pollination_animal_moth, max)))


all %>% 
  filter(trait == "pollination_syndrome",
         database == "TR8") %>% 
  pull(value) %>% unique() %>% 
  str_split(., ";") %>% unlist %>% unique %>% sort

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "pollination_syndrome",
                             database == "TR8") %>% 
                      select(WFO_species, value) %>% 
                      distinct() %>% 
                      mutate(value = tolower(value)) %>% 
                      mutate(TR8_pollination_syndrome_selfed = grepl("selfed",value),
                             TR8_pollination_syndrome_insect = grepl("insect",value),
                             TR8_pollination_syndrome_none = grepl("none",value),
                             TR8_pollination_syndrome_water = grepl("water",value),
                             TR8_pollination_syndrome_wind = grepl("wind",value)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(across(TR8_pollination_syndrome_selfed:TR8_pollination_syndrome_wind, max)))

all %>% 
  filter(trait == "pollination_syndrome",
         dataset == "TRY_68") %>% 
  select(WFO_species) %>% 
  distinct()


traits <- full_join(traits,
                    all %>% 
                      filter(trait == "pollination_syndrome",
                             dataset == "TRY_68") %>% 
                      drop_na(value) %>% 
                      mutate(value = ifelse(value == "insects", "insect", value)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(TRY_pollination_syndrome = get_mode(value)))

################ propagation_type ##################

all %>% 
  filter(trait == "propagation_type") %>% 
  mutate(value = tolower(value)) %>% 
  drop_na(value) %>% 
  group_by(dataset, value) %>% 
  count# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "propagation_type") %>% 
                      filter(dataset != "TRY_79") %>% 
                      mutate(value = tolower(value)) %>% 
                      mutate(propagation_type_seed = grepl("seed",value),
                             propagation_type_vegetative = grepl("vegetative",value)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(across(propagation_type_seed:propagation_type_vegetative, max)))


################ reproduction ##################

all %>% 
  filter(startsWith(trait, "reproduction_sexual_1")) %>% 
  group_by(trait, value) %>% 
  count# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait %in% c("reproduction_sexual_1","reproduction_asexual_1")) %>% 
                      group_by(trait2, WFO_species) %>% 
                      summarise(value = get_mode(value)) %>% 
                      pivot_wider(id_cols = WFO_species, names_from = trait2, values_from = value))

################ root_depth ##################

all %>% 
  filter(trait %in% c("root_depth")) %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(value = ifelse(dataset %in% c("FRED","TTT_54"), value/100, value)) %>%
  group_by(trait, dataset) %>% 
  summarise(value = median(as.numeric(value), na.rm = TRUE))# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait %in% c("root_depth")) %>% 
                      mutate(value = as.numeric(value)) %>% 
                      drop_na(value) %>% 
                      mutate(value = ifelse(dataset %in% c("FRED","TTT_54"), value/100, value)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(root_depth = mean(value)))

################ root_mass_fraction ##################

all %>% 
  filter(trait %in% c("root_mass_fraction")) %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(dataset) %>% 
  summarise(value = median(as.numeric(value), na.rm = TRUE))# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait %in% c("root_mass_fraction")) %>% 
                      mutate(value = as.numeric(value)) %>% 
                      drop_na(value) %>% 
                      group_by(WFO_species) %>% 
                      summarise(root_mass_fraction = mean(value)))

################ fruit_type ##################

all %>% 
  filter(startsWith(trait, "fruit_type_1")) %>% 
  filter(!value %in% c("pome","lomentum","capsule/follicle")) %>% 
  mutate(value = ifelse(value == "achene/nut", "achene", value)) %>% 
  group_by(trait, value) %>% 
  count# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(startsWith(trait, "fruit_type_1")) %>% 
                      filter(!value %in% c("pome","lomentum","capsule/follicle")) %>% 
                      mutate(value = ifelse(value == "achene/nut", "achene", value)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(fruit_type = get_mode(value)))


################ leaf_chlorophyll ##################

all %>% 
  filter(trait == "leaf_chlorophyll") %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(dataset) %>% 
  summarise(value = median(as.numeric(value), na.rm = TRUE))# %>% view

traits <- full_join(traits,
                    all %>% 
                      filter(trait == "leaf_chlorophyll") %>% 
                      mutate(value = as.numeric(value)) %>% 
                      group_by(WFO_species, dataset) %>% 
                      summarise(value = median(value, na.rm = TRUE)) %>% 
                      group_by(WFO_species) %>% 
                      summarise(leaf_chlorophyll = mean(value, na.rm = TRUE)) %>% 
                      drop_na())


##############################################################

traits %>% 
  group_by(WFO_species) %>% 
  mutate(n = n()) %>% 
  filter(n > 1)

traits_names[!traits_names %in% names(traits)]
# 

ph <- read_csv("output/phylo_eigen.csv") %>% 
  group_by(WFO_species) %>% 
  slice_head(n = 1) %>% 
  ungroup

traits <- full_join(ph, traits)

traits %>% write_csv("output/traits_missing.csv")

traits %>% 
  select(WFO_species, SLA:last_col()) %>% 
  mutate(across(everything(), as.character)) %>% 
  pivot_longer(cols = SLA:last_col(), names_to = "trait", values_to = "value") %>% 
  drop_na() %>% 
  group_by(trait) %>%
  summarise(n_species = n(),
            pasted_values = paste(head(unique(value), 5), collapse = ", "),
            n_unique = length(unique(value))) %>% 
  arrange(trait) %>% 
  writexl::write_xlsx("output/selected_traits.xlsx")


traits %>%
  mutate(na_proportion = round(rowMeans(is.na(select(traits, SLA:last_col())))*100)) %>% 
  select(WFO_species, na_proportion) %>% view

traits %>%
  mutate(na_proportion = round(rowMeans(is.na(select(traits, SLA:SRL)))*100)) %>% 
  select(WFO_species, na_proportion) %>% view

################################################################
# Post-process after selecting the traits

d <- read_csv("output/traits_missing.csv")

tr <- read_sheet("https://docs.google.com/spreadsheets/d/18K7Ff3yPcQ1ungiWKLiBc9FXZspxtp6LamimXXS3scE/edit?gid=832928905#gid=832928905", col_types = "c")

d <- d %>% 
  mutate(across(all_of((tr %>% filter(type == "numerical") %>% pull(trait))), as.numeric)) %>% 
  mutate(across(all_of((tr %>% filter(type != "numerical") %>% pull(trait))), as.character)) %>% 
  mutate(across(all_of((tr %>% filter(type == "binary") %>% pull(trait))), ~ifelse(.x == "0", "no", .x))) %>%
  mutate(across(all_of((tr %>% filter(type == "binary") %>% pull(trait))), ~ifelse(.x == "1", "yes", .x))) %>% 
  mutate(Tyler_parasitism = case_match(Tyler_parasitism,
                                       "0" ~ "none",
                                       "1" ~ "hemiparasite",
                                       "2" ~ "holoparasite")) %>%
  mutate(across(all_of((tr %>% filter(type != "numerical") %>% pull(trait))), as.factor)) %>%
  mutate(across(all_of((tr %>% filter(type == "numerical") %>% pull(trait))), ~round(.x, 4))) %>% 
  select(WFO_species:phyloeigen10, all_of((tr %>% filter(to_imputation == 1) %>% pull(trait))))

str(d)
d %>% select(all_of((tr %>% filter(type == "numerical") %>% pull(trait)))) %>% head(10) %>% view
summary(d)

table(d$TRY_pollination_syndrome)

d %>% write_csv("output/traits_missing_finalized.csv")

d %>%
  mutate(na_proportion = round(rowMeans(is.na(select(d, buoyancy:last_col())))*100)) %>% 
  select(WFO_species, na_proportion) %>% view

gg1 <- d %>%
  mutate(na_proportion = round(rowMeans(is.na(select(d, buoyancy:last_col())))*100)) %>% 
  select(WFO_species, na_proportion) %>% 
  ggplot(aes(x = na_proportion)) +
  geom_histogram() +
  theme_bw() +
  ylab("Count of species") + 
  xlab("Percentage of missing values")

gg2 <- d %>%
  mutate(na_proportion = round(rowMeans(is.na(select(d, buoyancy:last_col())))*100)) %>% 
  select(WFO_species, na_proportion) %>% 
  filter(!startsWith(WFO_species, "Ranunculus "),
         !startsWith(WFO_species, "Hieracium "),
         !startsWith(WFO_species, "Taraxacum ")) %>% 
  ggplot(aes(x = na_proportion)) +
  geom_histogram() +
  theme_bw() +
  ylab("Count of species") + 
  xlab("Percentage of missing values")

library(patchwork)

gg1 + gg2 + plot_annotation(tag_levels = 'A')

traits %>%
  mutate(na_proportion = round(rowMeans(is.na(select(traits, SLA:SRL)))*100)) %>% 
  select(WFO_species, na_proportion) %>% view

