#####################################################################
# Preprocess the LEDA datasets

library(tidyverse)
library(data.table)

f <- list.files("/scratch/project_2003061/trait_datasets/LEDA/", pattern = "txt$")
df <- tibble()

#
d <- fread(paste0("/scratch/project_2003061/trait_datasets/LEDA/age_of_first_flowering.txt")) %>% 
  select(`SBS name`, `age of first flowering`) %>% 
  mutate(Trait = "age_first_flowering") %>% 
  setNames(c("AccSpeciesName","Value","Trait"))
df <- bind_rows(df, d)

#
d <- fread(paste0("/scratch/project_2003061/trait_datasets/LEDA/branching.txt")) %>% 
  select(`SBS name`, `branching`) %>% 
  mutate(Trait = "branching") %>% 
  setNames(c("AccSpeciesName","Value","Trait"))
df <- bind_rows(df, d)

#
d <- fread("/scratch/project_2003061/trait_datasets/LEDA/buds_seasonality.txt") %>% 
  select(`SBS name`, `BBS above ground`, `BBS below ground`, `budb seas. at soil surface`, `budb. seas at layer -10-0cm`) %>% 
  setNames(c("AccSpeciesName","buds_above_ground","buds_below_ground","buds_surface","buds_below_ground")) %>% 
  pivot_longer(cols = -AccSpeciesName, names_to = "Trait", values_to = "Value") %>% 
  filter(Value != "") %>% 
  drop_na()
df <- bind_rows(df, d)

#
d <- fread(paste0("/scratch/project_2003061/trait_datasets/LEDA/buoyancy_2016.txt")) %>% 
  select(`SBS name`, `single value [%]`, `fixed time step`) %>% 
  arrange(`SBS name`, `fixed time step`) %>% 
  setNames(c("AccSpeciesName","Value","step")) %>% 
  filter(step != "-")

d <- d %>% 
  group_by(AccSpeciesName, step) %>% 
  summarise(Value = mean(Value)) %>% 
  ungroup %>% 
  mutate(step2 = parse_number(str_split_i(step, " ", 1)))

d <- d %>% 
  filter(step2 <= 6) %>% 
  mutate(Value = Value/100) %>% 
  filter(Value <= 1)
m <- glm(Value ~ step2*AccSpeciesName, data = d, family = "binomial")
d %>% group_by(AccSpeciesName) %>% count %>% filter(n == 1)
d %>% filter(AccSpeciesName == "Achillea millefolium")

preds <- expand_grid(AccSpeciesName = unique(d$AccSpeciesName),
                     step2 = 0:6)
preds$prob <- predict(m, preds,
        type = "response")

df <- bind_rows(df, preds %>% 
                  group_by(AccSpeciesName) %>% 
                  summarise(Value = as.character(round(mean(prob),3))) %>% 
                  mutate(Trait = "buoyancy"))

#
d <- fread(paste0("/scratch/project_2003061/trait_datasets/LEDA/canopy_height.txt")) %>% 
  select(`SBS name`, `single value [m]`, `maximum CH [m]`, `minimum CH [m]`) %>% 
  setNames(c("AccSpeciesName","mean_height","maximum_height","minimum_height")) %>% 
  pivot_longer(cols = -AccSpeciesName, names_to = "Trait", values_to = "Value") %>% 
  mutate(Value = as.character(Value),
         Unit = "m")
df <- bind_rows(df, d)

#
d <- fread("/scratch/project_2003061/trait_datasets/LEDA/dispersal_type.txt") %>% 
  select(`SBS name`, `gen. dispersal type`) %>% 
  mutate(Trait = "dispersal_type") %>% 
  setNames(c("AccSpeciesName","Value","Trait")) %>% 
  filter(Value != "") %>% 
  drop_na()
df <- bind_rows(df, d)

#
d <- fread("/scratch/project_2003061/trait_datasets/LEDA/LDMC_und_Geo.txt") %>% 
  select(`SBS name`, `mean LMDC [mg/g]`, `maximum LDMC [mg/g]`, `minimum LDMC [mg/g]`) %>% 
  setNames(c("AccSpeciesName","mean_LDMC","maximum_LDMC","minimum_LDMC")) %>% 
  pivot_longer(cols = -AccSpeciesName, names_to = "Trait", values_to = "Value") %>% 
  mutate(Trait = "LDMC",
         Value = as.character(round(Value)),
         Unit = "mg/g") %>% 
  drop_na()
df <- bind_rows(df, d)

#
d <- fread("/scratch/project_2003061/trait_datasets/LEDA/leaf_distribution.txt") %>% 
  select(`SBS name`, `leaf distribution`) %>% 
  mutate(Trait = "leaf_distribution") %>% 
  setNames(c("AccSpeciesName","Value","Trait"))
df <- bind_rows(df, d)

#
d <- fread("/scratch/project_2003061/trait_datasets/LEDA/leaf_mass.txt") %>% 
  select(`SBS name`, `mean LM [mg]`, `maximum LM [mg]`, `minimum LM [mg]`) %>% 
  setNames(c("AccSpeciesName","mean","maximum","minimum")) %>% 
  pivot_longer(cols = -AccSpeciesName, names_to = "Trait", values_to = "Value") %>% 
  mutate(Trait = "leaf_mass",
         Value = as.character(Value),
         Unit = "mg") %>% 
  drop_na() %>% 
  distinct()
df <- bind_rows(df, d)

#
d <- fread("/scratch/project_2003061/trait_datasets/LEDA/leaf_size.txt") %>% 
  select(`SBS name`, `mean LS [mm^2]`, `maximum LS [mm^2]`, `minimum LS [mm^2]`) %>% 
  setNames(c("AccSpeciesName","mean","maximum","minimum")) %>% 
  pivot_longer(cols = -AccSpeciesName, names_to = "Trait", values_to = "Value") %>% 
  mutate(Trait = "leaf_area",
         Value = as.character(Value),
         Unit = "mm^2") %>% 
  drop_na() %>% 
  distinct()
df <- bind_rows(df, d)

#
d <- fread("/scratch/project_2003061/trait_datasets/LEDA/morphology_dispersal_unit.txt") %>% 
  select(`SBS name`, `gen. seed structure`, `seed structure hooked`) %>% 
  setNames(c("AccSpeciesName","seed_dispersal_structure","seed_hooked_structure")) %>% 
  pivot_longer(cols = -AccSpeciesName, names_to = "Trait", values_to = "Value") %>% 
  filter(Value != "",
         Value != "unknown") %>% 
  drop_na()
df <- bind_rows(df, d)

#
d <- fread("/scratch/project_2003061/trait_datasets/LEDA/plant_growth_form.txt") %>% 
  select(`SBS name`, `gen. plant growth form`) %>% 
  setNames(c("AccSpeciesName","Value")) %>% 
  mutate(Trait = "growth_form") %>% 
  drop_na()
df <- bind_rows(df, d)

#
d <- fread("/scratch/project_2003061/trait_datasets/LEDA/plant_life_span.txt") %>% 
  select(`SBS name`, `gen. plant life span`) %>% 
  setNames(c("AccSpeciesName","Value")) %>% 
  mutate(Trait = "lifespan") %>% 
  drop_na()
df <- bind_rows(df, d)

#
d <- fread("/scratch/project_2003061/trait_datasets/LEDA/seed_longevity.txt") %>% 
  select(`SBS name`, `SSB seed longevity index`) %>% 
  setNames(c("AccSpeciesName","Value")) %>% 
  drop_na() %>% 
  group_by(AccSpeciesName) %>% 
  summarise(Value = as.character(round(mean(Value),3))) %>% 
  mutate(Trait = "seed_longevity_index")
df <- bind_rows(df, d)

#
d <- fread("/scratch/project_2003061/trait_datasets/LEDA/seed_mass.txt") %>% 
  select(`SBS name`, `mean SM [mg]`, `maximum SM [mg]`, `minimum SM [mg]`) %>% 
  setNames(c("AccSpeciesName","mean","maximum","minimum")) %>% 
  pivot_longer(cols = -AccSpeciesName, names_to = "Trait", values_to = "Value") %>% 
  mutate(Trait = "seed_mass",
         Value = as.character(Value),
         Unit = "mg") %>% 
  drop_na() %>% 
  distinct()
df <- bind_rows(df, d)

#
d <- fread("/scratch/project_2003061/trait_datasets/LEDA/seed_number.txt") %>% 
  filter(`reproduction unit measured` %in% c("per ramet/tussock or individual plant",
                                             "per single flower inflorescence")) %>% 
  select(`SBS name`, `reproduction unit measured`, `single value`) %>% 
  group_by(`SBS name`, `reproduction unit measured`) %>% 
  summarise(Value = mean(`single value`)) %>% ungroup %>% 
  pivot_wider(id_cols = `SBS name`, names_from = `reproduction unit measured`, values_from = Value) %>% 
  setNames(c("AccSpeciesName","seed_number_plant","seed_number_flower")) %>% 
  pivot_longer(cols = -AccSpeciesName, names_to = "Trait", values_to = "Value") %>% 
  mutate(Value = as.character(round(Value))) %>% 
  drop_na() %>% 
  distinct()
df <- bind_rows(df, d)

#
d <- fread("/scratch/project_2003061/trait_datasets/LEDA/shoot_growth_form.txt") %>% 
  select(`SBS name`, `shoot growth form`) %>% 
  mutate(Trait = "shoot_growth_form") %>% 
  setNames(c("AccSpeciesName","Value","Trait"))
df <- bind_rows(df, d)

#
d <- fread("/scratch/project_2003061/trait_datasets/LEDA/SLA_und_geo_neu2.txt") %>% 
  select(`SBS name`, `mean SLA [mm^2/mg]`, `maximum SLA [mm^2/mg]`, `minimum SLA [mm^2/mg]`) %>% 
  setNames(c("AccSpeciesName","mean","maximum","minimum")) %>% 
  pivot_longer(cols = -AccSpeciesName, names_to = "Trait", values_to = "Value") %>% 
  mutate(Trait = "SLA",
         Value = as.character(Value),
         Unit = "mm^2/mg") %>% 
  drop_na() %>% 
  distinct()
df <- bind_rows(df, d)

#
d <- fread("/scratch/project_2003061/trait_datasets/LEDA/SNP.txt") %>% 
  filter(`reproduction unit measured` %in% c("per ramet/tussock or individual plant",
                                             "per single flower inflorescence")) %>% 
  select(`SBS name`, `reproduction unit measured`, `single value`) %>% 
  group_by(`SBS name`, `reproduction unit measured`) %>% 
  summarise(Value = mean(`single value`)) %>% ungroup %>% 
  pivot_wider(id_cols = `SBS name`, names_from = `reproduction unit measured`, values_from = Value) %>% 
  setNames(c("AccSpeciesName","seed_number_plant","seed_number_flower")) %>% 
  pivot_longer(cols = -AccSpeciesName, names_to = "Trait", values_to = "Value") %>% 
  mutate(Value = as.character(round(Value))) %>% 
  drop_na() %>% 
  distinct()
df <- bind_rows(df, d)

#
d <- readxl::read_xlsx("/scratch/project_2003061/trait_datasets/LEDA/ssd.xlsx") %>% 
  select(`SBS name`, `woodiness`) %>% 
  mutate(Trait = "woodiness") %>% 
  setNames(c("AccSpeciesName","Value","Trait")) %>% 
  drop_na()
df <- bind_rows(df, d)

#
d <- fread("/scratch/project_2003061/trait_datasets/LEDA/TV_2016.txt")%>% 
  select(`SBS name`, `single value [m/s]`) %>% 
  setNames(c("AccSpeciesName","Value")) %>% 
  mutate(Trait = "terminal_velocity",
         Value = as.character(Value),
         Unit = "m/s") %>% 
  drop_na() %>% 
  distinct()
df <- bind_rows(df, d)

#############################################################################
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

df2 %>% select(-AccSpeciesName) %>% write_csv("/scratch/project_2003061/trait_datasets/LEDA/traits_prepared.csv")
