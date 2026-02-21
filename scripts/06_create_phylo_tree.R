#####################################################################
# Create a phylogenetic tree and extract Eigenvectors

# devtools::install_github("jinyizju/V.PhyloMaker2", lib = "/projappl/project_2003061/Rpackages")
library(tidyverse)
library(foreach)
library(parallel)
library(data.table)
library(V.PhyloMaker2)
library(PVR)
library(scales)
library(googlesheets4)

nam <- read_sheet("https://docs.google.com/spreadsheets/d/1qgBOjM_941h4zm0foy83PyT6Og0ocWlDOUHXWmdlWIA/edit?gid=211021139#gid=211021139", col_types = "c")

nam <- nam %>% 
  mutate(WFO_family = ifelse(taxon == "Hablitzia tamnoides", "Amaranthaceae", WFO_family)) %>% 
  mutate(WFO_family = ifelse(taxon == "Iljinskaea planisiliqua", "Brassicaceae", WFO_family)) %>% 
  mutate(WFO_family = ifelse(taxon == "Scandosorbus intermedia", "Rosaceae", WFO_family)) %>% 
  mutate(TPL_species = ifelse(taxon == "Huperzia acicularis", "Huperzia acicularis", TPL_species),
         TPL_family = ifelse(taxon == "Huperzia acicularis", "Lycopodiaceae", TPL_family)) %>% 
  mutate(TPL_species = ifelse(taxon == "Hieracium anderssonii", "Hieracium anderssonii", TPL_species),
         TPL_family = ifelse(taxon == "Hieracium anderssonii", "Asteraceae", TPL_family))

sum(is.na(nam$WFO_species))
sum(is.na(nam$TPL_species))
sum(is.na(nam$LCVP_species))

length(unique(nam$WFO_species))
length(unique(nam$TPL_species))
length(unique(nam$LCVP_species))

nam <- nam %>% 
  mutate(TPL_species = ifelse(is.na(TPL_species), WFO_species, TPL_species)) %>% 
  mutate(TPL_family = ifelse(is.na(TPL_family), WFO_family, TPL_family)) %>% 
  mutate(across(everything(), ~gsub("ë","e",.x)))

length(unique(nam$TPL_species))
sum(is.na(nam$TPL_family))

nam <- nam %>% 
  select(TPL_species, TPL_family) %>% 
  setNames(c("species", "family")) %>% 
  mutate(genus = str_split_i(species, " ", 1)) %>% 
  mutate(species = gsub(" ", "_", species))


data(GBOTB.extended.TPL)
data(tips.info.TPL)

nam <- left_join(tips.info.TPL, nam, by = c("species" = "species"))

nam %>% drop_na() %>% filter(family.x != family.y) %>% select(family.x, family.y) %>% table()

genera <- unique(tips.info.TPL$genus)
families <- unique(tips.info.TPL$family)
genera2 <- unique(nam$genus.y)
families2 <- unique(nam$family.y)

fn <- read_sheet("https://docs.google.com/spreadsheets/d/1qgBOjM_941h4zm0foy83PyT6Og0ocWlDOUHXWmdlWIA/edit?gid=211021139#gid=211021139", col_types = "c") %>% 
  mutate(WFO_family = ifelse(taxon == "Hablitzia tamnoides", "Amaranthaceae", WFO_family)) %>% 
  mutate(WFO_family = ifelse(taxon == "Iljinskaea planisiliqua", "Brassicaceae", WFO_family)) %>% 
  mutate(WFO_family = ifelse(taxon == "Scandosorbus intermedia", "Rosaceae", WFO_family)) %>% 
  mutate(TPL_species = ifelse(taxon == "Huperzia acicularis", "Huperzia acicularis", TPL_species),
         TPL_family = ifelse(taxon == "Huperzia acicularis", "Lycopodiaceae", TPL_family)) %>% 
  mutate(TPL_species = ifelse(taxon == "Hieracium anderssonii", "Hieracium anderssonii", TPL_species),
         TPL_family = ifelse(taxon == "Hieracium anderssonii", "Asteraceae", TPL_family)) %>% 
  mutate(TPL_species = ifelse(is.na(TPL_species), WFO_species, TPL_species)) %>% 
  mutate(TPL_family = ifelse(is.na(TPL_family), WFO_family, TPL_family)) %>% 
  select(WFO_species, TPL_species, TPL_family) %>% 
  setNames(c("WFO_species","species", "family")) %>% 
  mutate(genus = str_split_i(species, " ", 1)) %>% 
  mutate(species = gsub(" ", "_", species)) %>% 
  mutate(family = ifelse(family == "Compositae", "Asteraceae", family)) %>% 
  mutate(family = ifelse(family == "Leguminosae", "Fabaceae", family)) %>% 
  mutate(family = ifelse(family == "Molluginaceae", "Caryophyllaceae", family)) %>% 
  mutate(family = ifelse(family == "Phrymaceae", "Mazaceae", family)) %>% 
  mutate(family = ifelse(family == "Portulacaceae", "Montiaceae", family)) %>% 
  mutate(family = ifelse(family == "Xanthorrhoeaceae", "Asphodelaceae", family)) %>% 
  mutate(across(everything(), ~gsub("ë","e",.x)))

unique(fn$genus)[!unique(fn$genus) %in% genera]
unique(fn$family)[!unique(fn$family) %in% families]
unique(fn$genus)[!unique(fn$genus) %in% genera2]
unique(fn$family)[!unique(fn$family) %in% families2]

fn %>% filter(is.na(species))
tips.info.TPL %>% filter(grepl("Taraxacum_", species))

result <- phylo.maker(fn %>% select(species, genus, family) %>% distinct(), 
                      scenarios=c("S3"))

phyloeigens <- PVRdecomp(result$scenario.3, scale = T)

class(phyloeigens@Eigen$vectors)
dim(phyloeigens@Eigen$vectors[,1:10])
# First 10
phyloeigens10 <- phyloeigens@Eigen$vectors[,1:10]
summary(phyloeigens10)

phyloeigens10 <- phyloeigens10 %>% 
  as.data.frame() %>% 
  mutate(across(c1:c10, rescale)) %>% 
  mutate(species = phyloeigens@phylo$tip.label) %>% 
  right_join(., fn) %>% 
  distinct()

phyloeigens10 %>% slice_sample(n = 10)

names(phyloeigens10)[1:10] <- paste0("phyloeigen", 1:10)

phyloeigens10 <- phyloeigens10 %>% 
  select(WFO_species, all_of(paste0("phyloeigen", 1:10))) %>% 
  arrange(WFO_species)

write_csv(phyloeigens10, "output/phylo_eigen.csv")
write_rds(x = result, "output/Nordic_phylotree.rda", compress = "gz")
