#####################################################################
# Create a species metadata with coountry-level nomenclatures

library(tidyverse)
library(readxl)
library(googlesheets4)
library(U.Taxonstand, lib.loc = "/projappl/project_2003061/Rpackages")

fn <- read_sheet("https://docs.google.com/spreadsheets/d/1qgBOjM_941h4zm0foy83PyT6Og0ocWlDOUHXWmdlWIA/edit?gid=211021139#gid=211021139", col_types = "c")

fn <- fn %>% 
  select(taxon, author, Submitted_Name, Submitted_Author,
              starts_with("WFO_"))

# Finnish dataset names
fi <- read_xlsx("data/Finland/Lajien_uhanalaisuusarviointi_2019_v2_17052019.xlsx") %>% 
  filter(`Ryhmä 1` == "Putkilokasvit, Tracheophyta") %>% 
  filter(`Luokka...14` != "NA")

fi <- fi %>% 
  mutate(author = ifelse(is.na(Auktori), "", Auktori)) %>% 
  mutate(taxon = `Tieteellinen nimi`) %>% 
  mutate(taxon = gsub("ë","e",taxon)) %>% 
  mutate(taxon = gsub("-ryhmä","",taxon, ignore.case = T)) %>% 
  mutate(taxon = str_squish(taxon))

fi <- fi %>% select(taxon, author, Id, `Tieteellinen nimi`, `Suomenkielinen nimi`) %>% 
  rename(FI_Id = Id,
         FI_species = `Tieteellinen nimi`,
         FI_vernacular = `Suomenkielinen nimi`) %>% 
  distinct()

fi <- left_join(fn, fi) %>% 
  select(starts_with("WFO"), starts_with("FI_")) %>% 
  drop_na(FI_species) %>% 
  distinct()

# Norwegian dataset names
no <- read_xlsx("data/Norway/rødliste-2021.xlsx") %>% 
  mutate(author = ifelse(is.na(Autor), "", Autor)) %>% 
  mutate(taxon = `Vitenskapelig navn`) %>% 
  mutate(taxon = gsub("ë","e",taxon)) %>% 
  mutate(taxon = str_squish(taxon))

no <- no %>% select(taxon, author, `Vitenskapelig navn id`, `Vitenskapelig navn`, `Populærnavn`) %>% 
  rename(NO_Id = `Vitenskapelig navn id`,
         NO_species = `Vitenskapelig navn`,
         NO_vernacular = `Populærnavn`) %>% 
  distinct()

no <- left_join(fn, no) %>% 
  select(starts_with("WFO"), starts_with("NO_")) %>% 
  drop_na(NO_species) %>% 
  distinct()

# Swedish dataset names
ty <- read_xlsx("/scratch/project_2003061/trait_datasets/Tyler/1-s2.0-S1470160X20308621-mmc1.xlsx")
se <- bind_cols(ty,
                nameSplit(gsub(" sect. ", " ", ty$`Scientific name`)) %>% 
                  rename(taxon = Name,
                         author = Author))

se <- se %>% select(taxon, author, `Dyntaxa ID number`, `Svenskt namn`) %>% 
  mutate(SE_species = taxon) %>% 
  rename(SE_Id = `Dyntaxa ID number`,
         SE_vernacular = `Svenskt namn`) %>% 
  relocate(SE_species, .after = SE_Id) %>% 
  distinct()

se <- left_join(fn, se) %>% 
    select(starts_with("WFO"), starts_with("SE_")) %>% 
  drop_na(SE_species) %>% 
  distinct()

# Danish dataset names
de <- read_csv2("data/Denmark/redlist_extract_2030_35_E6757D19B379A33A8069CCE511247546_dk.csv") %>% 
  mutate(author = ifelse(is.na(scientificNameAuthorship), "", scientificNameAuthorship)) %>% 
  mutate(taxon = `scientificName`) %>% 
  mutate(taxon = gsub("ë","e", taxon)) %>% 
  mutate(taxon = str_squish(taxon))

de <- de %>% select(taxon, author, `scientificName`, `vernacularName`) %>% 
  rename(DE_species = `scientificName`,
         DE_vernacular = `vernacularName`) %>% 
  distinct()

de <- left_join(fn, de) %>% 
  select(starts_with("WFO"), starts_with("DE_")) %>% 
  drop_na(DE_species) %>% 
  distinct()

# Norwegian dataset names
d1 <- read.table("data/Iceland/taxon.txt", sep = "\t", header = TRUE)
ic <- bind_cols(d1,
                nameSplit(gsub(" sect. ", " ", d1$scientificName)) %>% 
                  select(Name, Author) %>% 
                  setNames(c("taxon", "author")) %>% 
                  select(taxon, author))

ic <- ic %>% select(taxon, author, `taxonID`) %>% 
  mutate(IC_species = taxon) %>% 
  rename(IC_Id = `taxonID`) %>% 
  distinct()

ic <- left_join(fn, ic) %>% 
  select(starts_with("WFO"), starts_with("IC_")) %>% 
  drop_na(IC_species) %>% 
  distinct()

all <- full_join(fi,no) %>% 
  full_join(., se) %>% 
  full_join(., de) %>% 
  full_join(., ic)

tr <- read_csv("output/Nordic_imputed_traits_wide.csv")

all <- bind_rows(all,
          fn %>% filter(WFO_species %in% tr$species[!tr$species %in% all$WFO_species]) %>% 
            select(starts_with("WFO_"))) %>% 
  rename(species = WFO_species) %>% 
  relocate(species, WFO_author, WFO_ID) %>% 
  arrange(species)

writexl::write_xlsx(all, "output/NordicTraits_metadata_species_V1.xlsx")
