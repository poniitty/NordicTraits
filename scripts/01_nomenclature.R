#####################################################################
# Gathers and harmonises plant species names across Nordic countries

library(tidyverse)
library(data.table)
library(readxl)
library(U.Taxonstand)

######################################################################
# Finnish dataset names
fnames <- read_xlsx("data/Finland/Lajien_uhanalaisuusarviointi_2019_v2_17052019.xlsx") %>% 
  filter(`Ryhmä 1` == "Putkilokasvit, Tracheophyta") %>% 
  filter(`Luokka...14` != "NA")

fnames <- fnames %>% 
  mutate(author = ifelse(is.na(Auktori), "", Auktori)) %>% 
  mutate(taxon = `Tieteellinen nimi`) %>% 
  mutate(taxon = gsub("ë","e",taxon)) %>% 
  mutate(taxon = gsub("-ryhmä","",taxon, ignore.case = T)) %>% 
  mutate(taxon = str_squish(taxon))

# Vector of all species names in Finnish dataset
FIN_species <- fnames %>% 
  select(taxon, author) %>% 
  distinct() %>% 
  arrange(taxon)

###################################################################################
# NORWAY

nnames <- read_xlsx("data/Norway/rødliste-2021.xlsx") %>% 
  mutate(author = ifelse(is.na(Autor), "", Autor)) %>% 
  mutate(taxon = `Vitenskapelig navn`) %>% 
  mutate(taxon = gsub("ë","e",taxon)) %>% 
  mutate(taxon = str_squish(taxon))

NOR_species <- nnames %>% 
  select(taxon, author) %>% 
  distinct()

###################################################################################
# SWEDEN
# Using the Tyler at al. trait dataset for Swedish species list
ty <- read_xlsx("/scratch/project_2003061/trait_datasets/Tyler/1-s2.0-S1470160X20308621-mmc1.xlsx")
SWE_species <- nameSplit(gsub(" sect. ", " ", ty$`Scientific name`)) %>% 
  select(Name, Author) %>% 
  setNames(c("taxon", "author")) %>% 
  select(taxon, author)

###################################################################################
# DENMARK

dnames <- read_csv2("data/Denmark/redlist_extract_2030_35_E6757D19B379A33A8069CCE511247546_dk.csv") %>% 
  mutate(author = ifelse(is.na(scientificNameAuthorship), "", scientificNameAuthorship)) %>% 
  mutate(taxon = `scientificName`) %>% 
  mutate(taxon = gsub("ë","e", taxon)) %>% 
  mutate(taxon = str_squish(taxon))

DNK_species <- dnames %>% 
  select(taxon, author) %>% 
  distinct()

###################################################################################
# ICELAND

d1 <- read.table("data/Iceland/taxon.txt", sep = "\t", header = TRUE)
d2 <- read.table("data/Iceland/distribution.txt", sep = "\t", header = TRUE)

inames <- full_join(d1, d2) %>% 
  as.data.table() %>% 
  filter(!threatStatus %in% c("Not Applicable","Not Evaluated"))

ICE_species <- nameSplit(gsub(" sect. ", " ", inames$scientificName)) %>% 
  select(Name, Author) %>% 
  setNames(c("taxon", "author")) %>% 
  select(taxon, author)

#########################################################################
# COMBINE ALL

all_species <- bind_rows(FIN_species, NOR_species, SWE_species, DNK_species, ICE_species) %>% 
  unique() %>% 
  filter(!grepl("×",taxon),
         !grepl(" X ",taxon),
         !grepl(" x ",taxon)) %>% 
  mutate(taxon = str_squish(taxon),
         author = str_squish(author)) %>% 
  mutate(author = ifelse(author == "", NA, author)) %>% 
  mutate(taxon = gsub(" sect. ", " ", taxon))

# the World Flora Online
load("/scratch/project_2003061/trait_datasets/Plants_WFO.rdata")
all <- nameMatch(spList = all_species %>% 
                   mutate(taxon = ifelse(taxon == "Rabelera holostea" & author == "(L.) M. T. Sharples & E. A. Tripp", "Stellaria holostea", taxon),
                          author = ifelse(taxon == "Rabelera holostea" & author == "(L.) M. T. Sharples & E. A. Tripp", NA, author),
                          taxon = ifelse(taxon == "Cherleria biflora var. serpentinicola" & author == "(Rune)", "Minuartia biflora", taxon),
                          author = ifelse(taxon == "Cherleria biflora var. serpentinicola" & author == "(Rune)", NA, author)) %>% 
                   setNames(c("Name","Author")), 
          spSource = database, max.distance = 2,
          author = TRUE, matchFirst = TRUE)

all %>% 
  filter(is.na(Accepted_SPNAME) & is.na(Name_spLev))

all <- all %>% 
  mutate(Accepted_SPNAME = ifelse(is.na(Accepted_SPNAME), Name_spLev, Accepted_SPNAME))

all3 <- nameMatch(spList = all %>% select(Accepted_SPNAME) %>% setNames(c("Name")), 
                 spSource = database, max.distance = 0,
                 author = TRUE, matchFirst = TRUE)

final <- bind_cols(all_species, all) %>% 
  select(taxon, author, Submitted_Name, Submitted_Author, Submitted_Rank, Fuzzy, Score, name.dist, author.dist) %>% 
  bind_cols(., 
            all3 %>% select(Family, Accepted_SPNAME, Author_in_database, ID_in_database)) %>% 
  rename(WFO_species = Accepted_SPNAME,
         WFO_family = Family,
         WFO_author = Author_in_database,
         WFO_ID = ID_in_database)

# The plant list names
load("/scratch/project_2003061/trait_datasets/Plants_TPL.rdata")
all <- nameMatch(spList = all_species %>% 
                   mutate(taxon = ifelse(taxon == "Rabelera holostea" & author == "(L.) M. T. Sharples & E. A. Tripp", "Stellaria holostea", taxon),
                          author = ifelse(taxon == "Rabelera holostea" & author == "(L.) M. T. Sharples & E. A. Tripp", NA, author),
                          taxon = ifelse(taxon == "Cherleria biflora var. serpentinicola" & author == "(Rune)", "Minuartia biflora", taxon),
                          author = ifelse(taxon == "Cherleria biflora var. serpentinicola" & author == "(Rune)", NA, author)) %>% 
                   setNames(c("Name","Author")), 
                 spSource = database, max.distance = 2,
                 author = TRUE, matchFirst = TRUE)

all %>% 
  filter(is.na(Accepted_SPNAME) & is.na(Name_spLev))

all <- all %>% 
  mutate(Accepted_SPNAME = ifelse(is.na(Accepted_SPNAME), Name_spLev, Accepted_SPNAME))

all3 <- nameMatch(spList = all %>% select(Accepted_SPNAME) %>% setNames(c("Name")), 
                  spSource = database, max.distance = 0,
                  author = TRUE, matchFirst = TRUE)

final <- bind_cols(final,
                  all3 %>% select(Family, Accepted_SPNAME, Author_in_database, ID_in_database) %>% 
                    rename(TPL_species = Accepted_SPNAME,
                           TPL_family = Family,
                           TPL_author = Author_in_database,
                           TPL_ID = ID_in_database))


# The Leipzig Catalogue of Vascular Plants (LCVP)
load("/scratch/project_2003061/trait_datasets/Plants_LCVP.rdata")
all <- nameMatch(spList = all_species %>% 
                   mutate(taxon = ifelse(taxon == "Rabelera holostea" & author == "(L.) M. T. Sharples & E. A. Tripp", "Stellaria holostea", taxon),
                          author = ifelse(taxon == "Rabelera holostea" & author == "(L.) M. T. Sharples & E. A. Tripp", NA, author),
                          taxon = ifelse(taxon == "Cherleria biflora var. serpentinicola" & author == "(Rune)", "Minuartia biflora", taxon),
                          author = ifelse(taxon == "Cherleria biflora var. serpentinicola" & author == "(Rune)", NA, author)) %>% 
                   setNames(c("Name","Author")), 
                 spSource = database, max.distance = 2,
                 author = TRUE, matchFirst = TRUE)

all %>% 
  filter(is.na(Accepted_SPNAME) & is.na(Name_spLev))

all <- all %>% 
  mutate(Accepted_SPNAME = ifelse(is.na(Accepted_SPNAME), Name_spLev, Accepted_SPNAME))

all3 <- nameMatch(spList = all %>% select(Accepted_SPNAME) %>% setNames(c("Name")), 
                  spSource = database, max.distance = 0,
                  author = TRUE, matchFirst = TRUE)

final <- bind_cols(final,
                   all3 %>% select(Family, Accepted_SPNAME, Author_in_database, ID_in_database) %>% 
                     rename(LCVP_species = Accepted_SPNAME,
                            LCVP_family = Family,
                            LCVP_author = Author_in_database,
                            LCVP_ID = ID_in_database))

final %>% 
  filter(duplicated(final %>% select(Submitted_Name,Submitted_Author)))

final <- final %>% 
  arrange(WFO_family, WFO_species, WFO_ID)
length(unique(final$WFO_species))

write_csv(final, "output/names/FENN_names_resolved.csv")
writexl::write_xlsx(final, "output/names/FENN_names_resolved.xlsx")

final %>% 
  arrange(Score) %>% 
  select(Submitted_Name, WFO_species, Score)


###################################################################
# Combine with Country-level information

library(googlesheets4)
fn <- read_sheet("https://docs.google.com/spreadsheets/d/1qgBOjM_941h4zm0foy83PyT6Og0ocWlDOUHXWmdlWIA/edit?gid=211021139#gid=211021139", col_types = "c") %>% 
  select(taxon, author, starts_with("WFO"), Submitted_Rank) %>% 
  distinct() %>% 
  rename(taxon_rank = Submitted_Rank)

# Finnish dataset names
fnames <- read_xlsx("data/Finland/Lajien_uhanalaisuusarviointi_2019_v2_17052019.xlsx") %>% 
  filter(`Ryhmä 1` == "Putkilokasvit, Tracheophyta") %>% 
  filter(`Luokka...14` != "NA") %>% 
  filter(`Luokka...14` != "NE") %>% 
  mutate(author = ifelse(is.na(Auktori), "", Auktori)) %>% 
  mutate(taxon = `Tieteellinen nimi`) %>% 
  mutate(taxon = gsub("ë","e",taxon)) %>% 
  mutate(taxon = gsub("-ryhmä","",taxon, ignore.case = T)) %>% 
  mutate(taxon = str_squish(taxon)) %>% 
  mutate(FI_scientific = taxon,
         FI_author = author) %>% 
  rename(FI_id = Id,
         FI_vernacular = `Suomenkielinen nimi`,
         FI_redlist = `Luokka...14`) %>% 
  select(taxon, author, FI_scientific, FI_author, FI_id, FI_vernacular, FI_redlist) %>% 
  filter(!grepl("×",taxon),
         !grepl(" X ",taxon),
         !grepl(" x ",taxon)) %>% 
  mutate(taxon = str_squish(taxon),
         author = str_squish(author)) %>% 
  mutate(author = ifelse(author == "", NA, author)) %>% 
  mutate(taxon = gsub(" sect. ", " ", taxon)) %>% 
  distinct()

anti_join(fnames, fn)

fnames <- right_join(fn, fnames) %>% 
  select(WFO_family:FI_redlist) %>% 
  mutate(FI_redlist = ifelse(FI_redlist == "CR●", "CR", FI_redlist)) %>% 
  distinct() %>% 
  drop_na(FI_redlist)

# NORWAY

nnames <- read_xlsx("data/Norway/rødliste-2021.xlsx") %>% 
  mutate(author = ifelse(is.na(Autor), "", Autor)) %>% 
  mutate(taxon = `Vitenskapelig navn`) %>% 
  mutate(taxon = gsub("ë","e",taxon)) %>% 
  mutate(taxon = str_squish(taxon)) %>% 
  mutate(NO_scientific = taxon,
         NO_author = author) %>% 
  rename(NO_id = `Vitenskapelig navn id`,
         NO_vernacular = `Populærnavn`,
         NO_redlist = `Kategori 2021`) %>% 
  select(taxon, author, NO_scientific, NO_author, NO_id, NO_vernacular, NO_redlist) %>% 
  filter(!grepl("×",taxon),
         !grepl(" X ",taxon),
         !grepl(" x ",taxon)) %>% 
  mutate(taxon = str_squish(taxon),
         author = str_squish(author)) %>% 
  mutate(author = ifelse(author == "", NA, author)) %>% 
  mutate(taxon = gsub(" sect. ", " ", taxon)) %>% 
  distinct()

anti_join(nnames, fn)

nnames <- right_join(fn, nnames) %>% 
  select(WFO_family:NO_redlist) %>% 
  mutate(NO_redlist = gsub("°","",NO_redlist)) %>% 
  distinct() %>% 
  drop_na(NO_redlist)

# SWEDEN

snames <- read_xlsx("/scratch/project_2003061/trait_datasets/Tyler/1-s2.0-S1470160X20308621-mmc1.xlsx")

snames <- snames %>% 
  mutate(`Red-listed` = ifelse(`Red-listed` == "Not Red-listed", "LC", `Red-listed`)) %>% 
  mutate(`Red-listed` = ifelse(Establishment == "Non-resident", NA, `Red-listed`))

snames <- bind_cols(nameSplit(gsub(" sect. ", " ", snames$`Scientific name`)) %>% 
            select(Name, Author) %>% 
            setNames(c("taxon", "author")) %>% 
            select(taxon, author),
            snames) %>% 
  mutate(taxon = str_squish(taxon)) %>% 
  mutate(SE_scientific = taxon,
         SE_author = author) %>% 
  rename(SE_id = `Dyntaxa ID number`,
         SE_vernacular = `Svenskt namn`,
         SE_redlist = `Red-listed`) %>% 
  select(taxon, author, SE_scientific, SE_author, SE_id, SE_vernacular, SE_redlist) %>% 
  filter(!grepl("×",taxon),
         !grepl(" X ",taxon),
         !grepl(" x ",taxon)) %>% 
  mutate(taxon = str_squish(taxon),
         author = str_squish(author)) %>% 
  mutate(author = ifelse(author == "", NA, author)) %>% 
  mutate(taxon = gsub(" sect. ", " ", taxon)) %>% 
  distinct()

anti_join(snames, fn)

snames <- right_join(fn, snames) %>% 
  select(WFO_family:SE_redlist) %>% 
  mutate(SE_redlist = toupper(SE_redlist)) %>% 
  distinct() %>% 
  drop_na(SE_scientific)

# DENMARK

dnames <- read_csv2("data/Denmark/redlist_extract_2030_35_E6757D19B379A33A8069CCE511247546_dk.csv") %>% 
  mutate(author = ifelse(is.na(scientificNameAuthorship), "", scientificNameAuthorship)) %>% 
  mutate(taxon = `scientificName`) %>% 
  mutate(taxon = gsub("ë","e", taxon)) %>% 
  mutate(taxon = str_squish(taxon)) %>% 
  mutate(DK_scientific = taxon,
         DK_author = author) %>% 
  rename(DK_id = `id`,
         DK_vernacular = `vernacularName`,
         DK_redlist = `redlistCategory`) %>% 
  select(taxon, author, DK_scientific, DK_author, DK_id, DK_vernacular, DK_redlist) %>% 
  filter(!grepl("×",taxon),
         !grepl(" X ",taxon),
         !grepl(" x ",taxon)) %>% 
  mutate(taxon = str_squish(taxon),
         author = str_squish(author)) %>% 
  mutate(author = ifelse(author == "", NA, author)) %>% 
  mutate(taxon = gsub(" sect. ", " ", taxon)) %>% 
  distinct()

anti_join(dnames, fn)

dnames <- right_join(fn, dnames) %>% 
  select(WFO_family:DK_redlist) %>% 
  mutate(DK_vernacular = tolower(DK_vernacular)) %>% 
  distinct() %>% 
  drop_na(DK_redlist)

# ICELAND

d1 <- read.table("data/Iceland/taxon.txt", sep = "\t", header = TRUE)
d2 <- read.table("data/Iceland/distribution.txt", sep = "\t", header = TRUE)

inames <- full_join(d1, d2) %>% 
  as.data.table() %>% 
  filter(!threatStatus %in% c("Not Applicable","Not Evaluated"))

inames <- bind_cols(inames,
          nameSplit(gsub(" sect. ", " ", inames$scientificName)) %>% 
            select(Name, Author) %>% 
            setNames(c("taxon", "author")) %>% 
            select(taxon, author)) %>% 
  mutate(taxon = str_squish(taxon)) %>% 
  mutate(IS_scientific = taxon,
         IS_author = author) %>% 
  rename(IS_id = `taxonID`,
         # IS_vernacular = `vernacularName`,
         IS_redlist = `threatStatus`) %>% 
  select(taxon, author, IS_scientific, IS_author, IS_id, IS_redlist) %>% 
  filter(!grepl("×",taxon),
         !grepl(" X ",taxon),
         !grepl(" x ",taxon)) %>% 
  mutate(taxon = str_squish(taxon),
         author = str_squish(author)) %>% 
  mutate(author = ifelse(author == "", NA, author)) %>% 
  mutate(taxon = gsub(" sect. ", " ", taxon)) %>% 
  distinct()

replacements <- c("Critically Endangered" = "CR", 
                  "Data Deficient" = "DD", 
                  "Endangered" = "EN", 
                  "Least Concern" = "LC", 
                  "Near Threatened" = "NT", 
                  "Regionally Extinct" = "RE", 
                  "Vulnerable" = "VU")

inames <- inames %>%
  mutate(IS_redlist = str_replace_all(IS_redlist, replacements))

anti_join(inames, fn)

inames <- right_join(fn, inames) %>% 
  select(WFO_family:IS_redlist) %>% 
  distinct() %>% 
  drop_na(IS_redlist)

#
table(snames$SE_redlist)


fnames %>% arrange(WFO_family, WFO_species) %>% rename(FI_taxon_rank = taxon_rank) %>% writexl::write_xlsx("output/names/FI_taxa.xlsx")
fnames %>% arrange(WFO_family, WFO_species) %>% rename(FI_taxon_rank = taxon_rank) %>% write_csv("output/names/FI_taxa.csv")

nnames %>% arrange(WFO_family, WFO_species) %>% rename(NO_taxon_rank = taxon_rank) %>% writexl::write_xlsx("output/names/NO_taxa.xlsx")
nnames %>% arrange(WFO_family, WFO_species) %>% rename(NO_taxon_rank = taxon_rank) %>% write_csv("output/names/NO_taxa.csv")

snames %>% arrange(WFO_family, WFO_species) %>% rename(SE_taxon_rank = taxon_rank) %>% writexl::write_xlsx("output/names/SE_taxa.xlsx")
snames %>% arrange(WFO_family, WFO_species) %>% rename(SE_taxon_rank = taxon_rank) %>% write_csv("output/names/SE_taxa.csv")

dnames %>% arrange(WFO_family, WFO_species) %>% rename(DK_taxon_rank = taxon_rank) %>% writexl::write_xlsx("output/names/DK_taxa.xlsx")
dnames %>% arrange(WFO_family, WFO_species) %>% rename(DK_taxon_rank = taxon_rank) %>% write_csv("output/names/DK_taxa.csv")

inames %>% arrange(WFO_family, WFO_species) %>% rename(IS_taxon_rank = taxon_rank) %>% writexl::write_xlsx("output/names/IS_taxa.xlsx")
inames %>% arrange(WFO_family, WFO_species) %>% rename(IS_taxon_rank = taxon_rank) %>% write_csv("output/names/IS_taxa.csv")
