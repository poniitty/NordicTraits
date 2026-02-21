#####################################################################
# Impute missing values in the species x trait table


library(itertools)
library(missForest)
library(tidyverse)
library(data.table)
library(doParallel)

d <- fread("output/traits_missing_finalized.csv")

d <- d %>% 
  mutate(across(where(is.character), as.factor))

d <- d %>% 
  mutate(Tyler_longevity = ifelse(Tyler_longevity == 0, NA, Tyler_longevity))

d <- d %>% 
  mutate(LEDA_seed_number_flower = log(LEDA_seed_number_flower+1),
         LEDA_seed_number_plant = log(LEDA_seed_number_plant+1),
         leaf_width = log(leaf_width+1),
         leaf_length = log(leaf_length+1),
         leaf_thickness = log(leaf_thickness+1),
         SRL = log(SRL+1),
         root_depth = log(root_depth+1),
         root_P = log(root_P+1),
         SLA = log(SLA+1))

1-(apply(d[,12:216], 2, function(x){mean(is.na(x))}) %>% summary)
1-(apply(d[,12:216], 2, function(x){mean(is.na(x))}) %>% sort)

summary(d[,1:50])
summary(d[,51:100])
summary(d[,101:150])
summary(d[,151:200])
summary(d[,201:216])


cl <- makePSOCKcluster(future::availableCores())
registerDoParallel(cl)
imtr <- missForest(d %>% select(-c(WFO_species)) %>% as.data.table,
                   maxiter = 10, ntree = 300, variablewise = TRUE, nodesize = c(1,2),
                   parallelize = "forests", verbose = TRUE)
stopCluster(cl)


OOBs <- imtr$OOBerror
names(OOBs) <- names(imtr$ximp)
imptrs <- imtr$ximp

imptrs <- bind_cols(d %>% select(WFO_species),
                    imptrs) %>% 
  select(-starts_with("phyloeigen"))

options(scipen = 999)
round(OOBs, 4)


dt_all %>% 
  select(-starts_with("phyloeigen")) %>% 
  arrange(class, order, family, genus, canonical_species_name)

saveRDS(OOBs, "output/imputation_OOBs_300.rds")
write_csv(imptrs, "output/imputation_traits_300.csv")
