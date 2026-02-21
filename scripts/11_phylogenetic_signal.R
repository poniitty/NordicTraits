#####################################################################
# Calculate phylogenetic signal in traits or trait missingess

library(ape)
library(phytools)
library(googlesheets4)
library(tidyverse)
library(parallel)

d <- read_csv("output/traits_missing.csv")
tr <- read_sheet("https://docs.google.com/spreadsheets/d/18K7Ff3yPcQ1ungiWKLiBc9FXZspxtp6LamimXXS3scE/edit?gid=832928905#gid=832928905", col_types = "c")

fn <- read_sheet("https://docs.google.com/spreadsheets/d/1qgBOjM_941h4zm0foy83PyT6Og0ocWlDOUHXWmdlWIA/edit?gid=211021139#gid=211021139", col_types = "c") %>% 
  dplyr::select(WFO_species, TPL_species) %>% 
  distinct()

d <- right_join(fn, d) %>% 
  dplyr::select(-WFO_species) %>% 
  distinct() %>% 
  rename(species = TPL_species) %>% 
  mutate(species = gsub(" ", "_", species)) %>% 
  drop_na(species)

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
  dplyr::select(species, all_of((tr %>% filter(to_imputation == 1) %>% pull(trait)))) %>% 
  dplyr::select(species, all_of((tr %>% filter(to_final_dataset == 1) %>% pull(trait))))

tree <- read_rds("output/Nordic_phylotree.rda")$scenario.3
d$species[!d$species %in% tree$tip.label]

sum(duplicated(tree$tip.label))

miss <- d %>% 
  mutate(across(-species, ~ifelse(is.na(.x), 0, 1))) %>% 
  group_by(species) %>% 
  summarise(across(everything(), max)) %>% 
  as.data.frame()

# Ensure species names match between data and tree
# (If your species column is not 'species', adjust the code)
rownames(miss) <- miss$species

my_traits_data <- miss[, -which(names(miss) == "species")]

shared_species <- intersect(tree$tip.label, rownames(miss))
my_tree_pruned <- keep.tip(tree, shared_species)
my_traits_data_pruned <- miss[shared_species, , drop = FALSE]

# Ensure the order of species in the data matches the tip labels in the tree (optional but good practice)
my_traits_data_pruned <- my_traits_data_pruned[my_tree_pruned$tip.label, , drop = FALSE]

# Iterate through each binary trait
res <- mclapply(colnames(my_traits_data_pruned)[-1], function(i){
  # i <- "height_reproductive"
  print(i)
  trait_values <- my_traits_data_pruned[, i]
  trait_numeric <- as.numeric(trait_values)
  names(trait_numeric) <- rownames(my_traits_data_pruned)
  
  tryCatch({
    signal_lambda <- phylosig(
      tree = my_tree_pruned,
      x = trait_numeric,
      method = "lambda",
      nsim = 1,
      niter = 1,
      test = FALSE # Perform a likelihood ratio test for lambda=0 vs lambda=estimated
    )
    
    return(tibble(trait = i,
                  phylo_signal = signal_lambda$lambda))
    
    
  }, error = function(e) {
    return(NULL)
  })
}, mc.cores = 1)

res_miss <- res %>% bind_rows()

left_join(res_miss, 
          tr %>% select(trait, final_trait_name)) %>% 
  rename(phylo_signal_missingness = phylo_signal) %>% 
  write_csv("output/phylo_signal_missingness.csv")

###################################################
# Species specific measurements

d <- read_csv("output/traits_missing.csv")
tr <- read_sheet("https://docs.google.com/spreadsheets/d/18K7Ff3yPcQ1ungiWKLiBc9FXZspxtp6LamimXXS3scE/edit?gid=832928905#gid=832928905", col_types = "c")

fn <- read_sheet("https://docs.google.com/spreadsheets/d/1qgBOjM_941h4zm0foy83PyT6Og0ocWlDOUHXWmdlWIA/edit?gid=211021139#gid=211021139", col_types = "c") %>% 
  dplyr::select(WFO_species, TPL_species) %>% 
  distinct()

d <- right_join(fn, d) %>% 
  dplyr::select(-WFO_species) %>% 
  distinct() %>% 
  rename(species = TPL_species) %>% 
  mutate(species = gsub(" ", "_", species)) %>% 
  drop_na(species)

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
  dplyr::select(species, all_of((tr %>% filter(to_imputation == 1) %>% pull(trait)))) %>% 
  dplyr::select(species, all_of((tr %>% filter(to_final_dataset == 1) %>% pull(trait))))

tree <- read_rds("output/Nordic_phylotree.rda")$scenario.3
d$species[!d$species %in% tree$tip.label]

sum(duplicated(tree$tip.label))

d2 <- d %>% 
  select(species, where(is.numeric)) %>% 
  group_by(species) %>% 
  summarise(across(everything(), mean)) %>% 
  as.data.frame()

# Ensure species names match between data and tree
# (If your species column is not 'species', adjust the code)
rownames(d2) <- d2$species

d2 <- d2[, -which(names(d2) == "species")]

# Iterate through each binary trait
res <- mclapply(colnames(d2), function(i){
  # i <- "buoyancy"
  print(i)
  
  dt <- d2 %>% select(all_of(i)) %>% drop_na()
  shared_species <- intersect(tree$tip.label, rownames(dt))
  my_tree_pruned <- keep.tip(tree, shared_species)
  
  trait_values <- dt[,i]
  trait_numeric <- as.numeric(trait_values)
  names(trait_numeric) <- rownames(dt)
  
  tryCatch({
    signal_lambda <- phylosig(
      tree = my_tree_pruned,
      x = trait_numeric,
      method = "lambda",
      nsim = 1,
      niter = 1,
      test = FALSE # Perform a likelihood ratio test for lambda=0 vs lambda=estimated
    )
    
    return(tibble(trait = i,
                  phylo_signal = signal_lambda$lambda))
    
    
  }, error = function(e) {
    return(NULL)
  })
}, mc.cores = 1)

res_num <- res %>% bind_rows()

# Categorical

d2 <- d %>% 
  dplyr::select(species, any_of((tr %>% filter(type == "categorical") %>% pull(trait))))

# Iterate through each binary trait
res <- mclapply(colnames(d2)[-1], function(i){
  # i <- "aquatic"
  print(i)
  
  dt <- d2 %>% select(species, all_of(i)) %>% drop_na() %>% 
    group_by(species) %>% 
    slice_head(n = 1) %>% 
    ungroup()
  
  dummy_vars_matrix <- model.matrix(as.formula(paste0("~ ", i, " - 1")), data = dt) %>% 
    as.data.frame()
  
  rownames(dummy_vars_matrix) <- dt$species
  
  shared_species <- intersect(tree$tip.label, rownames(dummy_vars_matrix))
  my_tree_pruned <- keep.tip(tree, shared_species)
  
  res2 <- lapply(colnames(dummy_vars_matrix), function(ii){
    print(ii)
    trait_values <- dummy_vars_matrix[,ii]
    trait_numeric <- as.numeric(trait_values)
    names(trait_numeric) <- rownames(dummy_vars_matrix)
    
    tryCatch({
      signal_lambda <- phylosig(
        tree = my_tree_pruned,
        x = trait_numeric,
        method = "lambda",
        nsim = 1,
        niter = 1,
        test = FALSE # Perform a likelihood ratio test for lambda=0 vs lambda=estimated
      )
      
      return(tibble(trait = ii,
                    phylo_signal = signal_lambda$lambda))
      
      
    }, error = function(e) {
      return(NULL)
    })
  })
  
  
  
  return(tibble(trait = i,
                phylo_signal = bind_rows(res2) %>% pull(phylo_signal) %>% mean))
  
  
}, mc.cores = 1)

res_cat <- res %>% bind_rows()

# Binomial

d2 <- d %>% 
  dplyr::select(species, any_of((tr %>% filter(type == "binary") %>% pull(trait)))) %>% 
  mutate(across(-species, ~as.numeric(.x)-1)) %>% 
  group_by(species) %>% 
  summarise(across(everything(), max)) %>% 
  as.data.frame()

rownames(d2) <- d2$species

d2 <- d2[, -which(names(d2) == "species")]

res <- mclapply(colnames(d2), function(i){
  # i <- "buoyancy"
  print(i)
  
  dt <- d2 %>% select(all_of(i)) %>% drop_na()
  shared_species <- intersect(tree$tip.label, rownames(dt))
  my_tree_pruned <- keep.tip(tree, shared_species)
  
  trait_values <- dt[,i]
  trait_numeric <- as.numeric(trait_values)
  names(trait_numeric) <- rownames(dt)
  
  tryCatch({
    signal_lambda <- phylosig(
      tree = my_tree_pruned,
      x = trait_numeric,
      method = "lambda",
      nsim = 1,
      niter = 1,
      test = FALSE # Perform a likelihood ratio test for lambda=0 vs lambda=estimated
    )
    
    return(tibble(trait = i,
                  phylo_signal = signal_lambda$lambda))
    
    
  }, error = function(e) {
    return(NULL)
  })
}, mc.cores = 1)

res_bin <- res %>% bind_rows()

res_all <- bind_rows(res_num,
                     res_cat,
                     res_bin)

left_join(res_all, 
          tr %>% select(trait, final_trait_name)) %>% 
  rename(phylo_signal_raw = phylo_signal) %>% 
  write_csv("output/phylo_signal_rawvalues.csv")

##################################################################
# Combine

d1 <- read_csv("output/phylo_signal_rawvalues.csv")
d2 <- read_csv("output/phylo_signal_missingness.csv")

tr <- read_sheet("https://docs.google.com/spreadsheets/d/18K7Ff3yPcQ1ungiWKLiBc9FXZspxtp6LamimXXS3scE/edit?gid=832928905#gid=832928905", col_types = "c")

d <- full_join(d1, d2) %>% 
  select(-trait) %>% 
  left_join(., 
            tr %>% select(final_trait_name, category, n_species, unit, type)) %>% 
  relocate(final_trait_name, category, type, unit, n_species) %>% 
  mutate(n_species = (as.numeric(n_species)/3099)) %>% 
  rename(prop_species_covered = n_species,
         trait = final_trait_name) %>% 
  arrange(category, trait) %>% 
  mutate(across(prop_species_covered:phylo_signal_missingness, ~round(.x, 2)))

d %>% writexl::write_xlsx("output/Final_trait_table.xlsx")
