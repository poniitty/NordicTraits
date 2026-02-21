#####################################################################
# Conduct a "leave random 1% of observations out" cross validation

library(itertools)
library(missForest)
library(tidyverse)
library(data.table)
library(doParallel)

d <- fread("output/traits_missing_finalized.csv")

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


traits <- names(d)[12:ncol(d)]

i <- 1

while(i < 100){
  print(i)
  dt <- d %>% 
    select(-starts_with("phyloeigen")) %>% 
    mutate(across(where(is.numeric), as.character)) %>% 
    pivot_longer(cols = -WFO_species, names_to = "trait", values_to = "values", values_drop_na = TRUE) %>%
    group_by(trait) %>%
    group_modify(~ {
      # Calculate 1% of the rows in the current group
      n_sample <- max(1, round(nrow(.x) * 0.01))
      # Sample the row indices
      sampled_rows <- sample(1:nrow(.x), size = n_sample)
      # Replace 'values' with NA for the sampled rows
      .x %>%
        mutate(values = ifelse(row_number() %in% sampled_rows, NA, values))
    }) %>%
    ungroup()
  
  true_values <- left_join(dt %>% 
                             filter(is.na(values)) %>% 
                             select(-values),
                           d %>% 
                             select(-starts_with("phyloeigen")) %>% 
                             mutate(across(where(is.numeric), as.character)) %>% 
                             pivot_longer(cols = -WFO_species, names_to = "trait", values_to = "values", values_drop_na = TRUE))
  
  dt <- bind_rows(d %>% 
                    select(WFO_species, starts_with("phyloeigen")) %>% 
                    mutate(across(where(is.numeric), as.character)) %>% 
                    pivot_longer(cols = -WFO_species, names_to = "trait", values_to = "values", values_drop_na = TRUE),
                  dt) %>% 
    pivot_wider(id_cols = WFO_species, names_from = trait, values_from = values)
  
  tmpfile <- paste0("/scratch/project_2003061/temp/", basename(tempfile()), ".csv")
  
  write_csv(dt, tmpfile)
  dt <- read_csv(tmpfile)
  unlink(tmpfile)
  
  dt <- dt %>% 
    mutate(across(where(is.character), as.factor))
  
  cl <- makePSOCKcluster(future::availableCores())
  registerDoParallel(cl)
  system.time({
    imtr <- missForest(dt %>% select(-c(WFO_species)) %>% as.data.table,
                       maxiter = 10, ntree = 300, variablewise = TRUE, nodesize = c(1,2),
                       parallelize = "forests", verbose = TRUE)
  })
  stopCluster(cl)
  # 19407/60/60
  
  imptrs <- imtr$ximp
  
  imptrs <- bind_cols(dt %>% select(WFO_species),
                      imptrs) %>% 
    select(-starts_with("phyloeigen"))
  
  true_values <- left_join(true_values,
                           imptrs %>% 
                             select(-starts_with("phyloeigen")) %>% 
                             mutate(across(where(is.numeric), as.character)) %>% 
                             pivot_longer(cols = -WFO_species, names_to = "trait", values_to = "imp_values", values_drop_na = TRUE))
  
  true_values %>% filter(trait == "SLA") %>% sample_n(size = 10)
  true_values %>% filter(trait == "LDMC") %>% select(values, imp_values) %>% mutate(across(everything(), as.numeric)) %>% cor
  
  random_string <- paste0(sample(c(0:9, letters, LETTERS), 6, replace = TRUE), collapse = "")
  
  write_csv(true_values, paste0("output/eval/randomobs_", paste0(sample(c(0:9, letters, LETTERS), 6, replace = TRUE), collapse = ""), ".csv"))
  
  i <- i + 1
}

