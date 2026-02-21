#####################################################################
# Gather the cross validation results

library(tidyverse)
library(googlesheets4)
library(caret)

calculate_mape <- function(observed, predicted) {
  mean(abs((observed - predicted) / observed)) * 100
}
calculate_nrmse_sd <- function(observed, predicted) {
  rmse <- sqrt(mean((observed - predicted)^2))
  sd_obs <- sd(observed)
  rmse / sd_obs
}
# tr <- readxl::read_xlsx("output/Final_trait_table.xlsx")
tr <- read_sheet("https://docs.google.com/spreadsheets/d/18K7Ff3yPcQ1ungiWKLiBc9FXZspxtp6LamimXXS3scE/edit?gid=832928905#gid=832928905", col_types = "c") %>% 
  filter(to_final_dataset == 1) %>% 
  select(trait, type, category, final_trait_name)

f <- list.files("output/eval/", pattern = "randomobs", full.names = TRUE)

d <- lapply(f, read_csv, show_col_types = FALSE) %>% 
  bind_rows()

d <- left_join(d, 
               tr %>% select(trait, final_trait_name)) %>% 
  drop_na(final_trait_name) %>% 
  select(-trait) %>% 
  rename(trait = final_trait_name) %>% 
  relocate(WFO_species, trait)

res_num <- lapply(unique(d$trait), function(x){
  # x <- "LDMC"
  
  dt <- d %>% filter(trait == x)
  
  if(tr %>% filter(final_trait_name == x) %>% pull(type) == "numerical"){
    dt <- dt %>% 
      mutate(values = as.numeric(values),
             imp_values  = as.numeric(imp_values))
    
    res <- tibble(trait = x,
                  R = cor(dt$values, dt$imp_values),
                  NRMSE = calculate_nrmse_sd(dt$values, dt$imp_values))
  } else {
    res <- NULL
  }
  return(res)
}) %>% 
  bind_rows()

res_cat <- lapply(unique(d$trait), function(x){
  # x <- "woodiness"
  
  dt <- d %>% filter(trait == x)
  
  if(tr %>% filter(final_trait_name == x) %>% pull(type) != "numerical"){
    
    res <- tibble(trait = x,
                  PCC = mean(dt$values == dt$imp_values),
                  Kappa = confusionMatrix(as.factor(dt$imp_values), as.factor(dt$values))$overall["Kappa"])
    
  } else {
    res <- NULL
  }
  return(res)
}) %>% 
  bind_rows()


tr2 <- readxl::read_xlsx("output/Final_trait_table.xlsx")

tr2 <- full_join(tr2 %>% select(-unit),
          bind_rows(res_num, res_cat))

###########################################################
# species-based cross validation

tr <- read_sheet("https://docs.google.com/spreadsheets/d/18K7Ff3yPcQ1ungiWKLiBc9FXZspxtp6LamimXXS3scE/edit?gid=832928905#gid=832928905", col_types = "c") %>% 
  filter(to_final_dataset == 1) %>% 
  select(trait, type, category, final_trait_name)

f <- list.files("output/eval/", pattern = "randomspecs", full.names = TRUE)

d <- lapply(f, read_csv, show_col_types = FALSE) %>% 
  bind_rows()

d <- left_join(d, 
               tr %>% select(trait, final_trait_name)) %>% 
  drop_na(final_trait_name) %>% 
  select(-trait) %>% 
  rename(trait = final_trait_name) %>% 
  relocate(WFO_species, trait)

res_num <- lapply(unique(d$trait), function(x){
  # x <- "LDMC"
  
  dt <- d %>% filter(trait == x)
  
  if(tr %>% filter(final_trait_name == x) %>% pull(type) == "numerical"){
    dt <- dt %>% 
      mutate(values = as.numeric(values),
             imp_values  = as.numeric(imp_values))
    
    res <- tibble(trait = x,
                  R = cor(dt$values, dt$imp_values),
                  NRMSE = calculate_nrmse_sd(dt$values, dt$imp_values))
  } else {
    res <- NULL
  }
  return(res)
}) %>% 
  bind_rows()

res_cat <- lapply(unique(d$trait), function(x){
  # x <- "woodiness"
  
  dt <- d %>% filter(trait == x)
  
  if(tr %>% filter(final_trait_name == x) %>% pull(type) != "numerical"){
    
    res <- tibble(trait = x,
                  PCC = mean(dt$values == dt$imp_values),
                  Kappa = confusionMatrix(as.factor(dt$imp_values), as.factor(dt$values))$overall["Kappa"])
    
  } else {
    res <- NULL
  }
  return(res)
}) %>% 
  bind_rows()

tr3 <- full_join(tr2,
                 bind_rows(res_num, res_cat) %>% rename(R2 = R, NRMSE2 = NRMSE, PCC2 = PCC, Kappa2 = Kappa))


tr3 %>% write_csv("output/CV_statistics.csv")
