# Trout CPE percentile breakdown for stream surveys on Class 1 trout streams
# By: Bryan Maitland
# Created: 2021-07-20
# Updated: 2024-02-07

# This script will:
#   - assign age classes and trophy status
#   - calculate total catch and CPEs
#   - link to WHD and filter to Class 1 streams
#   - calculate CPE quantiles
#   - make summary tables

## Libraries ==================================================================
library(tidyverse)
library(here)

## Data =======================================================================

df_surveys <- read_rds(here("data", "df_trout_surveys.rds"))
df_efforts <- read_rds(here("data", "df_trout_efforts.rds"))
df_trout <- read_rds(here("data", "df_trout_data.rds"))

# simplify columns a bit
df_trout <- df_trout %>% 
  relocate(length_mm, .after = "length") %>% 
  relocate(primary.survey.purpose, .after = "yoy") %>% 
  select(1:46)

# site data (has been cross-referenced with 24k WHDPLus)
df_sites_va <- read_rds(here("data","sites_list_va.rds"))

## Assign age classes ========================================================

# Roughly based off AFS benchmarks:

# Brookies: <4 (YOY), 4-7 (yearling), GREATER than 7 (adults), then preferred is >=10, memorable >=15
# Browns: <4 (YOY), 4-8(yearling), GREATER than  8 (adults), then preferred is >=12, memorable >=15


# size classes
df_trout <- df_trout %>% 
  mutate(
    size_class = case_when(
      species=="brook_trout" & length < 4 ~ 'YOY',
      species=="brook_trout" & between(length, 4, 7) ~ 'Yearling',
      species=="brook_trout" & length > 7 ~ 'Adult',
      species=="brown_trout" & length < 4 ~ 'YOY',
      species=="brown_trout" & between(length, 4, 8) ~ 'Yearling',
      species=="brown_trout" & length > 8 ~ 'Adult',
      TRUE ~ NA_character_
      ), 
    prefered = case_when(
      species=="brook_trout" & length >= 10 ~ 'Preferred',
      species=="brown_trout" & length >= 12 ~ 'Preferred',
      TRUE ~ NA_character_
      ), 
    memorable = case_when(
      species=="brook_trout" & length >= 15 ~ 'Memorable',
      species=="brown_trout" & length >= 15 ~ 'Memorable',
      TRUE ~ NA_character_
      )
    ) 


# Remove NAs (cases where we didn't have length data, ~ 2% of fish)
map(df_trout, ~sum(is.na(.)))
df_trout <- df_trout |> filter(! is.na(size_class))


## Calculate CPEs  =============================================

# efforts (dist shocked) by visit
total_effort <- df_efforts %>% 
  group_by(visit.fish.seq.no) %>% 
  summarize(total_effort = sum(distance.shocked), .groups = "drop") 
# total_effort <- df_efforts %>% select(visit.fish.seq.no, distance.shocked) # same thing

# cpe all sizes
cpes_all <- df_trout %>% 
  mutate(size_class = "All fish") %>%  
  group_by(waterbody.name, survey.year, survey.seq.no, visit.fish.seq.no, species, size_class) %>% 
  summarize(total_catch = sum(number.of.fish), .groups = "drop")  %>% 
  left_join(total_effort, by =  c("visit.fish.seq.no")) %>% 
  mutate(cpe = total_catch / total_effort)

# cpe by age classes
cpes_age <- df_trout %>% 
  group_by(waterbody.name, survey.year, survey.seq.no, visit.fish.seq.no, species, size_class) %>% 
  summarize(total_catch = sum(number.of.fish), .groups = "drop") %>% 
  left_join(total_effort, by =  c("visit.fish.seq.no")) %>% 
  mutate(cpe = total_catch / total_effort)

# cpe for prefered
cpes_pref <- df_trout %>% 
  filter(prefered == "Preferred") %>% 
  mutate(size_class = "Preferred") %>% 
  group_by(waterbody.name, survey.year, survey.seq.no, visit.fish.seq.no, species, size_class) %>% 
  summarize(total_catch = sum(number.of.fish), .groups = "drop") %>% 
  left_join(total_effort, by =  c("visit.fish.seq.no")) %>% 
  mutate(cpe = total_catch / total_effort)

# cpe for memorable
cpes_mem <- df_trout %>% 
  filter(memorable == "Memorable") %>% 
  mutate(size_class = "Memorable") %>% 
  group_by(waterbody.name, survey.year, survey.seq.no, visit.fish.seq.no, species, size_class) %>% 
  summarize(total_catch = sum(number.of.fish), .groups = "drop") %>% 
  left_join(total_effort, by =  c("visit.fish.seq.no")) %>% 
  mutate(cpe = total_catch / total_effort)

# Combine into one tibble
df_cpes <- 
  bind_rows(cpes_all, cpes_age, cpes_pref, cpes_mem) %>% 
  mutate(
    size_class = factor(
      size_class, levels = c("All fish","YOY","Yearling","Adult","Preferred", "Memorable"))
    ) %>% 
  arrange(species, visit.fish.seq.no, size_class)

# Add back metatdata from surveys and efforts tibbles
df_cpes <- df_cpes %>% 
  left_join(
    df_surveys %>% select(
      survey.seq.no, wbic, site.seq.no, latitude, longitude, primary.survey.purpose), 
    by = "survey.seq.no") %>% 
  left_join(
    df_efforts %>% select(visit.fish.seq.no, sample.date, gear), by = "visit.fish.seq.no") 

# check it
df_cpes


## Link to xref'ed WHD site data ======================================================

# this gets all the spatial info (ecoregion, HUCs, etc)
df_cpes <- df_cpes %>% 
  mutate(site.seq.no=as.character(site.seq.no)) %>% 
  left_join(df_sites_va %>% select(-swims.station.id, -wbic, -latitude, -longitude), 
            by = "site.seq.no")

# check it
df_cpes

## Calculate Quantiles ===============================================================

# function to calculate quantiles
get_quantiles <- function(data) {
  out <- data %>% 
    group_by(species, size_class) %>% 
    reframe(
      n = n(),
      quantile = scales::percent(c(.1,.25,.35,0.5,.65,.75,.9)),
      cpe_quant = quantile(cpe, c(.1,.25,.35,0.5,.65,.75,.9))
    ) %>%
    arrange(species, size_class, quantile)
  out
}

# Summarize for data collected last 10 years; 2012-2021 on Class 1 streams

# targs.survey.purpose.cpe <- c(
#   "fisheries_assessments_trout_trend",
#   "fisheries_assessments_trout_rotation"
#   )

df_cpes_sub <- df_cpes %>%
  # filter(primary.survey.purpose %in% targs.survey.purpose.cpe) %>%
  filter(between(survey.year, 2012, 2021)) %>%
  filter(trout_class == "CLASS I") |> 
  mutate(ecoregion = factor(ecoregion))

# check it 
df_cpes_sub

### state-wide quantiles -------------------------------------------

quants_wi <- df_cpes_sub %>% get_quantiles()

### ecoregion quantiles ----------------------------------

# split by ecoregion into list
eco_list <- split(df_cpes_sub, df_cpes_sub$ecoregion)

# get quants
quants_eco <- map(eco_list, get_quantiles)


### summary tables ----------------------------------------------

# function to make summary tables

# test
# quants_wi %>% 
#   pivot_wider(id_cols = -n, names_from = size_class, values_from = cpe_quant) %>% 
#   mutate(species = if_else(species=="brook_trout",'Brook Trout', 'Brown Trout')) %>% 
#   mutate(across(where(is.numeric), round, 1))

# function
make_summary_tbls <- function(data) {
  out <- data %>% 
    pivot_wider(id_cols = -n, names_from = size_class, values_from = cpe_quant) %>% 
    mutate(species = if_else(species=="brook_trout",'Brook Trout', 'Brown Trout')) %>% 
    mutate(across(where(is.numeric), round, 1))|> 
    rename(Species = species, Quantile = quantile)
  out
}

# make tables
wi_tbl <- map(list(quants_wi), make_summary_tbls)
eco_tbl <- map(quants_eco, make_summary_tbls)

# bind tables
names(wi_tbl) <- c("Statewide")
quant_tbls <- c(wi_tbl, eco_tbl)
str(quant_tbls, max.level = 1)

# write to csv
quant_tbls |> 
  names() |> 
  walk(~ write_csv(quant_tbls[[.]], paste0("output/cpe_qunatiles_", ., ".csv")))

### statewide / driftless table ----------------------------------

combo <- left_join(
  quants_eco$`Driftless Area` |> select(-n),
  quants_wi |> select(-n),
  by=c("species","size_class","quantile")
  ) |> 
  rename(Driftless = cpe_quant.x, Wisconsin = cpe_quant.y)

# make it wide and recreate table
wi_dft_tbl <- combo %>%
  pivot_wider(names_from = size_class, values_from = c(Wisconsin, Driftless)) %>%
  mutate(species = if_else(species=="brook_trout",'Brook Trout', 'Brown Trout')) %>%
  mutate(across(where(is.numeric), round, 1)) %>%
  relocate(`Driftless_All fish`, .before = `Wisconsin_All fish`) %>%
  relocate(Driftless_YOY, .before = Wisconsin_YOY) %>%
  relocate(Driftless_Yearling, .before = Wisconsin_Yearling) %>%
  relocate(Driftless_Adult, .before = Wisconsin_Adult) %>%
  relocate(Driftless_Preferred, .before = Wisconsin_Preferred) %>%
  relocate(Driftless_Memorable, .before = Wisconsin_Memorable) |> 
  rename(Species = species, Quantile = quantile)

wi_dft_tbl

# write to file
write_csv(wi_dft_tbl, here("output","cpe_qunatiles_Statewide-and-Driftless.csv"))


