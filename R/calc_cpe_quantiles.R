# Trout CPE percentile breakdown for stream surveys on Class 1 trout streams
# By: Bryan Maitland
# Created: 2021-07-20
# Updated: 2024-02-07


# Libraries ==================================================================
library(tidyverse)
library(here)

# Data =======================================================================

# Data source: Wisconsin Department of Natural Resources (WDNR)
# Data description: electrofishing data on wadeable streams
#   [min distance=100m] that targeted trout

# Load data 
df_surveys <- read_rds(here("data", "df_trout_surveys_cln.rds"))
df_efforts <- read_rds(here("data", "df_trout_efforts_cln.rds"))
df_trout <- read_rds(here("data", "df_trout_data.rds"))

# Load Site data (has been cross-referenced with 24k WHDPLus)
df_sites_va <- read_rds(here("data","sites_list_va.rds"))

# Data prep ==================================================================

# simplify columns a bit
df_trout <- df_trout %>% 
  relocate(length_mm, .after = "length") %>% 
  relocate(primary.survey.purpose, .after = "yoy") %>% 
  select(1:46)

# assign age classes:
#  - brook trout: <4" = age0, 4-6.9" = age1, >=7" = adult
#  - brown trout: <4" = age0, 4-7.9" = age1, >=8" = adult

df_trout <- df_trout %>% 
  mutate(
    size_class = case_when(
      species=="brook_trout" & length < 4 ~ 'age0',
      species=="brook_trout" & between(length, 4, 6.9) ~ 'age1',
      species=="brook_trout" & length >= 7 ~ 'adult',
      species=="brown_trout" & length < 4 ~ 'age0',
      species=="brown_trout" & between(length, 4, 7.9) ~ 'age1',
      species=="brown_trout" & length >= 8 ~ 'adult',
      TRUE ~ NA_character_
      )
    ) 

# add trophy identifier
df_trout <- df_trout %>% 
  mutate(
    size_trophy = case_when(
      species=="brook_trout" & length >= 10 ~ 'pref',
      species=="brown_trout" & length >= 12 ~ 'pref',
      TRUE ~ NA_character_
      )
    ) 

# Check and remove NAs
df_trout
map(df_trout, ~sum(is.na(.)))

# Remove NAs  (cases where we didn't have length data, ~ 2% of fish)
df_trout <- df_trout %>% 
  filter(! is.na(size_class))


# Calculate total catch /CPEs  =============================================

## tibble of total efforts
total_effort <- df_efforts %>% 
  group_by(visit.fish.seq.no) %>% 
  summarize(total_effort = sum(distance.shocked), .groups = "drop") 
# total_effort <- df_efforts %>% select(visit.fish.seq.no, distance.shocked) # same thing

## Calculate cpe for each group them bind together

# tibble of catch by age class
df_cpes_grps <- df_trout %>% 
  group_by(waterbody.name, survey.year, survey.seq.no, visit.fish.seq.no, species, size_class) %>% 
  summarize(total_catch = sum(number.of.fish), .groups = "drop") %>% 
  left_join(total_effort, by =  c("visit.fish.seq.no")) %>% 
  mutate(cpe = total_catch/total_effort)

# tibble of total catch
df_cpes_total <- df_trout %>% 
  mutate(size_class = "total") %>%  
  group_by(waterbody.name, survey.year, survey.seq.no, visit.fish.seq.no, species, size_class) %>% 
  summarize(total_catch = sum(number.of.fish), .groups = "drop")  %>% 
  left_join(total_effort, by =  c("visit.fish.seq.no")) %>% 
  mutate(cpe = total_catch/total_effort)

# tibble of catch for preferred sizes
df_cpes_trophy <- df_trout %>% 
  filter(size_trophy == "pref") %>% 
  mutate(size_class = "pref") %>% 
  group_by(waterbody.name, survey.year, survey.seq.no, visit.fish.seq.no, species, size_class) %>% 
  summarize(total_catch = sum(number.of.fish), .groups = "drop") %>% 
  left_join(total_effort, by =  c("visit.fish.seq.no")) %>% 
  mutate(cpe = total_catch/total_effort)

## Bind total and length-specific CPES into a tibble
df_cpes <- 
  bind_rows(df_cpes_total, df_cpes_grps, df_cpes_trophy) %>% 
  mutate(size_class = factor(size_class, levels = c("total","age0","age1","adult","pref"))) %>% 
  arrange(species, visit.fish.seq.no, size_class)


## Add back metatdata from surveys and efforts tibbles
df_cpes <- df_cpes %>% 
  left_join(df_surveys %>% select(survey.seq.no, wbic, site.seq.no, latitude, longitude, primary.survey.purpose), 
            by = "survey.seq.no")  %>% 
  left_join(df_efforts %>% select(visit.fish.seq.no, sample.date, gear), 
            by = "visit.fish.seq.no") 

# check it
df_cpes



# Link to xref'ed WHD site data ======================================================

# this gets all the spatial info (ecoregion, HUCs, etc)
df_cpes_va <- df_cpes %>% 
  mutate(site.seq.no=as.character(site.seq.no)) %>% 
  left_join(df_sites_va %>% select(-swims.station.id, -wbic, -latitude, -longitude), 
            by = "site.seq.no")


# CPE summaries ========================================================================

# Summarize data for data collected last 10 years; 2012-2021 on Class 1 streams

# Filter out relevant data subset for summaries ------------------

# targs.survey.purpose.cpe <- c(
#   "fisheries_assessments_trout_trend",
#   "fisheries_assessments_trout_rotation",
#   "fisheries_assessments_trout_potential",
#   "fisheries_assessments_rivers",
#   "baseline_monitoring",
#   "comprehensive_survey",
#   "general_survey",
#   "natural_community_reference",
#   "targeted_watershed_assessment",
#   "watershed_comprehensive_sites",
#   "watershed_long_term_reference_site_monitoring"
#   )

targs.survey.purpose.cpe <- c(
  "fisheries_assessments_trout_trend",
  "fisheries_assessments_trout_rotation"
  )


df_cpes_va_sub <- df_cpes_va %>%
  # filter(primary.survey.purpose %in% targs.survey.purpose.cpe) %>%
  # filter(between(survey.year, 2007, 2014)) %>% 
  filter(between(survey.year, 2012, 2021)) %>%
  filter(trout_class == "CLASS I")


# state-wide summary -------------------------------------------

statewide <- df_cpes_va_sub %>% 
  group_by(species, size_class) %>% 
  summarise(
    quantile = scales::percent(c(.1,.25,.35,0.5,.65,.75,.9)),
    cpe_quant = quantile(cpe, c(.1,.25,.35,0.5,.65,.75,.9)),
    .groups = 'drop') %>%
  arrange(species, size_class, quantile)


# # 'driftless'-wide summary ------------------------------------
# 
# drift <- df_cpes_va_sub %>% 
#   filter(ecoregion %in% c("Driftless Area", "Western Corn Belt Plains")) %>% 
#   group_by(species, size_class) %>% 
#   summarise(
#     quantile = scales::percent(c(.1,.25,.35,0.5,.65,.75,.9)),
#     cpe_dft = quantile(cpe, c(.1,.25,.35,0.5,.65,.75,.9)),
#     .groups = 'drop') %>%
#   arrange(species, size_class, quantile)
# 
# 
# # Make Summary table --------------------------------------------
# 
# data_summary <- left_join(drift, statewide, 
#                           by=c("species","size_class","quantile"))
# 
# # make it wide and recreate table
# data_summary_wide <- data_summary %>% 
#   pivot_wider(names_from = size_class, values_from = c(cpe_wi, cpe_dft)) %>% 
#   mutate(species = if_else(species=="brook_trout",'Brook Trout', 'Brown Trout')) %>% 
#   mutate(across(where(is.numeric), round, 1)) %>% 
#   relocate(cpe_dft_total, .before=cpe_wi_total) %>% 
#   relocate(cpe_dft_age0, .before=cpe_wi_age0) %>% 
#   relocate(cpe_dft_age1, .before=cpe_wi_age1) %>% 
#   relocate(cpe_dft_adult, .before=cpe_wi_adult) %>% 
#   relocate(cpe_dft_pref, .before=cpe_wi_pref)
# 
# print(data_summary_wide)
# data_summary_wide %>% filter(quantile=="50%")
# 
# # write to file
# data_summary_wide %>% 
#   write_csv(here("output","trout_cpe_quantiles.csv"))


# summary for each ecoregion ----------------------------------

# convert ecoregion to factor
df_cpes_va_sub <- df_cpes_va_sub |> mutate(ecoregion = factor(ecoregion))
# split by ecoregion
eco_list <- split(df_cpes_va_sub, df_cpes_va_sub$ecoregion)

get_quantiles <- function(data) {
  out <- data %>% 
    group_by(species, size_class) %>% 
    summarise(
      n = n(),
      quantile = scales::percent(c(.1,.25,.35,0.5,.65,.75,.9)),
      cpe_quant = quantile(cpe, c(.1,.25,.35,0.5,.65,.75,.9)),
      .groups = 'drop') %>%
    arrange(species, size_class, quantile)
  out
}

all_ecos <- map(eco_list, get_quantiles)
str(all_ecos)

make_summaries <- function(data) {
  out <- data %>% 
    pivot_wider(names_from = size_class, values_from = cpe_quant) %>% 
    mutate(species = if_else(species=="brook_trout",'Brook Trout', 'Brown Trout')) %>% 
    mutate(across(where(is.numeric), round, 1)) %>% 
    relocate(total, .before = total) %>% 
    relocate(age0, .before  = age0) %>% 
    relocate(age1, .before  = age1) %>% 
    relocate(adult, .before = adult) %>% 
    relocate(pref, .before  = pref)
  out
}

eco_summaries <- map(all_ecos, make_summaries)
wi_summary <- map(list(statewide), make_summaries)
names(wi_summary) <- c("Statewide")
all_summaries <- c(wi_summary, eco_summaries)
str(all_summaries)

all_summaries |> 
  # map(~as.data.frame(.)) |> 
  names() %>%
  walk(~ write_csv(all_summaries[[.]], paste0("output/", ., ".csv")))

list(iris = iris, mtcars = mtcars) %>%
  names(.) 


