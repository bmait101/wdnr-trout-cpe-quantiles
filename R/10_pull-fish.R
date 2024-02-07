# download raw data from FMDB
# Bryan Maitland
# 2020-09-10
# 2021-08-05

# Overview: download all proofed electrofishing data on 
# wadable stream that targeted trout from FMDB

# NOTE: fmdb can only handle up to 1000 requests at a time, so must loop/map

library(tidyverse)
library(here)
library(wdnr.fmdb)
# set_fmdb_credentials(username = "maitlb")



# Set parameters for data pull

yrs <- 1994:2021

waterbody_types <- c("wadable_stream", "non_wadable_stream", "stream")

gear_types <- c("stream_shocker","backpack_shocker")

targ.survs <- c(
  "data_entry_complete_and_proofed",
  "historical_data_complete_and_proofed",
  "historical_data_entry_complete",
  "historical_data_load_status_unknown"
  )

targ.spp <- c(
  "all_species","gamefish_species","gamefish_panfish",
  "trout_spp","brown_trout","brook_trout","rainbow_trout"
  )


# Pull surveys and efforts =====================================================

# Pull survey data
df_surveys_raw <- yrs %>%
  map_df(~get_fmdb_surveys(
    year = .,
    waterbody_type = waterbody_types
    ))

# Pull effort data above surveys, but only pull electrofishing efforts
df_efforts_raw <- yrs %>%
  map_df(~get_fmdb_efforts(
    year = .,
    waterbody_type = waterbody_types,
    gear = gear_types
    ))


# CHECK: There should be fewer in the efforts tibble b/c net data not pulled:
length(unique(df_surveys_raw$survey.seq.no))  
length(unique(df_efforts_raw$survey.seq.no))  


# Filter -----------------------------------------------------------------------

# filter surveys for unique efforts and target species
df_surveys_raw <- df_surveys_raw %>% 
  semi_join(df_efforts_raw, by = "survey.seq.no") %>% 
  filter(survey.status %in% targ.survs)


# filter efforts for proofed shocking that targeted trout
df_efforts_raw <- df_efforts_raw %>% 
  filter(target.species %in% targ.spp | secondary.target.species %in% targ.spp) %>% 
  filter(site.seq.no != 315) %>% 
  filter(survey.seq.no %in% df_surveys_raw$survey.seq.no)


# save
saveRDS(df_surveys_raw, here("data", "raw_fmdb_surveys_20220330.rds"))
saveRDS(df_efforts_raw, here("data", "raw_fmdb_efforts_20210919.rds"))

# df_surveys_raw <- read_rds(here("data", "raw_fmdb_surveys_20220330.rds"))
# df_efforts_raw <- read_rds(here("data", "raw_fmdb_efforts_20210919.rds"))

# Pull fish raw data ===========================================================

# Pull raw fish data 1000 efforts at a time (max for FMDB)
vs <- unique(df_efforts_raw$visit.fish.seq.no)
chunks <- split(vs, ceiling(seq_along(vs)/1000))

df_fish_raw <-chunks %>%
  map_df(~get_fmdb_fishraw(visit_seq = .))

# Check records
# length_deleted_rows
# length_warning_rows

# save raw fish data pull
saveRDS(df_fish_raw, here("data", "raw_fmdb_fish_20210919.rds"))

# df_fish_raw <- read_rds(here("data", "raw_fmdb_fish_20210919.rds"))


# END ==========================================================================