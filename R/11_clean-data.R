# Clean and filter raw data from FMDB data pull
# Bryan Maitland
# 2020-09-10
# 2022-03-30

# libraries
library(tidyverse)
library(here)

# Load raw FMDB data ===========================================================

# load if needed: all proofed data on wadable stream that targeted trout

df_surveys_raw <- read_rds(here("data", "raw_fmdb_surveys_20210919.rds"))
df_efforts_raw <- read_rds(here("data", "raw_fmdb_efforts_20210919.rds"))
df_fish_raw <- read_rds(here("data", "raw_fmdb_fish_20210919.rds"))


# Check coordinate data ========================================================

library(sf)
wi_poly <- wdnr.gis::wi_poly %>% st_transform(crs = 3071)

# # plot sites
# tmp_pts <- df_surveys_raw %>%
#   distinct(site.seq.no, .keep_all = TRUE) %>% 
#   mutate(across(c(swims.station.id, site.seq.no), as.character)) %>% 
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
#   st_transform(crs = 3071)
# 
# 
# # identify sites inside and outside polygon 
# # lengths() on intersects list: If zero,point is not in any polygon
# inside <- lengths(st_intersects(tmp_pts, wi_poly)) > 0
# outside <- !inside
# 
# # plot
# plot(wi_poly$geom)
# plot(tmp_pts[inside,"geometry"], pch=19, col="blue", add=TRUE)
# plot(tmp_pts[outside,"geometry"], pch=19, col="red", add=TRUE)
#
# rm(inside); rm(outside); rm(tmp_pts); rm(wi_poly)

# manually fix the bad one - others should be okay - on edges
df_surveys <- df_surveys_raw %>% 
  mutate(longitude = if_else(site.seq.no==51873965,-90.59000,longitude))



# Filter efforts  ==============================================================

### Summer samples with effort data, 1st pass only

## New tibble, and add Julian date
df_efforts <- df_efforts_raw %>% 
  mutate(julian = lubridate::yday(sample.date)) %>% 
  as_tibble()

## Remove samples taken outside of the summer period
df_efforts <- filter(df_efforts, julian >= 166 & julian <= 258) 
# 4,063 removed

## Remove samples with distance NA or lower than 100m
df_efforts <- df_efforts %>% 
  filter(!is.na(distance.shocked)) %>% 
  filter(distance.shocked >= 0.0621)
# 1,267 removed

## Keep surveys types for which cpes can be calculated
df_efforts <- df_efforts %>% 
  filter(primary.survey.type %in% c("cpe","spe","dpe","mpe")) 
# 424 removed

## Keep only 1st pass data from multi-pass surveys
df_efforts %>% count(run.number)

df_efforts <- df_efforts %>% 
  replace_na(list(run.number = 1)) %>%
  mutate(run.number = if_else(run.number == "0", "1", run.number)) %>%
  filter(run.number %in% c(1)) 
# 303 removed


## Filter fish data on clean efforts ===========================================

df_surveys <- df_surveys %>% 
  semi_join(df_efforts, by = "survey.seq.no")

df_fish <- df_fish_raw %>% 
  semi_join(df_efforts, by = "visit.fish.seq.no")

# check fish data has effort and surveys data
# anti_join(df_fish, df_efforts, by = "visit.fish.seq.no") %>%
#   distinct(visit.fish.seq.no)
# anti_join(df_fish, df_surveys, by = "survey.seq.no") %>%
#   distinct(survey.seq.no)
# good

# QAQC fish data   =============================================================

## number of fish column -------------------------------------------------------

# update field values to NA for no fish captured
df_fish <- df_fish %>% 
  mutate(
    number.of.fish = if_else(
      species == "no_fish_captured", 0, as.double(number.of.fish))) %>% 
  mutate(yoy = if_else(species == "no_fish_captured", NA_character_, yoy))

# look at counts
filter(df_fish, number.of.fish == 0) %>% count(species)
# 298 species==nofish

# this tells us that there are no records where a species was targeted, 
# but not caught, and still added a zero for it. 
# only species that are observed are given a record - this is good

# so, where an effort caught no species at all, 
# we add zeros for each target species;
# and where a species was targeted but not 
# caught (and other species caught), we add zeros


# Zeros 1: efforts were no fish were caught-------------------------------------

# deal with efforts where no fish species was caught
# depending on the surveys target species, these records should be 
# given zeros for those species

# df_fish_nofish <- 
#   filter(df_fish, species == "no_fish_captured") 
# 
# # how many surveys?
# length(unique(df_fish_nofish$survey.seq.no))  # 288
# 
# # survey purpose
# df_fish_nofish %>%
#   group_by(primary.survey.purpose) %>%
#   tally() %>%
#   arrange(desc(n)) %>%
#   select('Primary Survey Purpose' = primary.survey.purpose, Count = n) %>%
#   print(n = Inf)
# 
# # table of target species
# df_fish_nofish %>%
#   group_by(target.species) %>%
#   tally() %>%
#   arrange(desc(n)) %>%
#   mutate(
#     target.species = stringr::str_replace_all(target.species, "_", " ")) %>%
#   select('Target species' = target.species, Count = n)
# 
# # map locations of surveys where no fish were captured
# df_fish_nofish %>%
#   distinct(survey.seq.no, .keep_all = TRUE) %>%
#   left_join(df_surveys, by = "survey.seq.no") %>%
#   sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
#   ggplot() +
#   geom_sf(data = wdnr.gis::wi_poly, 
#           color = "black", fill = "gray93", alpha = .7) +
#   geom_sf(aes(fill = target.species), 
#           shape = 21, size = 2, color = "black", alpha = 0.75)  +
#   scale_fill_viridis_d(
#     option = "D",
#     labels = c("All species (n=161)","Brook Trout (n=17)",
#                "Brown Trout (n=4)","Gamefish (n=113)","All trout (n=1)")) +
#   labs(x="Longitude (°W)", y="Latitude (°N)", fill="Survey target species") +
#   coord_sf(expand = FALSE) +
#   theme_minimal()
# 
# # save to file
# path <- here::here("plots","explore","trout-surveys-nofishcaptured")
# ggsave(glue::glue("{path}.pdf"), width = 7, height = 4, device = cairo_pdf)
# pdftools::pdf_convert(pdf = glue::glue("{path}.pdf"),
#                       filenames = glue::glue("{path}.png"),
#                       format = "png", dpi = 300)
# 
# 
# 
# # list the two visits that do not id what species was targeted for removal
# ukn.tagrets <- 
#   filter(df_fish_nofish, target.species=="trouts_unsp") %>% 
#   pull(survey.seq.no)
# 
# # make df of zeros by converting no fish captured records into records for 
# # species that were targeted 
# 
# df_fish_nofish_0s <-
#   bind_rows(
#     # all species and game fish
#     df_fish %>%
#       filter(target.species %in% c("all_species","gamefish_species"), 
#              species=="no_fish_captured") %>%
#       mutate(species="brook_trout", number.of.fish = 0),
#     df_fish %>%
#       filter(target.species %in% c("all_species","gamefish_species"), 
#              species=="no_fish_captured") %>%
#       mutate(species = "brown_trout", number.of.fish = 0),
#     # brook trout targets
#     df_fish %>%
#       filter(target.species %in% c("brook_trout"), 
#              species=="no_fish_captured") %>%
#       mutate(species = "brook_trout", number.of.fish = 0),
#     # brown trout targets
#     df_fish %>%
#       filter(target.species %in% c("brown_trout"), 
#              species=="no_fish_captured") %>%
#       mutate(species = "brown_trout", number.of.fish = 0)
#     )
# 
# # # update the data frames
# df_fish <-
#   df_fish %>%
#   filter(!survey.seq.no %in% ukn.tagrets) %>%
#   filter(!species == "no_fish_captured") %>%
#   bind_rows(df_fish_nofish_0s)
# df_surveys <- filter(df_surveys, !survey.seq.no %in% ukn.tagrets)
# df_efforts <- filter(df_efforts, !survey.seq.no %in% ukn.tagrets)
# 
# # check fish data has effort and surveys data
# anti_join(df_fish, df_efforts, by = "visit.fish.seq.no") %>% 
# distinct(visit.fish.seq.no)
# anti_join(df_fish, df_surveys, by = "survey.seq.no") %>% 
# distinct(survey.seq.no)


# Zeros 2: targeted a species, not caught, and no record of zero ---------------

# targeted brookies, didnt catch no fish or brookies (so caught bnt)
# df_trout %>%
#   filter(target.species %in% c("brook_trout")) %>% 
#   filter(!species %in% c("no_fish_captured","brook_trout")) %>%
#   count(species)
# 
# df_trout %>%
#   filter(target.species %in% c("brown_trout")) %>% 
#   filter(!species %in% c("no_fish_captured","brown_trout")) %>%
#   count(species)

# Subsets trout data ===========================================================

# check species levels
levels(as.factor(df_fish$species))

# check species with trout in name
df_fish %>%
  distinct(species) %>%
  filter(str_detect(species, "trout"))

# subset fish data for brook trout and brown trout records (present or N.D.)
df_trout <- df_fish %>%  
  filter(species %in% c("brook_trout","brown_trout"))


# keep all surveys with a match in the trout data ==============================

# (i.e., surveys where trout were caught)
df_surveys <- semi_join(df_surveys, df_trout, by = "survey.seq.no")
df_efforts <- semi_join(df_efforts, df_trout, by = "visit.fish.seq.no")
df_fish <- semi_join(df_fish, df_trout, by = "visit.fish.seq.no")


## Expand trout counts =========================================================

df_trout <- df_trout %>% 
  wdnr.fmdb::expand_counts() %>% 
  wdnr.fmdb::length_bin_to_length()

# convert lengths to mm
df_trout <- df_trout %>% 
  mutate(length_mm = length * 25.4)

# Save =========================================================================

saveRDS(df_trout, here("output", "data", "df_trout.rds"))
saveRDS(df_fish, here("output", "data", "df_trout_all_fish.rds"))
saveRDS(df_efforts, here("output", "data", "df_efforts.rds"))
saveRDS(df_surveys, here("output", "data", "df_surveys.rds"))

