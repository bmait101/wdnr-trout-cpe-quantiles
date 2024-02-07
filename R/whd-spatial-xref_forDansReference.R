# Linking fish sites to WHD 24k Hydro Plus (reachIDs)
# Bryan Maitland
# 2021-08-10
# 2021-12-16

# ==== Overview =============================================================

# Key link to climate and stream data is reach_id (and hydro_id)

# WDNR Fish Management surveys should each have a Site ID and a SWIMS ID, 
# and SWIMS stations should be snapped onto WHD and have associated hydroids.

# BUT, there are tons of stations that were put in at confluences 
# so they snapped to multiple hydroids, or that were snapped 
# to a polygon instead of a flowline, etc 

# Objectives
# 1) site x HUC xrefs
# 2) sites x WHD xref

# TO DO 
# 1. 11 REACHIDs with 6s (16 sites; 51 surveys) so no WHD stream data
# 2. 10 REACHIDs with missing data (17 surveys)
# ** Generalize analysis for use with any list of sites with lat and long ******

# Prep =============

library(tidyverse)
library(here)
library(janitor)
library(wdnr.gis)
library(sf)
library(rgdal)
library(nngeo)


# Spatial  Data ================================================================

# WI polygon
sf_wi_poly <- wdnr.gis::wi_poly %>% st_transform(crs = 3071) # CRS of 24k WHD

# Check layers in 24kWHDPlus VA and Baselayer
st_layers(dsn = here("data","spatial","WDNR_HYDRO_24K_VA.gdb"))
st_layers(dsn = here("data","spatial","24k_Hydro_Copy_20210817.gdb"))

# Read in 24kWHDPlus VA flowlines (  NOTE: does not have WBICs )
lines_va <-
  st_read(dsn = here("data","spatial","WDNR_HYDRO_24K_VA.gdb"), 
          layer = "WD_HYDRO_VA_FLWLN_NTWRK_LN_24K") %>% 
  st_as_sf() %>%
  st_transform(crs = 3071) %>% 
  janitor::clean_names()
map(lines_va, ~sum(is.na(.)))

# Read 24k base flowlines to get WBICS and hydrotypes for hydroids 
lines <-
  rgdal::readOGR(
    dsn = here("data","spatial","24k_Hydro_Copy_20210817.gdb"),
    layer = "WD_HYDRO_FLOWLINE_LN_24K") %>%
  st_as_sf() %>%
  janitor::clean_names() 

lines <- lines %>%
  select(hydroid, hydrocode, hydrotype, river_sys_wbic, stream_order) %>%
  st_drop_geometry() %>%
  as_tibble()

## Link WHD to WHDPlus lines

# check matches
anti_join(lines_va %>% st_drop_geometry(), lines, by = "hydroid") %>% tally()
# links wbics to the VA lines
lines_va <- left_join(lines_va, lines, by = "hydroid")
# check for NAs
map(lines_va, ~sum(is.na(.)))


# Read classified trout stream lines
lines_classed <- 
  here("data","spatial","shapefiles","hydro","trout_water_ln.shp") %>% 
  st_read() %>% 
  st_transform(crs = 3071) 

# # read 24k VA catchements (HUC16s)
# sf_huc16 <-
#   st_read(dsn = here("data","spatial","WDNR_HYDRO_24K_VA.gdb"),
#           layer = "WD_HYDRO_VA_CATCHMENT_AR_24K") %>%
#   st_as_sf() %>%
#   st_transform(crs = 3071) %>%
#   clean_names()

# Read watershed layers
sf_huc8 <- st_read(here("data","spatial","shapefiles","hucs","huc8.shp")) %>% 
  clean_names() %>% st_transform(crs = 3071)
sf_huc10 <- st_read(here("data","spatial","shapefiles","hucs","huc10.shp")) %>% 
  clean_names() %>% st_transform(crs = 3071)
sf_huc12 <- st_read(here("data","spatial","shapefiles","hucs","huc12.shp")) %>% 
  clean_names() %>% st_transform(crs = 3071)
sf_ecoreg <- st_read(
  here("data","spatial","shapefiles","ecoregions","wi_eco_l3.shp")) %>%
  clean_names() %>% st_transform(crs = 3071)


# quick plots
# ggplot() + geom_sf(data = lines_classed) + 
#   ggspatial::annotation_scale()
# 
# ggplot() + geom_sf(data = sf_huc12) + 
#   ggspatial::annotation_scale()


# Trout site data ==============================================================

# load if needed
df_surveys <- readRDS(here("output", "data", "df_surveys.rds"))

# check it
# map(df_cpes, ~sum(is.na(.)))
length(unique(df_surveys$site.seq.no))
length(unique(df_surveys$swims.station.id))
# length(unique(df_cpes$site.seq.no))
# length(unique(df_cpes$swims.station.id))
length(unique(df_surveys$site.seq.no))
# length(unique(df_cpes$site.seq.no))

# Tibble of sites
df_sites <- df_surveys %>%
  distinct(site.seq.no, .keep_all = TRUE) %>% 
  select(site.seq.no, swims.station.id, wbic, latitude, longitude) %>%
  mutate(across(c(swims.station.id,site.seq.no), as.character)) 

# df_sites %>% write_rds(here("output","data","df_sites.rds"))

# Make spatial 
sf_sites <- df_sites %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = 3071)  # CRS of the 24k hydro lines


# 1. Site x WHD xref  ==========================================================


# 1a. merge sites with xwalk table =============================================
# xwalk table of all swims_station_ids in SWIMS and their hydro and reach IDS
# some SWIMS stations have multiple hydroids and reach ids
# - some with multiple 6s (on edge between two features)
# - some with 2s and 6s
# - some with multiples (site on a confluence)
# - some with NAs (station is not snapped)
# - Namekegon issue: while river is a lake with 6 reach id

# load xwalk
xwalk_swims_hydro <- read_rds(here("data","xref_swims_hydro.rds")) %>% 
  # remove hydro and reachs with a 6 (lakes); we only want stream data
  filter(!str_detect(reach_id, "^6")) %>%
  # if a station has more than two records after removing 6s, get rid of them
  # we use spatail joins to get these
  group_by(swims.station.id) %>% 
  filter(!n() > 1) %>% 
  ungroup()


# now we do an inner join to link all sites with a swims match in the xwalk
sites_x_swims <- inner_join(sf_sites, xwalk_swims_hydro, by="swims.station.id") 

# and an anti join to get the sites without a macth in the xwalk
sites_to_fix <- anti_join(sf_sites, xwalk_swims_hydro, by="swims.station.id") 
# 333

# check
nrow(sites_x_swims) + nrow(sites_to_fix)



# 1b. spatial analysis =========================================================

# This takes about 10-15 minutes for 333 sites (hence step #1)

# # identify nearest 3 WHDPlus flowlines to site and get their distances
# nn_trace <- st_nn(sites_to_fix, lines_va, k = 3, returnDist = TRUE)
# 
# # extract the nn flowlines indexes and distances for each
# nn_distances <- 
# bind_cols(index = unlist(nn_trace$nn), distance = unlist(nn_trace$dist))
# 
# # join sites to flowlines using the nearest neighbor
# nn_join <- st_join(sites_to_fix, lines_va, join = nngeo::st_nn, k = 3)
# 
# # bind the distances to the nn_join and check for wbic matches
# nn_join_full <-
#   bind_cols(nn_join, nn_distances) %>%
#   st_drop_geometry() %>%
#   as_tibble() %>% 
#   select(-confl2conflid, -traceid, -shape_length, -index) %>% 
#   mutate(across(where(is.integer), as.character)) %>%
#   mutate(wbic_match = river_sys_wbic == wbic)

# save intermediary
# saveRDS(nn_join_full, here("data","tmp","nn_join_full_20211215.rds"))
nn_join_full <- read_rds(here("data","tmp","nn_join_full_20211215.rds"))



# 1c. filter and clean up data =================================================

# if wbic matches, keep the hydro record closest to the point
sites_to_fix_matched <- 
  nn_join_full %>% 
  group_by(site.seq.no) %>% 
  mutate(rank = row_number(distance)) %>% 
  filter(rank==1 & wbic_match==TRUE) %>%
  select(site.seq.no, hydroid, reachid) 

# how many still not fixed?
anti_join(sites_to_fix, sites_to_fix_matched, by = "site.seq.no") %>% 
  st_drop_geometry() %>%
  count()
# 74

sites_to_fix_matched2 <- 
  nn_join_full %>% 
  # remove sites already done
  filter(!site.seq.no %in% sites_to_fix_matched$site.seq.no) %>% 
  group_by(site.seq.no) %>% 
  mutate(rank = row_number(distance)) %>% 
  filter(rank==2 & wbic_match==TRUE) %>%
  select(site.seq.no, hydroid, reachid) 

# how many still not fixed?
anti_join(sites_to_fix, bind_rows(sites_to_fix_matched,sites_to_fix_matched2),by="site.seq.no") %>% st_drop_geometry() %>% count()
# 39 on 2021-12-15
# 4

# the final 46 hydroids need manual checks; wbics are prob messed up - so, 
# take whichever is closest, and then go check it
manual_fixes <- 
  nn_join_full %>% 
  filter(!site.seq.no %in% sites_to_fix_matched$site.seq.no) %>% 
  filter(!site.seq.no %in% sites_to_fix_matched2$site.seq.no) %>% 
  group_by(site.seq.no) %>% 
  slice_min(distance) %>% 
  write_csv(here("output", "tmp","nn_manual_fixes2.csv"))
manual_fixes %>% print(n=Inf) 
# do manual checks #

# load corrected data
manual_fixed <- read_csv(here("output", "tmp","nn_manual_fixes_corrected.csv"))

# sites to remove
sites_to_remove <- manual_fixed %>% 
  filter(is.na(reachid_updated)) %>%
  pull(site.seq.no)

sites_to_fix_matched3 <- 
  manual_fixed %>% 
  filter(!site.seq.no %in% sites_to_remove) %>% 
  select(-notes) %>% 
  mutate(across(where(is.double), as.character)) %>%
  select(site.seq.no, hydroid, reachid=reachid_updated)

# bind up all the checked fixes
sites_to_fix_matched_final <- 
  bind_rows(
    sites_to_fix_matched, 
    sites_to_fix_matched2, 
    sites_to_fix_matched3
  )

# add hydroids to the list of sites of fix
sites_to_fix_cln <- 
  sites_to_fix %>% 
  st_drop_geometry() %>% 
  select(-wbic) %>% 
  inner_join(sites_to_fix_matched_final, by = "site.seq.no") %>% 
  as_tibble()

# 1d. Bind final table ========================

xwalk_sites_whd <- sites_x_swims %>%
  st_drop_geometry() %>% 
  select(-wbic) %>% 
  rename(hydroid=hydro_id, reachid=reach_id) %>% 
  bind_rows(sites_to_fix_cln) %>% 
  select(swims.station.id, site.seq.no, hydro_id=hydroid, reach_id=reachid) %>% 
  as_tibble() %>% 
  mutate(hydro_id = if_else(hydro_id=="200026528", "200026525", hydro_id)) %>% 
  distinct()  # takes care of 2 dups
xwalk_sites_whd

map(xwalk_sites_whd, ~sum(is.na(.)))


# 1e. Deal with 6s =========================

xwalk_sites_whd %>% filter(str_detect(reach_id, "^6")) %>% 
  distinct(reach_id, .keep_all=TRUE)
# 11 6s - these can be used in trends, but not in other analysis right now

# how many surreys?
sixes <- xwalk_sites_whd %>% filter(str_detect(reach_id, "^6")) %>% 
  pull(site.seq.no)
df_surveys %>% filter(site.seq.no %in% sixes) %>% as_tibble() 
# ~ 51 sites

# Fix for reaches ids 
xwalk_sites_whd <- xwalk_sites_whd %>% 
  mutate(reach_id = case_when(
    reach_id == "600012990" ~ "200181039",
    reach_id == "600046690" ~ "200107673",
    reach_id == "600055815" ~ "200076898", 
    reach_id == "600084294" ~ "200149549", 
    
    reach_id == "600091889" ~ "200194156", # Namekegon main stem; gave it lagre trib
    reach_id == "600018043" ~ "200171508",  # Just upstream on Trout Creek
    reach_id == "600013543" ~ "200212215",  # Just upstream on Beaver Brook
    
    reach_id == "600031894" ~ "200142791",  # Just upstream 
    reach_id == "600008762" ~ "200187067",  # Just upstream
    reach_id == "600089491" ~ "200036761",  # Just upstream 
    reach_id == "600018757" ~ "200170441",  # Just upstream
    TRUE ~ reach_id
  ))

xwalk_sites_whd %>% 
  filter(str_detect(reach_id, "^6")) %>% 
  distinct(reach_id, .keep_all=TRUE)



# # get manual codes for these
# df_sites %>% filter(site.seq.no %in% nas$site.seq.no) %>% 
#   as_tibble() %>% 
#   select(site.seq.no, latitude, longitude) %>% 
#   write_csv(here("output", "data","nn_manual_fixes_nas.csv"))
# # do manual checks #
# 
# # load corrected data
# manual_fixed <- read_csv(here("output", "data","nn_manual_fixes_nas.csv"))
# 
# # sites to remove
# sites_to_remove <- manual_fixed %>% 
#   filter(is.na(hydro_id)) %>%
#   pull(site.seq.no)
# 
# sites_to_fix_matched4 <- manual_fixed %>% 
#   filter(!site.seq.no %in% sites_to_remove) %>% 
#   select(-notes, -latitude, -longitude) %>% 
#   mutate(across(where(is.double), as.character))
# 
# xwalk_sites_whd <- xwalk_sites_whd %>% 
#   select(-swims.station.id) %>% 
#   bind_rows(sites_to_fix_matched4) %>% 
#   distinct()
# 
# xwalk_sites_whd

# 2. Site x Watershed xref =====================================================

# Find points within polygons
sites_in_hucs8 <- st_join(sf_sites, sf_huc8, join = st_within) %>% 
  st_drop_geometry() %>% 
  select(site.seq.no, huc8_code)
sites_in_hucs10 <- st_join(sf_sites, sf_huc10, join = st_within) %>% 
  st_drop_geometry() %>% 
  select(site.seq.no, huc10_code)
sites_in_hucs12 <- st_join(sf_sites, sf_huc12, join = st_within) %>% 
  st_drop_geometry() %>% 
  select(site.seq.no, huc12_code)
sites_in_ecoreg <- st_join(sf_sites, sf_ecoreg, join = st_within) %>% 
  st_drop_geometry() %>% 
  select(site.seq.no, eco_code = us_l3code, ecoregion = us_l3name)



# 3. Site x stream class xref  =================================================


# # identify nearest classes stream to site and get their distances
# nn_trace <- st_nn(sf_sites, lines_classed, k = 1, returnDist = TRUE)
# 
# # extract the nn flowlines indexes and distances for each
# nn_distances <- 
# bind_cols(index = unlist(nn_trace$nn), distance = unlist(nn_trace$dist))
# 
# # join sites to flowlines using the nearest neighbor
# nn_join <- st_join(sf_sites, lines_classed, join = nngeo::st_nn, k = 1)
# 
# # bind the distances to the nn_join and check for wbic matches
# nn_join_full_stmcls <-
#   bind_cols(nn_join, nn_distances) %>%
#   st_drop_geometry() %>%
#   as_tibble() %>% 
#   select(site.seq.no, TROUT_CLAS, distance) %>% 
#   mutate(across(where(is.integer), as.character)) 

# save intermediary
# saveRDS(nn_join_full_stmcls, here("output","tmp","nn_join_full_stmcls_20210922.rds"))
nn_join_full_stmcls <- 
  read_rds(here("output","tmp","nn_join_full_stmcls_20210922.rds")) %>% 
  rename(trout_class = TROUT_CLAS)

# tmp <- sf_sites %>% left_join(nn_join_full_stmcls, by = "site.seq.no")
# tmp %>% filter(is.na(TROUT_CLAS))
# tmp %>% filter(distance>=26) %>% arrange(distance) 

# plot it
# ggplot() + 
#   geom_sf(data = lines_classed, aes(color=TROUT_CLAS)) + 
#   geom_sf(data = tmp, aes(color=TROUT_CLAS), size=.5, alpha=0.5) + 
#   geom_sf(data = tmp %>% filter(distance>=26), color = "black", size=.7) 

# So its not perfect, but use 26 m to filter out bad spatial joins ()
# people need to snap their damned sites 

nn_join_full_stmcls_26m <- nn_join_full_stmcls %>% 
  filter(distance <= 26)%>% 
  select(-OFFICIAL_N, -distance)


# 4. Link keys to sites ========================================================

df_sites %>% as_tibble() 

df_sites_va <- df_sites %>% 
  as_tibble() %>% select(-swims.station.id) %>% 
  left_join(xwalk_sites_whd, by = "site.seq.no") %>% 
  left_join(sites_in_hucs8, by = "site.seq.no") %>%
  left_join(sites_in_hucs10, by = "site.seq.no") %>%
  left_join(sites_in_hucs12, by = "site.seq.no") %>%
  left_join(sites_in_ecoreg, by = "site.seq.no") %>% 
  left_join(nn_join_full_stmcls_26m, by = "site.seq.no")

# Replace huc codes for actual names using Paul's wdnr.gis package
df_sites_va <- df_sites_va %>%
  left_join(wdnr.gis::watershed_lookup %>% 
              filter(huc_level == "HUC_8") %>% 
              select(-huc_level),
            by = c("huc8_code"="huc_codes"))  %>% 
  rename(huc_names8 = huc_names) %>%
  left_join(wdnr.gis::watershed_lookup %>% 
              filter(huc_level == "HUC_10") %>%
              select(-huc_level),
            by = c("huc10_code"="huc_codes"))  %>% 
  rename(huc_names10 = huc_names) %>%
  left_join(wdnr.gis::watershed_lookup %>%
              filter(huc_level == "HUC_12") %>% 
              select(-huc_level),
            by = c("huc12_code"="huc_codes"))  %>% 
  rename(huc_names12 = huc_names) %>%
  mutate(across(c(huc_names8, huc_names10, huc_names12), as.factor))


map(df_sites_va, ~sum(is.na(.)))


# 4. Deal with  NAs ============================================================

df_sites_va %>% filter(is.na(reach_id))

# Fix 8 NA reaches ids 
df_sites_va <- df_sites_va %>% 
  mutate(reach_id = case_when(
    site.seq.no == "130032" ~ "200142691",
    site.seq.no == "34941584" ~ "200187018",
    site.seq.no == "81689389" ~ "200094875", 
    site.seq.no == "1606534" ~ "200211780", 
    site.seq.no == "62227241" ~ "200211455", 
    site.seq.no == "1576800" ~ "200136625", 
    site.seq.no == "1632786" ~ "200204063", 
    site.seq.no == "265060511" ~ "200199092", 
    TRUE ~ reach_id
  )) %>% 
  mutate(hydro_id = if_else(is.na(hydro_id), reach_id, hydro_id))

map(df_sites_va, ~sum(is.na(.)))


# 5. Final fixes ===============================================================

# Fix for reaches ids that need to be switched for good daymet data
df_sites_va <- df_sites_va %>% 
  mutate(reach_id = case_when(
    reach_id == "200048608" ~ "200048644",
    reach_id == "200148328" ~ "200148707",
    reach_id == "200173988" ~ "200174095", 
    reach_id == "200197203" ~ "200197197", 
    TRUE ~ reach_id
  ))



# Save key added site data =====================================================

# write_csv(df_sites_va, here("output","data","sites_list_va.csv"))
write_rds(df_sites_va, here("output","data","sites_list_va.rds"))


# # list of reach ids for Aaron Rusch for pulling WHDPlus data via SQL
# df_sites_va %>% distinct(reach_id) %>%
#   write_csv(here("output","tmp","trout_reachids.csv"))


# Clean up 
rm(sites_in_hucs8); rm(sites_in_hucs10); rm(sites_in_hucs12)
rm(sites_in_wtrmgnt); rm(sites_in_ecoreg)

rm(nn_trace)
rm(nn_join); rm(nn_distances); rm(nn_join_full)
rm(manual_fixed); rm(manual_fixes)
rm(sites_to_fix); rm(sites_to_fix_cln); rm(sites_to_fix_matched_final); 
rm(sites_to_fix_matched); rm(sites_to_fix_matched2); rm(sites_to_fix_matched3); rm(sites_to_fix_matched4)
rm(sites_to_fix)
rm(sites_to_fix_cln)
rm(sites_to_fix_matched_final)
rm(sites_x_swims)
rm(xwalk_sites_whd)
rm(xwalk_swims_hydro)
