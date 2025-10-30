
# SNAP and Shutdown
# Sources:
  # census population estimates (2024)
  # USDA SNAP beneficiaries by county
  # BEA income by source

# remove dependencies
rm(list = ls())

# load packages
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)
library(ggplot2)
library(readxl)
library(tidycensus)

project_path = "PASTE-PATH-TO-GITHUB-REPO-HERE"
project_path = "/Users/sarah/Documents/GitHub/SNAP-shutdown"
data_path = file.path(project_path, "data")
output_path = file.path(project_path, "output")

############################################################
# Read in Transfers Data, and fix county codes for mapping #
############################################################

# adjust BEA GeoFIPS combined for statistical purposes to approximate geographies
# for graphing purposes.

transfers = read_excel(file.path(data_path,"transfers_dataset_counties_master.xlsx")) %>%
  mutate(GeoFIPS = case_when(
    GeoFIPS == "46102" ~ "46113",
    GeoFIPS == "51958" ~ "51199",
    GeoFIPS == "51955" ~ "51195",
    GeoFIPS == "51953" ~ "51191",
    GeoFIPS == "51951" ~ "51177",
    GeoFIPS == "51949" ~ "51175",
    GeoFIPS == "51947" ~ "51165",
    GeoFIPS == "51945" ~ "51163",
    GeoFIPS == "51944" ~ "51161",
    GeoFIPS == "51942" ~ "51153",
    GeoFIPS == "51941" ~ "51149",
    GeoFIPS == "51939" ~ "51143",
    GeoFIPS == "51933" ~ "51121",
    GeoFIPS == "51931" ~ "51095",
    GeoFIPS == "51929" ~ "51089",
    GeoFIPS == "51923" ~ "51081",
    GeoFIPS == "51921" ~ "51069",
    GeoFIPS == "51919" ~ "51059",
    GeoFIPS == "51918" ~ "51053",
    GeoFIPS == "51913" ~ "51035",
    GeoFIPS == "51911" ~ "51031",
    GeoFIPS == "51907" ~ "51015",
    GeoFIPS == "51903" ~ "51005",
    GeoFIPS == "51901" ~ "51003",
    GeoFIPS == "55901" ~ "55115",
    TRUE ~ GeoFIPS))

# select 2022 data (most recent)

transfers_at_risk = transfers %>% filter(year==2022) %>%
  mutate(at_risk_SNAP_WIC_TANF_ACA = transfers_snap_pce + transfers_income_maintenance_other_pce + transfers_other_pce, 
         at_risk_SNAP_WIC_TANF =  transfers_snap_pce + transfers_income_maintenance_other_pce,
         
         at_risk_SNAP_WIC_TANF_ACA_share = 100*at_risk_SNAP_WIC_TANF_ACA/personal_income_pce,
         at_risk_SNAP_WIC_TANF_share = 100*at_risk_SNAP_WIC_TANF/personal_income_pce,
         SNAP_inc_share = 100*transfers_snap_pce/personal_income_pce) %>%
  
  select(GeoFIPS, GeoName, 
         at_risk_SNAP_WIC_TANF_ACA, at_risk_SNAP_WIC_TANF,
         at_risk_SNAP_WIC_TANF_ACA_share, at_risk_SNAP_WIC_TANF_share, SNAP_inc_share,
         transfers_all_pce, transfers_snap_pce, personal_income_pce,
         population_2022 = total_population)

################################
# Read in population estimates #
################################
# from Census

population = read.csv(file.path(data_path, "cc-est2024-alldata.csv")) %>%
  mutate(county = paste0(str_pad(STATE, side = "left", width = 2, pad = "0"),
                         str_pad(COUNTY, side = "left", width = 3, pad = "0"))) %>%
  filter(AGEGRP == 0) %>% filter(YEAR == 6) %>% select(county, population_2024 = TOT_POP) %>%
  mutate(county = ifelse(county == "46102", "46113",county)) # SD county name change

# pull in household counts by county, ACS 2019-2023 5-year averages 
households_2023 = get_acs(
  geography = "county",
  variables = "B11001_001",
  year = 2023,
  survey = "acs5"
) %>% select(GEOID, households = estimate)


############################
# Read in SNAP income data #
############################
# From USDA - combine most recent 2 periods to increase coverage


snap_jan_2025 = read.xlsx(file.path(data_path, "snap-zip-fns388a-7/JAN 2025.xlsx"),startRow=4) %>%
  mutate(county = substr(`Substate/Region`, 1,5)) %>% 
  mutate(USDA_pop_on_snap = as.numeric(`SNAP.All.Persons.Public.Assistance.Participation`) + as.numeric(`SNAP.All.Persons.Non-Public.Assistance.Participation`)) %>%
  select(county, `Substate/Region`, USDA_pop_on_snap) %>% 
  ungroup() %>% group_by(county) %>% summarise(USDA_pop_on_snap = sum(as.numeric(USDA_pop_on_snap)))

snap_jul_2024 = read.xlsx(file.path(data_path,"snap-zip-fns388a-7/JUL 2024.xlsx"),startRow=4) %>%
  mutate(county = substr(`Substate/Region`, 1,5)) %>% 
  mutate(USDA_pop_on_snap = as.numeric(`SNAP.All.Persons.Public.Assistance.Participation`) + as.numeric(`SNAP.All.Persons.Non-Public.Assistance.Participation`)) %>%
  select(county, `Substate/Region`, USDA_pop_on_snap) %>% 
  ungroup() %>% group_by(county) %>% summarise(USDA_pop_on_snap = sum(as.numeric(USDA_pop_on_snap)))

# select only july 2024 data not available in january 2025 
    snap_by_county_fips = snap_jan_2025$county
    snap_jul_2024 = snap_jul_2024 %>% filter(!county %in% snap_by_county_fips)

snap_by_county = bind_rows(snap_jan_2025, snap_jul_2024) %>%
  left_join(population, by = "county") %>%
  mutate(USDA_pop_share_snap = as.numeric(USDA_pop_on_snap)/as.numeric(population_2024)*100) %>% select(-population_2024)
rm(snap_jan_2025, snap_jul_2024)

# share of households on SNAP, from ACS 2019-2023 5-year
snap_acs = get_acs(
  geography = "county",
  variables = "B22002_002",
  year = 2023,
  survey = "acs5"
) %>%
  select(GEOID, ACS_hh_on_snap = estimate) %>% 
  left_join(households_2023, by = "GEOID") %>% mutate(ACS_hh_share_on_snap = ACS_hh_on_snap/households*100) %>%
  mutate(GEOID = ifelse(GEOID == "46102", "46113",GEOID))


############################
# Read in election results #
############################

# note- these data are proprietary and not publically available.

election_data = read_excel(file.path(data_path, "Election Master Dataset 2024_prelimV10.xlsx")) %>%
  mutate(`2024 margin` = 100*(`2024 Trump votes` - `2024 Harris votes`)/(`2024 Trump votes` + `2024 Harris votes`)) %>%
  select(`County ID`, `2024 margin`,
         `2024 Trump votes`, `2024 total votes`, `2024 Winner`) %>% 
  mutate(trump_share = 100*`2024 Trump votes`/`2024 total votes`,
         `County ID` = str_pad(`County ID`, side = "left", pad = "0", width = 5)) %>%
  mutate(`County ID` = ifelse(`County ID` == "46102", "46113", `County ID`))

#############################
# Pull in county typologies #
#############################

persistant_poverty = read_excel(file.path(data_path, "Persistent Poverty Dataset.xlsx"),
                                        sheet = "All counties") %>%
  mutate(FIPS = stringr::str_pad(FIPS, side = "left", pad = "0", width = 5)) %>%
  mutate(FIPS = ifelse(FIPS == "46102", "46113",FIPS)) %>%
  select(FIPS, Typology) %>%
  mutate(Typology = case_when(
    Typology =="Urban-high white or AAPI share" ~ "Metropolitan disproportionately white",
    Typology =="Urban-high Black share" ~ "Metropolitan disproportionately black",
    Typology =="Urban-high Hispanic share" ~ "Metropolitan disproportionately hispanic",
    TRUE ~ Typology
  ))

####################
# Combine all data #
####################

snap_master = transfers_at_risk %>% 
  full_join(snap_by_county, by = c("GeoFIPS" = "county")) %>%
  full_join(snap_acs, by = c("GeoFIPS" = "GEOID")) %>%
  full_join(persistant_poverty, by = c("GeoFIPS" = "FIPS")) %>%
#  full_join(election_data, by = c("GeoFIPS" = "County ID")) %>%
  # add in population estimates, full coverage
  left_join(population, by = c("GeoFIPS" = "county")) %>%
  mutate(state = substr(GeoFIPS, 1, 2)) %>%
  filter(!state %in% c("00", "03", "07", "14", "43", "52", "57", "58", "59", "60", "61", "62", "63", "64", "65", "66", "67",
                         "68", "69", "70", "71", "73", "72", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83", "84", "85",
                         "86", "87", "88", "89", "90", "91", "92", "93", "94", "95", "96", "97", "98", "99", "U.")) %>%
  filter(!is.na(state)) %>%
  filter(!is.na(GeoName))
  

# Note: 
    # Connecticut boundaries for 2024 population are not handled.
    # SNAP data from USDA is not available for all geographies.

################################################################################
# Tables/Figures 

#######################
# "Most reliant" table - top 15
reliant = snap_master %>% arrange(desc(SNAP_inc_share)) %>%
  mutate(transfer_share = transfers_all_pce/personal_income_pce*100)%>%
  select(GeoFIPS, GeoName, Typology, SNAP_inc_share, transfers_snap_pce, 
         transfer_share, ACS_hh_on_snap, population_2024) %>% 
  slice_head(n = 15)

################
# Topline table

# USDA based data
table = snap_master %>%
  mutate(Trump_won = ifelse(`2024 Winner` == "Trump", 1, 0)) %>%
  ungroup() %>% group_by(Typology) %>%
  summarise(total_population = sum(population_2024, na.rm = TRUE),
            on_snap = sum(USDA_pop_share_snap, na.rm = TRUE),
            SNAP_income = sum(transfers_snap_pce, na.rm = TRUE),
            total_income = sum(personal_income_pce, na.rm = TRUE),
            total_transfers = sum(transfers_all_pce, na.rm = TRUE),
            trump_won = sum(Trump_won, na.rm = TRUE),
            vote_margin = {
              keep <- !is.na(`2024 margin`) & !is.na(population_2024)
              if (sum(keep) == 0) NA_real_
              else weighted.mean(`2024 margin`[keep], population_2024[keep])
            },
            counties = n()) %>% na.omit() %>%
  mutate(share_on_snap =on_snap/total_population*100,
         snap_share_of_inc = SNAP_income/total_income*100,
         transfer_share = total_transfers/total_income*100,
         share_trump_won = trump_won/counties*100)

# add in true population totals
table_population = persistant_poverty %>% left_join(population, by = c("FIPS" = "county")) %>%
  ungroup() %>% group_by(Typology) %>% summarise(total_population = sum(population_2024, na.rm = TRUE))

table = table %>% select(-total_population) %>% left_join(table_population) %>% select(Typology, total_population, everything())

# ACS based data
table2 = snap_master %>%
  mutate(Trump_won = ifelse(`2024 Winner` == "Trump", 1, 0)) %>%
  ungroup() %>% group_by(Typology) %>%
  summarise(total_hh = sum(households, na.rm = TRUE),
            total_population = sum(population_2024, na.rm = TRUE),
            hh_on_snap = sum(ACS_hh_on_snap, na.rm = TRUE),
            SNAP_income = sum(transfers_snap_pce, na.rm = TRUE),
            total_income = sum(personal_income_pce, na.rm = TRUE),
            total_transfers = sum(transfers_all_pce, na.rm = TRUE),
            trump_won = sum(Trump_won, na.rm = TRUE),
            vote_margin = {
              keep <- !is.na(`2024 margin`) & !is.na(population_2024)
              if (sum(keep) == 0) NA_real_
              else weighted.mean(`2024 margin`[keep], population_2024[keep])
            },
            counties = n()) %>% na.omit() %>%
  mutate(share_hh_on_snap =hh_on_snap/total_hh*100,
         snap_share_of_inc = SNAP_income/total_income*100,
         transfer_share = total_transfers/total_income*100,
         share_trump_won = trump_won/counties*100)


################################################################################
# Export 

# Constructed dataset
write.csv(snap_master, file.path(output_path, "all_cleaned_data_snap.csv"))

# tables
write.csv(table, file.path(output_path, "tables","topline_table_USDA.csv"))
write.csv(table2, file.path(output_path, "tables","topline_table_ACS.csv"))
write.csv(reliant, file.path(output_path, "tables","top15_reliant_inc.csv"))
