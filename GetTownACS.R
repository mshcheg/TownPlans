library(tidycensus)
library(tigris)
library(sf)
library(tidyverse)

View(load_variables(2022, "acs5"))

VT_poverty <- get_acs(state = "VT", geography = "county subdivision", year = 2022, survey = "acs5", 
                         output = "wide", geometry = T, variables = c(
                           total_pov = "B14006_002",
                           total_pop_pov = "B14006_001"))

VT_population <- get_acs(state = "VT", geography = "county subdivision", year = 2022, survey = "acs5", 
                      output = "wide", geometry = F, variables = c(
                        total_pop = "B01003_001",
                        NH_white = "B03002_003",
                        NH_black = "B03002_004",
                        hisp = "B03002_012",
                        NH_asian = "B03002_006",
                        NH_AIAN = "B03002_005",
                        NH_NHOPI = "B03002_007"))

VT_Housing <- get_acs(state = "VT", geography = "county subdivision", year = 2022, survey = "acs5", 
                             output = "wide", geometry = F, variables = c(
                               total_units = "B25001_001",
                               total_occupied = "B25003_001",
                               renter = "B25003_003",
                               owner = "B25003_002",
                               mortgage = "B25087_002",
                               no_mortgage = "B25087_020",
                               total_vacant = "B25004_001",
                               vacant_seasonal = "B25004_006",
                               white_rent = "B25003A_003",
                               white_own = "B25003A_002",
                               black_rent = "B25003B_003",
                               black_own = "B25003B_002",
                               AIAN_rent = "B25003C_003",
                               AIAN_own = "B25003C_002",
                               Asian_rent = "B25003D_003",
                               Asian_own = "B25003D_002",
                               NHOPI_rent = "B25003E_003",
                               NHOPI_own = "B25003E_002",
                               hisp_rent = "B25003I_003",
                               hisp_own = "B25003I_002",
                               owner_1D = "B25032_003",
                               owner_1A = "B25032_004",
                               owner_2 = "B25032_005",
                               owner_3_4 = "B25032_006",
                               owner_5_9 = "B25032_007",
                               owner_10_19 = "B25032_008",
                               owner_20_49 = "B25032_009",
                               owner_50_plus = "B25032_010",
                               owner_mobile = "B25032_011",
                               renter_1D = "B25032_014",
                               renter_1A = "B25032_015",
                               renter_2 = "B25032_016",
                               renter_3_4 = "B25032_017",
                               renter_5_9 = "B25032_018",
                               renter_10_19 = "B25032_019",
                               renter_20_49 = "B25032_020",
                               renter_50_plus = "B25032_021",
                               renter_mobile = "B25032_022"))

derived_MOE <- function(MOEX, MOEY, Y, P){
  A <- 1/Y
  B <- MOEX^2
  C <- P^2 * MOEY^2
  D <- abs(B - C)
  A * sqrt(D)
}

VT_poverty %>%
  st_drop_geometry() %>%
  mutate(PovR_E = 100*total_povE/total_pop_povE) %>%
  mutate(PovR_M = derived_MOE(total_povM, total_pop_povM, total_pop_povE, PovR_E)) %>%
  mutate(PovR_EL = PovR_E - PovR_M,
         PovR_EU = PovR_E + PovR_M) %>%
  select(NAME, PovR_EL, PovR_E, PovR_EU) %>%
  View

VT_Housing %>%
  mutate(perRent = 100*renterE/total_unitsE,
         perOwn = 100*ownerE/total_unitsE,
         vacant = 100*(total_vacantE - vacant_seasonalE)/total_unitsE,
         seasonal = 100*vacant_seasonalE/total_unitsE) %>%
  select(NAME, perRent, perOwn, seasonal, vacant, total_unitsE, total_occupiedE, total_vacantE) %>%
  View

VT_population %>%
  mutate(NH_white = 100*NH_whiteE/total_popE) %>%
  select(NAME, total_popE, NH_white) %>%
  View
