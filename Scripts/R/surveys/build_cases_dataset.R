### Build households cases dataset and enumerators dataset, along with attached preloads
### AS 🐚🫧🪼🪸 + ADM
### 11.01.2026 (Last update)

rm(list = ls())
pacman::p_load(dplyr, tidyr, stringr, purrr, sf, stringi)
source("./clients/box_client.R")
source("./clients/scto_client.R")
source("./utils/misc_helpers.R")
source("./utils/anonymize_coordinates.R")
source("./utils/misc_helpers.R")

# helpers ======================================================================

# add the necessary columns for a SurveyCTO cases dataset
.make_cases_dataset <- function(dataframe, id_field) {
  dataframe %>%
    mutate(
      id = {{ id_field }},
      caseid = {{ id_field }},
      label = {{ id_field }},
      formids = "",
      users = "",
      roles = ""
    )
}

# data =========================================================================

# source -----------------------------------------------------------------------

.individuals_source <- 2022843328758 # /Data_Management/Cases management/participation records/individuals_participation.csv
.individuals_childs_match_source <- 1681667836665 # /Data_Management/Data/_Partner_CleanData/Parasitological_Data/Child_Matches_Dataset.xlsx
.individuals_childs_epls_source <- 2026466790865 # /Data_Management/Data/_Partner_CleanData/Parasitological_Data/Potential_EPLS_Kids.csv
.households_source <- 2022832396067 # /Data_Management/Cases management/participation records/households_participation.csv
.households_coordinates_source <- 2008125217426 # /Data_Management/Data/Location_Data/Households/households.gpkg
.households_attrition_source <- 2098001310875
.households_stratum_source <- 2098210206843 # /Surveys/Midline2/retained_and_replacement_households.csv
.villages_source <- 2031064987066 # /Data_Management/Data/Location_Data/Villages/villages_polygons.gpkg
.villages_epls_box <- 1484930740372 # /Data_Management/Data/_Partner_CleanData/Parasitological_Data/EPLS_Data/EPLS Village List.xlsx
.villages_ucad_box <- 1566668711413 #/Data_Management/Data/_Partner_CleanData/Parasitological_Data/UCAD_Data/UCAD_Village_list.xlsx
.schools_source <- 1805928472695 #  /Data_Management/Data/_CRDES_CleanData/Midline/Identified/DISES_Complete_Midline_SchoolPrincipal.dta
.schools_coordinates_source <- 2031058959833 # /Data_Management/Data/Location_Data/Schools/schools.gpkg
.replacements_source <- 2097947565050 # /Surveys/Midline2/replacement roster/replacement_households_full.csv



# output -----------------------------------------------------------------------

# folders
.datasets_folder <- 346240578431

# files
.households_file <- "households.csv"
.villages_file  <- "villages.csv"
.individuals_file <- "individuals.csv"
.enumerators_file <- "enumerators_crdes.csv"
.schools_file <- "schools.csv"
.replacements_file <- "replacements.csv"


# data in memory ---------------------------------------------------------------

#villages
villages_coordinates_box <- .box_read_sf(.villages_source)
villages_epls_box <- box_read(.villages_epls_box)
villages_ucad_box <- box_read(.villages_ucad_box)

#households
households_box <- box_read(.households_source)
households_coordinates_box <- .box_read_sf(.households_coordinates_source)
households_attrition_box <- box_read(.households_attrition_source)
households_stratum_box <- box_read(.households_stratum_source)

# individuals
individuals_box <- box_read(.individuals_source)
individuals_childs_match_box <- box_read(.individuals_childs_match_source)
individuals_childs_epls_box <- box_read(.individuals_childs_epls_source)

# Schools
schools_box <- box_read(.schools_source)
schools_coordinates_box <- .box_read_sf(.schools_coordinates_source)

# replacements 
replacements_box <- box_read(.replacements_source)

# datasets =====================================================================

# villages ---------------------------------------------------------------------

# EPLS villages list
villages_epls <- villages_epls_box %>%
  select(hhid_village = 'Village code') %>%
  mutate(parasitological_team_ea = "EPLS")

# UCAD villages list
villages_ucad <- villages_ucad_box %>%
  select(hhid_village = 'Village code') %>%
  mutate(parasitological_team_ea = "UCAD")

# format coordinates
villages_cases <- villages_coordinates_box %>%
  st_centroid(villages_coordinates_box) %>%
  st_transform(crs = 4326) %>%
  mutate(across(c(hhid_village, treatment_arm), ~ toupper(as.character(.x))))%>%
  mutate(longitude = st_coordinates(.)[, "X"], latitude = st_coordinates(.)[, "Y"]) %>%
  st_drop_geometry() %>%
  .make_cases_dataset(hhid_village)%>%
  left_join(villages_epls, by = "hhid_village")%>%
  left_join(villages_ucad, by = "hhid_village")%>%
  mutate(
    parasitological_team_ea = coalesce(
      parasitological_team_ea.x,
      parasitological_team_ea.y
    )
  ) %>%
  select(-parasitological_team_ea.x, -parasitological_team_ea.y)



# households -------------------------------------------------------------------

# format households coordinates
household_coordinates <- households_coordinates_box %>%
  rename(geometry = geom) %>%
  mutate(longitude = st_coordinates(.)[, "X"], latitude = st_coordinates(.)[, "Y"]) %>%
  st_drop_geometry() 

# generate cases dataset structure
households_cases <- households_box %>%
  .make_cases_dataset(hhid) %>%
  left_join(households_attrition_box, by="hhid")%>%
  left_join(households_stratum_box  %>% select(hhid, stratum_code), by="hhid") %>%
  filter(is.na(attrition) | attrition != 1) %>%
  select(-attrition) %>%
  left_join(villages_cases  %>% select(hhid_village, village_name), by="hhid_village") %>%
  left_join(household_coordinates, by="hhid")  %>%
  mutate(
    hh_ethnicity_label = recode(hh_ethnicity,
                                "1" = "Wolof",
                                "2" = "Sérer",
                                "3" = "Peulh",
                                "4" = "Diola",
                                "5" = "Moor",
                                "7" = "Lébou",
                                "8" = "Soniké",
                                "99" = "Inconnu"),
  )

# generate an anonymized version of the data
# set.seed(666)
# households_cases_randomized <- households_cases %>%
#   mutate(
#     hh_head_name_complet = sample(hh_head_name_complet),
#     hh_phone = sample(hh_phone),
#     trained_hh = sample(trained_hh),
#     n_still_member = sample(n_still_member),
#   ) %>%
#   select(-n_individuals, -n_not_anymore_member)  

# individuals ------------------------------------------------------------------


individuals_childs_match <- individuals_childs_match_box %>%
  select(individ='Individual ID (CRDES)', match_score = 'Match Score', parasitological_id = 'EPLS or UCAD ID', parasitological_team = 'EPLS (1) or UCAD (2)')

individuals_childs_epls <- individuals_childs_epls_box %>%
  select(individ) %>%
  mutate(match_score = "0.01")%>%
  mutate(match_score = as.numeric(match_score))

individuals_childs <- bind_rows(individuals_childs_match, individuals_childs_epls)


# generate cases dataset structure
individuals_cases <- individuals_box %>%
  filter(still_member == 1) %>%
  select(hhid, individ, indiv_index, hh_full_name_calc, hh_gender, hh_age, hh_grade = hh_35, hh_relation_with, hh_head_name_complet, hh_phone)%>%
  mutate(hhid_village = str_sub(hhid, 1, 4)) %>%
  left_join(villages_cases  %>% select(hhid_village, village_name, parasitological_team_ea), by="hhid_village") %>%
  .make_cases_dataset(individ) %>%
  left_join(individuals_childs, by = "individ") %>%
  mutate(
    hh_gender_label = recode(hh_gender,
                             "1" = "Homme",
                             "2" = "Femme"),
    hh_relation_with_label = recode(as.character(hh_relation_with),
                                    "1"  = "Chef de famille",
                                    "2"  = "Conjoint du chef de famille",
                                    "3"  = "Fils/fille du chef de famille",
                                    "4"  = "Conjoint du fils/fille du chef de famille",
                                    "5"  = "Petit-fils/petite-fille du chef de famille",
                                    "6"  = "Père/Mère du chef de famille",
                                    "7"  = "Père/Mère du conjoint du chef de famille",
                                    "8"  = "Frère/sœur du chef de famille",
                                    "9"  = "Frère/sœur du conjoint du chef de famille",
                                    "10" = "Enfant adopté",
                                    "11" = "Aide ménagère",
                                    "12" = "Autre personne apparentée au chef de famille",
                                    "13" = "Autre personne non-apparentée au chef de famille",
                                    "14" = "Neveu/Nièce du chef de famille"
    )
  ) %>%
  mutate( 
    desc = paste0(
      hh_full_name_calc, ", ",
      hh_gender_label, ", ",
      hh_age, " ans, ",
      hh_relation_with_label
    )
  ) %>%
  group_by(hhid) %>%
  mutate(
    hh_max_index = max(indiv_index, na.rm = TRUE)
  ) %>%
  ungroup()

# generate an anonymized version of the data
# set.seed(666)
# individuals_cases_randomized <- individuals_cases %>%
#  mutate(
#     hh_full_name_calc = sample(hh_full_name_calc),
#     hh_gender = sample(hh_gender),
#     hh_age = sample(hh_age),
#     hh_relation_with = sample(hh_relation_with)
#   )

# enumerators ------------------------------------------------------------------

# schools ----------------------------------------------------------------------

schools_coordinates <- schools_coordinates_box %>%
  st_set_crs(st_crs(schools_coordinates_box)) %>%
  st_transform(4326) %>%
  mutate(
    longitude = st_coordinates(.)[, "X"],
    latitude  = st_coordinates(.)[, "Y"]
  ) %>%
  st_drop_geometry() %>%
  select(school_id, school_name, longitude, latitude)

schools_cases <- schools_box %>%
  # Fix encoding for all character columns EXCEPT hhid_village
  mutate(across(where(is.character) & !matches("hhid_village"), .fix_encoding_fr)) %>%
  mutate(across(where(is.character) & !matches("hhid_village"), str_to_title)) %>%
  mutate(across(where(is.character), str_trim)) %>%
  mutate(across(where(is.character), ~ na_if(., ""))) %>%
  mutate(across(where(is.character) & !matches("hhid_village"), ~ stri_trans_general(., "Latin-ASCII"))) %>%
  # Keep hhid_village in UPPERCASE
  mutate(hhid_village = str_to_upper(hhid_village)) %>%
  # Rename coordinate columns directly from source
  # rename(
  #  latitude = geo_loclatitude,
  #  longitude = geo_loclongitude
  #) %>%
  # CREATE MAIN VILLAGE LOGIC (matching your Stata script)
  mutate(
    main_village = case_when(
      # PEULH DIOSS - School 032A services: 032A, 042A, 112A
      hhid_village %in% c("032A", "042A", "112A") ~ "032A",
      # TONGUEL - School 101B services: 101B, 103B
      hhid_village %in% c("101B", "103B") ~ "101B",
      # MINGUE BOYE - School 013B services: 013B, 013A
      hhid_village %in% c("013B", "031A") ~ "013B",
      # Diagambal - School 053A services: 053A, 061A
      hhid_village %in% c("053A", "061A") ~ "053A",
      # Feto - School 073B services: 073B, 121A
      hhid_village %in% c("073B", "121A") ~ "073B",
      # Guede - School 060B services: 060B, 070B 
      hhid_village %in% c("060B", "070B") ~ "060B",
      # All other villages are their own main village
      TRUE ~ hhid_village
    ),
    villages_serviced = case_when(
      hhid_village %in% c("032A", "042A", "112A") ~ "032A, 042A, 112A",
      hhid_village %in% c("101B", "103B") ~ "101B, 103B",
      hhid_village %in% c("013B", "031A") ~ "013B, 031A",
      hhid_village %in% c("053A", "061A") ~ "053A, 061A",
      hhid_village %in% c("073B", "121A") ~ "073B, 121A",
      hhid_village %in% c("060B", "070B") ~ "060B, 070B",
      TRUE ~ hhid_village
    )
  ) %>%
  # Rename hhid_village to school_village_id (for clarity)
  rename(school_village_id = hhid_village) %>%
  # Create unique school identifier using main_village
  group_by(main_village) %>%
  mutate(school_number = row_number()) %>%
  ungroup() %>%
  # Create school_id with format: 032AS1 (using main_village)
  mutate(school_id = paste0(main_village, "S", school_number)) %>%
  # Keep all the variables you specified
  select(
    school_id,
    school_village_id,
    main_village,
    villages_serviced,
    
    # Respondent information
    respondent_is_director,
    respondent_is_not_director,
    respondent_is_not_director_o,
    respondent_other_role,
    respondent_other_role_o,
    respondent_name,
    respondent_phone_primary,
    respondent_phone_secondary,
    respondent_gender,
    respondent_age,
    
    # School information
    school_name,
    director_experience_general,
    director_experience_specific,
    
    # Grade and enrollment data (2025)
    starts_with("grade_loop_2025"),
    starts_with("enrollment_2025"),
  ) %>%
  # ADD CLASSROOM COUNTS for each grade (count non-empty enrollment fields)
  mutate(
    classroom_2025_1 = rowSums(!is.na(select(., starts_with("enrollment_2025_total_1_")))),
    classroom_2025_2 = rowSums(!is.na(select(., starts_with("enrollment_2025_total_2_")))),
    classroom_2025_3 = rowSums(!is.na(select(., starts_with("enrollment_2025_total_3_")))),
    classroom_2025_4 = rowSums(!is.na(select(., starts_with("enrollment_2025_total_4_")))),
    classroom_2025_5 = rowSums(!is.na(select(., starts_with("enrollment_2025_total_5_")))),
    classroom_2025_6 = rowSums(!is.na(select(., starts_with("enrollment_2025_total_6_"))))
  ) %>%
  # Replace 0 with NA for classroom counts (if no data exists for that grade)
  mutate(
    across(starts_with("classroom_2025_"), ~ na_if(., 0))
  ) %>%
  # Create cases dataset structure using unique school_id as the identifier
  select(- school_name) %>%
  right_join(schools_coordinates, by = "school_id")%>%
  add_row(
    school_id = "072BS2",
    school_village_id = "072B",
    main_village = "072B",
    villages_serviced = "072B",
    school_name = "École Élémentaire de Rerde",
    longitude = NA_real_,
    latitude  = NA_real_
  ) %>%
  add_row(
    school_id = "103BS1",
    school_village_id = "103B",
    main_village = "103B",
    villages_serviced = "103B",
    school_name = "École Élémentaire de Guédé Diery",
    longitude = NA_real_,
    latitude  = NA_real_
  ) %>%
  .make_cases_dataset(school_id)


# replacements -----------------------------------------------------------------

replacements_list <- replacements_box %>%
  mutate(across(where(is.character), .fix_encoding_fr)) %>%
  mutate(across(where(is.character), str_to_title)) %>%
  mutate(across(where(is.character), str_trim)) %>%
  mutate(across(where(is.character), ~ na_if(., ""))) %>%
  mutate(across(where(is.character), ~ stri_trans_general(., "Latin-ASCII"))) %>%
  mutate(hhid_village = toupper(as.character(hhid_village))) %>%
  left_join(villages_cases%>%select(hhid_village, village_name), by="hhid_village")%>%
  group_by(hhid_village) %>%
  mutate(replacement_id = paste0(hhid_village, "R", row_number())) %>%
  ungroup()
  

# manage users, roles and forms ================================================

# villages ---------------------------------------------------------------------

villages_dataset <-  villages_cases %>%
#  mutate(
#    formids = paste0(formids, "dises_midline2b_community_survey, "),
#  )%>%
#  mutate(
#    formids = paste0(formids, "dises_child_identification, "),
#    users = paste0(users, " ")
#  )%>% 
  mutate(
    formids = paste0(formids, "dises_midline2_facilitator_survey, "),
    users = paste0(users, "formation")
  )

# households -------------------------------------------------------------------

households_dataset <-  households_cases %>%
  mutate(
    formids = paste0(formids, "dises_midline2b_household_survey, "),
  )


# individuals ------------------------------------------------------------------

# dises_child_identification
individuals_dataset <-  individuals_cases %>%
  mutate(
    formids = if_else(hh_age >= 5 & hh_age <= 18 & parasitological_team_ea == 'UCAD', paste0(formids, "dises_child_identification, "), formids),
    users = if_else(hh_age >= 5 & hh_age <= 18 & parasitological_team_ea == 'UCAD', paste0(users, "cherifo2c@gmail.com, "), users)
  )

# dises_midline2b_household_survey
individuals_dataset <-  individuals_dataset %>%
  mutate(
    formids = paste0(formids, "dises_midline2b_household_survey, "),
    users = paste0(users, ""),
  )

# dises_valuation_cognitive_midline_2 
individuals_dataset <-  individuals_dataset 

# schools ----------------------------------------------------------------------

schools_dataset <-  schools_cases %>%
  mutate(
    formids = paste0(formids, "dises_principal_survey_midline2b, "),
  )

# exports ======================================================================

#villages
#box_write(villages_dataset, .villages_file, .datasets_folder)
write_scto(villages_dataset, "cases_villages")

#households
#box_write(households_dataset, .households_file, .datasets_folder)
#write_scto(households_dataset, "cases_households")

#individuals
#box_write(individuals_dataset, .individuals_file, .datasets_folder)
#write_scto(individuals_dataset, "cases_individuals")

#schools
#box_write(schools_dataset, .schools_file, .datasets_folder)
#write_scto(schools_dataset, "cases_schools")

#replacements
#box_write(replacements_list, .replacements_file, .datasets_folder)
#write_scto(replacements_list, "cases_replacements")
