### Data summary for 'dises_child_identification'
### AS 🐚🫧🪼🪸
### 28.10.2025 (Last update)

rm(list = ls())
pacman::p_load(dplyr, stringr, lubridate, tidyr, tidyverse, broom)

source("./clients/scto_client.R")
source("./clients/box_client.R")
source("./utils/misc_helpers.R")


# parameters -------------------------------------------------------------------

.form_id <- "dises_child_identification"
.export_folder_box <- ""

# data--------------------------------------------------------------------------

df <- pull_scto(.form_id)

# summary ----------------------------------------------------------------------

ided_children <- df %>%
  select(
    "starttime", "caseid", "hhid_pl", "individ_pl", "indiv_index_pl", "hh_max_index_pl", "hh_full_name_calc_pl",
    "hh_gender_pl", "hh_age_pl", "hh_relation_with_pl", "hh_head_name_complet_pl", "hh_phone_pl", "hhid_village_pl",
    "village_name_pl", "hh_grade_pl", "parasitological_id_pl", "hh_other_members_list_count", "hh_other_members_pl", "parasitological_team",
    "parasitological_id", "id", "identity_confirmed", "identity_confirmator_1", "identity_confirmator_2", "identity_confidence",
    "hh_full_name_calc", "hh_full_name_alt", "hh_age_updated", "hh_35_updated", "hh_school_teacher", "hh_father_selected",
    "hh_father_other", "hh_father", "hh_father_alt", "hh_mother_selected", "hh_mother_other", "hh_mother",
    "hh_mother_alt", "hhid_village", "identity_not_confirmed", "hhid_village_identity_not_confirmed", "hh_full_name_identity_not_confirmed",
    "hh_age_identity_not_confirmed", "hh_35_identity_not_confirmed","hh_school_teacher_phone", "comment"
  )


df <- df %>%
  mutate(hh_age_final = coalesce(
    hh_age_identity_not_confirmed,
    hh_age_updated,
    hh_age_pl
  ))

sum(df$caseid != "", na.rm = TRUE)
