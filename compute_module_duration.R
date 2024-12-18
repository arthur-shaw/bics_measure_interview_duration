# ==============================================================================
# Prepare data
# ==============================================================================

# ------------------------------------------------------------------------------
# Paradata
# ------------------------------------------------------------------------------

# get path of all paradata files
paradata_file_paths <- fs::dir_ls(
  path = here::here("data", "01_paradata"),
  type = "file",
  regexp = "paradata.tab",
  recurse = TRUE
)

# combine raw paradata files
paradata_raw <- tidytable::map_dfr(
  .x = paradata_file_paths,
  .f = ~ data.table::fread(.x)
)

# parse the file into a form needed for analysis
paradata_processed <- susopara::parse_paradata(dt = paradata_raw)

# compute time between events
paradata_processed <- susopara::calc_time_btw_active_events(
  dt = paradata_processed
)

# remove extreme times
paradata_processed <- paradata_processed |>
  tidytable::filter(
    (elapsed_min < quantile(elapsed_min, 0.99, na.rm = TRUE)) &
    (elapsed_min > 0 & !is.na(elapsed_min))
  )

# clean up environment, removing large and unused objects
rm(paradata_raw)

# ------------------------------------------------------------------------------
# Metadata
# ------------------------------------------------------------------------------

# set path to the JSON representation of the questionnaire
json_file_path <- fs::path(here::here("data", "02_metadata", "document.json"))

# ingest
qnr_df <- susometa::parse_questionnaire(path = json_file_path)

# create section-variable table
variables_by_section <- qnr_df |>
	susometa::get_questions_by_section() |>
  tidytable::select(section, variable) |>
  data.table::data.table()

# clean up environment
rm(qnr_df)

# ------------------------------------------------------------------------------
# Amplified paradata
# ------------------------------------------------------------------------------

paradata_w_section <- paradata_processed |>
	tidytable::left_join(variables_by_section, by = "variable")

# clean up environment
rm(paradata_processed)

readr::write_tsv(
  x = paradata_w_section,
  file = here::here("data", "04_created", "paradata_w_section.tsv")
)

# ==============================================================================
# Compute overall durations
# ==============================================================================

duration_by_module <- paradata_w_section |>
  # remove empty section
  tidytable::filter(!is.na(section)) |>
  # compute total by interview
  tidytable::group_by(interview__id, section) |>
  tidytable::summarise(
    elapsed_min = sum(elapsed_min, na.rm = TRUE)
  ) |>
  tidytable::ungroup() |>
  # compute statistics by section
  tidytable::group_by(section) |>
  tidytable::summarise(
    med = median(x = elapsed_min, na.rm = TRUE),
    sd = sd(x = elapsed_min, na.rm = TRUE),
    min = min(elapsed_min, na.rm = TRUE),
    max = max(elapsed_min, na.rm = TRUE),
    n_obs = tidytable::n()
  ) |>
  tidytable::ungroup() |>
	# remove module without questions
  dplyr::filter(section != "Only for Working variables") |>
	# assign module numbers to facilitate sorting
  dplyr::mutate(
    section_num = dplyr::case_when(
      section == "SA: Interview information" ~ 1,
      section == "SB: Visits" ~ 2,
      section == "S1: Household Roster" ~ 3,
      section == "S2: Dwelling Characterstics (q10-q21 moved before S6 as S2b)" ~ 4,
      section == "S3: Household Assets" ~ 5,
      section == "S4A: Non-Food Expenditures, 30 Day" ~ 6,
      section == "S4C: Non-Food Expenditures, 12 Month" ~ 7,
      section == "S5A: Food, Last 7 Days" ~ 8,
      section == "S5B : Household Food Consumption (Recall)" ~ 9,
      section == "S5C: 24HR FOOD: SHARES" ~ 10,
      section == "S2b: Dwelling Characterstics (q10-q21)" ~ 11,
      section == "S6: Land Tenure" ~ 12,
      section == "S19: Total Expenditure" ~ 13,
      section == "S8: Health & Expenditure" ~ 14,
      section == "S7:Education & Expenditure" ~ 15,
      section == "S14: Non-Food Consumption (7 Days)" ~ 16,
      section == "S14B: Current Clothes" ~ 17,
      section == "S15: Food Away From Home" ~ 18,
      section == "S11: Access/Use Of ICT" ~ 19,
      section == "S12: Labor" ~ 20,
      section == "S10: FIES" ~ 21,
      section == "S16: Time Use" ~ 22,
      section == "S17: Child Care" ~ 23,
      section == "S9: Assets, Marriage" ~ 24,
      section == "S13o:  Social Norms respondents" ~ 25,
      section == "S13A: Social Norms - Reference Groups" ~ 26,
      section == "S13B-Q1a: Social Norms for  %s13pair_female%   [Female only]" ~ 27,
      section == "S13B-Q1b: Social Norms for   %s13pair_male%   [Male only]" ~ 28,
      section == "S13B-Q2: Social Norms [Both of Couple]" ~ 29,
      section == "S13B-Q3: Social Norms [Both of Couple]" ~ 30,
      section == "S13B-Q4: Social Norms [Both of Couple]" ~ 31,
      section == "S13B-Q567: Social Norms [Both of Couple]" ~ 32,
      section == "S13C: Social Norms - Conditionality" ~ 33,
      section == "S13D: Social Norms - Vignettes" ~ 34,
      section == "S13E: Social Norms - Social Desireablity" ~ 35,
      section == "S18: Anthropometrics" ~ 36,
      section == "To Complete Interview" ~ 37,
      TRUE ~ 99
    )
  ) |>
	dplyr::arrange(section_num) |>
	dplyr::select(-section_num)

saveRDS(
  object = duration_by_module,
  file = here::here("data", "04_created", "duration_by_module.rds")
)

# ==============================================================================
# Compute duration of non-individual modules
# ==============================================================================

non_indiv_sections <- c(
  "SA: Interview information",
  "SB: Visits",
  "S1: Household Roster",
  "S2: Dwelling Characterstics (q10-q21 moved before S6 as S2b)",
  "S3: Household Assets",
  "S4A: Non-Food Expenditures, 30 Day",
  "S4C: Non-Food Expenditures, 12 Month",
  "S5A: Food, Last 7 Days",
  "S5B : Household Food Consumption (Recall)",
  "S5C: 24HR FOOD: SHARES",
  "S2b: Dwelling Characterstics (q10-q21)",
  "S6: Land Tenure",
  "S19: Total Expenditure",
  "To Complete Interview"
)

duration_by_non_indiv_module <- paradata_w_section |>
  # subset to non-individual sections
	tidytable::filter(section %in% non_indiv_sections) |>
  # collapse/aggregate certain sections
  tidytable::mutate(
    section = tidytable::case_when(
      grepl(x = section, pattern = "(Food|Non-Food|Total Expenditure)") ~
        "Food and non-food expenditure",
      TRUE ~ section
    )
  ) |>
  # compute total by interview
  tidytable::group_by(interview__id, section) |>
  tidytable::summarise(
    elapsed_min = sum(elapsed_min, na.rm = TRUE)
  ) |>
  tidytable::ungroup() |>
  tidytable::group_by(section) |>
  tidytable::summarise(
    med = median(x = elapsed_min, na.rm = TRUE),
    sd = sd(x = elapsed_min, na.rm = TRUE),
    min = min(elapsed_min, na.rm = TRUE),
    max = max(elapsed_min, na.rm = TRUE),
    n_obs = tidytable::n()
  ) |>
  tidytable::ungroup() |>
	# assign module numbers to facilitate sorting
  dplyr::mutate(
    section_num = dplyr::case_when(
      section == "SA: Interview information" ~ 1,
      section == "SB: Visits" ~ 2,
      section == "S1: Household Roster" ~ 3,
      section == "S2: Dwelling Characterstics (q10-q21 moved before S6 as S2b)" ~ 4,
      section == "S3: Household Assets" ~ 5,
      section == "Food and non-food expenditure" ~ 6,
      section == "S5C: 24HR FOOD: SHARES" ~ 10,
      section == "S2b: Dwelling Characterstics (q10-q21)" ~ 11,
      section == "S6: Land Tenure" ~ 12,
      section == "S19: Total Expenditure" ~ 13,
      section == "S18: Anthropometrics" ~ 36,
      section == "To Complete Interview" ~ 37,
      TRUE ~ 99
    )
  ) |>
	dplyr::arrange(section_num) |>
	dplyr::select(-section_num)
	

saveRDS(
  object = duration_by_non_indiv_module,
  file = here::here("data", "04_created", "duration_by_non_indiv_module.rds")
)

# ==============================================================================
# Compute duration of individual module
# ==============================================================================

# identify individual sections
indiv_sections <- c(
  "S8: Health & Expenditure",
  "S7:Education & Expenditure",
  "S14: Non-Food Consumption (7 Days)",
  "S14B: Current Clothes",
  "S15: Food Away From Home",
  "S11: Access/Use Of ICT",
  "S12: Labor",
  "S10: FIES",
  "S16: Time Use",
  "S17: Child Care",
  "S9: Assets, Marriage",
  "S13o: Social Norms respondents",
  "S13A: Social Norms - Reference Groups",
  "S13B-Q1b: Social Norms for %s13pair_male% [Male only]",
  "S13B-Q1a: Social Norms for %s13pair_female% [Female only]",
  "S13C: Social Norms - Conditionality",
  "S18: Anthropometrics"
)

duration_by_module_per_person <- paradata_w_section |>
  # subset to individual sections
	tidytable::filter(section %in% indiv_sections) |>
	# create person ID from row vector
  tidytable::mutate(
    person_id = tidytable::if_else(
      # if row vector contains a comma ...
      condition = grepl(x = row, pattern = ","),
      # ... then extract the part before the comma ...
      true = stringr::str_extract(
        string = row,
        pattern = "[0-9]+(?=,)"
      ),
      # ... otherwise, return the unmodified value
      false = row,
      missing = row
    )
  ) |>
	tidytable::group_by(section, interview__id) |>
	tidytable::summarise(
    tot_duration = sum(elapsed_min, na.rm = TRUE),
    n_people = tidytable::n_distinct(person_id)
  ) |>
	tidytable::ungroup() |>
	tidytable::mutate(dur_per_person = tot_duration / n_people) |>
	tidytable::group_by(section) |>
  tidytable::summarise(
    med = median(x = dur_per_person, na.rm = TRUE),
    sd = sd(x = dur_per_person, na.rm = TRUE),
    min = min(dur_per_person, na.rm = TRUE),
    max = max(dur_per_person, na.rm = TRUE),
    n_obs = tidytable::n()
  ) |>
	tidytable::ungroup() |>
	# assign module numbers to facilitate sorting
  dplyr::mutate(
    section_num = dplyr::case_when(
      section == "SA: Interview information" ~ 1,
      section == "SB: Visits" ~ 2,
      section == "S1: Household Roster" ~ 3,
      section == "S2: Dwelling Characterstics (q10-q21 moved before S6 as S2b)" ~ 4,
      section == "S3: Household Assets" ~ 5,
      section == "S4A: Non-Food Expenditures, 30 Day" ~ 6,
      section == "S4C: Non-Food Expenditures, 12 Month" ~ 7,
      section == "S5A: Food, Last 7 Days" ~ 8,
      section == "S5B : Household Food Consumption (Recall)" ~ 9,
      section == "S5C: 24HR FOOD: SHARES" ~ 10,
      section == "S2b: Dwelling Characterstics (q10-q21)" ~ 11,
      section == "S6: Land Tenure" ~ 12,
      section == "S19: Total Expenditure" ~ 13,
      section == "S8: Health & Expenditure" ~ 14,
      section == "S7:Education & Expenditure" ~ 15,
      section == "S14: Non-Food Consumption (7 Days)" ~ 16,
      section == "S14B: Current Clothes" ~ 17,
      section == "S15: Food Away From Home" ~ 18,
      section == "S11: Access/Use Of ICT" ~ 19,
      section == "S12: Labor" ~ 20,
      section == "S10: FIES" ~ 21,
      section == "S16: Time Use" ~ 22,
      section == "S17: Child Care" ~ 23,
      section == "S9: Assets, Marriage" ~ 24,
      section == "S13o:  Social Norms respondents" ~ 25,
      section == "S13A: Social Norms - Reference Groups" ~ 26,
      section == "S13B-Q1a: Social Norms for  %s13pair_female%   [Female only]" ~ 27,
      section == "S13B-Q1b: Social Norms for   %s13pair_male%   [Male only]" ~ 28,
      section == "S13B-Q2: Social Norms [Both of Couple]" ~ 29,
      section == "S13B-Q3: Social Norms [Both of Couple]" ~ 30,
      section == "S13B-Q4: Social Norms [Both of Couple]" ~ 31,
      section == "S13B-Q567: Social Norms [Both of Couple]" ~ 32,
      section == "S13C: Social Norms - Conditionality" ~ 33,
      section == "S13D: Social Norms - Vignettes" ~ 34,
      section == "S13E: Social Norms - Social Desireablity" ~ 35,
      section == "S18: Anthropometrics" ~ 36,
      section == "To Complete Interview" ~ 37,
      TRUE ~ 99
    )
  ) |>
	dplyr::arrange(section_num) |>
	dplyr::select(-section_num)

saveRDS(
  object = duration_by_module_per_person,
  file = here::here("data", "04_created", "duration_by_module_per_person.rds")
)

# ==============================================================================
# Compute duration for particular questions
# ==============================================================================

# duration_by_question_per_person <- paradata_w_section |>
#   # drill down to particular questions
#   dplyr::filter(
#     variable %in% c(
# s4a_04
# s4a_05
# s4a_06_1

# # S5a
# s5a_11
# s5a_12_1
#     )
#   )


# ==============================================================================
# Produce report
# ==============================================================================

quarto::quarto_render(
  input = here::here("inst", "duration_report.qmd")
)
