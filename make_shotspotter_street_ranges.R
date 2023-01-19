library(dplyr)
library(sf)
library(stringr)

# no response data from:
# https://data.cincinnati-oh.gov/Safety/ShotSpotter-Incidents-by-Police-District/hgdz-xudz on 16 September 2022
download.file(
  "https://data.cincinnati-oh.gov/api/views/gexm-h6bt/files/8e9a29a3-1120-4187-ab83-655d17f05403?download=true&filename=CPD%20Disposition%20Text_No%20Response.xlsx",
  "CPD_Disposition_Text_No_Response.xlsx"
)

no_resp_dispos <-
  readxl::read_xlsx("CPD_Disposition_Text_No_Response.xlsx") |>
  magrittr::set_names(c("disposition", "response")) |>
  filter(!response == "response") |>
  pull(disposition)

shot_spotter_csv_url <-
  glue::glue(
    "https://data.cincinnati-oh.gov/api",
    "/views/gexm-h6bt/rows.csv",
    "?query=select%20*%20where%20(%60incident_type_desc%60%20%3D%20%27SHOT%20SPOTTER%20ACTIVITY%27)",
    "&read_from_nbe=true&version=2.1&accessType=DOWNLOAD"
  )

raw_data <-
  readr::read_csv(shot_spotter_csv_url,
                  col_types = readr::cols_only(
                    ADDRESS_X = "character",
                    CREATE_TIME_INCIDENT = readr::col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
                    DISPOSITION_TEXT = "factor"
                  )) |>
  rename(
    address_x = ADDRESS_X,
    date_time = CREATE_TIME_INCIDENT,
    disposition = DISPOSITION_TEXT
  )

#' exclude data with missing address, date, or disposition
d <-
  raw_data |>
  na.omit() |>
  filter(!disposition %in% no_resp_dispos) |>
  select(-disposition)

#' create unique set of address ranges & parse name, min, max street number
d_address_ranges <-
  d |>
  group_by(address_x) |>
  tally() |>
  arrange(desc(n)) |>
  tidyr::extract(address_x,
                 into = c("x_min", NA, "x_max", NA, "x_name"),
                 regex = "(^[0-9X]*)([-]?)([0-9X]*)([ ]*)(.*)",
                 remove = FALSE) |>
  mutate(across(c(x_max, x_min), na_if, "")) |>
  mutate(across(x_max, coalesce, x_min)) |>
  mutate(x_min = gsub("X", "0", x_min),
         x_max = gsub("X", "9", x_max)) |>
  mutate(x_name = str_to_lower(x_name))

#' changes in city street names to match tigris street names
d_address_ranges <-
  d_address_ranges |>
  mutate(x_name = str_replace_all(x_name, fixed(" av"), " ave")) |>
  mutate(x_name = str_replace_all(x_name, fixed(" wy"), " way")) |>
  mutate(x_name = str_replace_all(x_name, fixed("east "), "e "))

#' add suffixes to the end of these specific city street names to match tigris street names
add_suffix <-
  list(
    "ave" = c("e mcmicken", "w mcmicken", "mcgregor", "st james", "blair", "mckeone"))

suffix_replacements <-
  paste(add_suffix$ave, names(add_suffix["ave"])) |>
  purrr::set_names(add_suffix$ave)

d_address_ranges <-
  d_address_ranges |>
  mutate(x_name = str_replace_all(x_name, suffix_replacements)) |>
  mutate(x_name = str_replace_all(x_name, fixed("ave ave"), "ave"))
                  
# https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2020/TGRSHP2020_TechDoc.pdf
streets <-
  tigris::address_ranges(state = "39", county = "061", year = 2021) |>
  select(tlid = TLID,
         name = FULLNAME,
         LFROMHN, RFROMHN,
         LTOHN, RTOHN) |>
  mutate(across(ends_with("HN"), as.numeric)) |>
  dplyr::rowwise() |>
  transmute(name = str_to_lower(name),
            tlid = as.character(tlid),
            number_min = min(LFROMHN, RFROMHN, LTOHN, RTOHN, na.rm = TRUE),
            number_max = max(LFROMHN, RFROMHN, RTOHN, LTOHN, na.rm = TRUE)) |>
  ungroup()

#' returns all "intersecting" tigris street range address lines within intput street name and min/max for street number
query_street_ranges <- function(x_name, x_min, x_max, ...) {
  ## find street
  streets_contain <-
    streets |>
    filter(name == x_name)
  ## return range that contains both min and max street number, if available
  range_contain <-
    streets_contain |>
    filter(number_min < x_min & number_max > x_max)
  if (nrow(range_contain) > 0) return(range_contain)
  # if not available, return ranges containing either the min or max street number
  range_partial <-
    streets_contain |>
    filter(between(number_min, x_min, x_max) |
            between(number_max, x_min, x_max))
  if (nrow(range_partial) > 0) return(range_partial)
  # if nothing available, return NA
  return(NA)
}

query_street_ranges("vine st", 2300, 2399)
query_street_ranges("westwood northern blvd", 1900, 1999)
query_street_ranges("rockdale ave", 600, 699)
query_street_ranges("westwood northern blvd", 11900, 21999) # should return NA
query_street_ranges("e mcmicken ave", 00, 99)
query_street_ranges("rapid run pike", 4530, 4599)

## streets |>
##   filter(name %in% c("saint james ave", "st james ave")) |>
##   mapview::mapview(zcol = "name")

## streets |>
##   filter(name == "mcgregor ave")

streets |>
  st_drop_geometry() |>
  select(name) |>
  distinct() |>
  arrange(name) |>
  readr::write_csv("streets.csv")

d_address_ranges$street_ranges <-
  purrr::pmap(d_address_ranges, query_street_ranges, .progress = "querying street ranges")

d_address_ranges |>
  rowwise() |>
  mutate(n_street_ranges = list(nrow(street_ranges))) |>
  group_by(n_street_ranges) |>
  summarize(n = n()) |>
  mutate(`%` = scales::percent(n / sum(n), 1)) |>
  knitr::kable()

d_address_ranges |>
  filter(is.na(street_ranges)) |>
  filter(!is.na(x_min)) |>
  knitr::kable()

d <- left_join(d, d_address_ranges, by = "address_x")

d |>
  select(-x_max, -x_min, -x_name, -n) |>
  saveRDS("shotspotter_street_ranges.rds")
