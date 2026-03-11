#--------------------------------------
# script to create county lat/long table
#--------------------------------------

state_fips <- structure(list(V1 = c(
  "01", "02", "04", "05", "06", "08", "09",
  "10", "11", "12", "13", "15", "16", "17", "18", "19", "20", "21",
  "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32",
  "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "44",
  "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56"
), V2 = c(
  "ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA",
  "COLORADO", "CONNECTICUT", "DELAWARE", "DISTRICT OF COLUMBIA",
  "FLORIDA", "GEORGIA", "HAWAII", "IDAHO", "ILLINOIS", "INDIANA",
  "IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND",
  "MASSACHUSETTS", "MICHIGAN", "MINNESOTA", "MISSISSIPPI", "MISSOURI",
  "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY",
  "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA", "OHIO",
  "OKLAHOMA", "OREGON", "PENNSYLVANIA", "RHODE ISLAND", "SOUTH CAROLINA",
  "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "VERMONT", "VIRGINIA",
  "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING"
)), row.names = c(
  NA,
  -51L
), class = "data.frame")

data.table::setDT(state_fips)
data.table::setnames(state_fips, new = c("state_fips", "state_name"))

state_fips[, state_lower := tolower(state_name)]

state_abbs <- rbind(
  data.frame(state2 = state.abb, state_name = state.name),
  data.frame(state2 = "DC", state_name = "District of Columbia")
)
data.table::setDT(state_abbs)
state_abbs[, state_lower := tolower(state_name)]

state_fips <- state_fips[, .(state_fips, state_lower)] |>
  _[state_abbs, on = "state_lower"]


# get the use county shape file
us_county_shape <- tigris::counties()

# reduce to only the fips latitude, and longitude
data.table::setDT(us_county_shape)

us_county_shape <- us_county_shape[, list(
  state_fips = STATEFP,
  fips = GEOID,
  name = NAME,
  name_lsad = NAMELSAD,
  longitude = as.numeric(INTPTLON),
  latitude = as.numeric(INTPTLAT)
)]

# merge in state abbreviation and name
counties <- us_county_shape[state_fips, on = "state_fips"]
counties <- counties[
  , .(state_name, state = state2, state_fips, fips, longitude, latitude)
]

# USE THIS FOR USERS/EXPORT
usethis::use_data(counties, overwrite = TRUE)


## code to prepare `zipcodes` dataset goes here
zipcodes <- data.table::fread(
  "data-raw/zipcode_mapping.csv",
  encoding = "Latin-1"
)

data.table::setnames(
  zipcodes,
  new = c(
    "id", "zip_code", "state", "county", "region",
    "region_id", "region_name", "pop", "modified",
    "latitude", "longitude"
  )
)

# Normalize all character columns to valid UTF-8 for reliable rendering.
char_cols <- names(zipcodes)[vapply(zipcodes, is.character, logical(1))]
zipcodes[
  ,
  (char_cols) := lapply(.SD, enc2utf8),
  .SDcols = char_cols
]

# Correct known Puerto Rico municipality mojibake to proper UTF-8 accents.
pr_name_map <- c(
  "PR_A+¦asco" = "PR_Añasco",
  "PR_Pe+¦uelas" = "PR_Peñuelas",
  "PR_Gu+ínica" = "PR_Guánica",
  "PR_Las Mar+¡as" = "PR_Las Marías",
  "PR_Manat+¡" = "PR_Manatí",
  "PR_Rinc+¦n" = "PR_Rincón",
  "PR_San Germ+ín" = "PR_San Germán",
  "PR_San Sebasti+ín" = "PR_San Sebastián",
  "PR_R+¡o Grande" = "PR_Río Grande",
  "PR_Can+¦vanas" = "PR_Canóvanas",
  "PR_Lo+¡za" = "PR_Loíza",
  "PR_Comer+¡o" = "PR_Comerío",
  "PR_Juana D+¡az" = "PR_Juana Díaz",
  "PR_Bayam+¦n" = "PR_Bayamón",
  "PR_Cata+¦o" = "PR_Cataño",
  "PR_Mayag++ez" = "PR_Mayagüez"
)

for (nm in c("county", "region_name")) {
  ndx <- zipcodes[[nm]] %in% names(pr_name_map)
  zipcodes[ndx, (nm) := unname(pr_name_map[zipcodes[[nm]][ndx]])]
}

usethis::use_data(zipcodes, overwrite = TRUE)

# Use the spline-lookup tables (these are the same as are in the app)
sl <- data.table::fread("data-raw/spline_lookups.csv")
spline_001 <- sl[pval == "0.001", .(observed, spl_thresh)]
spline_005 <- sl[pval == "0.005", .(observed, spl_thresh)]
spline_01 <- sl[pval == "0.01", .(observed, spl_thresh)]
spline_05 <- sl[pval == "0.05", .(observed, spl_thresh)]

usethis::use_data(
  spline_001, spline_005, spline_01, spline_05,
  overwrite = TRUE
)


# USE THIS AS INTERNAL DATASETS
usethis::use_data(
  counties,
  zipcodes,
  spline_001, spline_005, spline_01, spline_05,
  internal = TRUE,
  overwrite = TRUE
)
