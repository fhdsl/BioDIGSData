
#' Gets relevant data from a specified snapshot.
#'
#' @param local TRUE or FALSE, whether to use a local file or pull from GitHub. Can be used if you don't have an internet connection.
#' @param snapshot string specifying which snapshot to use, e.g., "BioDIGS_20250206.csv"
#'
#' @return an uncleaned `data.frame`
#' @export
#' @keywords internal
#'
#' @examples
#' getdata()
#' getdata(snapshot = "BioDIGS_20250206.csv")
getdata <- function(local = T, snapshot = NULL) {
  # Specific snapshot name
  if (is.null(snapshot)){
    filename <- "BioDIGS_20250206.csv"
  } else {
    filename <- snapshot
  }

  # read in the data locally or from remote snapshot
  if (local) {
    thedata <- read.csv(filename)
  } else {
    theurl <- paste0(
      "https://raw.githubusercontent.com/FredHutch/GDSCNsoilsites/refs/heads/main/data/snapshots/",
      filename
    )
    thedata <- read.csv(theurl)
  }
  return(thedata)
}


#' Cleans data from Google Sheets to make GPS coordinates consistent.
#'
#' @param the_data
#'
#' @return Cleaned `data.frame`
#' @import tidyr
#' @import dplyr
#' @import stringr
#' @export
#' @keywords internal
#'
#' @examples
#' clean_gps_points(getdata())
clean_gps_points <- function(the_data) {
  # Clean up GPS points:
  # Remove parentheses, split column, fix negatives, and make numeric
  the_data <- the_data %>%
    mutate(coord = str_replace_all(gps, "[//(,//)]*", "")) %>%
    tidyr::separate(coord,
                    into = c("latitude", "longitude"),
                    sep = " ") %>%
    mutate(longitude = case_when(
      as.numeric(longitude) > 0 ~ as.numeric(longitude) * -1,
      TRUE ~ as.numeric(longitude)
    )) %>%
    mutate(latitude = as.numeric(latitude))

  return(the_data)
}


#' Produce data in a nice clean format for use in R.
#'
#' @details
#' | **Field Name** |      | **Description** |
#' |----------------|------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
#' |  site_id         | `  ` | Unique letter and number site name |
#' |  type            | `  ` | Management type. Can be managed or unmanaged |
#' |  latitude        | `  ` | Latitude of the sampling location |
#' |  longitude       | `  ` | Longitude of the sampling location |
#'
#' @param info if set to TRUE, print important information to the console. This can be disabled with `info = FALSE`.
#'
#' @return a `data.frame`
#' @import dplyr
#' @export
#'
#' @examples
#' BioDIGS_metadata()
BioDIGS_metadata <- function(info = TRUE) {
  metadata_ <-
    BioDIGSData:::clean_gps_points(BioDIGSData:::getdata())

  metadata_ <-
    metadata_ %>%
    rename(type = mgmt_type) %>%
    dplyr::select(site_id, type, origin, latitude, longitude, partner_faculty, google_img_id) %>%
    distinct()

  if(info){
    cli_alert(col_cyan("See the data dictionary by typing ?BioDIGS_metadata() ."))
    cli_alert(col_cyan("Visit us at {.url https://biodigs.org/}"))
  }

  return(metadata_)
}


#' BioDIGS DNA metadata
#'
#' @description BioDIGS DNA concentration data in a clean format for use in R. For more about BioDIGS, see <https://biodigs.org/>.
#'
#' @details
#' | **Field Name** |      | **Description** |
#' |----------------|------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
#' |  purpose               | `  ` | Values can be "Troubleshooting" or "Project". Troubleshooting samples were collected to understand what would be possible with sequencing and typically did not involve students and faculty outside of the organizing team. |
#' |  vessel                | `  ` | Method for collecting and storing soil samples prior to sequencing. |
#' |  collection_date       | `  ` | Date sample was collected (soil was removed from a site). |
#' |  site_id               | `  ` | Unique letter and number site name. Check `BioDIGS_metadata()` for GPS coordinates, origin, and more. |
#' |  sample_id             | `  ` | Unique sequencing sample identifier. |
#' |  site_name_rep_detail  | `  ` | Detailed label for the sample, intended to help disambiguate in case of confusion. |
#' |  sequencing_facility   | `  ` | Facility/lab at which the sample was sequenced. |
#' |  sequencing_instrument | `  ` | Sequencing technology/machine used to generate sequencing data. |
#' |  DNA_OK                | `  ` | Flag to indicate whether DNA was successfully extracted from the sample. |
#' |  Qubit_conc_ng_ul      | `  ` | Concentration of the DNA as measured by the Qubit instrument, in nanograms per microliter. |
#'
#' @param info if set to TRUE, print important information to the console. This can be disabled with `info = FALSE`.
#'
#' @return a `data.frame`
#' @import dplyr
#' @import cli
#' @export
#'
#' @seealso [BioDIGS_metadata()]
#'
#' @examples
#' BioDIGS_DNA_conc_data()
BioDIGS_DNA_conc_data <- function(info = TRUE) {
  # Read in from Google, clean GPS points
  dna_data <- BioDIGSData:::getdata()

  dna_data_ <-
    dna_data %>%
    dplyr::filter(bulk_type == "Molecular") %>%
    dplyr::select(purpose,
           vessel,
           collection_date,
           site_id,
           sample_id,
           site_name_rep_detail,
           sequencing_facility,
           sequencing_instrument,
           DNA_OK,
           Qubit_conc_ng_ul)

  if(info){
    cli_alert(col_cyan("See the data dictionary by typing ?BioDIGS_DNA_conc_data() ."))
    cli_alert(col_cyan("Visit us at {.url https://biodigs.org/}"))
  }

  return(dna_data_)
}


#' Produce soil testing data in a clean format for use in R.
#'
#' @details
#' | **Field Name** |      | **Description** |
#' |----------------|------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
#' |  site_id       | `  ` | Unique letter and number site name |
#' |  full_name     | `  ` | Full site name                                                    |
#' |  As_EPA3051    | `  ` | Arsenic (mg/kg), EPA Method 3051A. Quantities < 3.0 are not detectable.  |
#' |  Cd_EPA3051    | `  ` | Cadmium (mg/kg), EPA Method 3051A. Quantities < 0.2 are not detectable.  |
#' |  Cr_EPA3051    | `  ` | Chromium (mg/kg), EPA Method 3051A                           |
#' |  Cu_EPA3051    | `  ` | Copper (mg/kg), EPA Method 3051A                             |
#' |  Ni_EPA3051    | `  ` | Nickel (mg/kg), EPA Method 3051A                             |
#' |  Pb_EPA3051    | `  ` | Lead (mg/kg), EPA Method 3051A                               |
#' |  Zn_EPA3051    | `  ` | Zinc (mg/kg), EPA Method 3051A                              |
#' |  water_pH      | `  ` | Water pH                                                     |
#' |  A-E_Buffer_pH | `  ` | Buffer pH                                                   |
#' |  OM_by_LOI_pct | `  ` | Organic Matter by Loss on Ignition                       |
#' |  P_Mehlich3    | `  ` | Phosphorus (mg/kg), using the Mehlich 3 soil test extractant |
#' |  K_Mehlich3    | `  ` | Potassium (mg/kg), using the Mehlich 3 soil test extractant |
#' |  Ca_Mehlich3   | `  ` | Calcium (mg/kg), using the Mehlich 3 soil test extractant |
#' |  Mg_Mehlich3   | `  ` | Magnesium (mg/kg), using the Mehlich 3 soil test extractant |
#' |  Mn_Mehlich3   | `  ` | Manganese (mg/kg), using the Mehlich 3 soil test extractant |
#' |  Zn_Mehlich3   | `  ` | Zinc (mg/kg), using the Mehlich 3 soil test extractant  |
#' |  Cu_Mehlich3   | `  ` | Copper (mg/kg), using the Mehlich 3 soil test extractant |
#' |  Fe_Mehlich3   | `  ` | Iron (mg/kg), using the Mehlich 3 soil test extractant |
#' |  B_Mehlich3    | `  ` | Boron (mg/kg), using the Mehlich 3 soil test extractant |
#' |  S_Mehlich3    | `  ` | Sulfur (mg/kg), using the Mehlich 3 soil test extractant |
#' |  Na_Mehlich3   | `  ` | Sodium (mg/kg), using the Mehlich 3 soil test extractant |
#' |  Al_Mehlich3   | `  ` | Aluminum (mg/kg), using the Mehlich 3 soil test extractant |
#' |  Est_CEC       | `  ` | Cation Exchange Capacity (meq/100g) at pH 7.0 (CEC) |
#' |  Base_Sat_pct  | `  ` | Base saturation (BS). This represents the percentage of CEC occupied by bases (Ca2+, Mg2+, K+, and Na+). The %BS increases with increasing soil pH (Figure 5). The availability of Ca2+, Mg2+, and K+ increases with increasing %BS. |
#' |  P_Sat_ratio   | `  ` | Phosphorus saturation ratio. This is the ratio between the amount of phosphorus present in the soil and the total capacity of that soil to retain phosphorus. The ability of phosphorus to be bound in the soil is primary a function of iron (Fe) and aluminum (Al) content in that soil. |
#'
#' @param info if set to TRUE, print important information to the console. This can be disabled with `info = FALSE`.
#'
#' @return a `data.frame`
#' @import dplyr
#' @import cli
#' @export
#'
#' @examples
#' BioDIGS_soil_data()
BioDIGS_soil_data <- function(info = TRUE) {
  testing_data_ <- BioDIGSData:::getdata()

  testing_data_ <-
    testing_data_ %>%
    select(
      site_id,
      site_name,
      type,
      tidyr::ends_with("EPA3051"),
      water_pH,
      OM_by_LOI_pct,
      tidyr::ends_with("Mehlich3"),
      Est_CEC,
      Base_Sat_pct,
      P_Sat_ratio
    ) %>%
    mutate(As_EPA3051 = as.numeric(case_when(As_EPA3051 == "< 3.0" ~ "0",
                                             TRUE ~ As_EPA3051))) %>%      # As can't be detected lower than 3.0
    mutate(Cd_EPA3051 = as.numeric(case_when(Cd_EPA3051 == "< 0.2" ~ "0",
                                             TRUE ~ Cd_EPA3051))) %>%      # Cd can't be detected lower than 0.2
    mutate(
      region = case_when(
        startsWith(site_id, "M") ~ "Montgomery County",
        startsWith(site_id, "B") ~ "Baltimore City",
        startsWith(site_id, "S") ~ "Seattle"
      )
    )

  if(info){
    cli_alert_info(
      col_magenta(
        "Arsenic (As_EPA3051) is not detectable below 3.0 mg/kg. Cadmium (Cd_EPA3051) is not detectable below 0.2 mg/kg."
      )
    )
    cli_alert(col_cyan("See the data dictionary by typing {.code ?BioDIGS_soil_data()}."))
    cli_alert(col_cyan("Visit us at {.url https://biodigs.org/}"))
  }

  return(testing_data_)
}
