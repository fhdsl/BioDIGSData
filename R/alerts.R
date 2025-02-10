#' BioDIGS CLI alert
#'
#' @description Tell users to see metadata, go to the BioDIGS website, etc.
#'
#' @returns A command-line alert
#' @import cli
#' @export
#' @keywords internal
#'
#' @examples
#' BioDIGS_note("?BioDIGS_soil_data()")
#' BioDIGS_note(website = T)
BioDIGS_note <- function(x = NULL, website = F) {
  if (website) {
    thealert <- cli_alert(col_cyan(
      "Visit us at ",
      style_hyperlink("https://biodigs.org/", "https://biodigs.org/"),
      "."
    ))
  } else {
    thealert <- cli_alert(col_cyan(
      "See the data dictionary by typing ",
      code_highlight(x),
      "."
    ))
  }
  return(thealert)
}


#' BioDIGS Soil CLI warning
#'
#' @description Tell users about important caveat in the measurement of arsenic and cadmium.
#'
#' @returns A command-line alert
#' @import cli
#' @export
#' @keywords internal
#'
#' @examples
#' BioDIGS_soil_warning()
BioDIGS_soil_warning <- function() {
  thealert <-
    cli_alert_warning(
      style_italic(col_yellow(
        "Arsenic (As_EPA3051) is not detectable below 3.0 mg/kg. Cadmium (Cd_EPA3051) is not detectable below 0.2 mg/kg. Observations under these values appear as zeros in the dataset."
      ))
    )
  return(thealert)
}
