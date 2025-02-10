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
