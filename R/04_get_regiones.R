#' Obtain geographical units (comunas and regions)
#' @export
#' @keywords functions

get_geo_units <- function() {
  ua <- user_agent("httr")

  url <- "https://chilecube.datachile.io/cubes/exports/dimensions/Geography/levels/Region/members"

  resp <- GET(url, ua)

  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- fromJSON(content(resp, "text"), simplifyVector = TRUE)

  parsed[5] <- NULL; parsed[5] <- NULL

  if (http_error(resp)) {
    stop(
      sprintf(
        "DataChile API request failed [%s]\n%s\n<%s>",
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }

  structure(
    list(
      regiones = parsed,
      path_comunas = url,
      response_comunas = resp
    ),
    class = "datachile_api"
  )
}
