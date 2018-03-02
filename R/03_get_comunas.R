#' Obtain geographical units (comunas)
#' @export
#' @keywords functions

get_comunas <- function() {
  ua <- user_agent("httr")

  url <- "https://chilecube.datachile.io/cubes/exports/dimensions/Geography/levels/Comuna/members"

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
      comunas = parsed,
      path_comunas = url,
      response_comunas = resp
    ),
    class = "datachile_api"
  )
}
