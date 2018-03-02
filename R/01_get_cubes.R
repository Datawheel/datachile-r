#' Obtain available cubes
#' @export
#' @keywords functions

get_cubes <- function() {
  ua <- user_agent("httr")

  path <- "https://chilecube.datawheel.us/cubes"

  url <- modify_url("https://chilecube.datawheel.us/cubes")

  resp <- GET(url, ua)

  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- fromJSON(content(resp, "text"), simplifyVector = TRUE)

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

  parsed <- tibble(cube = parsed$cubes$name) %>% distinct()

  structure(
    list(
      cubes = parsed,
      path = path,
      response = resp
    ),
    class = "datachile_api"
  )
}
