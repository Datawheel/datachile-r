#' Obtain cube's details
#' @export
#' @param path Must be a valid cube name that can be accesed by running `get_cubes()`
#' @keywords functions

get_cube_info <- function(path) {
  ua <- user_agent("httr")

  path <- paste0("cubes/", path)

  url <- modify_url("https://chilecube.datawheel.us", path = path)

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

  parsed$hierarchies <- parsed$dimensions$hierarchies

  fix_hierarchies <- function(x) {
    parsed$hierarchies[[x]] %>%
      mutate(levels_name = map(levels, "name"),
             levels_full_name = map(levels, "full_name"),
             levels_depth = map(levels, "depth"),
             levels_caption = map(levels, "caption")) %>%
      select(-levels) %>%
      unnest_(.drop = F) %>%
      as_tibble()
  }

  parsed$hierarchies <- lapply(1:length(parsed$hierarchies), fix_hierarchies)

  parsed$dimensions <- tibble(
    name = parsed$dimensions$name,
    caption = parsed$dimensions$caption,
    type = parsed$dimensions$type,
    index_as = parsed$dimensions$annotations$index_as,
    index_max_depth = parsed$dimensions$annotations$index_max_depth
  )

  parsed$measures <- tibble(
    name = parsed$measures$name,
    caption = parsed$measures$caption,
    full_name = parsed$measures$full_name,
    aggregator = parsed$measures$aggregator
  )

  structure(
    list(
      cube = parsed,
      path = path,
      response = resp
    ),
    class = "datachile_api"
  )
}
