#' Get Your Top Tracks
#'
#' @param range Timespan to cover: long_term (years), medium_term (6 months), short_term (4 weeks)
#'
#' @import httr stringr dplyr purrr
#'
#' @export

get_top_tracks = function(range = "medium_term") {
  authorize_user()
  endpoint = str_glue("https://api.spotify.com/v1/me/top/tracks")
  response = RETRY("GET",
                   endpoint,
                   query = lst(limit = 50,
                               time_range = range),
                   config(token = pkg.env[["user_token"]]))
  stop_for_status(response)
  content = content(response)
  items = content[["items"]] %>%
    transpose() %>%
    as_tibble() %>%
    transmute(Artist = map_depth(artists,2,"name") %>% map_chr(str_c,collapse = ", "),
              Title = flatten_chr(name),
              Album = map_chr(album,"name"),
              Id = flatten_chr(id))
  return(items)
}
