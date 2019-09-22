#' Get Album Tracks
#' 
#' @param id ID of album
#' 
#' @import httr stringr dplyr purrr
#' 
#' @export

get_album_tracks = function(id) {
  authorize_app()
  tracks = list()
  endpoint = str_glue("https://api.spotify.com/v1/albums/{id}/tracks")
  while(! is.null(endpoint)) {
    response = RETRY("GET",
                     endpoint,
                     query = list(market = "US",
                                  limit = 50,
                                  access_token = pkg.env[["app_token"]]))
    stop_for_status(response)
    content = content(response)
    items = content[["items"]] %>%
      transpose() %>%
      as_tibble() %>%
      mutate_at(vars(name,id),flatten_chr) %>%
      mutate_at(vars(disc_number,track_number),flatten_int) %>%
      mutate_at(vars(explicit),flatten_lgl)
    results = transmute(items,
                        Track = disc_number*100L + track_number,
                        Artist = map_depth(artists,2,"name") %>% map_chr(str_c,collapse = ", "),
                        Title = name,
                        Explicit = explicit,
                        ID = id)
    offset = content[["offset"]] + 1
    tracks[[offset]] = results
    endpoint = content[["next"]]
  }
  tracks = bind_rows(tracks)
  return(tracks)
}
