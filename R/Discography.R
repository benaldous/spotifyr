#' Get a Discography
#' 
#' @param id ID of artist
#' 
#' @import httr stringr
#' @importFrom lubridate ymd
#' 
#' @export

get_discography = function(id) {
  authorize_app()
  albums = list()
  endpoint = str_glue("https://api.spotify.com/v1/artists/{id}/albums")
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
      mutate_at(vars(name,album_group,release_date,id),flatten_chr)
    artists = transpose(map(items[["artists"]],transpose))
    results = transmute(items,
                        AlbumArtist = map_depth(artists,2,"name") %>%
                          map_chr(str_c,collapse = ", "),
                        Album = name,
                        AlbumGroup = album_group,
                        Released = release_date,
                        AlbumID = id)
    offset = content[["offset"]] + 1
    albums[[offset]] = results
    endpoint = content[["next"]]
  }
  albums = bind_rows(albums)
  return(albums)
}
