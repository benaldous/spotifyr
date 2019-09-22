#' Search Spotify
#' 
#' @param query Search query. Supports track:, artist:, etc.
#' @param type Result type--one of track, artist, album, playlist
#' @param limit Number of results to return (max 50)
#' 
#' @import httr purrr dplyr stringr
#' 
#' @export

search_spotify = function(query,type = "track",limit = 1) {
  authorize_app()
  response = RETRY("GET",
                   "https://api.spotify.com/v1/search",
                   query = list(q = query,
                                market = "US",
                                type = type,
                                limit = limit,
                                access_token = pkg.env[["app_token"]]))
  stop_for_status(response)
  content = content(response)
  if(content[[1]][["total"]] == 0) return(invisible())
  items = content[[1]][["items"]] %>%
    transpose() %>%
    as_tibble() %>%
    mutate_if(names(.) %in% c("name","id","album_type"),flatten_chr) %>%
    mutate_if(names(.) %in% c("popularity","total_tracks"),flatten_int)
  if(type == "track") results = transmute(items,
                                          Artist = map_depth(artists,2,"name") %>%
                                            map_chr(str_c,collapse = ", "),
                                          Title = name,
                                          Album = map_chr(album,"name"),
                                          Popularity = popularity,
                                          ID = id)
  if(type == "artist") results = transmute(items,
                                           Artist = name,
                                           Popularity = popularity,
                                           ArtistID = id)
  if(type == "album") results = transmute(items,
                                          AlbumArtist = map_depth(artists,2,"name") %>%
                                            map_chr(str_c,collapse = ", "),
                                          Album = name,
                                          AlbumType = album_type,
                                          Tracks = total_tracks,
                                          AlbumID = id)
  if(type == "playlist") results = transmute(items,
                                             Playlist = name,
                                             Owner = map_chr(owner,"id"),
                                             Tracks = map_int(tracks,"total"),
                                             PlaylistID = id)
  return(results)
}
