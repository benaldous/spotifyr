#' Manage Playlists
#' 
#' @param user Username of owner
#' @param private Include private playlists?
#' 
#' @import httr stringr dplyr purrr
#' 
#' @export

get_playlists = function(user,private = F) {
  endpoint = str_glue("https://api.spotify.com/v1/users/{user}/playlists")
  playlists = list()
  while(! is.null(endpoint)) {
    if(private) {
      authorize_user()
      response = RETRY("GET",
                       endpoint,
                       config(token = pkg.env[["user_token"]]))
    } else {
      authorize_app()
      response = RETRY("GET",
                       endpoint,
                       query = list(access_token = pkg.env[["app_token"]]))
    }
    stop_for_status(response)
    content = content(response)
    if(content[["total"]] == 0) stop("No playlists")
    items = content[["items"]] %>%
      transpose() %>%
      as_tibble() %>%
      mutate_at(vars(name,id),flatten_chr) %>%
      mutate_at(vars(collaborative,public),flatten_lgl)
    results = transmute(items,
                        Playlist = name,
                        Owner = map_chr(owner,"id"),
                        Collaborative = collaborative,
                        Public = public,
                        Tracks = map_int(tracks,"total"),
                        PlaylistID = id)
    offset = content[["offset"]] + 1
    playlists[[offset]] = results
    endpoint = content[["next"]]
  }
  playlists = bind_rows(playlists)
  return(playlists)
}

#' @describeIn get_playlists
#' 
#' @param playlist ID of playlist
#' 
#' @export

get_playlist_tracks = function(playlist) {
  authorize_app()
  tracks = list()
  endpoint = str_glue("https://api.spotify.com/v1/playlists/{playlist}/tracks")
  while(! is.null(endpoint)) {
    response = RETRY("GET",
                     endpoint,
                     query = list(access_token = pkg.env[["app_token"]]))
    stop_for_status(response)
    content = content(response)
    items = content[["items"]] %>%
      map("track") %>%
      transpose() %>%
      as_tibble() %>%
      mutate_at(vars(name,id),flatten_chr) %>%
      mutate_at(vars(explicit),flatten_lgl) %>%
      mutate_at(vars(popularity),flatten_int)
    results = transmute(items,
                        Artist = map_depth(artists,2,"name") %>%
                          map_chr(str_c,collapse = ", "),
                        Title = name,
                        Explicit = explicit,
                        Album = map_chr(album,"name"),
                        Popularity = popularity,
                        ID = id)
    offset = content[["offset"]] + 1
    tracks[[offset]] = results
    endpoint = content[["next"]]
  }
  tracks = bind_rows(tracks)
  return(tracks)
}

#' @describeIn get_playlists
#' 
#' @param name Name of playlist
#' 
#' @importFrom jsonlite toJSON
#' 
#' @export

add_playlist = function(name) {
  authorize_user()
  endpoint = str_glue("https://api.spotify.com/v1/me/playlists")
  body_args = toJSON(list(name = name,public = F),auto_unbox = T)
  response = RETRY("POST",
                   endpoint,
                   config(token = user_token),
                   body = body_args)
  stop_for_status(response)
  content = content(response)
  id = content[["id"]]
  return(id)
}

#' @describeIn get_playlists
#' 
#' @param ids Vector of track IDs
#' 
#' @export

add_tracks = function(ids,playlist) {
  authorize_user()
  endpoint = str_glue("https://api.spotify.com/v1/playlists/{playlist}/tracks")
  ids = str_c("spotify:track:",ids)
  body_args = toJSON(list(uris = ids))
  response = RETRY("POST",
                   endpoint,
                   config(token = user_token),
                   content_type_json(),
                   body = body_args)
  stop_for_status(response)
  return(invisible())
}
