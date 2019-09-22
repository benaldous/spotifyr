#' Get Track Data
#' 
#' @param ids Vector of track IDs
#' 
#' @import httr stringr dplyr purrr
#' 
#' @export

get_features = function(ids) {
  authorize_app()
  ids = str_c(ids,collapse = ",")
  response = RETRY("GET",
                   "https://api.spotify.com/v1/audio-features",
                   query = list(ids = ids,
                                type = "track",
                                access_token = pkg.env[["app_token"]]))
  stop_for_status(response)
  content = content(response)
  audio_features = content[["audio_features"]] %>%
    discard(is.null) %>%
    transpose() %>%
    as_tibble() %>%
    mutate_at(vars(id),flatten_chr) %>%
    mutate_at(vars(danceability:loudness,speechiness:tempo),flatten_dbl) %>%
    mutate_at(vars(mode,duration_ms,time_signature),flatten_int)
  if(nrow(audio_features) == 0) return(list())
  results = transmute(audio_features,
                      ID = id,
                      Energy = energy,
                      Valence = valence,
                      Danceability = danceability,
                      Speechiness = speechiness,
                      Acousticness = acousticness,
                      Instrumentalness = instrumentalness,
                      Liveness = liveness,
                      Loudness = loudness,
                      Tempo = tempo,
                      TimeSignature = time_signature,
                      Key = key,
                      Mode = mode,
                      Duration = duration_ms)
  return(results)
}

#' @describeIn get_features
#' 
#' @export

get_details = function(ids) {
  authorize_app()
  ids = str_c(ids,collapse = ",")
  response = RETRY("GET",
                   "https://api.spotify.com/v1/tracks",
                   query = list(ids = ids,
                                market = "US",
                                access_token = pkg.env[["app_token"]]))
  stop_for_status(response)
  content = content(response)
  items = content[["tracks"]] %>%
    transpose() %>%
    as_tibble() %>%
    mutate_at(vars(id,name),flatten_chr) %>%
    mutate_at(vars(disc_number,track_number,popularity),flatten_int) %>%
    mutate_at(vars(explicit),flatten_lgl)
  results = transmute(items,
                      ID = id,
                      ArtistID = map_depth(artists,2,"id") %>%
                        map_chr(str_c,collapse = ", "),
                      AlbumID = map_chr(album,"id"),
                      Artist = map_depth(artists,2,"name") %>%
                        map_chr(str_c,collapse = ", "),
                      Album = map_chr(album,"name"),
                      Released = map_chr(album,"release_date"),
                      Track = disc_number*100L + track_number,
                      Title = name,
                      Explicit = explicit,
                      Popularity = popularity)
  return(results)
}
