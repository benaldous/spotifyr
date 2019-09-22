#' Get Tokens
#' 
#' @import httr
#' 
#' @export

authorize_user = function() {
  if(! is.null(pkg.env[["user_token"]])) return(invisible())
  endpoint = oauth_endpoint(authorize = "https://accounts.spotify.com/authorize", 
                            access = "https://accounts.spotify.com/api/token")
  app = oauth_app("Spotify",Sys.getenv("spotify_client"),Sys.getenv("spotify_secret"))
  scope = paste("user-library-read",
                "user-library-modify",
                "playlist-read-private",
                "playlist-read-collaborative",
                "playlist-modify-private",
                "playlist-modify-public",
                "user-top-read",
                "user-read-recently-played")
  token = oauth2.0_token(endpoint,app,scope,cache = F)
  assign("user_token",token,pkg.env)
}

#' @describeIn authorize_user
#' 
#' @export

authorize_app = function() {
  if(! is.null(pkg.env[["app_token"]]))
    if(Sys.time() < attr(pkg.env[["app_token"]],"expires")) return(invisible())
  response = RETRY("POST",
                   "https://accounts.spotify.com/api/token",
                   authenticate(Sys.getenv("spotify_client"),
                                Sys.getenv("spotify_secret")),
                   body = list(grant_type = "client_credentials"),
                   encode = "form")
  stop_for_status(response)
  content = content(response)
  token = content[["access_token"]]
  attr(token,"expires") = Sys.time() + content[["expires_in"]]
  assign("app_token",token,pkg.env)
}
