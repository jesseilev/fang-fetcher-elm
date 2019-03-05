module Repos exposing (Model, init)


type Model
    = Loading
    | Loaded (List Repo)
    | NotLoaded


type alias Repo =
    { repoName : String
    , htmlUrl : String
    , stargazersCount : Int
    , description : String
    }


init : Model
init =
    NotLoaded
