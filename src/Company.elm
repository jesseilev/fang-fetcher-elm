module Company exposing (Model, init)

import Repos


type alias Model =
    { repos : Repos.Model
    , companyName : String
    , githubName : String
    }


init : String -> String -> Model
init =
    Model Repos.init
