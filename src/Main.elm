module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Http
import Json.Decode as Json exposing (Decoder)
import Dict exposing (Dict)
import Company


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> initApp
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { selectedCompany : String
    , companies : Dict String Company
    }


type alias Company =
    { repos : Repos
    , companyName : String
    , githubOrgName : String
    }


type Repos
    = Loading
    | Loaded (Result Http.Error (List Repo))
    | NotLoaded


type alias Repo =
    { repoName : String
    , htmlUrl : String
    , stargazersCount : Int
    , description : Maybe String
    }


initApp : ( Model, Cmd Msg )
initApp =
    { selectedCompany = "Facebook"
    , companies =
        Dict.fromList
            [ ( "Facebook", initCompany "Facebook" "facebook" )
            , ( "Amazon", initCompany "Amazon" "amzn" )
            , ( "Netflix", initCompany "Netflix" "netflix" )
            , ( "Google", initCompany "Google" "google" )
            ]
    }
        |> withNoCmds


initCompany : String -> String -> Company
initCompany =
    Company NotLoaded



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


type Msg
    = NoOp
    | SelectCompany String
    | RequestRepos String
    | GotRepos String (Result Http.Error (List Repo))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectCompany companyName ->
            { model | selectedCompany = companyName }
                |> withNoCmds

        RequestRepos githubOrgName ->
            let
                updateReposInCompany _ company =
                    if company.githubOrgName == githubOrgName then
                        { company | repos = Loading }
                    else
                        company
            in
                ( { model | companies = Dict.map updateReposInCompany model.companies }
                , Http.get
                    { url = githubUrl githubOrgName
                    , expect = Http.expectJson (GotRepos githubOrgName) reposDecoder
                    }
                )

        GotRepos githubOrgName result ->
            let
                top5ReposResult =
                    Result.map (List.take 5) result
                        |> Debug.log "result"

                updateReposInCompany _ company =
                    if company.githubOrgName == githubOrgName then
                        { company | repos = Loaded top5ReposResult }
                    else
                        company
            in
                { model | companies = Dict.map updateReposInCompany model.companies }
                    |> withNoCmds

        _ ->
            model |> withNoCmds



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        [ viewSelector model
        , Maybe.map viewCompany
            (Dict.get model.selectedCompany model.companies)
            |> Maybe.withDefault (Html.text "")
        ]


viewSelector : Model -> Html Msg
viewSelector model =
    let
        viewCompanyOption c =
            viewOption c.companyName
                c.companyName
                (c.companyName == model.selectedCompany)
    in
        Html.div []
            <| List.map viewCompanyOption (Dict.values model.companies)


viewOption : String -> String -> Bool -> Html Msg
viewOption key title isSelected =
    Html.span [ Events.onClick <| SelectCompany key ]
        [ Html.text
            <| if isSelected then
                String.toUpper title
               else
                title
        ]


viewCompany : Company -> Html Msg
viewCompany company =
    Html.div []
        [ Html.text company.githubOrgName
        , viewRepos company
        ]


viewRepos : Company -> Html Msg
viewRepos company =
    let
        viewRepo repo =
            Html.div [] [ Html.text repo.repoName ]

        viewNotLoaded maybeError =
            Html.div []
                [ Html.text <| Maybe.withDefault "" maybeError
                , Html.button
                    [ Events.onClick <| RequestRepos company.githubOrgName
                    ]
                    [ Html.text <| "Load repos from " ++ company.companyName ]
                ]

        viewLoading =
            Html.text "Loading ..."
    in
        Html.div []
            <| case company.repos of
                Loaded result ->
                    case result of
                        Ok repos ->
                            List.map viewRepo repos

                        Err error ->
                            [ viewNotLoaded (Just "Hmm, something went wrong. Try again?") ]

                NotLoaded ->
                    [ viewNotLoaded Nothing ]

                Loading ->
                    [ viewLoading ]



-- HTTP REQUESTS


reposDecoder : Decoder (List Repo)
reposDecoder =
    Json.field "items"
        <| Json.list repoDecoder


repoDecoder : Decoder Repo
repoDecoder =
    Json.map4 Repo
        (Json.field "name" Json.string)
        (Json.field "html_url" Json.string)
        (Json.field "stargazers_count" Json.int)
        (Json.field "description" <| Json.nullable Json.string)


githubUrl : String -> String
githubUrl githubOrgName =
    "https://api.github.com/search/repositories?q=sort:stars+org:"
        ++ githubOrgName


httpErrorToString error =
    case error of
        Http.BadBody str ->
            "BadBody: " ++ str

        _ ->
            "Error!"



-- UTIL


withNoCmds : a -> ( a, Cmd msg )
withNoCmds x =
    ( x, Cmd.none )
