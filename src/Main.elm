module Main exposing (..)

import Browser
import Html as Html exposing (Html)
import Element as El exposing (Element)
import Element.Input as Input
import Element.Events as Events
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Http
import Json.Decode as Json exposing (Decoder)
import Dict exposing (Dict)
import NumberSuffix as Number


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> initApp
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL


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
    , stars : Int
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
                    Result.map (List.take 30) result
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



-- VIEW


colorPalette =
    { darkPurple = El.rgb 0.17 0.15 0.18
    , darkestGrey = El.rgb 0.04 0.04 0.04
    , darkGrey = El.rgb 0.23 0.23 0.23
    , white = El.rgb 1 1 1
    , lightGrey = El.rgb 0.7 0.7 0.7
    , grey = El.rgb 0.4 0.4 0.4
    , red = El.rgb 0.99 0.25 0.1
    , clear = El.rgba 0 0 0 0
    }


colors =
    { optionSelected = colorPalette.darkPurple
    , optionUnselected = colorPalette.darkPurple
    , color1 =
        colorPalette.lightGrey
        --, color2 = colorPalette.darkPurple
    }


layout =
    { paddingSmall = 8
    , paddingLarge = 16
    }


fontScale =
    round << El.modular 18 1.25


paddingScale =
    round << El.modular 12 1.5


view : Model -> Html Msg
view model =
    El.layoutWith
        { options =
            [ El.focusStyle
                { borderColor = Nothing
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        [ El.height El.fill
        , Font.size <| fontScale 1
        , Font.color colorPalette.darkestGrey
        , Background.color colorPalette.darkPurple
        , El.scrollbarY
        ]
        <| El.column
            [ El.width El.fill
            , El.height El.fill
            , El.spacing <| paddingScale 4
            ]
            [ viewHeader
            , viewMain model
            , viewFooter
            ]


viewHeader : Element Msg
viewHeader =
    El.column
        [ El.paddingXY (paddingScale 3) (paddingScale 3)
        , El.width <| El.fill
        , Background.color <| colorPalette.darkestGrey
        , El.spacing 8
        , Border.widthXY 0 3
        , Border.color colorPalette.darkPurple
        ]
        [ El.el
            [ Font.size <| fontScale 3
            , Font.color colorPalette.grey
            , El.centerX
            , El.centerY
            ]
            <| El.text "FANG Fetcher"
        , El.paragraph
            [ Font.size <| fontScale -1
            , Font.light
            , Font.italic
            , Font.color colorPalette.darkGrey
            , Font.center
            , El.centerX
            , El.centerY
            ]
            [ El.text
                <| "Sink your teeth into the repositories "
                ++ "of tech's most eternally young-blooded companies."
            ]
        ]


viewFooter : Element Msg
viewFooter =
    El.row
        [ El.width El.fill
        , El.padding <| paddingScale 1
        , El.alignBottom
        , Background.color colorPalette.darkestGrey
        , Font.color colorPalette.grey
        , Font.size <| fontScale -1
        ]
        [ El.link []
            { label = El.text " Github"
            , url = "https://github.com/jesseilev/fang-fetcher-elm"
            }
        ]


viewMain : Model -> Element Msg
viewMain model =
    El.column
        [ El.centerX
        , El.width (El.fill |> El.minimum 300 |> El.maximum 900)
        , El.height El.fill
        , Border.color <| colorPalette.darkestGrey
        , Border.width 3
        ]
        [ viewSelector model
        , Maybe.map viewCompany
            (Dict.get model.selectedCompany model.companies)
            |> Maybe.withDefault El.none
        ]


viewSelector : Model -> Element Msg
viewSelector model =
    let
        viewCompanyOption c =
            viewOption c.companyName
                c.companyName
                (c.companyName == model.selectedCompany)
    in
        El.row
            [ El.width El.fill
            , El.spaceEvenly
            , El.pointer
            ]
            <| List.map viewCompanyOption (Dict.values model.companies)


viewOption : String -> String -> Bool -> Element Msg
viewOption key title isSelected =
    El.el
        [ Events.onClick <| SelectCompany key
        , El.padding <| paddingScale 2
        , El.width <| El.fillPortion 1
        , Background.color
            <| if isSelected then
                colorPalette.darkPurple
               else
                colorPalette.darkestGrey
        , Border.color colorPalette.darkestGrey
        , Font.color colorPalette.grey
        , El.mouseOver [ Font.color colorPalette.lightGrey ]
        ]
        <| El.text title


viewCompany : Company -> Element Msg
viewCompany company =
    El.column
        [ El.width El.fill
        , El.height El.fill
        , Background.color colors.optionSelected
        , El.height El.fill
        ]
        [ viewRepos company
        ]


viewRepos : Company -> Element Msg
viewRepos company =
    let
        shadowWithBlur blur =
            { offset = ( 1, 1 )
            , size = 0
            , blur = blur
            , color = colorPalette.darkestGrey
            }

        viewNotLoaded maybeError =
            El.column
                [ El.centerX
                , El.centerY
                , El.width El.fill
                , El.height El.fill
                ]
                [ El.text <| Maybe.withDefault "" maybeError
                , Input.button
                    [ El.centerX
                    , El.centerY
                    , El.padding <| paddingScale 4
                    , Font.color colorPalette.grey
                    , Border.color colorPalette.clear
                    , Border.rounded 2
                    , Border.width 0
                    , Border.shadow <| shadowWithBlur 1
                    , El.mouseOver
                        [ Font.color colorPalette.red
                        , Border.color <| colorPalette.red
                        , Border.shadow <| shadowWithBlur 8
                        ]
                    ]
                    { onPress = Just <| RequestRepos company.githubOrgName
                    , label =
                        El.el [ El.centerX ]
                            <| El.text ("Load repos from " ++ company.companyName)
                    }
                ]

        viewLoading =
            El.el
                [ El.centerX
                , El.centerY
                , Font.color colorPalette.grey
                ]
                <| El.text "One moment please ..."
    in
        El.el
            [ El.width El.fill
            , El.height El.fill
            ]
            <| case company.repos of
                Loaded result ->
                    case result of
                        Ok repos ->
                            El.wrappedRow
                                [ El.spaceEvenly
                                , El.spacing <| paddingScale 3
                                , El.padding <| paddingScale 4
                                ]
                                <| List.map viewRepo repos

                        Err error ->
                            viewNotLoaded (Just "Hmm, something went wrong. Try again?")

                NotLoaded ->
                    viewNotLoaded Nothing

                Loading ->
                    viewLoading


viewRepo : Repo -> Element Msg
viewRepo repo =
    let
        viewDescription description =
            El.paragraph
                [ Font.size <| fontScale -1
                , Font.light
                , Font.color colorPalette.grey
                ]
                [ El.text description ]

        shadowWithBlur blur =
            { offset = ( 1, 1 )
            , size = 0
            , blur = blur
            , color = colorPalette.darkestGrey
            }
    in
        El.column
            [ El.width (El.fill |> El.minimum 250 |> El.maximum 500)
            , El.spacing <| paddingScale 1
            , El.padding <| paddingScale 3
            , El.clipX
            , El.scrollbarX
            , Background.color colorPalette.darkPurple
            , Border.color colorPalette.darkestGrey
            , Border.width 0
            , Border.rounded 2
            , Border.shadow <| shadowWithBlur 1
            , El.mouseOver [ Border.shadow <| shadowWithBlur 8 ]
            ]
            [ El.link
                [ Font.color colorPalette.lightGrey
                , Font.extraLight
                , Font.size <| fontScale 0
                , El.mouseOver [ Font.color colorPalette.red ]
                ]
                { label = El.text repo.repoName
                , url = repo.htmlUrl
                }
            , El.el
                [ Font.color colorPalette.darkestGrey
                , Font.size <| fontScale 0
                ]
                <| El.text ("★ " ++ prettyInt repo.stars)
            , Maybe.map viewDescription repo.description
                |> Maybe.withDefault El.none
            ]



-- UTIL


prettyInt : Int -> String
prettyInt =
    let
        standardConfig =
            Number.standardConfig

        config =
            { standardConfig | getSuffix = Number.suffixStandardShort }
    in
        Number.format config << toFloat


withNoCmds : a -> ( a, Cmd msg )
withNoCmds x =
    ( x, Cmd.none )
