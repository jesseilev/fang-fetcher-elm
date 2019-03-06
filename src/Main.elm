module Main exposing (..)

import Browser
import Time
import Http
import Json.Decode as Json exposing (Decoder)
import Html as Html exposing (Html)
import Element as El exposing (Element)
import Element.Input as Input
import Element.Events as Events
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import List.Extra as ListEx
import NumberSuffix as Number


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> initApp
        , subscriptions = subscriptions
        , update = update
        , view =
            \model ->
                { title = "FANG FETCHER"
                , body = [ viewRoot model ]
                }
        }



-- MODEL


type alias Model =
    { selectedCompany : String
    , companies : List Company
    }


type alias Company =
    { repos : Repos
    , companyName : String
    , githubOrgName : String
    }


type Repos
    = Loading Int
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
        [ initCompany "Facebook" "facebook"
        , initCompany "Amazon" "amzn"
        , initCompany "Netflix" "netflix"
        , initCompany "Google" "google"
        ]
        --Dict.fromList
        --    [ ( "Facebook", initCompany "Facebook" "facebook" )
        --    , ( "Amazon", initCompany "Amazon" "amzn" )
        --    , ( "Netflix", initCompany "Netflix" "netflix" )
        --    , ( "Google", initCompany "Google" "google" )
        --    ]
    }
        |> withNoCmds


initCompany : String -> String -> Company
initCompany =
    Company NotLoaded


isFetching : Model -> Bool
isFetching model =
    let
        companyIsLoading c =
            case c.repos of
                Loading _ ->
                    True

                _ ->
                    False
    in
        List.any companyIsLoading model.companies



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if isFetching model then
        Time.every 250 LoadingTick
    else
        Sub.none



-- UPDATE


type Msg
    = NoOp
    | SelectCompany String
    | RequestRepos String
    | GotRepos String (Result Http.Error (List Repo))
    | LoadingTick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectCompany companyName ->
            { model | selectedCompany = companyName }
                |> withNoCmds

        RequestRepos githubOrgName ->
            let
                updateReposInCompany company =
                    if company.githubOrgName == githubOrgName then
                        { company | repos = Loading 0 }
                    else
                        company
            in
                ( { model | companies = List.map updateReposInCompany model.companies }
                , Http.get
                    { url = githubUrl githubOrgName
                    , expect = Http.expectJson (GotRepos githubOrgName) reposDecoder
                    }
                )

        GotRepos githubOrgName result ->
            let
                top5ReposResult =
                    Result.map (List.take 18) result

                updateReposInCompany company =
                    if company.githubOrgName == githubOrgName then
                        { company | repos = Loaded top5ReposResult }
                    else
                        company
            in
                { model | companies = List.map updateReposInCompany model.companies }
                    |> withNoCmds

        LoadingTick _ ->
            let
                incrementTick company =
                    case company.repos of
                        Loading ticksSinceLoadStart ->
                            { company | repos = Loading <| ticksSinceLoadStart + 1 }

                        _ ->
                            company
            in
                { model | companies = List.map incrementTick model.companies }
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



-- STYLES


fontScale =
    round << El.modular 18 1.25


colorPalette =
    { darkestGrey = El.rgb 0.05 0.05 0.05
    , fadedPurple = El.rgb 0.19 0.17 0.21
    , darkGrey = El.rgb 0.2 0.2 0.2
    , white = El.rgb 1 1 1
    , lightGrey = El.rgb 0.7 0.7 0.7
    , grey = El.rgb 0.4 0.4 0.4
    , red = El.rgb 0.99 0.1 0.2
    , clear = El.rgba 0 0 0 0
    }


styles =
    { root =
        [ Font.size <| fontScale 1
        , Font.color colorPalette.fadedPurple
        , Font.family
            [ Font.external
                { name = "Laila"
                , url = "https://fonts.googleapis.com/css?family=Laila"
                }
            ]
        , Background.color colorPalette.darkestGrey
        ]
    , header =
        [ Background.color <| colorPalette.fadedPurple
        , Font.size <| fontScale 3
        , Font.color colorPalette.grey
        ]
    , headerSubtitle =
        [ Font.size <| fontScale 0
        , Font.color colorPalette.darkestGrey
        , Font.center
        ]
    , footer =
        [ Background.color colorPalette.fadedPurple
        , Font.color colorPalette.grey
        , Font.size <| fontScale -1
        , Border.color <| colorPalette.darkestGrey
        ]
    , footerLink =
        [ El.mouseOver [ Font.color colorPalette.lightGrey ] ]
    , main =
        [ Border.color <| colorPalette.fadedPurple
        , Border.width 3
        ]
    , selector = [ El.pointer ]
    , selectorOption =
        [ Border.color colorPalette.fadedPurple
        , Font.color colorPalette.grey
        , El.mouseOver
            [ Font.color <| El.rgb 0.6 0.6 0.6
            ]
        , El.focused [ Font.color colorPalette.red ]
        , El.mouseDown [ Font.color colorPalette.red ]
        ]
    , selectorOptionAcronymLetter =
        [ Font.color colorPalette.lightGrey ]
    , button =
        [ Font.color colorPalette.grey
        , Border.color colorPalette.clear
        , Border.rounded 2
        , Border.shadow
            { offset = ( 1, -1 )
            , size = -1
            , blur = 1
            , color = colorPalette.fadedPurple
            }
        , El.mouseOver
            [ Font.color colorPalette.lightGrey
            , El.moveDown 1
            , El.moveLeft 1
            , Border.shadow
                { offset = ( 2, -2 )
                , size = -2
                , blur = 1
                , color = colorPalette.fadedPurple
                }
            ]
        , El.focused [ Font.color colorPalette.red ]
        , El.mouseDown [ Font.color colorPalette.red ]
        ]
    , company =
        [ Background.color colorPalette.darkestGrey ]
    , repo =
        [ Background.color colorPalette.darkestGrey
        , Border.color colorPalette.fadedPurple
        , Border.width 0
        , Border.rounded 2
        , Border.shadow
            { offset = ( 1, -1 )
            , size = -1
            , blur = 1
            , color = colorPalette.fadedPurple
            }
        , El.mouseOver
            [ El.moveDown 1
            , El.moveLeft 1
            , Border.shadow
                { offset = ( 2, -2 )
                , size = -2
                , blur = 1
                , color = colorPalette.fadedPurple
                }
            ]
        ]
    , repoLink =
        [ Font.color colorPalette.lightGrey
        , Font.extraLight
        , Font.size <| fontScale 0
        , El.mouseOver [ Font.color colorPalette.red ]
        ]
    , repoDescription =
        [ Font.size <| fontScale -1
        , Font.light
        , Font.color colorPalette.grey
        ]
    , repoStarsCount =
        [ Font.color colorPalette.fadedPurple
        , Font.size <| fontScale 0
        ]
    }



-- VIEW


spaceScale =
    round << El.modular 12 1.5


viewRoot : Model -> Html Msg
viewRoot model =
    El.layoutWith
        { options =
            [ El.focusStyle
                { borderColor = Just colorPalette.red
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        (styles.root ++ [ El.height El.fill, El.scrollbarY ])
        <| El.column
            [ El.width El.fill
            , El.height El.fill
            , El.spacing <| spaceScale 4
            ]
            [ viewHeader
            , viewMain model
            , viewFooter
            ]


viewHeader : Element Msg
viewHeader =
    El.column
        (styles.header
            ++ [ El.padding <| spaceScale 4
               , El.width <| El.fill
               , El.spacing <| spaceScale 2
               ]
        )
        [ El.el
            [ El.centerX
            , El.centerY
            ]
            <| El.text "F A N G   F E T C H E R"
        , El.paragraph (styles.headerSubtitle ++ [ El.centerX, El.centerY ])
            [ El.text
                <| "Sink your teeth into the repositories "
                ++ "of the biggest, most red-blooded companies in tech."
            ]
        ]


viewFooter : Element Msg
viewFooter =
    El.row
        (styles.footer
            ++ [ El.width El.fill
               , El.padding <| spaceScale 1
               , El.spacing <| spaceScale 2
               , El.alignBottom
               ]
        )
        [ El.link (styles.footerLink ++ [ El.alignRight ])
            { label = El.text "View source on Github"
            , url = "https://github.com/jesseilev/fang-fetcher-elm"
            }
        , El.text "|"
        , El.link (styles.footerLink ++ [ El.alignRight ])
            { label = El.text "What's the deal with FANG?"
            , url = "https://www.investopedia.com/terms/f/fang-stocks-fb-amzn.asp"
            }
        ]


viewVampire : Int -> Element Msg
viewVampire dripCount =
    let
        viewLine color lineContent =
            El.el [ Font.color color ]
                <| El.text lineContent
    in
        El.column
            [ El.alignBottom
            , Font.size <| fontScale 1
            , Font.bold
            ]
            <| (viewLine colorPalette.lightGrey asciiVampire)
            :: List.map (viewLine colorPalette.red)
                (List.repeat dripCount asciiBloodDrip)


viewMain : Model -> Element Msg
viewMain model =
    El.column
        (styles.main
            ++ [ El.centerX
               , El.width (El.fill |> El.minimum 300 |> El.maximum 950)
               , El.height El.fill
               ]
        )
        [ viewSelector model
        , ListEx.find (\c -> c.companyName == model.selectedCompany) model.companies
            |> Maybe.map viewCompany
            |> Maybe.withDefault El.none
        ]


viewSelector : Model -> Element Msg
viewSelector model =
    let
        viewCompanyOption c =
            viewOption c.companyName
                (String.toUpper c.companyName)
                (c.companyName == model.selectedCompany)
    in
        El.row (styles.selector ++ [ El.width El.fill, El.spaceEvenly ])
            <| List.map viewCompanyOption model.companies


viewOption : String -> String -> Bool -> Element Msg
viewOption key title isSelected =
    El.el
        (styles.selectorOption
            ++ [ El.padding <| spaceScale 2
               , El.width <| El.fillPortion 1
               , Events.onClick <| SelectCompany key
               , Background.color
                    <| if isSelected then
                        colorPalette.darkestGrey
                       else
                        colorPalette.fadedPurple
               ]
        )
        <| El.row []
            [ El.el styles.selectorOptionAcronymLetter
                <| El.text (String.left 1 title)
            , El.text <| String.dropLeft 1 title
            ]


viewCompany : Company -> Element Msg
viewCompany company =
    El.column
        (styles.company
            ++ [ El.width El.fill
               , El.height El.fill
               , El.height El.fill
               ]
        )
        [ viewRepos company
        ]


viewRepos : Company -> Element Msg
viewRepos company =
    let
        viewNotLoaded maybeError =
            El.column
                [ El.centerX
                , El.centerY
                , El.width El.fill
                , El.height El.fill
                ]
                [ El.text <| Maybe.withDefault "" maybeError
                , Input.button
                    (styles.button
                        ++ [ El.centerX
                           , El.centerY
                           , El.padding <| spaceScale 4
                           ]
                    )
                    { onPress = Just <| RequestRepos company.githubOrgName
                    , label =
                        El.el [ El.centerX ]
                            <| El.text ("Fetch repos from " ++ company.companyName)
                    }
                ]

        viewLoading ticksSinceLoadStart =
            El.el
                [ El.centerX
                , El.paddingEach
                    { top = spaceScale 6
                    , bottom = 0
                    , left = 0
                    , right = 0
                    }
                ]
                (viewVampire ticksSinceLoadStart)
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
                                , El.spacing <| spaceScale 4
                                , El.padding <| spaceScale 4
                                ]
                                <| List.map viewRepo repos

                        Err error ->
                            viewNotLoaded (Just "Hmmm, something went wrong. Try again?")

                NotLoaded ->
                    viewNotLoaded Nothing

                Loading ticksSinceLoadStart ->
                    viewLoading ticksSinceLoadStart


viewRepo : Repo -> Element Msg
viewRepo repo =
    let
        viewDescription description =
            El.paragraph styles.repoDescription
                [ El.text description ]
    in
        El.column
            (styles.repo
                ++ [ El.width (El.fill |> El.minimum 250 |> El.maximum 500)
                   , El.spacing <| spaceScale 1
                   , El.padding <| spaceScale 3
                   , El.clipX
                   , El.scrollbarX
                   , Font.alignLeft
                   ]
            )
            [ El.link styles.repoLink
                { label = El.text repo.repoName
                , url = repo.htmlUrl
                }
            , El.el styles.repoStarsCount
                <| El.text ("★ " ++ prettyInt repo.stars)
            , Maybe.map viewDescription repo.description
                |> Maybe.withDefault El.none
            ]


asciiVampire =
    "(^,..,^)"


asciiBloodDrip =
    "    '"



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
