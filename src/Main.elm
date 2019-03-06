module Main exposing (..)

import Browser
import Browser.Events
import Browser.Dom
import Task
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
    , windowSize : WindowSize
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


type alias WindowSize =
    { width : Int
    , height : Int
    }


initApp : ( Model, Cmd Msg )
initApp =
    ( { selectedCompany = "Facebook"
      , companies =
            [ initCompany "Facebook" "facebook"
            , initCompany "Amazon" "amzn"
            , initCompany "Netflix" "netflix"
            , initCompany "Google" "google"
            ]
      , windowSize = WindowSize 0 0
      }
    , Task.perform windowResizeFromViewport
        Browser.Dom.getViewport
    )


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


windowResizeFromViewport : Browser.Dom.Viewport -> Msg
windowResizeFromViewport { viewport } =
    WindowResize (round viewport.width) (round viewport.height)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize WindowResize
        , if isFetching model then
            Time.every 250 LoadingTick
          else
            Sub.none
        ]



-- UPDATE


type Msg
    = NoOp
    | SelectCompany String
    | RequestRepos String
    | GotRepos String (Result Http.Error (List Repo))
    | LoadingTick Time.Posix
    | WindowResize Int Int


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

        WindowResize w h ->
            { model | windowSize = { width = w, height = h } }
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


deviceIsPhonePortrait windowSize =
    let
        device =
            El.classifyDevice windowSize
    in
        case ( device.class, device.orientation ) of
            ( El.Phone, El.Portrait ) ->
                True

            _ ->
                False


fontScale windowSize =
    let
        baseFontSize =
            if deviceIsPhonePortrait windowSize then
                15
            else
                17
    in
        round << El.modular baseFontSize 1.25


colorPalette =
    { lighterGrey = El.rgb 0.7 0.7 0.7
    , lightGrey = El.rgb 0.6 0.6 0.6
    , grey = El.rgb 0.4 0.4 0.4
    , darkestGrey = El.rgb 0.05 0.05 0.05
    , fadedPurple = El.rgb 0.19 0.17 0.21
    , red = El.rgb 0.99 0.1 0.2
    }


styles windowSize =
    { root =
        [ Font.size <| fontScale windowSize 1
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
        , Font.size <| fontScale windowSize 3
        , Font.color colorPalette.grey
        ]
    , headerSubtitle =
        [ Font.size <| fontScale windowSize 0
        , Font.color colorPalette.darkestGrey
        , Font.center
        ]
    , footer =
        [ Background.color colorPalette.fadedPurple
        , Font.color colorPalette.grey
        , Font.size <| fontScale windowSize -1
        , Border.color <| colorPalette.darkestGrey
        ]
    , footerLink =
        [ El.mouseOver [ Font.color colorPalette.lighterGrey ] ]
    , main =
        [ Border.color <| colorPalette.fadedPurple
        , Border.width 3
        ]
    , selector = [ El.pointer ]
    , selectorOption =
        [ Border.color colorPalette.fadedPurple
        , Font.color colorPalette.grey
        , Font.size
            <| if deviceIsPhonePortrait windowSize then
                fontScale windowSize -2
               else
                fontScale windowSize -1
        , El.mouseOver
            [ Font.color <| colorPalette.lightGrey
            ]
        , El.focused [ Font.color colorPalette.red ]
        , El.mouseDown [ Font.color colorPalette.red ]
        ]
    , selectorOptionAcronymLetter =
        [ Font.size
            <| if deviceIsPhonePortrait windowSize then
                fontScale windowSize 2
               else
                fontScale windowSize 3
        , El.alignBottom
          --, Font.color colorPalette.lightGrey
        ]
    , button =
        [ Font.color colorPalette.grey
        , Border.rounded 2
        , Border.shadow
            { offset = ( 1, -1 )
            , size = -1
            , blur = 1
            , color = colorPalette.fadedPurple
            }
        , El.mouseOver
            [ Font.color colorPalette.lighterGrey
            , El.moveDown 1
            , El.moveLeft 1
            , Border.shadow
                { offset = ( 1, -1 )
                , size = -2
                , blur = 5
                , color = colorPalette.grey
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
            { offset = ( 2, -2 )
            , size = -2
            , blur = 1
            , color = colorPalette.fadedPurple
            }
        , El.mouseOver
            [ Border.shadow
                { offset = ( 1, -1 )
                , size = -2
                , blur = 5
                , color = colorPalette.grey
                }
            ]
        ]
    , repoLink =
        [ Font.color colorPalette.lighterGrey
        , Font.extraLight
        , Font.size <| fontScale windowSize 0
        , El.mouseOver [ Font.color colorPalette.red ]
        ]
    , repoDescription =
        [ Font.size <| fontScale windowSize -1
        , Font.light
        , Font.color colorPalette.grey
        ]
    , repoStarsCount =
        [ Font.color colorPalette.fadedPurple
        , Font.size <| fontScale windowSize 0
        ]
    }



-- VIEW


spaceScale windowSize =
    let
        baseSpace =
            if deviceIsPhonePortrait windowSize then
                6
            else
                12
    in
        round << El.modular baseSpace 1.5


maxMainWidth =
    1000


minMainWidth =
    320


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
        ((styles model.windowSize).root ++ [ El.height El.fill, El.scrollbarY ])
        <| El.column
            [ El.width El.fill
            , El.height El.fill
            , El.spacing
                <| if maxMainWidth < model.windowSize.width then
                    spaceScale model.windowSize 4
                   else
                    0
            ]
            [ viewHeader model.windowSize
            , viewMain model
            , viewFooter model.windowSize
            ]


viewHeader : WindowSize -> Element Msg
viewHeader windowSize =
    El.column
        ((styles windowSize).header
            ++ [ El.padding <| spaceScale windowSize 4
               , El.width <| El.fill
               , El.spacing <| spaceScale windowSize 2
               ]
        )
        [ El.el
            [ El.centerX
            , El.centerY
            ]
            <| El.text "F A N G   F E T C H E R"
        , El.paragraph ((styles windowSize).headerSubtitle ++ [ El.centerX, El.centerY ])
            [ El.text
                <| "Sink your teeth into the repositories "
                ++ "of the biggest, most red-blooded companies in tech."
            ]
        ]


viewFooter : WindowSize -> Element Msg
viewFooter windowSize =
    El.row
        ((styles windowSize).footer
            ++ [ El.width El.fill
               , El.padding <| spaceScale windowSize 1
               , El.spacing <| spaceScale windowSize 2
               , El.alignBottom
               ]
        )
        [ El.link ((styles windowSize).footerLink ++ [ El.alignRight ])
            { label = El.text "View source on Github"
            , url = "https://github.com/jesseilev/fang-fetcher-elm"
            }
        , El.text "|"
        , El.link ((styles windowSize).footerLink ++ [ El.alignRight ])
            { label = El.text "What's the deal with FANG?"
            , url = "https://www.investopedia.com/terms/f/fang-stocks-fb-amzn.asp"
            }
        ]


viewMain : Model -> Element Msg
viewMain model =
    El.column
        ((styles model.windowSize).main
            ++ [ El.centerX
               , El.width (El.fill |> El.minimum minMainWidth |> El.maximum maxMainWidth)
               , El.height El.fill
               ]
        )
        [ viewSelector model
        , ListEx.find (\c -> c.companyName == model.selectedCompany) model.companies
            |> Maybe.map (viewCompany model.windowSize)
            |> Maybe.withDefault El.none
        ]


viewSelector : Model -> Element Msg
viewSelector model =
    let
        viewCompanyOption c =
            viewOption model.windowSize
                c.companyName
                (String.toUpper c.companyName)
                (c.companyName == model.selectedCompany)
    in
        El.row ((styles model.windowSize).selector ++ [ El.width El.fill, El.spaceEvenly ])
            <| List.map viewCompanyOption model.companies


viewOption : WindowSize -> String -> String -> Bool -> Element Msg
viewOption windowSize key title isSelected =
    El.el
        ((styles windowSize).selectorOption
            ++ [ El.padding <| spaceScale windowSize 2
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
            [ El.el (styles windowSize).selectorOptionAcronymLetter
                <| El.text (String.left 1 title)
            , El.text <| String.dropLeft 1 title
            ]


viewCompany : WindowSize -> Company -> Element Msg
viewCompany windowSize company =
    El.column
        ((styles windowSize).company
            ++ [ El.width El.fill
               , El.height El.fill
               , El.height El.fill
               ]
        )
        [ viewRepos windowSize company
        ]


viewRepos : WindowSize -> Company -> Element Msg
viewRepos windowSize company =
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
                    ((styles windowSize).button
                        ++ [ El.centerX
                           , El.centerY
                           , El.padding <| spaceScale windowSize 4
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
                    { top = spaceScale windowSize 6
                    , bottom = 0
                    , left = 0
                    , right = 0
                    }
                ]
                (viewVampire windowSize ticksSinceLoadStart)
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
                                , El.spacing <| spaceScale windowSize 4
                                , El.padding <| spaceScale windowSize 4
                                ]
                                <| List.map (viewRepo windowSize) repos

                        Err error ->
                            viewNotLoaded (Just "Hmmm, something went wrong. Try again?")

                NotLoaded ->
                    viewNotLoaded Nothing

                Loading ticksSinceLoadStart ->
                    viewLoading ticksSinceLoadStart


viewRepo : WindowSize -> Repo -> Element Msg
viewRepo windowSize repo =
    let
        viewDescription description =
            El.paragraph (styles windowSize).repoDescription
                [ El.text description ]
    in
        El.column
            ((styles windowSize).repo
                ++ [ El.width (El.fill |> El.minimum 220 |> El.maximum 500)
                   , El.spacing <| spaceScale windowSize 1
                   , El.padding <| spaceScale windowSize 3
                   , El.clipX
                   , El.scrollbarX
                   , Font.alignLeft
                   ]
            )
            [ El.link (styles windowSize).repoLink
                { label = El.text repo.repoName
                , url = repo.htmlUrl
                }
            , El.el (styles windowSize).repoStarsCount
                <| El.text ("★ " ++ prettyInt repo.stars)
            , Maybe.map viewDescription repo.description
                |> Maybe.withDefault El.none
            ]


viewVampire : WindowSize -> Int -> Element Msg
viewVampire windowSize dripCount =
    let
        viewLine color lineContent =
            El.el [ Font.color color ]
                <| El.text lineContent
    in
        El.column
            [ El.alignBottom
            , Font.size <| fontScale windowSize 1
            , Font.bold
            ]
            <| (viewLine colorPalette.lighterGrey asciiVampire)
            :: List.map (viewLine colorPalette.red)
                (List.repeat dripCount asciiBloodDrip)


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
            { standardConfig
                | getSuffix = Number.suffixStandardShort
                , minSuffix = 1000
                , sigfigs = 2
            }
    in
        Number.format config << toFloat


withNoCmds : a -> ( a, Cmd msg )
withNoCmds x =
    ( x, Cmd.none )
