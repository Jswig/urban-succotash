module Shared exposing (Data, Model, Msg(..), SharedMsg(..), template)

import Browser.Navigation
import DataSource
import Html exposing (Html)
import Pages.Flags
import Pages.PageUrl exposing (PageUrl)
import Path exposing (Path)
import Route exposing (Route)
import SharedTemplate exposing (SharedTemplate)
import String
import Time
import View exposing (View)


template : SharedTemplate Msg Model Data msg
template =
    { init = init
    , update = update
    , view = view
    , data = data
    , subscriptions = subscriptions
    , onPageChange = Just OnPageChange
    }


type Msg
    = OnPageChange
        { path : Path
        , query : Maybe String
        , fragment : Maybe String
        }
    | SharedMsg SharedMsg
    | Tick Time.Posix


type alias Data =
    ()


type SharedMsg =
    NoOp


type alias Model =
    { showMobileMenu : Bool
    , currentTime : Time.Posix
    }


init :
    Maybe Browser.Navigation.Key
    -> Pages.Flags.Flags
    ->
        Maybe
            { path :
                { path : Path
                , query : Maybe String
                , fragment : Maybe String
                }
            , metadata : route
            , pageUrl : Maybe PageUrl
            }
    -> ( Model, Cmd Msg )
init navigationKey flags maybePagePath =
    ( { showMobileMenu = False

      -- initial timestamp corresponds to March 12th, 2023
      , currentTime = Time.millisToPosix 1678663839000
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnPageChange _ ->
            ( { model | showMobileMenu = False }, Cmd.none )

        Tick newTime ->
            ( { model | currentTime = newTime }
            , Cmd.none
            )
        
        SharedMsg _ ->
            ( model, Cmd.none )


subscriptions : Path -> Model -> Sub Msg
subscriptions _ _ =
    Time.every 1000 Tick


data : DataSource.DataSource Data
data =
    DataSource.succeed ()


header : Html msg
header =
    Html.header [] [ Html.text "Anders Poirel" ]


footer : Time.Posix -> Html msg
footer currentTime =
    let
        year =
            String.fromInt (Time.toYear Time.utc currentTime)
    in
    Html.footer [] [ Html.text ("©" ++ year ++ " Anders Poirel") ]


view :
    Data
    ->
        { path : Path
        , route : Maybe Route
        }
    -> Model
    -> (Msg -> msg)
    -> View msg
    -> { body : Html msg, title : String }
view sharedData page model toMsg pageView =
    let
        bodyContent =
            [ header
            , Html.div [] pageView.body
            , footer model.currentTime
            ]
    in
    { body = Html.div [] bodyContent
    , title = pageView.title
    }
