port module TemplateModulesBeta exposing (..)

import Api
import ApiRoute
import Browser.Navigation
import Route exposing (Route)
import View
import Json.Decode
import Json.Encode
import Pages.Flags
import Pages.Internal.Platform
import Pages.Manifest as Manifest
import Shared
import Site
import Head
import Html exposing (Html)
import Pages.Internal.NotFoundReason
import Pages.PageUrl exposing (PageUrl)
import Path exposing (Path)
import Pages.Internal.RoutePattern
import Url
import DataSource exposing (DataSource)
import QueryParams

import Page.About
import Page.Index
import Page.Me


type alias Model =
    { global : Shared.Model
    , page : PageModel
    , current :
        Maybe
            { path :
                { path : Path
                , query : Maybe String
                , fragment : Maybe String
                }
            , metadata : Maybe Route
            , pageUrl : Maybe PageUrl
            }
    }


type PageModel
    = ModelAbout Page.About.Model
    | ModelIndex Page.Index.Model
    | ModelMe Page.Me.Model

    | NotFound




type Msg
    = MsgGlobal Shared.Msg
    | OnPageChange
        { protocol : Url.Protocol
        , host : String
        , port_ : Maybe Int
        , path : Path
        , query : Maybe String
        , fragment : Maybe String
        , metadata : Maybe Route
        }
    | MsgAbout Page.About.Msg
    | MsgIndex Page.Index.Msg
    | MsgMe Page.Me.Msg



type PageData
    = Data404NotFoundPage____
    | DataAbout Page.About.Data
    | DataIndex Page.Index.Data
    | DataMe Page.Me.Data




view :
    { path : Path
    , route : Maybe Route
    }
    -> Maybe PageUrl
    -> Shared.Data
    -> PageData
    ->
        { view : Model -> { title : String, body : Html Msg }
        , head : List Head.Tag
        }
view page maybePageUrl globalData pageData =
    case ( page.route, pageData ) of
        ( Just Route.About, DataAbout data ) ->
                  { view =
                      \model ->
                          case model.page of
                              ModelAbout subModel ->
                                  Page.About.page.view
                                      maybePageUrl
                                      model.global
                                      subModel
                                      { data = data
                                      , sharedData = globalData
                                      , routeParams = {}
                                      , path = page.path
                                      }
                                      |> View.map MsgAbout
                                      |> Shared.template.view globalData page model.global MsgGlobal

                              _ ->
                                  { title = "Model mismatch", body = Html.text <| "Model mismatch" }
                  , head = []
                  }


        ( Just Route.Index, DataIndex data ) ->
                  { view =
                      \model ->
                          case model.page of
                              ModelIndex subModel ->
                                  Page.Index.page.view
                                      maybePageUrl
                                      model.global
                                      subModel
                                      { data = data
                                      , sharedData = globalData
                                      , routeParams = {}
                                      , path = page.path
                                      }
                                      |> View.map MsgIndex
                                      |> Shared.template.view globalData page model.global MsgGlobal

                              _ ->
                                  { title = "Model mismatch", body = Html.text <| "Model mismatch" }
                  , head = []
                  }


        ( Just Route.Me, DataMe data ) ->
                  { view =
                      \model ->
                          case model.page of
                              ModelMe subModel ->
                                  Page.Me.page.view
                                      maybePageUrl
                                      model.global
                                      subModel
                                      { data = data
                                      , sharedData = globalData
                                      , routeParams = {}
                                      , path = page.path
                                      }
                                      |> View.map MsgMe
                                      |> Shared.template.view globalData page model.global MsgGlobal

                              _ ->
                                  { title = "Model mismatch", body = Html.text <| "Model mismatch" }
                  , head = []
                  }

        _ ->
            { head = []
            , view =
                \_ ->
                    { title = "Page not found"
                    , body =
                            Html.div [] 
                            [ Html.text "This page could not be found."
                            ]
                    }

            }



init :
    Maybe Shared.Model
    -> Pages.Flags.Flags
    -> Shared.Data
    -> PageData
    -> Maybe Browser.Navigation.Key
    ->
        Maybe
            { path :
                { path : Path
                , query : Maybe String
                , fragment : Maybe String
                }
            , metadata : Maybe Route
            , pageUrl : Maybe PageUrl
            }
    -> ( Model, Cmd Msg )
init currentGlobalModel userFlags sharedData pageData navigationKey maybePagePath =
    let
        ( sharedModel, globalCmd ) =
            currentGlobalModel |> Maybe.map (\m -> ( m, Cmd.none )) |> Maybe.withDefault (Shared.template.init navigationKey userFlags maybePagePath)

        ( templateModel, templateCmd ) =
            case ( ( Maybe.map2 Tuple.pair (maybePagePath |> Maybe.andThen .metadata) (maybePagePath |> Maybe.map .path) ), pageData ) of
                ( Just ( Route.About, justPath ), DataAbout thisPageData ) ->
                    Page.About.page.init
                        (Maybe.andThen .pageUrl maybePagePath)
                        sharedModel
                        { data = thisPageData
                        , sharedData = sharedData
                        , routeParams = {}
                        , path = justPath.path
                        }
                        |> Tuple.mapBoth ModelAbout (Cmd.map MsgAbout)

                ( Just ( Route.Index, justPath ), DataIndex thisPageData ) ->
                    Page.Index.page.init
                        (Maybe.andThen .pageUrl maybePagePath)
                        sharedModel
                        { data = thisPageData
                        , sharedData = sharedData
                        , routeParams = {}
                        , path = justPath.path
                        }
                        |> Tuple.mapBoth ModelIndex (Cmd.map MsgIndex)

                ( Just ( Route.Me, justPath ), DataMe thisPageData ) ->
                    Page.Me.page.init
                        (Maybe.andThen .pageUrl maybePagePath)
                        sharedModel
                        { data = thisPageData
                        , sharedData = sharedData
                        , routeParams = {}
                        , path = justPath.path
                        }
                        |> Tuple.mapBoth ModelMe (Cmd.map MsgMe)

                _ ->
                    ( NotFound, Cmd.none )
    in
    ( { global = sharedModel
      , page = templateModel
      , current = maybePagePath
      }
    , Cmd.batch
        [ templateCmd
        , globalCmd |> Cmd.map MsgGlobal
        ]
    )



update : Shared.Data -> PageData -> Maybe Browser.Navigation.Key -> Msg -> Model -> ( Model, Cmd Msg )
update sharedData pageData navigationKey msg model =
    case msg of
        MsgGlobal msg_ ->
            let
                ( sharedModel, globalCmd ) =
                    Shared.template.update msg_ model.global
            in
            ( { model | global = sharedModel }
            , globalCmd |> Cmd.map MsgGlobal
            )

        OnPageChange record ->
            (init (Just model.global) Pages.Flags.PreRenderFlags sharedData pageData navigationKey <|
                Just
                    { path =
                        { path = record.path
                        , query = record.query
                        , fragment = record.fragment
                        }
                    , metadata = record.metadata
                    , pageUrl =
                        Just
                            { protocol = record.protocol
                            , host = record.host
                            , port_ = record.port_
                            , path = record.path
                            , query = record.query |> Maybe.map QueryParams.fromString
                            , fragment = record.fragment
                            }
                    }
            )
                |> (\( updatedModel, cmd ) ->
                        case Shared.template.onPageChange of
                            Nothing ->
                                ( updatedModel, cmd )

                            Just thingy ->
                                let
                                    ( updatedGlobalModel, globalCmd ) =
                                        Shared.template.update
                                            (thingy
                                                { path = record.path
                                                , query = record.query
                                                , fragment = record.fragment
                                                }
                                            )
                                            model.global
                                in
                                ( { updatedModel
                                    | global = updatedGlobalModel
                                  }
                                , Cmd.batch [ cmd, Cmd.map MsgGlobal globalCmd ]
                                )
                   )


        
        MsgAbout msg_ ->
            let
                ( updatedPageModel, pageCmd, ( newGlobalModel, newGlobalCmd ) ) =
                    case ( model.page, pageData, Maybe.map3 (\a b c -> ( a, b, c )) (model.current |> Maybe.andThen .metadata) (model.current |> Maybe.andThen .pageUrl) (model.current |> Maybe.map .path) ) of
                        ( ModelAbout pageModel, DataAbout thisPageData, Just ( Route.About, pageUrl, justPage ) ) ->
                            Page.About.page.update
                                pageUrl
                                { data = thisPageData
                                , sharedData = sharedData
                                , routeParams = {}
                                , path = justPage.path
                                }
                                navigationKey
                                msg_
                                pageModel
                                model.global
                                |> mapBoth ModelAbout (Cmd.map MsgAbout)
                                |> (\( a, b, c ) ->
                                        case c of
                                            Just sharedMsg ->
                                                ( a, b, Shared.template.update sharedMsg model.global )

                                            Nothing ->
                                                ( a, b, ( model.global, Cmd.none ) )
                                   )

                        _ ->
                            ( model.page, Cmd.none, ( model.global, Cmd.none ) )
            in
            ( { model | page = updatedPageModel, global = newGlobalModel }
            , Cmd.batch [ pageCmd, newGlobalCmd |> Cmd.map MsgGlobal ]
            )

        
        MsgIndex msg_ ->
            let
                ( updatedPageModel, pageCmd, ( newGlobalModel, newGlobalCmd ) ) =
                    case ( model.page, pageData, Maybe.map3 (\a b c -> ( a, b, c )) (model.current |> Maybe.andThen .metadata) (model.current |> Maybe.andThen .pageUrl) (model.current |> Maybe.map .path) ) of
                        ( ModelIndex pageModel, DataIndex thisPageData, Just ( Route.Index, pageUrl, justPage ) ) ->
                            Page.Index.page.update
                                pageUrl
                                { data = thisPageData
                                , sharedData = sharedData
                                , routeParams = {}
                                , path = justPage.path
                                }
                                navigationKey
                                msg_
                                pageModel
                                model.global
                                |> mapBoth ModelIndex (Cmd.map MsgIndex)
                                |> (\( a, b, c ) ->
                                        case c of
                                            Just sharedMsg ->
                                                ( a, b, Shared.template.update sharedMsg model.global )

                                            Nothing ->
                                                ( a, b, ( model.global, Cmd.none ) )
                                   )

                        _ ->
                            ( model.page, Cmd.none, ( model.global, Cmd.none ) )
            in
            ( { model | page = updatedPageModel, global = newGlobalModel }
            , Cmd.batch [ pageCmd, newGlobalCmd |> Cmd.map MsgGlobal ]
            )

        
        MsgMe msg_ ->
            let
                ( updatedPageModel, pageCmd, ( newGlobalModel, newGlobalCmd ) ) =
                    case ( model.page, pageData, Maybe.map3 (\a b c -> ( a, b, c )) (model.current |> Maybe.andThen .metadata) (model.current |> Maybe.andThen .pageUrl) (model.current |> Maybe.map .path) ) of
                        ( ModelMe pageModel, DataMe thisPageData, Just ( Route.Me, pageUrl, justPage ) ) ->
                            Page.Me.page.update
                                pageUrl
                                { data = thisPageData
                                , sharedData = sharedData
                                , routeParams = {}
                                , path = justPage.path
                                }
                                navigationKey
                                msg_
                                pageModel
                                model.global
                                |> mapBoth ModelMe (Cmd.map MsgMe)
                                |> (\( a, b, c ) ->
                                        case c of
                                            Just sharedMsg ->
                                                ( a, b, Shared.template.update sharedMsg model.global )

                                            Nothing ->
                                                ( a, b, ( model.global, Cmd.none ) )
                                   )

                        _ ->
                            ( model.page, Cmd.none, ( model.global, Cmd.none ) )
            in
            ( { model | page = updatedPageModel, global = newGlobalModel }
            , Cmd.batch [ pageCmd, newGlobalCmd |> Cmd.map MsgGlobal ]
            )



type alias SiteConfig =
    { canonicalUrl : String
    , manifest : Manifest.Config
    }

templateSubscriptions : Maybe Route -> Path -> Model -> Sub Msg
templateSubscriptions route path model =
    case ( model.page, route ) of
        
        ( ModelAbout templateModel, Just Route.About ) ->
            Page.About.page.subscriptions
                Nothing -- TODO wire through value
                {}
                path
                templateModel
                model.global
                |> Sub.map MsgAbout

        
        ( ModelIndex templateModel, Just Route.Index ) ->
            Page.Index.page.subscriptions
                Nothing -- TODO wire through value
                {}
                path
                templateModel
                model.global
                |> Sub.map MsgIndex

        
        ( ModelMe templateModel, Just Route.Me ) ->
            Page.Me.page.subscriptions
                Nothing -- TODO wire through value
                {}
                path
                templateModel
                model.global
                |> Sub.map MsgMe



        _ ->
            Sub.none


main : Pages.Internal.Platform.Program Model Msg PageData Shared.Data
main =
    Pages.Internal.Platform.application
        { init = init Nothing
        , urlToRoute = Route.urlToRoute
        , routeToPath = \route -> route |> Maybe.map Route.routeToPath |> Maybe.withDefault []
        , site = Nothing
        , getStaticRoutes = DataSource.succeed []
        , handleRoute = handleRoute
        , view = view
        , update = update
        , subscriptions =
            \route path model ->
                Sub.batch
                    [ Shared.template.subscriptions path model.global |> Sub.map MsgGlobal
                    , templateSubscriptions route path model
                    ]
        , onPageChange = OnPageChange
        , toJsPort = toJsPort
        , fromJsPort = fromJsPort identity
        , data = dataForRoute
        , sharedData = Shared.template.data
        , apiRoutes = \_ -> []
        , pathPatterns = routePatterns3
        , basePath = [  ]
        }

dataForRoute : Maybe Route -> DataSource PageData
dataForRoute route =
    case route of
        Nothing ->
            DataSource.succeed Data404NotFoundPage____
        Just Route.About ->
            Page.About.page.data {} |> DataSource.map DataAbout
        Just Route.Index ->
            Page.Index.page.data {} |> DataSource.map DataIndex
        Just Route.Me ->
            Page.Me.page.data {} |> DataSource.map DataMe

handleRoute : Maybe Route -> DataSource (Maybe Pages.Internal.NotFoundReason.NotFoundReason)
handleRoute maybeRoute =
    case maybeRoute of
        Nothing ->
            DataSource.succeed Nothing

        Just (Route.About) ->
            Page.About.page.handleRoute { moduleName = [ "About" ], routePattern = { segments = [ Pages.Internal.RoutePattern.StaticSegment "about" ], ending = Nothing } } (\param -> [  ]) {}
        Just (Route.Index) ->
            Page.Index.page.handleRoute { moduleName = [ "Index" ], routePattern = { segments = [  ], ending = Nothing } } (\param -> [  ]) {}
        Just (Route.Me) ->
            Page.Me.page.handleRoute { moduleName = [ "Me" ], routePattern = { segments = [ Pages.Internal.RoutePattern.StaticSegment "me" ], ending = Nothing } } (\param -> [  ]) {}


stringToString : String -> String
stringToString string =
    "\"" ++ string ++ "\""


nonEmptyToString : ( String, List String ) -> String
nonEmptyToString ( first, rest ) =
    "( "
        ++ stringToString first
        ++ ", [ "
        ++ (rest
                |> List.map stringToString
                |> String.join ", "
           )
        ++ " ] )"


listToString : List String -> String
listToString strings =
    "[ "
        ++ (strings
                |> List.map stringToString
                |> String.join ", "
           )
        ++ " ]"


maybeToString : Maybe String -> String
maybeToString maybeString =
    case maybeString of
        Just string ->
            "Just " ++ stringToString string

        Nothing ->
            "Nothing"




routePatterns : ApiRoute.ApiRoute ApiRoute.Response
routePatterns =
    ApiRoute.succeed
        (Json.Encode.list
            (\{ kind, pathPattern } ->
                Json.Encode.object
                    [ ( "kind", Json.Encode.string kind )
                    , ( "pathPattern", Json.Encode.string pathPattern )
                    ]
            )
            [ { kind = Page.About.page.kind, pathPattern = "/about" }
            , { kind = Page.Me.page.kind, pathPattern = "/me" }
            , { kind = Page.Index.page.kind, pathPattern = "/" }
          
            ]
            |> (\json -> DataSource.succeed { body = Json.Encode.encode 0 json })
        )
        |> ApiRoute.literal "route-patterns.json"
        |> ApiRoute.single


routePatterns2 : List String
routePatterns2 =
    [ "/about"
    , "/me"
    , "/"
    ]


routePatterns3 : List Pages.Internal.RoutePattern.RoutePattern
routePatterns3 =
    [ { segments = [ Pages.Internal.RoutePattern.StaticSegment "about" ], ending = Nothing }
    , { segments = [ Pages.Internal.RoutePattern.StaticSegment "me" ], ending = Nothing }
    , { segments = [  ], ending = Nothing }
    ]

getStaticRoutes : DataSource (List Route)
getStaticRoutes =
    DataSource.combine
        [ Page.About.page.staticRoutes |> DataSource.map (List.map (\_ -> Route.About))
        , Page.Me.page.staticRoutes |> DataSource.map (List.map (\_ -> Route.Me))
        , Page.Index.page.staticRoutes |> DataSource.map (List.map (\_ -> Route.Index))
        ]
        |> DataSource.map List.concat


pathsToGenerateHandler : ApiRoute.ApiRoute ApiRoute.Response
pathsToGenerateHandler =
    ApiRoute.succeed
        (DataSource.map2
            (\pageRoutes apiRoutes ->
                { body =
                    (pageRoutes ++ (apiRoutes |> List.map (\api -> "/" ++ api)))
                        |> Json.Encode.list Json.Encode.string
                        |> Json.Encode.encode 0
                }
            )
            (DataSource.map
                (List.map
                    (\route ->
                        route
                            |> Route.toPath
                            |> Path.toAbsolute
                    )
                )
                getStaticRoutes
            )
            ((manifestHandler :: Api.routes getStaticRoutes (\_ -> ""))
                |> List.map ApiRoute.getBuildTimeRoutes
                |> DataSource.combine
                |> DataSource.map List.concat
            )
        )
        |> ApiRoute.literal "all-paths.json"
        |> ApiRoute.single


manifestHandler : ApiRoute.ApiRoute ApiRoute.Response
manifestHandler =
    ApiRoute.succeed
        (Site.config
            |> .data
            |> DataSource.map
                (\data ->
                    Site.config.manifest data
                        |> manifestToFile (Site.config.canonicalUrl)
                )
        )
        |> ApiRoute.literal "manifest.json"
        |> ApiRoute.single


manifestToFile : String -> Manifest.Config -> { body : String }
manifestToFile resolvedCanonicalUrl manifestConfig =
    manifestConfig
        |> Manifest.toJson resolvedCanonicalUrl
        |> (\manifestJsonValue ->
                { body = Json.Encode.encode 0 manifestJsonValue
                }
           )


port toJsPort : Json.Encode.Value -> Cmd msg

port fromJsPort : (Json.Decode.Value -> msg) -> Sub msg


mapBoth : (a -> b) -> (c -> d) -> ( a, c, e ) -> ( b, d, e )
mapBoth fnA fnB ( a, b, c ) =
    ( fnA a, fnB b, c )
