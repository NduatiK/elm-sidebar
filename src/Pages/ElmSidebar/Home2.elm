module Pages.ElmSidebar.Home2 exposing (Model, Msg, page)

import Element exposing (..)
import Gen.Params.Example exposing (Params)
import Gen.Route exposing (Route)
import Page
import Request
import Shared
import UI
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init req shared.pageOptions
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { req : Request.With Params, palette : UI.Palette }


init : Request.With Params -> Shared.PageOptions -> ( Model, Cmd Msg )
init req sharedOptions =
    ( Model req sharedOptions.palette, Cmd.none )



-- UPDATE


type Msg
    = ReplaceMe


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReplaceMe ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Home"
    , element = UI.layout model.req.route model.palette none
    }
