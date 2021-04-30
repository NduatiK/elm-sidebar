module Pages.Settings exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Input as Input
import Gen.Params.Settings exposing (Params)
import Page
import Request
import Shared
import UI
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init req shared.pageOptions
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { req : Request.With Params
    , isDark : Bool
    }


init : Request.With Params -> Shared.PageOptions -> ( Model, Effect Msg )
init req pageOptions =
    ( Model req pageOptions.isDark, Effect.none )



-- UPDATE


type Msg
    = ToggleDarkMode


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ToggleDarkMode ->
            ( { model | isDark = not model.isDark }, Effect.fromShared Shared.ToggleDarkMode )



-- ( model, Effect.none )
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Home"
    , element =
        UI.layout model.req.route model.isDark <|
            el [ padding 40, alignTop ] <|
                column [ alignTop ]
                    [ Input.checkbox []
                        { checked = model.isDark
                        , icon = Input.defaultCheckbox
                        , label = Input.labelLeft [] (text "Dark Mode ")
                        , onChange = always ToggleDarkMode
                        }
                    ]
    }
