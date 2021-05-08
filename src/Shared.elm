module Shared exposing
    ( Flags
    , Model
    , Msg(..)
    , PageOptions
    , init
    , subscriptions
    , update
    )

import Element
import Json.Decode as Json
import Request exposing (Request)
import UI


type alias Flags =
    Json.Value


type alias Model =
    { pageOptions : PageOptions }


type alias PageOptions =
    { palette : UI.Palette }


type Msg
    = NoOp
    | UpdatePalette UI.Palette


init : Request -> Flags -> ( Model, Cmd Msg )
init _ _ =
    ( { pageOptions = PageOptions UI.defaultPalette
      }
    , Cmd.none
    )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg ({ pageOptions } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdatePalette newPalette ->
            ( { model | pageOptions = { pageOptions | palette = newPalette } }, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
