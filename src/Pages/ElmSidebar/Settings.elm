module Pages.ElmSidebar.Settings exposing (Model, Msg, page)

import Css
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background
import Element.Border
import Element.Font
import Element.Input as Input
import Gen.Params.Settings exposing (Params)
import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Html.Events as E
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
    , palette : UI.Palette
    }


init : Request.With Params -> Shared.PageOptions -> ( Model, Effect Msg )
init req pageOptions =
    ( { req = req
      , palette = pageOptions.palette
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = NoOp
    | SetDefault UI.Palette
    | ToggleDarkMode
    | PaletteChanged Field Color


type Field
    = BackgroundColorField
    | DarkBackgroundColorField
    | AccentColorField
    | DarkAccentColorField
    | TextColorField
    | DarkTextColorField


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    let
        palette =
            model.palette
    in
    case msg of
        NoOp ->
            ( model, Effect.none )

        SetDefault default ->
            ( { model | palette = default }, Effect.fromShared (Shared.UpdatePalette default) )

        ToggleDarkMode ->
            let
                newPalette =
                    { palette | isDark = not palette.isDark }
            in
            ( { model | palette = newPalette }, Effect.fromShared (Shared.UpdatePalette newPalette) )

        PaletteChanged field color ->
            let
                newPalette =
                    case field of
                        BackgroundColorField ->
                            { palette | backgroundColor = color }

                        DarkBackgroundColorField ->
                            { palette | darkBackgroundColor = color }

                        AccentColorField ->
                            { palette | accentColor = color }

                        DarkAccentColorField ->
                            { palette | darkAccentColor = color }

                        TextColorField ->
                            { palette | textColor = color }

                        DarkTextColorField ->
                            { palette | darkTextColor = color }
            in
            ( { model | palette = newPalette }, Effect.fromShared (Shared.UpdatePalette newPalette) )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Home"
    , element =
        UI.layout model.req.route model.palette <|
            el [ padding 40, alignTop ] <|
                column [ alignTop, spacing 10 ]
                    [ row [ spacing 8 ]
                        [ defaultButton "Default 1" UI.defaultPalette
                        , defaultButton "Default 2" UI.defaultPalette2
                        ]
                    , Input.checkbox []
                        { checked = model.palette.isDark
                        , icon = Input.defaultCheckbox
                        , label = Input.labelLeft [] (text "Dark Mode ")
                        , onChange = always ToggleDarkMode
                        }
                    , colorView model BackgroundColorField
                    , colorView model TextColorField
                    , colorView model AccentColorField

                    ---
                    , el [] none
                    , colorView model DarkBackgroundColorField
                    , colorView model DarkAccentColorField
                    , colorView model DarkTextColorField
                    ]
    }



-- Thanks https://github.com/passiomatic/elm-designer


colorView : Model -> Field -> Element Msg
colorView model field =
    row [ spacing 8 ]
        [ el [] <| html <| colorPickerView model (colorForField model field) field
        , text (fieldToString field)
        ]


colorPickerView : Model -> Color -> Field -> Html Msg
colorPickerView _ value field =
    H.input
        [ A.type_ "color"
        , A.value (Css.colorToStringWithHash value)
        , E.onInput
            (Css.stringToColor
                >> PaletteChanged field
            )
        , A.classList
            [ ( "form-control form-control-sm mr-1", True )
            ]
        ]
        []


fieldToString field =
    case field of
        BackgroundColorField ->
            "Background Color"

        DarkBackgroundColorField ->
            "Dark Background Color"

        AccentColorField ->
            "Accent Color"

        DarkAccentColorField ->
            "Dark Accent Color"

        TextColorField ->
            "Text Color"

        DarkTextColorField ->
            "Dark Text Color"


colorForField model field =
    case field of
        BackgroundColorField ->
            model.palette.backgroundColor

        DarkBackgroundColorField ->
            model.palette.darkBackgroundColor

        AccentColorField ->
            model.palette.accentColor

        DarkAccentColorField ->
            model.palette.darkAccentColor

        TextColorField ->
            model.palette.textColor

        DarkTextColorField ->
            model.palette.darkTextColor


defaultButton : String -> UI.Palette -> Element Msg
defaultButton string palette =
    Input.button
        [ paddingXY 10 5
        , Element.Border.width 2
        , Element.Border.rounded 5
        , Element.Font.color
            (if palette.isDark then
                palette.darkTextColor

             else
                palette.textColor
            )
        , Element.Border.color
            (if palette.isDark then
                palette.darkAccentColor

             else
                palette.accentColor
            )
        , Element.Background.color
            (if palette.isDark then
                palette.darkBackgroundColor

             else
                palette.backgroundColor
            )
        ]
        { label = text string
        , onPress = Just (SetDefault palette)
        }
