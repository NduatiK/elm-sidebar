module UI exposing (Palette, black, defaultPalette, layout, red, transparent, white, withAlpha, defaultPalette2)

import Element exposing (..)
import Element.Background
import Element.Border
import Element.Font
import FeatherIcons
import Gen.Route exposing (Route(..))
import Html.Attributes


type alias Palette =
    { isDark : Bool
    , backgroundColor : Element.Color
    , darkBackgroundColor : Element.Color
    , accentColor : Element.Color
    , darkAccentColor : Element.Color
    , textColor : Element.Color
    , darkTextColor : Element.Color
    }


defaultPalette : Palette
defaultPalette =
    { isDark = False
    , backgroundColor = white
    , darkBackgroundColor = black
    , accentColor = red
    , darkAccentColor = red
    , textColor = withAlpha 0.8 black
    , darkTextColor = withAlpha 0.7 white
    }


defaultPalette2 : Palette
defaultPalette2 =
    { isDark = False
    , backgroundColor = white
    , darkBackgroundColor = black
    , accentColor = rgb255 0 96 128
    , darkAccentColor = rgb255 25 148 190
    , textColor = withAlpha 0.8 black
    , darkTextColor = withAlpha 0.7 white
    }


layout : Route -> Palette -> Element msg -> Element msg
layout currentRoute palette child =
    row
        [ htmlAttribute (Html.Attributes.style "transition" "color 200ms, background-color 200ms")
        , height fill
        , scrollbarY
        , width fill
        , Element.Font.color
            (if palette.isDark then
                palette.darkTextColor

             else
                palette.textColor
            )
        , Element.Background.color
            (if palette.isDark then
                palette.darkBackgroundColor

             else
                palette.backgroundColor
            )
        ]
        [ sidebar currentRoute palette
        , child
        ]


sidebar : Route -> Palette -> Element msg
sidebar currentRoute palette =
    let
        pages =
            buildPages currentRoute sidebarPages palette

        _ =
            pages
                |> List.map (\( a, b, c ) -> ( b, c ))

        topOffset =
            pages
                |> List.foldl
                    (\( _, shouldStop, height ) ( total, alreadyStopped ) ->
                        if alreadyStopped || shouldStop then
                            ( total, True )

                        else
                            ( total + height + 25, alreadyStopped || shouldStop )
                    )
                    ( 0, False )
                |> Tuple.first
                |> toFloat
                |> (\x -> x + 15)

        indicator =
            el
                [ Element.Background.color
                    (if palette.isDark then
                        palette.darkAccentColor

                     else
                        palette.accentColor
                    )
                , Element.Border.rounded 6
                , htmlAttribute (Html.Attributes.style "transition" "transform 200ms")
                , width (px 8)
                , height (px 28)
                , moveDown topOffset
                , alignRight
                , moveRight 4
                ]
                none
    in
    el [ inFront indicator, height fill ] <|
        column
            [ paddingEach
                { bottom = 20
                , left = 30
                , right = 35
                , top = 20
                }
            , spacing 25
            , height fill
            , Element.Border.widthEach
                { bottom = 0
                , left = 0
                , right = 1
                , top = 0
                }
            , Element.Border.color
                (withAlpha 0.1
                    (if palette.isDark then
                        palette.backgroundColor

                     else
                        palette.darkBackgroundColor
                    )
                )
            ]
        <|
            List.map (\( a, _, _ ) -> a) pages


buildPages : Route -> List SidebarElement -> Palette -> List ( Element msg, Bool, Int )
buildPages currentRoute pages palette =
    List.map
        (\x ->
            case x of
                Gap ->
                    renderGap

                Header title ->
                    renderHeader title

                Page page ->
                    renderPage page currentRoute palette
        )
        pages


type SidebarElement
    = Page ( String, FeatherIcons.Icon, Route )
    | Header String
    | Gap


sidebarPages : List SidebarElement
sidebarPages =
    [ Gap
    , Gap
    , Header "Services"
    , Page ( "Home", FeatherIcons.home, ElmSidebar__Home_ )
    , Page ( "Bookonly", FeatherIcons.bookOpen, ElmSidebar__Bookonly )
    , Page ( "Movieonly", FeatherIcons.video, ElmSidebar__Movieonly )
    , Page ( "Coursify", FeatherIcons.book, ElmSidebar__Coursify )
    , Gap
    , Header "Account"
    , Page ( "Profile", FeatherIcons.user, ElmSidebar__Profile )
    , Page ( "Settings", FeatherIcons.settings, ElmSidebar__Settings )
    , Page ( "Notifications", FeatherIcons.messageCircle, ElmSidebar__Notifications )
    , Page ( "Favourites", FeatherIcons.star, ElmSidebar__Favourites )
    , Gap
    , Gap
    ]


renderGap : ( Element msg, Bool, Int )
renderGap =
    ( el [ height (px 20), width fill ] none
    , False
    , 20
    )


renderPage : ( String, FeatherIcons.Icon, Route ) -> Route -> Palette -> ( Element msg, Bool, Int )
renderPage ( title, icon, route ) currentRoute palette =
    let
        hInPx =
            20

        iconColor =
            if currentRoute == route then
                [ Element.Font.color
                    (if palette.isDark then
                        palette.darkAccentColor

                     else
                        palette.accentColor
                    )
                , alpha 0.8
                ]

            else
                [ alpha 0.8 ]

        label =
            row [ spacing 10 ]
                [ icon
                    |> FeatherIcons.withSize 18
                    |> FeatherIcons.withStrokeWidth 2.4
                    |> FeatherIcons.toHtml []
                    |> Element.html
                    |> el iconColor
                , el [ Element.Font.size 17 ] (text title)
                ]
    in
    if currentRoute == route then
        ( el [ height (px hInPx) ] label
        , True
        , hInPx
        )

    else
        ( link [ height (px hInPx), moveUp 1 ]
            { url = Gen.Route.toHref route
            , label = label
            }
        , False
        , hInPx
        )


renderHeader : String -> ( Element msg, Bool, Int )
renderHeader title =
    ( el [ height (px 20), Element.Font.size 14 ] (text (String.toUpper title))
    , False
    , 20
    )



-- COLORS


red : Color
red =
    rgb255 246 52 32


black : Color
black =
    rgb255 25 25 27


white : Color
white =
    rgb255 249 249 251


transparent : Color
transparent =
    withAlpha 0 white


withAlpha : Float -> Color -> Color
withAlpha alpha color =
    let
        rgbColor =
            toRgb color
    in
    rgba rgbColor.red rgbColor.green rgbColor.blue alpha
