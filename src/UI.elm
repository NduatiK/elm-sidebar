module UI exposing (layout, red)

import Element exposing (..)
import Element.Background
import Element.Border
import Element.Font
import FeatherIcons
import Gen.Route exposing (Route(..))
import Html.Attributes


layout : Route -> Bool -> Element msg -> Element msg
layout currentRoute isDark child =
    row
        [ height fill
        , width fill
        , htmlAttribute (Html.Attributes.style "transition" "color 200ms, background-color 200ms")
        , Element.Font.color
            (if isDark then
                withAlpha 0.7 white

             else
                withAlpha 0.8 black
            )
        , Element.Background.color
            (if isDark then
                black

             else
                white
            )
        ]
        [ sidebar currentRoute isDark
        , child
        ]


sidebar currentRoute isDark =
    let
        pages =
            buildPages currentRoute sidebarPages

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
    in
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
                (if isDark then
                    white

                 else
                    black
                )
            )
        , inFront
            (el
                [ Element.Background.color red
                , Element.Border.rounded 6
                , htmlAttribute (Html.Attributes.style "transition" "transform 200ms")
                , width (px 8)
                , height (px 28)
                , moveDown topOffset
                , alignRight
                , moveRight 4
                ]
                none
            )
        ]
    <|
        List.map (\( a, _, _ ) -> a) pages


buildPages currentRoute pages =
    List.map
        (\x ->
            case x of
                Gap ->
                    renderGap

                Header title ->
                    renderHeader title

                Page page ->
                    renderPage page currentRoute
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
    ]


renderGap : ( Element msg, Bool, Int )
renderGap =
    ( el [ height (px 20) ] none
    , False
    , 20
    )


renderPage : ( String, FeatherIcons.Icon, Route ) -> Route -> ( Element msg, Bool, Int )
renderPage ( title, icon, route ) currentRoute =
    let
        hInPx =
            20

        iconColor =
            if currentRoute == route then
                [ Element.Font.color red, alpha 0.8 ]

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


red =
    rgb255 246 52 32


black =
    rgb255 25 25 27


white =
    rgb255 249 249 251


withAlpha : Float -> Color -> Color
withAlpha alpha color =
    let
        rgbColor =
            toRgb color
    in
    rgba rgbColor.red rgbColor.green rgbColor.blue alpha
