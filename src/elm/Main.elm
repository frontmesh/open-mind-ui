module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, a, button, div, footer, h1, input, label, li, main_, p, span, text, textarea, ul)
import Html.Attributes exposing (class, for, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, fill, height, strokeLinecap, strokeLinejoin, strokeWidth, viewBox, width)


main : Program () Model Msg
main =
    Browser.sandbox { init = initialModel, update = update, view = view }


type Msg
    = UpdateMessage String
    | SubmitMessage String


type alias Model =
    { newMessage : String
    , messages : List String
    , charCount : Int
    }


initialModel : Model
initialModel =
    { newMessage = ""
    , messages = []
    , charCount = 0
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateMessage message ->
            { model | newMessage = message, charCount = String.length message }

        SubmitMessage message ->
            if String.isEmpty message then
                model

            else
                { model | newMessage = "", charCount = 0, messages = model.messages ++ [ message ] }


renderHeaderMenuButton : Html Msg
renderHeaderMenuButton =
    label [ for "app-drawer", class "btn btn-round drawer-button" ]
        [ svg [ fill "none", viewBox "0 0 24 24", Svg.Attributes.class "inline-block w-5 h-5 stroke-current" ]
            [ path [ d "M4 6h16M4 12h16M4 18h16", strokeWidth "2", strokeLinecap "round", strokeLinejoin "round" ] []
            ]
        ]


renderHeaderMenu : Html Msg
renderHeaderMenu =
    div [ class "items-center align-baseline" ]
        [ div [ class "flex-none" ] [ renderHeaderMenuButton ]
        , div [ class "flex-1 px-2 mx-2" ]
            [ h1
                [ class "mb-2 text-xl font-bold normal-case text-xl" ]
                [ text "Open Mind UI" ]
            ]
        ]


renderDrawer : Html Msg -> Html Msg
renderDrawer content =
    div [ class "drawer" ]
        [ input [ id "app-drawer", type_ "checkbox", class "drawer-toggle" ] []
        , div [ class "drawer-content" ]
            [ div [ class "p-4 text-center" ]
                [ content ]
            ]
        , renderDrawerMenu
        ]


renderDrawerMenu : Html Msg
renderDrawerMenu =
    div [ class "drawer-side" ]
        [ label [ for "app-drawer", class "drawer-overlay" ] []
        , ul [ class "menu p-4 w-80 bg-base-100 text-base-content" ]
            [ li [] [ a [] [ text "Sidebar Item 1" ] ]
            , li []
                [ a [] [ text "Sidebar Item 2" ]
                ]
            ]
        ]


renderFooter : Html Msg
renderFooter =
    footer [ class "footer items-center p-4 bg-neutral text-neutral-content bottom-0 z-31 sticky" ]
        [ p [] [ text "Made with Elm, Daisy UI, Vite and ❤️" ]
        , div [ class "grid-flow-col gap-4 md:place-self-center md:justify-self-end" ]
            [ a []
                [ svg [ width "24", height "24", viewBox "0 0 24 24", Svg.Attributes.class "fill-current" ]
                    [ path [ d "M24 4.557c-.883.392-1.832.656-2.828.775 1.017-.609 1.798-1.574 2.165-2.724-.951.564-2.005.974-3.127 1.195-.897-.957-2.178-1.555-3.594-1.555-3.179 0-5.515 2.966-4.797 6.045-4.091-.205-7.719-2.165-10.148-5.144-1.29 2.213-.669 5.108 1.523 6.574-.806-.026-1.566-.247-2.229-.616-.054 2.281 1.581 4.415 3.949 4.89-.693.188-1.452.232-2.224.084.626 1.956 2.444 3.379 4.6 3.419-2.07 1.623-4.678 2.348-7.29 2.04 2.179 1.397 4.768 2.212 7.548 2.212 9.142 0 14.307-7.721 13.995-14.646.962-.695 1.797-1.562 2.457-2.549z" ] []
                    ]
                ]
            ]
        ]


renderChat : List String -> Html Msg
renderChat messages =
    let
        chatLine =
            List.map (\message -> li [ class "chat-bubble mb-4" ] [ text message ]) messages
    in
    ul [ class "chat chat-end" ]
        chatLine


view : Model -> Html Msg
view model =
    main_ [ class "container m-auto text-center items-center min-h-screen" ]
        [ div [ class "navbar bg-base-100 sticky top-0 z-30" ]
            [ renderHeaderMenu ]
        , renderDrawer
            (div []
                [ renderChat model.messages
                , div
                    [ class "text-center m-auto" ]
                    [ span
                        [ class "" ]
                        [ text ("Chars: " ++ (String.fromInt <| model.charCount)) ]
                    ]
                , div [ class "flex items-center h-full container mx-auto max-w-lg" ]
                    [ textarea
                        [ class "textarea textarea-sm textarea-accent textarea-bordered max-w-xl w-full m-2 h-12"
                        , placeholder "Type your message here"
                        , onInput UpdateMessage
                        , value model.newMessage
                        ]
                        []
                    , button [ class "btn btn-sm btn-accent ml-4 px-4 py-2", onClick (SubmitMessage model.newMessage) ] [ text "Send" ]
                    ]

                -- , renderFooter
                ]
            )
        , renderFooter
        ]
