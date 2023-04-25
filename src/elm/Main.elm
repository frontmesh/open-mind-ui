module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, footer, h1, main_, text, textarea, input)
import Html.Attributes exposing (class, placeholder)
import Html.Events exposing (onClick, onInput)
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, fill, strokeLinecap, strokeLinejoin, strokeWidth, viewBox)
import Html.Attributes exposing (id)
import Html.Attributes exposing (type_)


main : Program () Model Msg
main =
    Browser.sandbox { init = initialModel, update = update, view = view }


type Msg
    = UpdateMessage String
    | SubmitMessage String


type alias Model =
    { message : String
    , charCount : Int
    }


initialModel : Model
initialModel =
    { message = ""
    , charCount = 0
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateMessage newMessage ->
            { model | message = newMessage, charCount = String.length newMessage }

        SubmitMessage newMessage ->
            { model | message = newMessage, charCount = String.length newMessage }


renderHeaderMenuButton : Html Msg
renderHeaderMenuButton =
    button
        [ class "btn btn-ghost btn-sm rounded-btn"]
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

renderDrawer : Html Msg
renderDrawer = 
  div [ class "drawer"] [
    input [id "my-drawer", type_ "checkbox", class "drawer-toggle"] []
    ]


view : Model -> Html Msg
view model =
    main_ [ class "container m-auto text-center items-center" ]
        [ div [ class "navbar bg-base-100" ]
            [ renderHeaderMenu ]
        , div
            [ class "text-center m-auto" ]
            [ button
                [ class "btn btn-sm btn-outline m-2" ]
                [ text "-" ]
            , text <| "Count is: " ++ String.fromInt model.charCount
            , button
                [ class "btn btn-sm btn-outline m-2" ]
                [ text "+" ]
            ]
        , div [ class "flex items-center h-full container mx-auto max-w-lg" ]
            [ textarea
                [ class "textarea textarea-sm textarea-accent textarea-bordered max-w-xl w-full m-2 h-12"
                , placeholder "Type your message here"
                , onInput UpdateMessage
                ]
                []
            , button [ class "btn btn-sm btn-accent ml-4 px-4 py-2", onClick (SubmitMessage model.message) ] [ text "Send" ]
            ]
        , footer
            [ class "text-center m-auto" ]
            [ text "Made with Elm, Daisy UI, Vite and ❤️" ]
        ]

