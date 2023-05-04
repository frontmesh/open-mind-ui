port module Main exposing (Model, Msg(..), main, update, view)

import Api exposing (postCompletion)
import Browser
import Dict exposing (update)
import Html exposing (Html, a, button, div, footer, h1, h3, input, label, li, main_, p, span, text, textarea, ul)
import Html.Attributes exposing (checked, class, for, href, id, placeholder, rel, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, fill, height, strokeLinecap, strokeLinejoin, strokeWidth, viewBox, width)


main : Program Encode.Value Model Msg
main =
    -- Browser.sandbox { init = initialModel, update = update, view = view }
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = UpdateMessage String
    | SubmitMessage String
    | ApiKeyChanged String
    | ApiKeySubmitted
    | ToggleModal Bool
    | SendApiKeyRequest
    | GotCompletion (Result Http.Error String)


type alias Model =
    { newMessage : String
    , messages : List String
    , charCount : Int
    , apikey : String
    , apiModal : Bool
    }


init : Encode.Value -> ( Model, Cmd Msg )
init flags =
    case Decode.decodeValue apiKeyDecoder flags of
        Ok storage ->
            ( { initialModel | apikey = storage.apikey }, Cmd.none )

        Err _ ->
            ( initialModel, Cmd.none )


apiKeyDecoder : Decode.Decoder { apikey : String }
apiKeyDecoder =
    Decode.map (\apikey -> { apikey = apikey }) (Decode.field "apikey" Decode.string)


apiKeyEncoder : Model -> Encode.Value
apiKeyEncoder model =
    Encode.object [ ( "apikey", Encode.string model.apikey ) ]


initialModel : Model
initialModel =
    { newMessage = ""
    , messages = []
    , charCount = 0
    , apikey = ""
    , apiModal = False
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateMessage message ->
            ( { model | newMessage = message, charCount = String.length message, apiModal = False }, Cmd.none )

        SubmitMessage message ->
            if String.isEmpty message then
                ( model, Cmd.none )

            else if String.isEmpty model.apikey then
                ( { model | apiModal = True }, Cmd.none )

            else
                ( { model | apiModal = False, newMessage = "", charCount = 0, messages = model.messages ++ [ message ] }, Cmd.none )

        ApiKeyChanged key ->
            ( { model | apikey = key, apiModal = True }, Cmd.none )

        ApiKeySubmitted ->
            if String.isEmpty model.apikey then
                ( model, Cmd.none )

            else
                ( model, Cmd.batch [ setStorage (apiKeyEncoder model) ] )

        ToggleModal state ->
            ( { model | apiModal = state }, Cmd.none )

        SendApiKeyRequest ->
            ( model, postCompletion model.apikey )

        GotCompletion (Ok response) ->
            ( model, Cmd.none )


port setStorage : Encode.Value -> Cmd msg


renderHeaderMenuButton : Html Msg
renderHeaderMenuButton =
    label [ for "app-drawer", class "btn btn-round btn-ghost drawer-button" ]
        [ svg [ fill "none", viewBox "0 0 24 24", Svg.Attributes.class "inline-block w-5 h-5 stroke-current" ]
            [ path [ d "M4 6h16M4 12h16M4 18h16", strokeWidth "2", strokeLinecap "round", strokeLinejoin "round" ] []
            ]
        ]


renderHeaderMenu : Html Msg
renderHeaderMenu =
    div [ class "navbar bg-base-300 sticky rounded-box" ]
        [ div [ class "navbar-start" ] [ renderHeaderMenuButton ]
        , div [ class "navbar-center" ]
            [ h1
                [ class "mb-2 text-xl" ]
                [ text "Open Mind UI" ]
            ]
        , div [ class "navbar-end" ]
            [ label [ for "api-modal", class "btn btn-circle btn-ghost" ]
                [ svg [ fill "none", viewBox "0 0 24 24", Svg.Attributes.class "inline-block w-5 h-5 stroke-current" ]
                    [ path [ d "M5 12h.01M12 12h.01M19 12h.01M6 12a1 1 0 11-2 0 1 1 0 012 0zm7 0a1 1 0 11-2 0 1 1 0 012 0zm7 0a1 1 0 11-2 0 1 1 0 012 0z", strokeWidth "2", strokeLinecap "round", strokeLinejoin "round" ] []
                    ]
                ]
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


renderChat : List String -> Html Msg
renderChat messages =
    let
        chatLine =
            List.map (\message -> li [ class "chat-bubble mb-4" ] [ text message ]) messages
    in
    ul [ class "chat chat-start" ]
        chatLine


renderChatInput : Model -> Html Msg
renderChatInput model =
    div [ class "fixed bottom-0 left-0 right-0 bg-base-100" ]
        [ div [ class "flex items-center h-full container mx-auto max-w-lg" ]
            [ textarea
                [ class "textarea textarea-sm textarea-accent textarea-bordered max-w-xl w-full m-2 h-12"
                , placeholder "Type your message here"
                , onInput UpdateMessage
                , value model.newMessage
                ]
                []
            , button [ class "btn btn-sm btn-accent ml-4 px-4 py-2", onClick (SubmitMessage model.newMessage) ] [ text "Send" ]
            ]
        , div
            [ class "text-center m-auto" ]
            [ span
                [ class "" ]
                [ text ("Chars: " ++ (String.fromInt <| model.charCount)) ]
            ]
        ]


viewModal : Model -> Html Msg
viewModal model =
    div []
        [ input [ type_ "checkbox", id "api-modal", class "modal-toggle", checked model.apiModal ] []
        , label [ for "api-modal", class "modal cursor-pointer" ]
            [ label [ class "modal-box relative", for "" ]
                [ h3 [ class "text-lg font-bold" ] [ text " Set your OpenAI API key" ]
                , p [ class "py-4" ] [ text "You need an OpenAI API Key to use open mind UI" ]
                , p [ class "my-2 text-xs" ] [ text "Your API Key is stored locally on your browser and never sent anywhere else." ]
                , div [ class "input-group" ]
                    [ input
                        [ type_ "text"
                        , class "input input-md input-primary min-w-[80%]"
                        , placeholder "Your api key"
                        , onInput ApiKeyChanged
                        , value model.apikey
                        , placeholder "sk-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
                        ]
                        []
                    , button [ class "btn btn-md btn-primary", onClick ApiKeySubmitted ] [ text "Submit" ]
                    ]
                , div [ class "my-4 text-center" ]
                    [ a
                        [ class "text-blue-500 text-xs hover:underline"
                        , href "https://platform.openai.com/account/api-keys"
                        , target "_blank"
                        , rel "noopener noreferrer"
                        ]
                        [ text "→ Get your API key from Open AI dashboard." ]
                    ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    main_ [ class "" ]
        [ renderHeaderMenu
        , renderDrawer
            (div []
                [ renderChat model.messages ]
            )
        , renderChatInput model
        , viewModal model
        ]
