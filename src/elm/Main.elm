port module Main exposing (Model, Msg(..), main, update, view)

import Api exposing (Comp, postCompletion)
import Browser
import Dict exposing (update)
import Error exposing (buildErrorMessage)
import Html exposing (Html, a, button, div, h1, h3, input, label, li, main_, p, span, text, textarea, ul)
import Html.Attributes exposing (checked, class, for, href, id, placeholder, rel, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import RemoteData exposing (WebData)
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, fill, strokeLinecap, strokeLinejoin, strokeWidth, viewBox)


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
    = ChatMessage ChatMessage
    | AppOptionsMessage AppOptionsMessage
    | ModalMessage ModalMessage
    | ThreadMessage ThreadMessage
    | CompReceived (WebData Comp)


type AppOptionsMessage
    = ApiKeyChanged String
    | ApiKeySubmitted
    | ToggleSendOnEnter


type ModalMessage
    = ToggleModal Bool
    | CloseModal


type ChatMessage
    = UpdateMessage String
    | SubmitMessage
    | UpdateTitle String


type ThreadMessage
    = AddThread String
    | RemoveThread ChatThread


type ApiKey
    = ApiKey String


type alias Model =
    { newMessage : String
    , messages : List String
    , newTitle : String
    , charCount : Int
    , activeThread : Int
    , threads : List ChatThread
    , apikey : ApiKey
    , apiModal : Bool
    , sendOnEnter : Bool
    , comps : WebData Comp

    -- , appOptions : AppOptions
    }


type alias ChatThread =
    { title : String
    , messages : List String
    }



-- using extensible records for app options


type alias AppOptions o =
    { o
        | apikey : ApiKey
        , apiModal : Bool
        , sendOnEnter : Bool
    }


init : Encode.Value -> ( Model, Cmd Msg )
init flags =
    case Decode.decodeValue apiKeyDecoder flags of
        Ok storage ->
            ( { initialModel | apikey = ApiKey storage.apikey }, Cmd.none )

        Err _ ->
            ( initialModel, Cmd.none )



-- not needed, but it is a good practice to have it wrapped in a custom type


apiKeyToString : ApiKey -> String
apiKeyToString (ApiKey key) =
    key


apiKeyDecoder : Decode.Decoder { apikey : String }
apiKeyDecoder =
    Decode.map (\apikey -> { apikey = apikey }) (Decode.field "apikey" Decode.string)


apiKeyEncoder : ApiKey -> Encode.Value
apiKeyEncoder apikey =
    Encode.object [ ( "apikey", apikey |> (Encode.string << apiKeyToString) ) ]


initialModel : Model
initialModel =
    { newMessage = ""
    , messages = []
    , charCount = 0
    , newTitle = ""
    , apikey = ApiKey ""
    , apiModal = False
    , sendOnEnter = False
    , comps = RemoteData.NotAsked
    , activeThread = 0
    , threads =
        [ { title = "General"
          , messages = [ "Hello there", "Hi" ]
          }
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChatMessage subMsg ->
            updateChatMessage subMsg model

        AppOptionsMessage subMsg ->
            updateAppOptionsMessage subMsg model

        ModalMessage subMsg ->
            updateModalMessage subMsg model

        ThreadMessage subMsg ->
            ( { model | threads = updateChatThreads subMsg model.threads }, Cmd.none )

        CompReceived comps ->
            ( { model | comps = comps }, Cmd.none )


updateAppOptionsMessage : AppOptionsMessage -> AppOptions o -> ( AppOptions o, Cmd Msg )
updateAppOptionsMessage msg o =
    case msg of
        ApiKeyChanged key ->
            ( { o | apikey = ApiKey key, apiModal = True }, Cmd.none )

        ApiKeySubmitted ->
            if (String.isEmpty << apiKeyToString) o.apikey then
                ( o, Cmd.none )

            else
                let
                    effect =
                        postCompletion (apiKeyToString o.apikey) CompReceived
                in
                ( { o | apiModal = True }
                , Cmd.batch [ setStorage (apiKeyEncoder o.apikey), effect ]
                  -- TODO: setStorage should happen after the happy flow of the effect
                )

        ToggleSendOnEnter ->
            ( { o | sendOnEnter = not o.sendOnEnter }, Cmd.none )


updateModalMessage : ModalMessage -> Model -> ( Model, Cmd Msg )
updateModalMessage msg model =
    case msg of
        ToggleModal state ->
            ( { model | apiModal = state }, Cmd.none )

        CloseModal ->
            ( { model | apiModal = False, comps = RemoteData.NotAsked }, Cmd.none )


updateChatMessage : ChatMessage -> Model -> ( Model, Cmd Msg )
updateChatMessage msg model =
    case msg of
        UpdateMessage message ->
            ( { model | newMessage = message, charCount = String.length message, apiModal = False }, Cmd.none )

        SubmitMessage ->
            if String.isEmpty model.newMessage then
                ( model, Cmd.none )

            else if (String.isEmpty << apiKeyToString) model.apikey then
                ( { model | apiModal = True }, Cmd.none )

            else
                ( { model | messages = model.messages ++ [ model.newMessage ], apiModal = False, newMessage = "", charCount = 0 }, Cmd.none )

        UpdateTitle title ->
            ( { model | newTitle = title }, Cmd.none )


updateChatThreads : ThreadMessage -> List ChatThread -> List ChatThread
updateChatThreads msg threads =
    case msg of
        AddThread title ->
            if String.isEmpty title then
                threads

            else
                threads ++ [ { title = title, messages = [] } ]

        RemoveThread chat ->
            List.filter ((/=) chat) threads


port setStorage : Encode.Value -> Cmd msg


viewHeaderMenuButton : Html Msg
viewHeaderMenuButton =
    label [ for "app-drawer", class "btn btn-round btn-ghost drawer-button" ]
        [ svg
            [ fill "none"
            , viewBox "0 0 24 24"
            , Svg.Attributes.class "inline-block w-5 h-5 stroke-current"
            ]
            [ path
                [ d "M4 6h16M4 12h16M4 18h16"
                , strokeWidth "2"
                , strokeLinecap "round"
                , strokeLinejoin "round"
                ]
                []
            ]
        ]


viewHeaderMenu : Html Msg
viewHeaderMenu =
    div [ class "navbar bg-base-300 sticky rounded-box" ]
        [ div [ class "navbar-start" ] [ viewHeaderMenuButton ]
        , div [ class "navbar-center" ]
            [ h1
                [ class "mb-2 text-xl" ]
                [ text "Open Mind UI" ]
            ]
        , div [ class "navbar-end" ]
            [ label [ for "api-modal", class "btn btn-circle btn-ghost" ]
                [ svg
                    [ fill "none"
                    , viewBox "0 0 24 24"
                    , Svg.Attributes.class "inline-block w-5 h-5 stroke-current"
                    ]
                    [ path
                        [ d "M5 12h.01M12 12h.01M19 12h.01M6 12a1 1 0 11-2 0 1 1 0 012 0zm7 0a1 1 0 11-2 0 1 1 0 012 0zm7 0a1 1 0 11-2 0 1 1 0 012 0z"
                        , strokeWidth "2"
                        , strokeLinecap "round"
                        , strokeLinejoin "round"
                        ]
                        []
                    ]
                ]
            ]
        ]


viewDrawer : Model -> Html Msg -> Html Msg
viewDrawer model content =
    div [ class "drawer" ]
        [ input [ id "app-drawer", type_ "checkbox", class "drawer-toggle" ] []
        , div [ class "drawer-content" ]
            [ div [ class "p-4 text-center" ]
                [ content ]
            ]
        , viewDrawerMenu model
        ]


viewAddNewThread : String -> Html Msg
viewAddNewThread title =
    div [ class "p-4" ]
        [ div [ class "input-group" ]
            [ input
                [ type_ "text"
                , placeholder "Add new thread"
                , class "input input-md input-bordered"
                , onInput (ChatMessage << UpdateTitle)
                ]
                []
            , button [ class "btn btn-md btm-primary", onClick (title |> (ThreadMessage << AddThread)) ] [ text "+" ]
            ]
        ]


viewDrawerMenu : Model -> Html Msg
viewDrawerMenu model =
    let
        threads =
            model.threads

        threadList =
            List.map (\thread -> li [] [ a [] [ text thread.title ] ]) threads
    in
    div [ class "drawer-side" ]
        [ label [ for "app-drawer", class "drawer-overlay" ] []
        , div [ class "flex flex-col flex-wrap w-80 bg-base-100 text-base-content" ]
            [ viewAddNewThread model.newTitle
            , ul [ class "menu p-4 w-80" ]
                threadList
            ]
        ]


viewCurrentChat : List String -> Html Msg
viewCurrentChat messages =
    let
        chatLine =
            List.map (\message -> li [ class "chat-bubble mb-4" ] [ text message ]) messages
    in
    div [ class "transition-all z-20 relative max-w-5xl mx-auto px-12" ]
        [ ul [ class "chat chat-start" ]
            chatLine
        ]


viewChatInput : Model -> Html Msg
viewChatInput model =
    let
        inputOption =
            if not (String.isEmpty model.newMessage) then
                div [ class "flex items-center justify-center gap-2 my-2 text-sm" ]
                    [ input [ id "send-enter", type_ "checkbox", checked model.sendOnEnter ] []
                    , label [ for "send-enter", onClick (AppOptionsMessage ToggleSendOnEnter) ]
                        [ text "Send message on enter"
                        ]
                    ]

            else
                span [] []
    in
    div [ class "fixed bottom-0 left-0 right-0 bg-base-100" ]
        [ inputOption
        , div [ class "flex items-center h-full container mx-auto max-w-lg" ]
            [ textarea
                [ class "textarea textarea-sm textarea-accent textarea-bordered max-w-xl w-full m-2 h-12"
                , placeholder "Type your message here"
                , onInput (ChatMessage << UpdateMessage)
                , value model.newMessage
                ]
                []
            , button [ class "btn btn-sm btn-accent ml-4 px-4 py-2", onClick (ChatMessage SubmitMessage) ] [ text "Send" ]
            ]
        , div
            [ class "text-center m-auto" ]
            [ span
                [ class "" ]
                [ text ("Chars: " ++ (String.fromInt <| model.charCount)) ]
            ]
        ]


viewFetchError : String -> Html Msg
viewFetchError errorMessage =
    let
        errorHeading =
            "A following error occurred:"
    in
    div [ class "text-sm text-center text-red-500" ]
        [ h3 [] [ text errorHeading ]
        , text errorMessage
        ]


handleApiKeyCheckResponse : WebData Comp -> Html Msg
handleApiKeyCheckResponse comp =
    case comp of
        RemoteData.NotAsked ->
            div [] []

        RemoteData.Loading ->
            div [] [ text "Loading..." ]

        RemoteData.Success _ ->
            p [ class "text-sm text-center text-green-500" ] [ text "Success!" ]

        RemoteData.Failure error ->
            viewFetchError (buildErrorMessage error)


viewModal : Model -> Html Msg
viewModal model =
    div []
        [ input [ type_ "checkbox", id "api-modal", class "modal-toggle", checked model.apiModal ] []
        , div [ for "api-modal", class "modal cursor-pointer" ]
            [ div [ class "modal-box relative" ]
                [ label [ for "api-modal", class "btn btn-sm btn-circle absolute right-2 top-2", onClick (ModalMessage CloseModal) ] [ text "x" ]
                , h3 [ class "text-lg font-bold" ] [ text "🔑 Set your OpenAI API key" ]
                , p [ class "py-4" ] [ text "You need an OpenAI API Key to use open mind UI" ]
                , p [ class "my-2 text-xs" ] [ text "Your API Key is stored locally on your browser and never sent anywhere else." ]
                , div [ class "input-group" ]
                    [ input
                        [ type_ "text"
                        , class "input input-md input-primary min-w-[80%]"
                        , placeholder "Your api key"
                        , onInput (AppOptionsMessage << ApiKeyChanged)
                        , value (apiKeyToString model.apikey)
                        , placeholder "sk-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
                        ]
                        []
                    , button [ class "btn btn-md btn-primary", onClick (AppOptionsMessage ApiKeySubmitted) ] [ text "Submit" ]
                    ]
                , handleApiKeyCheckResponse model.comps
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
        [ viewHeaderMenu
        , viewDrawer model
            (div []
                [ viewCurrentChat model.messages ]
            )
        , viewChatInput model
        , viewModal model
        ]
