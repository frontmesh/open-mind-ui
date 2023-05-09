module Api exposing (..)

-- import Json.Decode exposing (Decoder)

import Http exposing (emptyBody, expectJson, request)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import RemoteData exposing (WebData)


baseUrl : String
baseUrl =
    "https://api.openai.com/v1/"


type CompId
    = CompId String


type alias Comp =
    { id : CompId
    }


type alias Message =
    { role : String
    , content : String
    }


type alias CompExample =
    { messages : List Message
    , model : String
    }



-- postCompletion : String -> (a -> msg) -> Cmd msg


exampleData : CompExample
exampleData =
    { messages =
        [ { role = "user", content = "Hello, How are you" }
        ]
    , model = "gpt-3.5-turbo"
    }


postCompletion : String -> (WebData Comp -> msg) -> Cmd msg
postCompletion token toMsg =
    let
        url =
            baseUrl ++ "chat/completions"
    in
    request
        { method = "POST"
        , url = url
        , headers = List.map (Http.header "Authorization") [ "Bearer " ++ token ]
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.jsonBody (compEncoder exampleData)
        , expect = compDecoder |> expectJson (RemoteData.fromResult >> toMsg)
        }


compEncoder : CompExample -> Encode.Value
compEncoder compExmp =
    Encode.object
        [ ( "model", Encode.string compExmp.model )
        , ( "messages", Encode.list encodeMessage compExmp.messages )
        ]


encodeMessage : Message -> Encode.Value
encodeMessage msg =
    Encode.object
        [ ( "role", Encode.string msg.role )
        , ( "content", Encode.string msg.content )
        ]


compDecoder : Decoder Comp
compDecoder =
    Decode.succeed Comp
        |> required "id" idDecoder


idDecoder : Decoder CompId
idDecoder =
    Decode.map CompId string
