module Error exposing (..)

import Http


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            if statusCode == 429 then
                errorMessage429

            else if statusCode == 401 then
                errorMessage401

            else
                "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message


errorMessage429 : String
errorMessage429 =
    "Your API key is not working. You need a paid API account on OpenAI in order to use the ChatGPT API Key (the free trial won't work). To verify that you have a paid API account, go here and make sure you have your billing info added: https://platform.openai.com/account/billing/overview. Note that you do not need to have a ChatGPT Plus subscription, it's not needed. If you already have a paid OpenAI account, check to see if you still have sufficient credits. Also, try creating a new API key and trying again. If this problem persists, please contact support."


errorMessage401 : String
errorMessage401 =
    "Incorrect API key provided. You can find your API key at https://platform.openai.com/account/api-keys"
