import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


type alias Model =
  { topic : String
  , gifUrl : String
  , err: String
  }

init : (Model, Cmd Msg)
init =
  (Model "cats" "waiting.gif" "error goes here", Cmd.none)


-- UPDATE

type Msg
  = MorePlease
  | NewGif (Result Http.Error String)
  | NewTopic String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (model, getRandomGif model.topic)
    NewGif (Ok newUrl) ->
      ({model | gifUrl = newUrl}, Cmd.none)
    NewGif (Err err) ->
      ({model | err = parseHttpError err}, Cmd.none)
    NewTopic t ->
      ({model | topic = t}, Cmd.none)

parseHttpError : Http.Error -> String
parseHttpError error =
  case error of
    Http.BadUrl url ->
      "Bad url provided: " ++ url

    Http.Timeout ->
      "Request timeout"

    Http.NetworkError ->
      "NetworkError"

    Http.BadStatus response ->
      "Unexpected status code: " ++ toString response.status.code

    Http.BadPayload _ _->
      "Unexpected response body"



getRandomGif : String -> Cmd Msg
getRandomGif topic =
  let
    url =
      "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic

    request =
      Http.get url decodeGifUrl
  in
    Http.send NewGif request

decodeGifUrl : Decode.Decoder String
decodeGifUrl =
  Decode.at ["data", "image_url"] Decode.string

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text model.topic]
    , input [ type_ "text", placeholder "Topic", onInput NewTopic ] []
    , p [] [text model.err]
    , img [src model.gifUrl] []
    , button [ onClick MorePlease ] [ text "More Please!" ]
    ]
