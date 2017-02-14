import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Html.Events exposing (onClick)


main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , age: String
  , valid: Bool
  }


model : Model
model =
  Model "" "" "" "" False



-- UPDATE


type Msg
    = Submit
    | Name String
    | Password String
    | PasswordAgain String
    | Age String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Submit ->
      if (String.length model.password > 8)
      then {model | valid = True}
      else model

    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }

    Age age ->
      { model | age = age }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [ type_ "text", placeholder "Name", onInput Name ] []
    , input [type_ "text", placeholder "Age", onInput Age] []
    , input [ type_ "password", placeholder "Password", onInput Password ] []
    , input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
    , button [onClick Submit][text "Submit"]
    ]


viewValidation : Model -> Html msg
viewValidation model =
  let
    (color, message) =
      if (String.length model.password <= 8) then
        ("red", "Passwords must be greater than 8 characters")
      else if (model.password /= model.passwordAgain) then
        ("red", "Passwords do not match!")
      else
        ("green", "OK")
  in
    div [ style [("color", color)] ] [ text message ]
