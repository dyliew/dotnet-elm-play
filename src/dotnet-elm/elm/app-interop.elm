port module AppInterop exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

main: Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type DateStatus
  = Empty
  | Valid Bool String


type alias Model =  
  { input: String
  , result: DateStatus
  }

init: (Model, Cmd Msg)
init =
  ( Model "" Empty
  , Cmd.none)

-- UPDATE
type Msg
  = UpdateInput String
  | UpdateResult Bool
  | Validate

port dateValidation : String -> Cmd msg

testFunc: Bool -> Result String String
testFunc bool =
  if bool then Ok "true" else Err "false"

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateInput input ->
      ({model | input = input}, Cmd.none)
    UpdateResult isValid ->
      let 
        result = if isValid then (Valid True "Date is valid") else (Valid False "Date is invalid")
      in
        ({model | result = result}
        , Cmd.none
        )
    Validate ->
      (model, dateValidation model.input)

-- SUBSCRIPTIONS
port dateValidationResult : (Bool -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  dateValidationResult UpdateResult

-- VIEW
view : Model -> Html Msg
view model =
  div [][]