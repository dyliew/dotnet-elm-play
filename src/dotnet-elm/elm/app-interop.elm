port module AppInterop exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode

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
  -- | Invalid String


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
  | UpdateResult (Maybe Bool)
  | Validate

port dateValidation : String -> Cmd msg

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateInput input ->
      ({model | input = input}, Cmd.none)
    UpdateResult maybeIsValid ->
      let 
        result = 
          case maybeIsValid of
            Nothing ->
              (Valid False "Error response from moment.js")
            Just isValid ->
              if isValid then (Valid True "Date is valid") else (Valid False "Date is invalid")
      in
        ({model | result = result}
        , Cmd.none
        )
    Validate ->
      (model, dateValidation model.input)

-- SUBSCRIPTIONS
port dateValidationResult : (Maybe Bool -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  dateValidationResult UpdateResult

-- VIEW
view : Model -> Html Msg
view model =
  div [] [
    div [ class "row"]
      [ div [class "col-xs-12 col-sm-6 interop-form"]
        [ Html.form [(onWithOptions "submit" (Options True True) (Decode.succeed Validate))]
          [ div [class "form-group"]
            [ label [ for "dateInput" ][text "Date string"]
              , input [id "dateInput", type_ "text", class "form-control", placeholder "DD-MM-YYYY", value model.input , onInput UpdateInput] []
            ]
          ]
          , input [ type_ "submit", class "btn btn-primary", value "Validate", onClick Validate ] [] 
        ]
      ]
    , div [ class "row" ]
      [ div [class "col-xs-12 col-sm-6 validation-result"] [ validationResultView model.result ]
      ]
  ]

validationResultView : DateStatus -> Html Msg
validationResultView dateStatus =
  case dateStatus of
    Empty ->
      text ""
    Valid isValid message ->
      let
        alertType = if isValid then "success" else "danger"
      in
        div [ class ("alert alert-" ++ alertType) ]
          [ p [] [ text message ] ]