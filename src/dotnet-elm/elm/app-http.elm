port module AppHttp exposing (..)

import Html exposing (..)
import String exposing (trim, isEmpty)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode

main : Program Never Model Msg
main =
  Html.program
    { init = init "0"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias User =
  { login: String
    , id: Int
    , avatar_url: String
  }

type alias Model =
  { since : String
  , users : UsersData
  }


init : String -> (Model, Cmd Msg)
init since =
  ( Model since NotLoaded
  , Cmd.none
  )

-- UPDATE

type UsersData
  = NotLoaded
  | Loading
  | Success (List User)
  | Error Http.Error

type Msg
  = Clear
  | Fetching
  | Fetched (Result Http.Error (List User))
  | Since String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Clear ->
      ({model | users = NotLoaded}, Cmd.none)
    Since since ->
      ({model | since = since}, Cmd.none)
    Fetching ->
      ({model | users = Loading}, getUsers model.since)
    Fetched (Ok users) ->
      ({model | users = Success users}, Cmd.none)
    Fetched (Err error) ->
      ({model | users = Error error}, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div [] 
    [ div [class "row"]
      [ div [class "col-xs-12 col-sm-6 http-form"]
        [ Html.form [(onWithOptions "submit" (Options True True) (Decode.succeed Fetching))]
          [ div [class "form-group"]
            [ label [ for "sinceInput" ][text "From (ID)"]
              , input [id "sinceInput", type_ "number", class "form-control", placeholder "From (ID)", value model.since ,onInput Since] []
            ]
          ]
          , input [ type_ "submit", class "btn btn-primary", value "Get users", onClick Fetching ] [] 
          , input [ type_ "button", class "btn btn-default pull-right", value "Clear", onClick Clear ] [] 
        ]
      ]
    , div [ class "row" ]
      [ div [class "col-xs-12 col-sm-6 http-users-data"] [ usersDataView model.users ]
      ]
    ]

usersDataView: UsersData -> Html Msg
usersDataView usersData =
  case usersData of
    NotLoaded ->
      div [class "alert alert-info"] [p [] [ text "Not loaded" ]]
    Loading ->
      img [ height 60, class "loading-spinner", src "https://www.quodfinancial.com/wp-content/themes/pro-child/QUOD-Diagram/loading_spinner.gif" ] []
    Success users ->
      div [] (List.map userView users)
    Error error ->
      div [class "alert alert-error"][ p [] [text "Cannot load users. Please try again."]]
        

userView: User -> Html Msg
userView user =
  div [] 
  [ p [] [text ("User id" ++ (toString user.id))]
    , ul []
    [ li [] [ text ("Login: " ++ user.login)]
      , li [] [ img [ class "user-image", height 150, src user.avatar_url ][] ]
    ]
  ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- HTTP

getUsers : String -> Cmd Msg
getUsers since =
  let
    input = if (trim since) |> isEmpty then "0" else since
    url = "https://api.github.com/users?since=" ++ input
  in
    Http.send Fetched (Http.get url (Decode.list userDecoder))


userDecoder : Decode.Decoder User
userDecoder =
  Decode.map3 User
    (Decode.field "login" Decode.string )
    (Decode.field "id" Decode.int )
    (Decode.field "avatar_url" Decode.string )
