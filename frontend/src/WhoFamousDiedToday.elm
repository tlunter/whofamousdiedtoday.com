module WhoFamousDiedToday exposing (main)

import List exposing (..)
import String exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Json.Decode as Json
import Json.Decode exposing ((:=))
import Http
import Task

main : Program Never
main =
  Html.program { init = init, view = view, update = update, subscriptions = subscriptions }

--
-- My type declarations
--
type alias Death = { firstName : String, lastName : String, link : String }

type alias Model = List Death

type Msg
    = FetchSucceed Model
    | FetchFail Http.Error

--
-- App functions
--
init : (Model, Cmd Msg)
init = ([], getDeaths)

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    FetchSucceed nextModel -> (nextModel, Cmd.none)
    FetchFail err          -> (model, Cmd.none)

viewDeath : Death -> Html Msg
viewDeath d = div [class "death"]
                [ p [class "name"] [a [href ("http://reddit.com" ++ d.link)] [text d.firstName, text " ", text d.lastName] ] ]

view : Model -> Html Msg
view model = div [class "app"] (List.map viewDeath model)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

parseDeaths : Json.Decoder Model
parseDeaths =
    Json.list (
        Json.object3 Death
            ("firstName" := Json.string)
            ("lastName" := Json.string)
            ("link" := Json.string)
    )

getDeaths : Cmd Msg
getDeaths =
    Task.perform FetchFail FetchSucceed (Http.get parseDeaths "http://localhost:3000/deaths/")
