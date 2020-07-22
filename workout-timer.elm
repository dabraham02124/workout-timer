module Main exposing (..)
--   https://guide.elm-lang.org/effects/time.html

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Task
import Time
import Html.Events exposing (onClick)


-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model = 
  { soFar : Int
  , go : Int
  , rest : Int
  , time : Int
  , times : Int
  , active : Bool
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model -5 8 10 0 12 True
  , Cmd.none
  )


-- UPDATE

type Msg
  = Tick Time.Posix
  | Go Int
  | Rest Int
  | Times Int
  | Reset
  | Flip 


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  ( case msg of
      Tick newTime -> { model | soFar = 
          if model.active then
              model.soFar + 1
          else
              model.soFar
         , time = 
             if ((modBy (model.go + model.rest) (model.soFar + 1)) == 0) then
                 model.time + 1
             else
                 model.time
         , active = if (not model.active || (((model.go + model.rest) * model.times) - model.rest) <= model.soFar) then
                 False
             else
                 True }
      Go    go     -> { model | go = go }
      Rest  rest   -> { model | rest = rest }
      Times times  -> { model | times = times }
      Reset  -> { model | soFar = -5 }
      Flip   -> { model | active = not model.active }
    , Cmd.none )
-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick


-- VIEW

view : Model -> Html Msg
view model =
  let
    size = "2500%"
    subTime1 = modBy (model.go + model.rest) model.soFar
    subTime2 = subTime1 - model.go
    startStop = if model.active then "Stop" else "Start"
  in
    div [] [
    div [] [ button [ onClick Reset ] [text "Reset" ]],
    div [] [ button [ onClick Flip ] [text startStop ]],
    div [style "font-size" "400%"] [ text ((String.fromInt (model.time)) ++" / "++ (String.fromInt model.times)) ],
    if not model.active then
      div [style "color" "red", style "font-size" size] [ text "DONE" ]
    else if model.soFar < 0 then
      div [style "color" "purple", style "font-size" size] [ text (String.fromInt model.soFar) ]
    else if subTime2 < 0 then
      div [style "color" "green", style "font-size" size] [ text (String.fromInt (model.go - subTime1)) ]
    else
      div [style "color" "blue", style "font-size" size] [ text (String.fromInt (model.rest - subTime2)) ]
    ]
