module Main exposing (..)
--   https://guide.elm-lang.org/effects/time.html

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Task
import Time
import Html.Events exposing (onClick, onInput)


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
  ( Model -5 8 10 0 12 False
  , Cmd.none
  )


-- UPDATE

type Msg
  = Tick Time.Posix
  | Go String
  | Rest String
  | Times String
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
         , active = not (shouldBeActive model) }
      Go    go     -> { model | go = stringToInt go model.go }
      Rest  rest   -> { model | rest = stringToInt rest model.rest }
      Times times  -> { model | times = stringToInt times model.times }
      Reset  -> { model | soFar = -5
          , time = 0
          , active = False}
      Flip   -> { model | active = not model.active }
    , Cmd.none )

shouldBeActive : Model -> Bool
shouldBeActive model =
  (not model.active || (((model.go + model.rest) * model.times) - model.rest) <= (model.soFar+1))

stringToInt : String -> Int -> Int
stringToInt string default =
  Maybe.withDefault default (String.toInt string)

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
      viewInput "Go time" "text" model.go Go
      , viewInput "Rest time" "text" model.rest Rest
      , viewInput "Number of rounds" "text" model.times Times
      , div [] [ button [ onClick Reset ] [text "Reset" ]]
      , div [] [ button [ onClick Flip ] [text startStop ]]
      , div [style "font-size" "400%"] [ text ((String.fromInt (model.time)) ++" / "++ (String.fromInt model.times)) ]
      , if not model.active then
          div [style "color" "red", style "font-size" size] [ text "DONE" ]
        else if model.soFar < 0 then
          div [style "color" "purple", style "font-size" size] [ text (String.fromInt model.soFar) ]
        else if subTime2 < 0 then
          div [style "color" "green", style "font-size" size] [ text (String.fromInt (model.go - subTime1)) ]
        else
          div [style "color" "blue", style "font-size" size] [ text (String.fromInt (model.rest - subTime2)) ]
    ]

viewInput : String -> String -> Int -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value (String.fromInt v), onInput toMsg ] []
