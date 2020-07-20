--   https://guide.elm-lang.org/effects/time.html

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Task
import Time

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
  , times : Int
  , active : Bool
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model -5 3 2 2 True
  , Cmd.none
  )


-- UPDATE

type Msg
  = Tick Time.Posix
  | Go Int
  | Rest Int
  | Times Int
  | Reset Int
  | Stop Bool


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =

  ( case msg of
      Tick newTime -> { model | soFar = if model.active then model.soFar + 1 else model.soFar}
      Go    go     -> { model | go = go }
      Rest  rest   -> { model | rest = rest }
      Times times  -> { model | times = times }
      Reset soFar  -> { model | soFar = soFar }
      Stop  active -> { model | active = active }
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
    done = (((model.go + model.rest) * model.times) - model.rest) <= model.soFar
  in
    div [] [ button [] [text "foo"], 
    if done then
      div [style "color" "red", style "font-size" size] [ text "DONE" ]
    else if model.soFar < 0 then
      div [style "color" "purple", style "font-size" size] [ text (String.fromInt model.soFar) ]
    else if subTime2 < 0 then
      div [style "color" "green", style "font-size" size] [ text (String.fromInt (model.go - subTime1)) ]
    else
      div [style "color" "blue", style "font-size" size] [ text (String.fromInt (model.rest - subTime2)) ]
    ]  
      