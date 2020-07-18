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

type alias Model = Int


init : () -> (Model, Cmd Msg)
init _ =
  ( -2
  , Cmd.none
  )


-- UPDATE

type Msg
  = Tick Time.Posix


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( model + 1
      , Cmd.none
      )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick


-- VIEW

view : Model -> Html Msg
view model =
  let
    go = 4
    rest = 3
    times = 2
    subTime1 = modBy (go + rest) model
    subTime2 = subTime1 - go
    done = (((go + rest) * times) - rest) <= model
  in
    if model < 0 then
      div [style "color" "purple"] [
        h1 [style "font-size" "800%"] [ text (String.fromInt model) ]
      ]
    else if done then
      div [style "color" "red"] [
        h1 [style "font-size" "800%"] [ text "DONE" ]
      ]
    else if subTime2 < 0 then
      div [style "color" "green"] [
        h1 [style "font-size" "800%"] [ text (String.fromInt (go - subTime1)) ]
      ]
    else
      div [style "color" "blue"] [
        h1 [style "font-size" "800%"] [ text (String.fromInt (rest - subTime2)) ]
      ]