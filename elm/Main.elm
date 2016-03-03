module Main (main, valid) where

import Effects exposing (none)
import Html exposing (Html, h1, text, div, button)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List exposing (member, sort)
import Random exposing (Generator, Seed, int, list, initialSeed)
import Signal exposing (Address, map)
import StartApp exposing (App, start)
import Time exposing (every, millisecond)

-- Generators

upperGenerator : Generator Values
upperGenerator = list 5 <| int 1 50

lowerGenerator : Generator Values
lowerGenerator = list 2 <| int 1 10

valid : Values -> Bool
valid values =
  case values of
    [] ->
      True
    (value :: values) ->
      (not <| member value values) && valid values

generate : Generator Values -> Seed -> (Values, Seed)
generate generator prevSeed =
  let
    (values, nextSeed) = Random.generate generator prevSeed
  in
    if valid values
      then
        (values, nextSeed)
      else
        generate generator nextSeed

-- Model

type alias Value =
  Int

type alias Values =
  List Value

type alias Model =
  {
    state : Bool,
    upperValues : Values,
    lowerValues : Values
  }

init : Model
init =
  {
    state = True,
    upperValues = [],
    lowerValues = []
  }

-- Update

type Action =
  Refresh Float |
  Toggle

update: Action -> Model -> Model
update action model =
  case action of
    Refresh time ->
      if model.state
        then
          let
            upperSeed = initialSeed <| round <| time
            (upperValues, lowerSeed) = generate upperGenerator upperSeed
            (lowerValues, _) = generate lowerGenerator lowerSeed
          in
            {
              model |
              upperValues = sort upperValues,
              lowerValues = sort lowerValues
            }
        else
          model
    Toggle ->
      {
        model |
        state = not model.state
      }

-- View

value : Value -> Html
value =
  toString >> (++) " " >> text

values : Values -> Html
values =
  List.map value >> h1 []

toggle : Address Action -> Bool -> Html
toggle address state =
  button
    [
      onClick address Toggle
    ]
    [text <| if state then "Stop" else "Start"]

view : Address Action -> Model -> Html
view address model =
  div
    [
      style [("text-align", "center")]
    ]
    [
      values model.upperValues,
      values model.lowerValues,
      toggle address model.state
    ]

-- Inputs

inputs : List (Signal Action)
inputs = [map (\time -> Refresh time) (every millisecond)]

-- App

app : App Model
app =
  start 
    {
      init = (init, none),
      update = (\action model -> (update action model, none)),
      view = view,
      inputs = inputs
    }

-- Main

main : Signal Html
main = app.html

