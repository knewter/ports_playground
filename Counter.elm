module Counter where

import Html exposing (..)
import StartApp
import Html.Events exposing (onClick)
import Effects exposing (Effects)

type alias Model =
  { count     : Int
  , increment : Int
  , decrement : Int
  }
type Action = Increment | Decrement | NoOp


initialModel : Model
initialModel =
  { count = 0
  , increment = 0
  , decrement = 0
  }


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Increment ->
      ( { model |
            count = model.count + 1,
            increment = model.increment + 1
        }
      , Effects.none
      )
    Decrement ->
      ( { model |
            count = model.count - 1,
            decrement = model.decrement + 1
        }
      , Effects.none
      )
    NoOp ->
      ( model, Effects.none )


view : Signal.Address Action -> Model -> Html
view address model =
  div []
  [
    button [ onClick address Decrement ] [ text "-" ]
  , div [] [text (toString model.count)]
  , button [ onClick address Increment ] [ text "+" ]
  , h3 [] [ text ("- clicked " ++ (toString model.decrement) ++ " times") ]
  , h3 [] [ text ("+ clicked " ++ (toString model.increment) ++ " times") ]
  ]


app =
  StartApp.start
    { init = (initialModel, Effects.none)
    , update = update
    , view = view
    , inputs =
      [ Signal.map mapJsAction jsActions
      ]
    }


main =
  app.html


mapJsAction : Int -> Action
mapJsAction int =
  case int of
    1 ->
      Increment
    _ ->
      NoOp


port jsActions : Signal Int
