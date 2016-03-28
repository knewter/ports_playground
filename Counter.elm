module Counter where

import Html exposing (..)
import StartApp
import Html.Events exposing (onClick)
import Effects exposing (Effects)
import Task exposing (Task)

type alias Model =
  { count     : Int
  , increment : Int
  , decrement : Int
  }
type Action = Increment | Decrement | SetCounter Int | NoOp


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
      let
          newModel =
            { model |
                count = model.count + 1,
                increment = model.increment + 1
            }
      in
        ( newModel
        , Effects.batch
            [ sendToIncrementMailbox
            , sendToStorageMailbox newModel.count
            ]
        )
    Decrement ->
      let
        newModel ={ model |
            count = model.count - 1,
            decrement = model.decrement + 1
        }
      in
      ( newModel
      , sendToStorageMailbox newModel.count
      )
    SetCounter int ->
      ( { model |
            count = int
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
      , Signal.map mapStorageInput storageInput
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


mapStorageInput : Int -> Action
mapStorageInput int =
  SetCounter int


incrementMailbox : Signal.Mailbox ()
incrementMailbox =
  Signal.mailbox ()


sendToIncrementMailbox : Effects Action
sendToIncrementMailbox =
  Signal.send incrementMailbox.address ()
  |> Effects.task
  |> Effects.map (always NoOp)


storageMailbox : Signal.Mailbox Int
storageMailbox =
  Signal.mailbox 0


sendToStorageMailbox : Int -> Effects Action
sendToStorageMailbox count =
  Signal.send storageMailbox.address count
  |> Effects.task
  |> Effects.map (always NoOp)


-- INPUT PORTS
port jsActions : Signal Int

port storageInput : Signal Int

-- OUTPUT PORTS
port increment : Signal ()
port increment =
  incrementMailbox.signal


port storage : Signal Int
port storage =
  storageMailbox.signal


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
