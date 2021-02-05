module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

-- MAIN

main: Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Todo =
  { name: String, done: Bool, id: Int }

type alias Model =
  { todo: Todo, todos: List Todo }

init : Model
init =
  Model (Todo "" False 0) []



-- UPDATE


type Msg
  = Add Todo
  -- | Delete Int
  -- | Complete Int

update : Msg -> Model -> Model
update msg _ =
  case msg of
    Add todo ->
      Model todo (List.append [todo] init.todos)



-- VIEW


view : Model -> Html Msg
view model =
  div [] [
    button [ onClick <| Add <| Todo "" False 1 ] [ text "Add todo" ]
  , button [] [ text "Delete todo" ]
  , button [] [ text "Complete todo" ]
  , div [] [ text <| Debug.toString model.todos ]
  ]
