module Main exposing (..)

import Browser
import Html exposing (Html, button, input, div, text, li, ul)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (placeholder, value)
import Random

-- MAIN

main: Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- MODEL


type alias Todo =
  { text: String, done: Bool, id: Int }

type alias Model =
  { content: String, todo: Todo, todos: List Todo }

init : () -> (Model, Cmd msg)
init _ =
  ( Model "" (Todo "" False 0) []
  , Cmd.none
  )



-- UPDATE


type Msg
  = Roll
    | Add Int
    | Content String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      ( model
      , Random.generate Add (Random.int 1 999)
      )

    Add id ->
      ( { model | todos = Todo model.content False id :: model.todos }
      , Cmd.none
      )

    Content content ->
      ( { model | content = content}
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW

roll : Random.Generator Int
roll =
  Random.int 1 999

view : Model -> Html Msg
view model =
  div [] [
    div [] [
      input [ placeholder "Todo name", value model.content, onInput Content ] []
    ]
    , div [] [
        button [ onClick Roll ] [ text "Add todo" ]
      , button [] [ text "Delete todo" ]
      , button [] [ text "Complete todo" ]
      , div [] [
          viewList model.todos
        ]
    ]
  ]

viewList : List Todo -> Html msg
viewList todos =
  ul [] (List.map viewTodo todos)

viewTodo : Todo -> Html msg
viewTodo todo =
  li [] [ text todo.text ]