module Main exposing (..)

import Browser
import Html exposing (Html, button, input, div, text, li, ul)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (placeholder, value, class, classList)
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
    | Create Int
    | Content String
    | Delete Int
    | Update Int Bool

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      ( model
      , Random.generate Create (Random.int 1 9999)
      )

    Create id ->
      if not (String.isEmpty (String.trim model.content)) then
        ( { model | todos = Todo model.content False id :: model.todos }
        , Cmd.none
        )
      else
        (
          model
        , Cmd.none
        )

    Content content ->
      ( { model | content = content}
      , Cmd.none
      )

    Delete id ->
      ( { model | todos = List.filter (\todo -> todo.id /= id) model.todos }
      , Cmd.none
      )

    Update id done ->
      let
        updateTodo todo =
          if todo.id == id then
            { todo | done = done }
          else
            todo

        todos = List.map updateTodo model.todos
      in
        ( { model | todos = todos  }
        , Cmd.none
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div [] [
    div [] [
      input [ placeholder "Todo name", value model.content, onInput Content ] []
    ]
    , div [] [
        button [ onClick Roll ] [ text "Add todo" ]
      , div [] [
          viewList model.todos
        ]
    ]
  ]

viewList : List Todo -> Html Msg
viewList todos =
  ul [ class "list" ] (List.map viewTodo todos)

viewTodo : Todo -> Html Msg
viewTodo todo =
  li [
    classList [
      ("list-item", True),
      ("done", todo.done)
    ],
    onClick (Update todo.id (not todo.done))
  ] [
    text todo.text
    , button [ class "delete-button", onClick (Delete todo.id) ] [ text "x" ]
  ]