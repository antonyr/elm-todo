import Html exposing (Html, button, div, text, input, node)
import Html.App as HtmlApp
import Html.Attributes exposing(class, style, placeholder, type', id, autofocus, value)
import Html.Events exposing (onClick, onInput, on, keyCode)
import Debug exposing (log)
import String
import Json.Decode as Json

main =
  HtmlApp.beginnerProgram { model = model, view = view, update = update }

type alias Model = {
  complete: List String
  , incomplete: List String
  , count: Int
  , text: String
}

type Msg
  = TextContent String
  | PressToCreate Int
  | Create 
  | DeleteAll

model : Model
model =
  {complete = [], incomplete = [], count = 0, text = ""}

onKeyUp : (Int -> msg) -> Html.Attribute msg
onKeyUp tagger =
  on "keyup" (Json.map tagger keyCode)

update: Msg -> Model -> Model
update action model = 
  case action of
    TextContent inputText ->
      log inputText
      {model | text = inputText}

    PressToCreate code->
      if code == 13 then
        createRecord model
      else
        model

    Create ->
      createRecord model

    DeleteAll ->
      {model | incomplete = [], complete = []}

createRecord: Model -> Model
createRecord model =
  {model | incomplete = model.incomplete ++ [model.text], text = "", count = model.count + 1}

view : Model -> Html Msg
view model =
  div []
    [
    input [type' "text", placeholder "Add a task", 
      onInput TextContent, autofocus True,
      value model.text,
      onKeyUp PressToCreate
      ] []
    , div [id "tasks-holder"]
      (List.map 
        (\t -> div [] [text t])  (List.reverse model.incomplete))
    , div [class "button-group"] [
      button [ type' "button", class "success button", onClick Create ] [ text "Add Task" ]
    , button [ type' "button", class "alert button", onClick DeleteAll ] [ text "Clear All" ]
    ]]
