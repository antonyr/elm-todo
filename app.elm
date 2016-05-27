import Html exposing (Html, button, div, text, input, label)
import Html.App as HtmlApp
import Html.Attributes exposing(class, style, placeholder, type', id, autofocus, value, for)
import Html.Events exposing (onClick, onInput, on, keyCode, targetChecked, onCheck)
import Debug exposing (log)
import String
import Json.Decode as Json

main =
  HtmlApp.beginnerProgram { model = model, view = view, update = update }

type alias Task = {
  name: String
  , task_id: Int
  , complete: Bool
}

type alias Model = {
  tasks: List Task
  , text: String
  , count: Int

}

type Msg
  = TextContent String
  | PressToCreate Int
  | CompleteOrIncomplete Bool Int
  | Create
  | DeleteAll

model : Model
model =
  {text = "", tasks = [], count= 1}

onKeyUp : (Int -> msg) -> Html.Attribute msg
onKeyUp tagger =
  on "keyup" (Json.map tagger keyCode)

update: Msg -> Model -> Model
update action model = 
  case action |> log "action" of

    TextContent inputText ->
      {model | text = inputText}

    PressToCreate code->
      if code == 13 then
        createRecord model
      else
        model

    CompleteOrIncomplete checked task_id ->
      {model | tasks = List.map (\task -> update_task task task_id checked) model.tasks}

    Create ->
      createRecord model

    DeleteAll ->
      {model | tasks = [], count = 1}

update_task: Task -> Int -> Bool -> Task
update_task task task_id checked = 
  if task.task_id == task_id then
    {task | complete = checked}
  else
    task

createRecord: Model -> Model
createRecord model =
  if String.isEmpty model.text then
    model
  else
    {model | count = model.count + 1, text = "", tasks = model.tasks ++ [{name = model.text, task_id = model.count, complete = False}]}

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
        taskHtml  (model.tasks)) 
    , div [class "button-group"] [
      button [ type' "button", class "success button", onClick Create ] [ text "Add Task" ]
    , button [ type' "button", class "alert button", onClick DeleteAll ] [ text "Clear All" ]
    ]]

taskHtml: Task -> Html Msg
taskHtml task =
  div [class "row"] [
    div [class "small-12 columns"] [
      input [type' "checkbox", id ("checkbox_" ++ toString task.task_id), onCheck (\bool -> CompleteOrIncomplete bool task.task_id)][],
      label [for ("checkbox_" ++ toString task.task_id), class (if task.complete then "strikethrough" else "")] [text task.name]
    ]
  ]