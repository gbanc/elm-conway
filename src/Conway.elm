module Conway exposing (..)

import Browser
import Html exposing (Html, text, pre, div, table, button)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (range)
import List exposing (indexedMap)
import Html exposing (datalist)
import Time
import Task


-- MAIN

main =
    Browser.element { init = init, update=update, view = view, subscriptions=subscriptions}

mapSize = 32


type alias Model =
  { matrix : List Int
  , time : Time.Posix
  , generate: Bool
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model generateInitialMatrix (Time.millisToPosix 0) False
  , Cmd.none
  )

generateInitialMatrix : List Int
generateInitialMatrix = List.map (\n -> 0) (range 1 (mapSize*mapSize))

type Msg = 
     NewGeneration Int 
    | SetCellValue Int 
    | GenerateX Bool



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1 (\n-> if (model.generate==True) then NewGeneration 1 else NewGeneration 0)



-- UPDATE 

updateMatrix : Int -> List Int -> List Int
updateMatrix cellpos matrix =
    List.indexedMap (mapListByIndex cellpos) matrix

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GenerateX x ->
        ({model | generate=x}, Cmd.none)
    NewGeneration x ->
      case x of
          1 -> ({model | matrix=(life model.matrix)}, Cmd.none)
          0 -> ({ model | matrix=model.matrix}, Cmd.none)
          _ -> update (NewGeneration (x - 1)) ({model| matrix=life model.matrix})
          
    SetCellValue cellpos ->
      ({model | matrix=updateMatrix cellpos model.matrix}, Cmd.none)



-- GENERATE VIEW

mapListByIndex : Int -> Int -> Int -> Int
mapListByIndex cellpos pos val =
    if pos == cellpos then
        if val == 0 then 1
        else 0
    else val

generateRow = 
    let
        rangelist = range 1 mapSize
    in
    List.map (\n -> 0) rangelist

getBackgroundColor : Int -> String
getBackgroundColor celval =
    if celval == 1
    then "black"
    else "white"

toHtmlText : Int -> Int  -> Int -> Html Msg
toHtmlText counter cellindex cellval  = 
    let realCellIdx = cellindex + (counter * mapSize)
    in
    Html.td [style "padding" "1px"][
        button [style "background-color" (getBackgroundColor cellval), onClick (SetCellValue realCellIdx) ] [
            text (String.fromInt (cellval))
        ]
    ]

toTableRow :  List Int -> Int -> Html Msg
toTableRow longlist counter =
    let
        limitlist = List.drop (mapSize*(counter+1) - mapSize) (List.take (mapSize*(counter+1)) longlist)
    in
    
    if (List.length limitlist == mapSize) then
        Html.tr []
            (List.indexedMap (toHtmlText counter) limitlist)
    else 
        text ""
        
view : Model -> Html Msg
view model =
  div []
    [ button [ onClick (NewGeneration 1)] [ text "New Generation" ]
    , button [ onClick (GenerateX (not model.generate)) ] [ text "Start/Stop Generations" ]
    , table [] (List.map (toTableRow model.matrix) (range 0 (mapSize - 1)))
    ]



-- CREATE LIFE

life : List Int -> List Int
life model =
    List.map (neighborCount model) (range 0 (mapSize*mapSize))

getDeltaPermutations delta = 
    delta 
    |> List.map (\n-> List.map (\i->(i, n)) delta )
    |> List.concat 
    

neighborCount : List Int -> Int -> Int
neighborCount matrix cellidx =
    let cellRow = cellidx//mapSize
        cellCol = modBy mapSize cellidx
        delta = [-1, 0, 1]
        permutations = getDeltaPermutations delta
        cell_neighbors_count  = 
            permutations
                |> List.map (mapDeltas cellRow cellCol matrix)
                |> List.sum 
        cellVal = Maybe.withDefault 0 (get cellidx matrix)
    in

    if  (cell_neighbors_count == 2 && cellVal == 1) 
        then 1
    else if (cell_neighbors_count == 3)
        then 1
    else if (cell_neighbors_count > 3 && cellVal == 1)
        then 0
    else if (cell_neighbors_count < 2 && cellVal == 1)
        then 0
    else 0

mapDeltas : Int -> Int -> List Int -> (Int, Int) -> Int
mapDeltas row column model (delta_row, delta_col) =
    let neighbor_row = modBy mapSize (row + delta_row)
        neighbor_col = modBy mapSize (column + delta_col)
        idx = (neighbor_row*mapSize) + neighbor_col
    in

    if delta_row == 0 && delta_col == 0 then
        0
    else
        Maybe.withDefault 0 (get idx model)
get : Int -> List Int -> Maybe Int       
get n xs  = List.head (List.drop n xs)


