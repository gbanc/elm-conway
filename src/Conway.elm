module Conway exposing (..)

import Browser
import Html exposing (Html, text, pre, div, table, button)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (range)
import List exposing (indexedMap)
import Html exposing (datalist)
import Array


-- MAIN

main =
    Browser.sandbox { init = init, update=update, view = view}

mapSize = 32

init = List.map (\n -> 0) (range 1 (mapSize*mapSize))

type Msg = NewGeneration | SetCellValue Int



-- UPDATE 

updateMatrix : Int -> List Int -> List Int
updateMatrix cellpos matrix =
    List.indexedMap (mapListByIndex cellpos) matrix

update msg model =
  case msg of
    NewGeneration ->
      life model
    SetCellValue cellpos ->
      updateMatrix cellpos model



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
        
view : List Int -> Html Msg
view model =
  div []
    [ button [ onClick NewGeneration ] [ text "-" ]
    , table [] (List.map (toTableRow model) (range 0 (mapSize - 1)))
    ]



-- CREATE LIFE

life : List Int -> List Int
life model =
    List.map (neighborCount model) (range 0 (mapSize*mapSize))
        
neighborCount : List Int -> Int -> Int
neighborCount matrix cellidx =
    let cellRow = cellidx//mapSize
        cellCol = modBy mapSize cellidx
        delta = [-1, 0, 1]
        cell_neighbors_count  = List.sum (List.map (mapDeltaRows cellRow cellCol matrix) delta)
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

mapDeltaRows : Int -> Int -> List Int -> Int -> Int
mapDeltaRows row column matrix deltaRow = 
    let delta = [-1, 0, 1]
    in
    List.foldl (+) 0 (List.map (mapDeltaCols deltaRow row column matrix) delta)

mapDeltaCols : Int -> Int -> Int -> List Int -> Int -> Int
mapDeltaCols delta_row row column model delta_col =
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


