module Conway exposing (..)

import Browser
import Html exposing (Html, text, div, table, button)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (range)
import Time
import Random


-- MAIN

main =
    Browser.element { init = init, update=update, view = view, subscriptions=subscriptions}

mapSize : Int
mapSize = 38


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
    | AddRandomGosperGun



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1 (\n-> if (model.generate==True) then NewGeneration 1 else NewGeneration 0)



-- GENERATE VIEW

mapListByIndex : Int -> Int -> Int -> Int
mapListByIndex cellpos pos val =
    if pos == cellpos then
        if val == 0 then 1
        else 0
    else val


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
        limitlist = 
            longlist 
                |> List.take (mapSize*(counter+1))
                |> List.drop (mapSize*(counter+1) - mapSize)
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
    , button [ onClick (AddRandomGosperGun) ] [ text "Add random Gosper Gun" ]
    , table [] (List.map (toTableRow model.matrix) (range 0 (mapSize - 1)))
    ]



-- UPDATE 

updateMatrix : Int -> List Int -> List Int
updateMatrix cellpos matrix =
    List.indexedMap (mapListByIndex cellpos) matrix

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddRandomGosperGun ->
        let
            randomInt = roll
            truncateMatrix = List.drop (mapSize + (List.length getGosperGun)) model.matrix
        in
        ({model | matrix = List.concat [getGosperGun, truncateMatrix]}, Cmd.none)
    GenerateX x ->
        ({model | generate=x}, Cmd.none)
    NewGeneration x ->
      case x of
          1 -> ({model | matrix=(life model.matrix)}, Cmd.none)
          _ -> (model, Cmd.none)

    SetCellValue cellpos ->
      ({model | matrix=updateMatrix cellpos model.matrix}, Cmd.none)

roll : Random.Generator Int
roll =
  Random.int 1 6

-- CREATE LIFE

life : List Int -> List Int
life model =
    List.map (livesOrDies model) (range 0 (mapSize*mapSize))

getDeltaPermutations : List Int -> List (Int, Int)
getDeltaPermutations delta = 
    delta 
        |> List.map (\n-> List.map (\i->(i, n)) delta )
        |> List.concat 
    

livesOrDies : List Int -> Int -> Int
livesOrDies matrix cellidx =
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


getGosperGun : List Int
getGosperGun =
    let gun = """
.........................O............
.......................O.O............
.............OO......OO............OO.
............O...O....OO............OO.
.OO........O.....O...OO...............
.OO........O...O.OO....O.O............
...........O.....O.......O............
............O...O.....................
.............OO......................."""
    in

    gun 
        |> String.toList
        |> List.map String.fromChar 
        |> List.filter (\n -> n /= "\n")
        |> List.map (\n->if ( n==".") then 0 else if ( n=="O") then 1 else 0)