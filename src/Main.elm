module Main exposing (main)

import Browser
import Browser.Events exposing (onMouseMove)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as D
import List exposing (concat, drop, filter, head, map, range, repeat, reverse)
import Random as R
import String exposing (fromFloat, fromInt, join)
import Svg exposing (Svg, clipPath, defs, image, path, svg)
import Svg.Attributes as SA exposing (fill, height, stroke, viewBox, width, xlinkHref)


main : Program () { positions : List { pos : Vec, col : Int, row : Int }, picked : Maybe { col : Int, row : Int, pos : Vec } } Msg
main =
    Browser.element
        { init = init, update = update, view = view, subscriptions = subscriptions }


subscriptions { picked } =
    case picked of
        Nothing ->
            Sub.none

        Just _ ->
            onMouseMove moveDecode


moveDecode =
    D.map2 Moved
        (D.field "movementX" D.float)
        (D.field "movementY" D.float)


init _ =
    let
        rFloat =
            R.float 0 100

        ( randomOffsets, _ ) =
            R.step (R.list (cfg.tileSize * cfg.tileSize) (R.pair rFloat rFloat)) (R.initialSeed 42)

        addOffset ( rx, ry ) tilePos =
            { tilePos | pos = add tilePos.pos (vec rx ry) }
    in
    ( { positions =
            range 0 (cfg.res - 1)
                |> map (\row -> range 0 (cfg.res - 1) |> map (\col -> { col = col, row = row, pos = vec 0 0 }))
                |> concat
                |> List.map2 addOffset randomOffsets
      , picked = Nothing
      }
    , Cmd.none
    )


type Msg
    = Pick Int Int
    | Moved Float Float
    | Drop


update msg model =
    case msg of
        Drop ->
            case model.picked of
                Nothing ->
                    ( model, Cmd.none )

                Just pos ->
                    ( { model | picked = Nothing, positions = model.positions ++ [ pos ] }, Cmd.none )

        Moved dx dy ->
            case model.picked of
                Nothing ->
                    ( model, Cmd.none )

                Just pos ->
                    ( { model | picked = Just { pos | pos = add pos.pos (vec dx dy) } }, Cmd.none )

        Pick col row ->
            let
                hasCoords pos =
                    pos.col == col && pos.row == row

                picked =
                    head <| filter hasCoords model.positions

                positions =
                    filter (not << hasCoords) model.positions
            in
            ( { model | picked = picked, positions = positions }, Cmd.none )


type Edge
    = HEdge Vec Float
    | VEdge Vec Float
    | HJigSawEdge Vec (List Vec)


type alias Vec =
    { x : Float, y : Float }


cfg =
    { tileSize = 25, res = 4 }


vec x y =
    { x = x, y = y }


hEdge col row =
    if row == 0 || row == cfg.res then
        HEdge (origin col row) ((origin col row).x + cfg.tileSize)

    else
        hJigSawEdge col row


vEdge col row =
    if col == 0 || col == cfg.res then
        VEdge (origin col row) ((origin col row).y + cfg.tileSize)

    else
        vJigSawEdge col row


vJigSawEdge col row =
    HJigSawEdge (origin col row) <|
        List.map (add (origin col row)) <|
            List.map (times cfg.tileSize) <|
                List.map transpose bezierPoints


transpose { x, y } =
    vec y x


bezierPoints =
    [ vec 0.5 0
    , vec 0.4 0.1
    , vec 0.4 0.2
    , vec 0.4 0.3
    , vec 0.6 0.3
    , vec 0.6 0.2
    , vec 0.6 0.1
    , vec 0.5 0.0
    , vec 1.0 0.0
    ]


hJigSawEdge col row =
    HJigSawEdge (origin col row) <|
        List.map (add (origin col row)) <|
            List.map (times cfg.tileSize) bezierPoints


origin col row =
    vec (toFloat col * cfg.tileSize) (toFloat row * cfg.tileSize)


times n { x, y } =
    vec (x * n) (y * n)


add v0 v1 =
    vec (v0.x + v1.x) (v0.y + v1.y)


show { x, y } =
    fromFloat x ++ "," ++ fromFloat y


viewEdge edge =
    let
        d =
            case edge of
                HEdge start end ->
                    "M " ++ show start ++ " H " ++ fromFloat end

                VEdge start end ->
                    "M " ++ show start ++ " V " ++ fromFloat end

                HJigSawEdge start points ->
                    "M " ++ show start ++ "C " ++ (join " " <| List.map show points)
    in
    path [ SA.d d, stroke "red", fill "none" ] []


type Segment
    = HSeg Float
    | VSeg Float
    | JigSawSeg (List Vec)


mkTile col row =
    let
        segment edgeInvert =
            case edgeInvert of
                ( HEdge _ x, False ) ->
                    HSeg x

                ( VEdge _ y, False ) ->
                    VSeg y

                ( HJigSawEdge _ points, False ) ->
                    JigSawSeg points

                ( HEdge { x } _, True ) ->
                    HSeg x

                ( VEdge { y } _, True ) ->
                    VSeg y

                ( HJigSawEdge start points, True ) ->
                    JigSawSeg <|
                        (\ps -> ps ++ [ start ]) <|
                            drop 1 <|
                                reverse points

        top =
            hEdge col row

        right =
            vEdge (col + 1) row

        bottom =
            hEdge col (row + 1)

        left =
            vEdge col row
    in
    { start = origin col row
    , segments = map segment [ ( top, False ), ( right, False ), ( bottom, True ), ( left, True ) ]
    }


viewTile col row =
    let
        { start, segments } =
            mkTile col row

        viewSeg s =
            case s of
                HSeg x ->
                    "H " ++ fromFloat x

                VSeg y ->
                    "V " ++ fromFloat y

                JigSawSeg points ->
                    "C " ++ (join " " <| List.map show points)

        d =
            "M " ++ show start ++ " " ++ (join " " <| List.map viewSeg segments)
    in
    clipPath [ SA.id <| clipId col row ]
        [ Svg.path [ stroke "blue", fill "none", SA.d d ] [] ]


clipId col row =
    "cid-" ++ fromInt col ++ "-" ++ fromInt row


tileDefs =
    range 0 (cfg.res - 1)
        |> List.map (\row -> range 0 (cfg.res - 1) |> List.map (\col -> viewTile col row))
        |> List.concat


viewImage { col, row, pos } =
    image
        [ xlinkHref "/cat.png"
        , width "100"
        , height "100"
        , SA.clipPath <| "url(#" ++ clipId col row ++ ")"
        , SA.transform <| "translate(" ++ fromFloat pos.x ++ "," ++ fromFloat pos.y ++ ")"
        , onClick <| Pick col row
        ]
        []


viewPicked p =
    case p of
        Nothing ->
            []

        Just pos ->
            [ viewImage pos ]


view { positions, picked } =
    div (Maybe.withDefault [] <| Maybe.map (\_ -> [ onClick Drop ]) picked)
        [ svg [ viewBox "-10 -10 220 220", height "100%" ]
            (defs [] tileDefs :: map viewImage positions ++ viewPicked picked)
        ]
