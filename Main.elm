module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import String
import Css exposing (..)
import Time exposing (Time, millisecond)


--import Css.Elements exposing (body, li)
--import Css.Namespace exposing (namespace)
-- model


type alias Model =
    { players : List Player
    , playerName : String
    , playerId : Maybe Int
    , plays : List Play
    }


type alias Player =
    { playerId : Int
    , playerName : String
    , points : Int
    , transparent : Bool
    , newlymade : Bool
    }


type alias Play =
    { id : Int
    , playerId : Int
    , playerName : String
    , points : Int
    , transparent : Bool
    , newlymade : Bool
    }


model : Model
model =
    { players = []
    , playerName = ""
    , playerId = Nothing
    , plays = []
    }



-- update


type Msg
    = Edit Player
    | PrepScore Player Int
    | Score Player Int
    | Input String
    | Save
    | Cancel
    | DeletePlay Play
    | HidePlay Play
    | DeletePlayer Player
    | Tick Time


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input playerName ->
            { model | playerName = playerName }

        Cancel ->
            { model | playerName = "", playerId = Nothing }

        DeletePlay play ->
            deletePlay model play

        HidePlay play ->
            hidePlay model play

        DeletePlayer player ->
            deletePlayer model player

        Edit player ->
            { model | playerName = player.playerName, playerId = Just player.playerId }

        Save ->
            if (String.isEmpty model.playerName) then
                model
            else
                save model

        Score player points ->
            score model player points

        PrepScore player points ->
            prepareScore model player points

        _ ->
            model


hidePlay : Model -> Play -> Model
hidePlay model play =
    let
        newPlays =
            List.map
                (\p ->
                    if p.id == play.id then
                        { play | transparent = True, points = 0 }
                    else
                        p
                )
                model.plays

        newPlayers =
            List.map
                (\player ->
                    if player.playerId == play.playerId then
                        { player | points = player.points - 1 * play.points }
                    else
                        player
                )
                model.players
    in
        { model | plays = newPlays, players = newPlayers }


deletePlay : Model -> Play -> Model
deletePlay model play =
    if play.transparent == True then
        let
            newPlays =
                List.filter (\p -> p.id /= play.id) model.plays
        in
            { model | plays = newPlays }
    else
        model


deletePlayer : Model -> Player -> Model
deletePlayer model player =
    let
        newPlays =
            {--List.map
                (\plays ->
                    if play.playerId == player.playerId then
                        deletePlay model play
                    else
                        play
                )
                model.plays--}
            List.filter (\p -> p.playerId /= player.playerId) model.plays

        newPlayers =
            List.filter (\p -> p.playerId /= player.playerId) model.players
    in
        { model | plays = newPlays, players = newPlayers }


prepareScore : Model -> Player -> Int -> Model
prepareScore model scorer points =
    let
        newPlayers =
            List.map
                (\player ->
                    if player.playerId == scorer.playerId then
                        { player | points = player.points + points }
                    else
                        player
                )
                model.players

        play =
            Play (List.length model.plays) scorer.playerId scorer.playerName points True True
    in
        { model | players = newPlayers, plays = play :: model.plays }


score : Model -> Player -> Int -> Model
score model scorer points =
    let
        newPlayers =
            List.map
                (\player ->
                    if player.playerId == scorer.playerId then
                        { player | points = player.points + points }
                    else
                        player
                )
                model.players

        play =
            Play (List.length model.plays) scorer.playerId scorer.playerName points False True
    in
        { model | players = newPlayers, plays = play :: model.plays }


save : Model -> Model
save model =
    case model.playerId of
        Just id ->
            edit model id

        Nothing ->
            add model


edit : Model -> Int -> Model
edit model playerId =
    let
        newPlayers =
            List.map
                (\player ->
                    if player.playerId == playerId then
                        { player | playerName = model.playerName }
                    else
                        player
                )
                model.players

        newPlays =
            List.map
                (\play ->
                    if play.playerId == playerId then
                        { play | playerName = model.playerName }
                    else
                        play
                )
                model.plays
    in
        { model
            | players = newPlayers
            , plays = newPlays
            , playerName = ""
            , playerId = Nothing
        }


add : Model -> Model
add model =
    let
        player =
            Player (List.length model.players) model.playerName 0 False True

        newPlayers =
            player :: model.players
    in
        { model | players = newPlayers, playerName = "" }



-- view


view : Model -> Html Msg
view model =
    div [ class "scoreboard", styles [ Css.width auto, maxWidth (px 900), marginLeft auto, marginRight auto, paddingLeft (px 10) ] ]
        [ h1
            [ styles [ color (rgb 255 255 255) ] ]
            [ Html.text "Score Keeper" ]
        , div [ styles [ displayFlex, flexDirection row, flexWrap Css.wrap ] ]
            [ div [ styles [ flexGrow (int 3), maxWidth (px 500), minWidth (px 400), marginBottom (px 20), marginRight auto, paddingRight (px 15) ] ]
                [ playerSection model
                , playerForm model
                ]
            , playSection model
            ]
        ]


playSection : Model -> Html Msg
playSection model =
    div [ styles [ flexGrow (int 1), maxWidth (px 400), minWidth (px 200), backgroundColor (rgba 255 255 255 0.8), borderRadius (px 3), padding (px 7) ] ]
        [ playListHeader
        , playList model
        ]


playListHeader : Html Msg
playListHeader =
    header []
        [ div
            [ styles cellStyles, styles [ textDecoration underline ] ]
            [ Html.text "Plays" ]
        , div
            [ styles cellStyles, styles [ textDecoration underline ], class "pull-right" ]
            [ Html.text "Points" ]
        ]


playList : Model -> Html Msg
playList model =
    model.plays
        |> List.map play
        |> div []


play : Play -> Html Msg
play play =
    Html.div
        [ {--if play.newlymade == True then
            styles prepRow
          else --}
          if play.transparent == True then
            styles hideRow
          else
            styles rowStyles
        ]
        [ i
            [ styles cellStyles
            , class "remove btn btn-xs btn-danger glyphicon glyphicon-remove"
            , onClick (HidePlay play)
            , onMouseLeave (DeletePlay play)
            ]
            []
        , div [ styles cellStyles ] [ Html.text play.playerName ]
        , div [ styles cellStyles, class "pull-right" ] [ Html.text (toString play.points) ]
        ]


playerSection : Model -> Html Msg
playerSection model =
    div [ styles [ backgroundColor (rgba 255 255 255 0.8), borderRadius (px 3), padding (px 7), marginBottom (px 10) ] ]
        [ playerListHeader
        , playerList model
        , pointTotal model
        ]


playerListHeader : Html Msg
playerListHeader =
    header []
        [ div [ styles cellStyles, styles [ textDecoration underline ] ] [ Html.text "Name" ]
        , div [ styles cellStyles, styles [ textDecoration underline ], class "pull-right" ] [ Html.text "Points" ]
        ]


playerList : Model -> Html Msg
playerList model =
    --ul []
    --  (List.map player model.players)
    model.players
        |> List.sortBy .playerName
        |> List.map (player model.playerId)
        |> div []


player : Maybe Int -> Player -> Html Msg
player editPlayerId player =
    Html.div [ styles rowStyles ]
        [ div [ styles cellStyles, styles [ Css.width (px 200), lineHeight (px 22) ], styles (editPlayerClass editPlayerId player) ]
            [ Html.text player.playerName ]
        , i
            [ styles cellStyles
            , class "btn btn-xs btn-info glyphicon glyphicon-pencil"
            , onClick (Edit player)
            ]
            []
        , i
            [ styles cellStyles
            , class "delete btn btn-xs btn-danger glyphicon glyphicon-remove"
            , onClick (DeletePlayer player)
            ]
            []
        , div [ class "btn-group", styles cellStyles ]
            [ button
                [ type' "button"
                  --, onMouseDown (PrepScore player 1)
                , onClick (Score player 1)
                , class "btn btn-default btn-xs"
                ]
                [ Html.text "1pt" ]
            , button
                [ type' "button"
                , onClick (Score player 2)
                , class "btn btn-default btn-xs"
                ]
                [ Html.text "2pt" ]
            , button
                [ type' "button"
                , onClick (Score player 3)
                , class "btn btn-default btn-xs"
                ]
                [ Html.text "3pt" ]
            ]
        , div [ styles cellStyles, class "pull-right" ]
            [ Html.text (toString player.points) ]
        ]


pointTotal : Model -> Html Msg
pointTotal model =
    let
        total =
            List.map .points model.plays
                |> List.sum
    in
        footer [ styles rowStyles ]
            [ div [ class "pull-right" ]
                [ Html.div [ styles cellStyles ] [ Html.text "Total: " ]
                , Html.div [ styles cellStyles ] [ Html.text (toString total) ]
                ]
            ]


playerForm : Model -> Html Msg
playerForm model =
    Html.form [ onSubmit Save, styles [ displayFlex ] ]
        [ input
            [ type' "text"
            , placeholder "Add/Edit Player ..."
            , onInput Input
            , value model.playerName
            , styles (editInputClass model.playerId)
            , styles cellStyles
            , styles [ borderRadius (px 3), border3 (px 1) solid (rgba 255 255 255 0.8), paddingLeft (px 4) ]
            ]
            []
        , button [ type' "submit", class "btn btn-xs btn-success", styles cellStyles ] [ Html.text "Save" ]
        , button [ type' "button", class "btn btn-xs btn-warning", styles cellStyles, onClick Cancel ] [ Html.text "Cancel" ]
        ]


editInputClass : Maybe Int -> List Mixin
editInputClass editPlayerId =
    case editPlayerId of
        Just id ->
            editing

        Nothing ->
            []


editPlayerClass : Maybe Int -> Player -> List Mixin
editPlayerClass editPlayerId player =
    case editPlayerId of
        Just id ->
            if player.playerId == id then
                editing
            else
                []

        Nothing ->
            []



-- Styles


styles : List Mixin -> Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style


rowStyles : List Css.Mixin
rowStyles =
    [ Css.minHeight (px 34)
    , paddingTop (px 5)
    , Css.width (pct 100)
    , transition "top 1000ms"
    ]


cellStyles : List Css.Mixin
cellStyles =
    [ display inlineBlock
    , marginRight
        (px 5)
    ]


editing : List Css.Mixin
editing =
    [ backgroundColor (rgba 0 0 100 0.3)
    , borderRadius (px 2)
    , color (rgb 255 255 255)
    , paddingLeft (px 3)
    ]


prepRow : List Css.Mixin
prepRow =
    [ opacity (float 0)
    , Css.minHeight (px 34)
    , transition "opacity 500ms"
    ]


hideRow : List Css.Mixin
hideRow =
    [ opacity (float 0)
    , Css.minHeight (px 0)
    , Css.height (px 0)
    , transition "opacity 1000ms, height 2000ms"
    ]


transition : String -> Css.Mixin
transition =
    Css.property "transition"


transform : String -> Css.Mixin
transform =
    Css.property "transform"



{--animation: String -> Css.Mixin
animation =
    Css.property "animation"--}


main : Program Never
main =
    App.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every millisecond Tick
