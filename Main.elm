module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import String
import Css exposing (..)
import Time exposing (Time, millisecond, second)
import Process exposing (..)
import Task exposing (..)


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
    , hidden : Bool
    }


type alias Play =
    { id : Int
    , playerId : Int
    , playerName : String
    , points : Int
    , hidden : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { players = []
      , playerName = ""
      , playerId = Nothing
      , plays = []
      }
    , Cmd.none
    )



-- update


type Msg
    = Edit Player
    | Input String
    | Save
    | Cancel
    | PrepPlay Player Int
    | Score
    | HidePlay Play
    | DeletePlay Play
    | HidePlayer Player
    | DeletePlayer Player
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input playerName ->
            ( { model | playerName = playerName }, Cmd.none )

        Cancel ->
            ( { model | playerName = "", playerId = Nothing }, Cmd.none )

        HidePlayer player ->
            ( hidePlayer model player, Process.sleep (500 * millisecond) |> Task.perform (\_ -> NoOp) (\_ -> DeletePlayer player) )

        DeletePlayer player ->
            ( deletePlayer model player, Cmd.none )

        Edit player ->
            ( { model | playerName = player.playerName, playerId = Just player.playerId }, Cmd.none )

        Save ->
            if (String.isEmpty model.playerName) then
                ( model, Cmd.none )
            else
                ( save model, Cmd.none )

        PrepPlay player points ->
            ( preparePlay model player points, Process.sleep (500 * millisecond) |> Task.perform (\_ -> NoOp) (\_ -> Score) )

        Score ->
            ( score model, Cmd.none )

        HidePlay play ->
            ( hidePlay model play, Process.sleep (500 * millisecond) |> Task.perform (\_ -> NoOp) (\_ -> DeletePlay play) )

        DeletePlay play ->
            ( deletePlay model play, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


hidePlay : Model -> Play -> Model
hidePlay model play =
    let
        newPlays =
            List.map
                (\p ->
                    if p.id == play.id then
                        { play | hidden = True, points = 0 }
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
    let
        newPlays =
            List.filter (\p -> p.id /= play.id) model.plays
    in
        { model | plays = newPlays }


hidePlayer : Model -> Player -> Model
hidePlayer model player =
    let
        newPlays =
            List.filter (\p -> p.playerId /= player.playerId) model.plays

        newPlayers =
            List.map
                (\p ->
                    if p.playerId == player.playerId then
                        { player | hidden = True, points = 0 }
                    else
                        p
                )
                model.players
    in
        { model | plays = newPlays, players = newPlayers }


deletePlayer : Model -> Player -> Model
deletePlayer model player =
    let
        newPlayers =
            List.filter (\p -> p.playerId /= player.playerId) model.players
    in
        { model | players = newPlayers }


preparePlay : Model -> Player -> Int -> Model
preparePlay model scorer points =
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
            Play (List.length model.plays) scorer.playerId scorer.playerName points True
    in
        { model | players = newPlayers, plays = play :: model.plays }


score : Model -> Model
score model =
    {--let
        newPlays =
            List.map
                (\p ->
                    if p.hidden == True then
                        { p | hidden = False }
                    else
                        p
                )
                model.plays
    in
        { model | plays = newPlays }
--}
    {--let
        play =
            List.head model.plays

        newPlays =
            List.map
                (\p ->
                    if p.id == play.id then
                        { p | hidden = False }
                    else
                        p
                )
                model.plays
    in
        { model | plays = newPlays }--}
    case List.head model.plays of
        Just play ->
            let
                newPlays =
                    List.map
                        (\p ->
                            if p.id == play.id then
                                { p | hidden = False }
                            else
                                p
                        )
                        model.plays
            in
                { model | plays = newPlays }

        Nothing ->
            model


save : Model -> Model
save model =
    case model.playerId of
        Just id ->
            editPlayer model id

        Nothing ->
            addPlayer model


editPlayer : Model -> Int -> Model
editPlayer model playerId =
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


createPlayer : Model -> Model
createPlayer model =
    let
        player =
            Player (List.length model.players) model.playerName 0 True

        newPlayers =
            player :: model.players
    in
        { model | players = newPlayers, playerName = "" }


addPlayer : Model -> Model
addPlayer model =
    let
        player =
            Player (List.length model.players) model.playerName 0 False

        newPlayers =
            player :: model.players
    in
        { model | players = newPlayers, playerName = "" }



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



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
        , p [ styles [ color (rgb 255 255 255) ] ] [ Html.text (toString model) ]
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
        [ if play.hidden then
            styles hideRow
          else
            styles rowStyles
        ]
        [ i
            [ styles cellStyles
            , class "remove btn btn-xs btn-danger glyphicon glyphicon-remove"
            , onMouseDown (HidePlay play)
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
    Html.div
        [ if player.hidden then
            styles hideRow
          else
            styles rowStyles
        ]
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
            , onMouseDown (HidePlayer player)
            ]
            []
        , div [ class "btn-group", styles cellStyles ]
            [ button
                [ type' "button"
                , onMouseDown (PrepPlay player 1)
                , class "btn btn-default btn-xs"
                ]
                [ Html.text "1pt" ]
            , button
                [ type' "button"
                , onMouseDown (PrepPlay player 2)
                , class "btn btn-default btn-xs"
                ]
                [ Html.text "2pt" ]
            , button
                [ type' "button"
                , onMouseDown (PrepPlay player 3)
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
    , transition "opacity 500ms"
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


hideRow : List Css.Mixin
hideRow =
    [ opacity (float 0)
    , Css.minHeight (px 34)
    , paddingTop (px 5)
    , transition "opacity 500ms, top 1000ms"
    ]


transition : String -> Css.Mixin
transition =
    Css.property "transition"


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
