port module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Json.Decode as D exposing (Decoder, field, string)

import Html exposing (Html, div, text, br, input, button)
import Html.Attributes exposing (id, class, type_, min, max, step, value)
import Html.Events exposing (onClick, onInput, targetValue)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (width, height, viewBox, x, y, fill)
import Time exposing (Posix, Zone, every, posixToMillis, millisToPosix, utc, Month(..), Weekday(..))
import Task exposing (perform)

port audioPlay : String -> Cmd msg
port volumeChange : Float -> Cmd msg
port audioReset : String -> Cmd msg

type alias Model =
    { startTime : Posix
    , currentTime : Posix
    , requestTime : String
    , volume : Float
    , zone : Zone
    , isCountingDown : Bool
    , remainingTime : Int
    }

initialModel =
    { startTime = millisToPosix 0
    , currentTime = millisToPosix 0
    , requestTime = "00:00:00"
    , volume = 1
    , zone = utc
    , isCountingDown = False
    , remainingTime = -1000
    }

type Msg
    = Pressed Option
    | ChangeVolume Float
    | Tick Posix
    | Start
    | Stop
    | SetTimer String
    | GetStartTime Posix
    | SetSystemTime ( Zone, Posix )

type Option
    = Set
    | Pause
    | Resume
    | Reset
    | Else

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pressed opt ->
            case opt of
                _ -> ( model, Cmd.none )
        ChangeVolume volume ->
            ( { model | volume = volume }, Cmd.none )
        Tick posix ->
            let
                newModel = { model | currentTime = posix }
            in
                ( newModel
                    |> updateRemainingTime
                , volumeChange model.volume
                )
                    |> checkTimer
        Start ->
            ( { model | isCountingDown = True }, perform GetStartTime Time.now )
        Stop ->
            ( { model | startTime = millisToPosix 0 ,isCountingDown = False }
            , audioReset ""
            )

        SetTimer str ->
            ( { model | requestTime = str }, Cmd.none )

        GetStartTime posix ->
            ( { model | startTime = posix }, Cmd.none )
        SetSystemTime ( zone, posix ) ->
            ( { model | zone = zone, currentTime = posix }, Cmd.none )

checkTimer : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
checkTimer ( model, cmd ) =
    let
        timeError = abs model.remainingTime
    in
        if timeError <= 5
        then ( model, audioPlay "" )
        else ( model, cmd )

updateRemainingTime model=
    let
        timeDifference =
            (posixToMillis model.currentTime) - (posixToMillis model.startTime)
    in
        { model |
            remainingTime =
                (timeStringToMillis model.requestTime) - timeDifference
        }

view : Model -> Html Msg
view model =
    let
        monthToString month =
            case month of
                Jan -> "01"
                Feb -> "02"
                Mar -> "03"
                Apr -> "04"
                May -> "05"
                Jun -> "06"
                Jul -> "07"
                Aug -> "08"
                Sep -> "09"
                Oct -> "10"
                Nov -> "11"
                Dec -> "12"

        weekdayToString weekday =
            case weekday of
                Mon -> "(Mon.)"
                Tue -> "(Tue.)"
                Wed -> "(Wed.)"
                Thu -> "(Thu.)"
                Fri -> "(Fri.)"
                Sat -> "(Sat.)"
                Sun -> "(Sun.)"

        intToTimeString num =
            if num//10 == 0
            then "0" ++ (String.fromInt num)
            else String.fromInt num

        viewCurrentTime =
            let
                z = model.zone
                t = model.currentTime
                yr = Time.toYear z t    |> String.fromInt
                mt = Time.toMonth z t   |> monthToString
                dy = Time.toDay z t     |> String.fromInt
                wk = Time.toWeekday z t |> weekdayToString
                hr = Time.toHour z t    |> intToTimeString
                mn = Time.toMinute z t  |> intToTimeString
                sc = Time.toSecond z t  |> intToTimeString
                ms =(Time.toMillis z t) // 10 |> intToTimeString

            in
                [ text
                    <| yr ++ "/" ++ mt ++ "/" ++ dy ++ wk
                , br[][]
                , text
                    <| hr ++ ":" ++ mn ++ "'" ++ sc ++ (String.fromChar '"') ++ ms
                ]

        viewVolumeSetting =
            [ text "Volume"
            , br[][]
            , input
                [ type_ "range"
                , min "0"
                , max "1"
                , step "0.01"
                , value <| String.fromFloat <| model.volume
                , onInput (String.toFloat >> Maybe.withDefault model.volume >> ChangeVolume)
                ][]
            , text <| (String.fromInt <| round <| model.volume * 100) ++ "%"
            ]

        viewTimer =
            let
                viewRemainingTime =
                    if model.remainingTime < 0
                    then text ""
                    else text <| (String.fromInt <| model.remainingTime//1000) ++ "sec remaining..."

                settingTimer =
                    [ input
                        [ type_ "time"
                        , min "00:00:01"
                        , max "23:59:59"
                        , step "1"
                        , value model.requestTime
                        , onInput SetTimer
                        ][]
                    , br[][]
                    , if (timeStringToMillis model.requestTime)/=0
                        then
                            button [ onClick Start ][ text "start"]
                        else
                            br[][]
                    ]

            in
                List.append
                    [ text "Timer"
                    , br[][]
                    ]
                    <| if model.isCountingDown
                        then
                            [ viewRemainingTime
                            , br[][]
                            , button [ onClick Stop ][ text "stop" ]
                            ]
                        else
                            settingTimer

    in
        div[]
            <| List.concat
                [ viewCurrentTime
                , [br[][]]
                , viewVolumeSetting
                , [br[][],br[][]]
                , viewTimer
                ]

timeStringToMillis : String -> Int
timeStringToMillis str =
    let
        strHrs = str
            |> String.left 2

        strMin = str
            |> String.slice 3 5

        strSec = str
            |> String.right 2

        stringToInt stringInt =
            stringInt
                |> String.toInt
                |> Maybe.withDefault 0

        hrs = stringToInt strHrs
        min = stringToInt strMin
        sec = stringToInt strSec

    in
        (3600 * hrs + 60 * min + sec)*1000


keyDownDecoder model =
    (field "key" string)
        |> D.map (\str -> keyToOption model str)
        |> D.map Pressed

keyToOption model str =
    case str of
        " " ->
            if model.isCountingDown
            then Pause
            else Resume
        "Enter" -> Set
        "r" -> Reset
        _ -> Else

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 10 Tick
        , onKeyDown (keyDownDecoder model)
        ]

setSystemTime =
    perform SetSystemTime <| Task.map2 Tuple.pair Time.here Time.now

main : Program () Model Msg
main =
    Browser.element
        { init = \_-> ( initialModel, setSystemTime )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }