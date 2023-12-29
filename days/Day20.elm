module Day20 exposing (Module(..), ModuleDef(..), Pulse(..), State(..), calculatePart1, calculatePart2, parser, puzzle, toNetwork)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser, Trailing(..), oneOf, spaces, succeed, symbol, variable)
import Puzzle exposing (Puzzle)
import Set


type Module
    = Broadcaster { outputs : List Id }
    | Flipflop { state : State, outputs : List Id }
    | Conjunction { inputs : Dict Id Pulse, outputs : List Id }
    | Untyped


type alias Network =
    Dict Id Module


type State
    = On
    | Off


type Pulse
    = Low
    | High


type alias Id =
    String


type alias Signal =
    { from : Id
    , to : Id
    , pulse : Pulse
    }


type alias Count =
    { low : Int, high : Int }


calculatePart1 : List ModuleDef -> Result String Int
calculatePart1 =
    toNetwork
        >> process 1000
        >> (\{ low, high } -> low * high)
        >> Ok


process : Int -> Network -> Count
process pushes =
    processHelp { low = 0, high = 0 } pushes []


processHelp : Count -> Int -> List Signal -> Network -> Count
processHelp count pushes signals network =
    if pushes == 0 && signals == [] then
        count

    else
        let
            ( newPushes, bumpedSignals ) =
                if signals == [] then
                    ( pushes - 1, [ { from = "button", to = "broadcaster", pulse = Low } ] )

                else
                    ( pushes, signals )

            newCount =
                bumpedSignals
                    |> List.foldl
                        (\signal acc ->
                            case signal.pulse of
                                High ->
                                    { acc | high = acc.high + 1 }

                                Low ->
                                    { acc | low = acc.low + 1 }
                        )
                        count

            updated =
                List.foldl
                    (\signal acc ->
                        let
                            ( updatedNetwork, newSignals ) =
                                processSignal signal acc.network
                        in
                        { acc | network = updatedNetwork, signals = acc.signals ++ newSignals }
                    )
                    { network = network, signals = [] }
                    bumpedSignals
        in
        processHelp newCount newPushes updated.signals updated.network


processSignal : Signal -> Network -> ( Network, List Signal )
processSignal signal network =
    case Dict.get signal.to network of
        Just (Broadcaster broadcaster) ->
            ( network, broadcaster.outputs |> List.map (\output -> { from = signal.to, to = output, pulse = signal.pulse }) )

        Just (Flipflop flipflop) ->
            case ( signal.pulse, flipflop.state ) of
                ( High, _ ) ->
                    ( network, [] )

                ( Low, Off ) ->
                    ( Dict.insert signal.to (Flipflop { flipflop | state = On }) network
                    , flipflop.outputs |> List.map (\output -> { from = signal.to, to = output, pulse = High })
                    )

                ( Low, On ) ->
                    ( Dict.insert signal.to (Flipflop { flipflop | state = Off }) network
                    , flipflop.outputs |> List.map (\output -> { from = signal.to, to = output, pulse = Low })
                    )

        Just (Conjunction conjunction) ->
            let
                inputs =
                    Dict.insert signal.from signal.pulse conjunction.inputs

                pulse =
                    if inputs |> Dict.values |> List.all ((==) High) then
                        Low

                    else
                        High
            in
            ( Dict.insert signal.to (Conjunction { conjunction | inputs = inputs }) network
            , conjunction.outputs |> List.map (\output -> { from = signal.to, to = output, pulse = pulse })
            )

        Just Untyped ->
            ( network, [] )

        Nothing ->
            ( network, [] )


calculatePart2 : List ModuleDef -> Result String Int
calculatePart2 _ =
    Err "not implemented"


toNetwork : List ModuleDef -> Network
toNetwork =
    List.foldl
        (\def ->
            case def of
                BroadcasterDef ids ->
                    Dict.insert "broadcaster" (Broadcaster { outputs = ids })

                FlipflopDef id ids ->
                    Dict.insert id (Flipflop { state = Off, outputs = ids })

                ConjunctionDef id ids ->
                    Dict.insert id (Conjunction { outputs = ids, inputs = Dict.empty })
        )
        Dict.empty
        >> (\initial ->
                let
                    toCables input outputs =
                        List.map (\output -> { input = input, output = output }) outputs

                    cables =
                        Dict.foldl
                            (\input mod acc ->
                                case mod of
                                    Broadcaster { outputs } ->
                                        acc ++ toCables input outputs

                                    Flipflop { outputs } ->
                                        acc ++ toCables input outputs

                                    Conjunction { outputs } ->
                                        acc ++ toCables input outputs

                                    Untyped ->
                                        acc
                            )
                            []
                            initial
                in
                List.foldl
                    (\{ input, output } acc ->
                        Dict.update output
                            (\maybeModule ->
                                case maybeModule of
                                    Nothing ->
                                        Just Untyped

                                    Just mod ->
                                        case mod of
                                            Conjunction conjunction ->
                                                Just <| Conjunction { conjunction | inputs = Dict.insert input Low conjunction.inputs }

                                            _ ->
                                                Just mod
                            )
                            acc
                    )
                    initial
                    cables
           )


type ModuleDef
    = BroadcasterDef (List Id)
    | FlipflopDef Id (List Id)
    | ConjunctionDef Id (List Id)


parser : Parser (List ModuleDef)
parser =
    Parser.sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = spaces
        , item = moduleParser
        , trailing = Optional
        }


moduleParser : Parser ModuleDef
moduleParser =
    oneOf
        [ succeed BroadcasterDef
            |. symbol "broadcaster"
            |. symbol " -> "
            |= idsParser
        , succeed FlipflopDef
            |. symbol "%"
            |= idParser
            |. symbol " -> "
            |= idsParser
        , succeed ConjunctionDef
            |. symbol "&"
            |= idParser
            |. symbol " -> "
            |= idsParser
        ]


idsParser : Parser (List String)
idsParser =
    Parser.sequence
        { start = ""
        , separator = ","
        , end = ""
        , spaces = spaces
        , item = idParser
        , trailing = Optional
        }


idParser : Parser String
idParser =
    variable
        { start = Char.isLower
        , inner = \c -> Char.isAlpha c
        , reserved = Set.empty
        }


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart1
    , calculatePart2 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart2
    }
