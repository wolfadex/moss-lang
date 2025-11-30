module Haltable exposing (..)


type HaltableResult e a b
    = ResultHalted (List a) b
    | Crashed e
    | Success b


foldlResult : (a -> b -> Result e (Step b)) -> b -> List a -> HaltableResult e a b
foldlResult fn b list =
    case list of
        [] ->
            Success b

        a :: rest ->
            case fn a b of
                Ok HaltBefore ->
                    ResultHalted list b

                Ok (HaltAfter intermediateB) ->
                    ResultHalted rest intermediateB

                Err e ->
                    Crashed e

                Ok (Continue nextB) ->
                    foldlResult fn nextB rest


type Haltable a b
    = Halted (List a) b
    | Complete b


type Step b
    = Continue b
    | HaltBefore
    | HaltAfter b


foldl : (a -> b -> Step b) -> b -> List a -> Haltable a b
foldl fn b list =
    case list of
        [] ->
            Complete b

        a :: rest ->
            case fn a b of
                HaltBefore ->
                    Halted list b

                HaltAfter intermediateB ->
                    Halted rest intermediateB

                Continue nextB ->
                    foldl fn nextB rest
