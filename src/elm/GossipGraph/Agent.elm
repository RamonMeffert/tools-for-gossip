module GossipGraph.Agent exposing (..)

{-| The `Agent` type is used to model the agents in a gossip graph.


# Definition

@docs Agent


# Helpers

@docs toNode, getDotAttrs

-}

import Dict exposing (Dict)
import Graph exposing (Node)
import Utils.List


type alias AgentId =
    Int


{-| An agent in a gossip graph.
-}
type alias Agent =
    { id : AgentId
    , name : Char
    }


{-| Converts an agent to a `Node` for use with the `elm-community/graph` package.

    toNode 1 'A' == { id = 1, label = 'A' }

-}
toNode : Agent -> Node Agent
toNode agent =
    { id = agent.id, label = agent }


{-| Gets the style attributes for rendering the current agent as a node in a GraphViz graph.

    getDotAttrs { id = 0, name = 'A' } == Dict.singleton "label" "A"

-}
getDotAttrs : Agent -> Dict String String
getDotAttrs a =
    Dict.singleton "label" (String.fromChar a.name)


{-| Given a list of agents and an agent name, return that agent if it exists.
If it does not exist, return an error.
-}
fromChar : List Agent -> Char -> Result String Agent
fromChar agents char =
    agents
        |> Utils.List.find (\agent -> agent.name == Char.toUpper char)
        |> Result.fromMaybe ("Agent “" ++ String.fromChar (Char.toUpper char) ++ "” does not exist.")


{-| Given a list of agents and an agent id, return that agent if it exists.
If it does not exist, return an error.
-}
fromId : List Agent -> AgentId -> Result String Agent
fromId agents id =
    agents
        |> Utils.List.find (\agent -> agent.id == id)
        |> Result.fromMaybe ("An agent with id" ++ String.fromInt id ++ " does not exist.")


{-| Given a list of agents and a string containing agent names, return a list of agents if all agents exists.
If it does not exist, return an error. Also returns an error if the string contains invalid characters.
-}
fromString : List Agent -> String -> Result String (List Agent)
fromString agents string =
    let
        list =
            String.toList string
    in
    case list of
        [] ->
            Ok []

        x :: xs ->
            case fromChar agents x of
                Ok agent ->
                    case fromString agents (String.fromList xs) of
                        Ok ys ->
                            Ok (agent :: ys)

                        Err e ->
                            Err e

                Err e ->
                    Err e
