module GossipGraph.Call exposing (..)

import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Solid as Icon
import GossipGraph.Agent as Agent exposing (Agent, AgentId)
import GossipGraph.Relation exposing (Kind(..), Relation)
import Graph exposing (Graph, NodeContext)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import IntDict
import Utils.List exposing (get)


{-| A call from one agent to another.
-}
type alias Call =
    { from : AgentId
    , to : AgentId
    }


{-| Converts a tuple of two agents to a call.
-}
fromTuple : ( AgentId, AgentId ) -> Call
fromTuple ( x, y ) =
    { from = x, to = y }


{-| Converts a list of exactly two agents into a call.

TODO: (maybe) move validation outside this function so it can also be used for fromTuple

-}
fromList : List Agent -> Result String Call
fromList agents =
    let
        from =
            get agents 0

        to =
            get agents 1
    in
    if List.length agents > 2 then
        Err "A call must contain two agents."

    else
        case ( from, to ) of
            ( Just f, Just t ) ->
                if f.id == t.id then
                    Err "An agent cannot call itself."

                else
                    Ok { from = f.id, to = t.id }

            _ ->
                -- Seems like this only occurs when there's only one agent
                Err "A call must contain two agents."


{-| Returns whether an agent is in a call
-}
includes : Call -> AgentId -> Bool
includes call agent =
    call.from == agent || call.to == agent


{-| Renders a single call
-}
render : List Agent -> Call -> Html msg
render agents call =
    div [ class "call" ] (renderString agents call)


renderString : List Agent -> Call -> List (Html msg)
renderString agents call =
    case ( Agent.fromId agents call.from, Agent.fromId agents call.to ) of
        ( Ok from, Ok to ) ->
            [ text <| String.fromChar from.name ++ " "
            , Icon.viewIcon Icon.phoneAlt
            , text <| " " ++ String.fromChar to.name
            ]

        _ ->
            [ text "❌" ]


{-| Execute a call on a gossip graph

Make sure the caller knows everything the receiver knows and vice versa. Since
knowledge is represented as outgoing edges, we can simply add the outgoing edges
of one agent to the other, making sure no duplicates are created and that
relations are upgraded when necessary (i.e. number -> secret)

Built on `Graph.update` and `IntDict.merge`.

-}
execute : Graph Agent Relation -> Call -> Graph Agent Relation
execute graph { from, to } =
    let
        knowledge : AgentId -> Maybe (NodeContext Agent Relation)
        knowledge id =
            Graph.get id graph

        -- merges the nodecontext for newId into currentContext
        merge : AgentId -> Maybe (NodeContext Agent Relation) -> Maybe (NodeContext Agent Relation)
        merge newId currentContext =
            let
                newContext : Maybe (NodeContext Agent Relation)
                newContext =
                    knowledge newId
            in
            case ( newContext, currentContext ) of
                ( Just new, Just current ) ->
                    -- change the current context so it includes the relations from the new context
                    Just
                        { current
                          -- c (current) and n (new) are of type Relation. k is the key of the IntDict,
                          -- indicating to which node the edge is pointing in the case of NodeContext.outgoing
                            | outgoing =
                                IntDict.merge
                                    -- occurs only in current: just keep it
                                    (\k c acc -> IntDict.insert k c acc)
                                    -- occurs in both: check Kind, keep "most knowledge" (S > N)
                                    (\k c n acc ->
                                        if c.kind == Secret then
                                            -- current relation is secret
                                            IntDict.insert k c acc

                                        else if n.kind == Secret then
                                            -- new relation is secret
                                            IntDict.insert k { n | from = current.node.id } acc

                                        else
                                            -- both relations are Number, so just keep the original
                                            IntDict.insert k c acc
                                    )
                                    -- occurs in new: change n.from and insert
                                    (\k n acc -> IntDict.insert k { n | from = current.node.id } acc)
                                    current.outgoing
                                    new.outgoing
                                    IntDict.empty
                        }

                _ ->
                    Nothing
    in
    graph
        -- first update the caller
        |> Graph.update from (merge to)
        -- then update the receiver
        |> Graph.update to (merge from)
