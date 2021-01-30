module GossipProtocol.Creator exposing (..)

import Html exposing (Html, span, sub, sup, text)
import Html.Attributes exposing (class)
import Tree exposing (Tree)
import Tree.Zipper



-- TYPES AND TYPE ALIASES


type Junction
    = And
    | Or


type Constituent
    = Connective Junction
    | Condition Bool ProtocolConstituent
    | Group Bool
    | ProtocolRoot


type alias CNode =
    { constituent : Constituent
    , id : Int
    , parentId : Int
    }


type NodeType
    = GroupNode
    | ConditionNode ProtocolConstituent


type ProtocolConstituent
    = Verum
    | Falsum
    | Empty
    | LastTo
    | LastFrom
    | HasCalled
    | WasCalledBy
    | KnowsSecret


renderProtocolConstituent : ProtocolConstituent -> Html msg
renderProtocolConstituent constituent =
    case constituent of
        Empty ->
            span [ class "protocol-constituent" ]
                [ text "σ"
                , sub [] [ text "x" ]
                , text " = ϵ"
                ]

        Verum ->
            span [ class "protocol-constituent" ]
                [ text "⊤" ]

        Falsum ->
            span [ class "protocol-constituent" ]
                [ text "⊥" ]

        LastTo ->
            span [ class "protocol-constituent" ]
                [ text "σ"
                , sub [] [ text "x" ]
                , text " = τ;zx"
                ]

        LastFrom ->
            span [ class "protocol-constituent" ]
                [ text "σ"
                , sub [] [ text "x" ]
                , text " = τ;xz"
                ]

        HasCalled ->
            span [ class "protocol-constituent" ]
                [ text "xy ∈ σ"
                , sub [] [ text "x" ]
                ]

        WasCalledBy ->
            span [ class "protocol-constituent" ]
                [ text "yx ∈ σ"
                , sub [] [ text "x" ]
                ]

        KnowsSecret ->
            span [ class "protocol-constituent" ]
                [ text "S"
                , sup [] [ text "σ" ]
                , text "xy"
                ]


addNode : Tree CNode -> NodeType -> Tree CNode
addNode tree nodeType =
    let
        highestId =
            Tree.foldr
                (\label acc ->
                    if label.id > acc then
                        label.id

                    else
                        acc
                )
                0
                tree
    in
    -- TODO: make nodetype an ADT
    tree
        |> (if Tree.count tree > 1 then
                -- Only add a connective if the tree is empty, i.e. only the root node is present
                Tree.appendChild (Tree.singleton { id = highestId + 1, constituent = Connective Or, parentId = -1 })

            else
                -- If there are more nodes, just pass the unmodified tree
                identity
           )
        |> (case nodeType of
                GroupNode ->
                    Tree.appendChild (Tree.singleton { id = highestId + 2, constituent = Group False, parentId = -1 })

                ConditionNode constituent ->
                    Tree.appendChild (Tree.singleton { id = highestId + 2, constituent = Condition False constituent, parentId = -1 })
           )


{-| Deletes a node and the relevant connective so no duplicate connectives are used

First tries to remove the next connective. If it doesn't exist, tries to remove the previous
(only one is every removed). Then removes the node (and its subtree).

-}
deleteNode : Tree CNode -> CNode -> Tree CNode
deleteNode tree node =
    Tree.Zipper.fromTree tree
        |> Tree.Zipper.findNext (\n -> n.id == node.id)
        |> Maybe.map
            (\zip ->
                case ( Tree.Zipper.nextSibling zip, Tree.Zipper.previousSibling zip ) of
                    ( Just s, _ ) ->
                        -- if there is a next sibling, delete it and the node
                        case Tree.Zipper.label s |> .constituent of
                            Connective _ ->
                                -- remove connective
                                Tree.Zipper.removeTree s
                                    |> Maybe.andThen (Tree.Zipper.findNext (\n -> n.id == node.id))
                                    -- remove node
                                    |> Maybe.andThen Tree.Zipper.removeTree
                                    |> Maybe.map Tree.Zipper.toTree
                                    |> Maybe.withDefault tree

                            _ ->
                                -- if the next sibling is not a connective, something has gone wrong; just return the original tree
                                tree

                    ( Nothing, Just s ) ->
                        -- if there is no next sibling, but there is a previous sibling
                        -- (i.e. this node is the last in this group) remove that and the node
                        case Tree.Zipper.label s |> .constituent of
                            Connective _ ->
                                -- remove connective
                                Tree.Zipper.removeTree s
                                    -- removing returns parent, find original node again
                                    |> Maybe.andThen (Tree.Zipper.findNext (\n -> n.id == node.id))
                                    -- remove node
                                    |> Maybe.andThen Tree.Zipper.removeTree
                                    |> Maybe.map Tree.Zipper.toTree
                                    |> Maybe.withDefault tree

                            _ ->
                                -- if the previous sibling is not a connective, something has gone wrong; just return the original tree
                                tree

                    _ ->
                        -- if there are no connective siblings, just remove the node
                        Tree.Zipper.removeTree zip
                            |> Maybe.map Tree.Zipper.toTree
                            |> Maybe.withDefault tree
            )
        |> Maybe.withDefault tree


toggleNegated : Tree CNode -> CNode -> Tree CNode
toggleNegated tree node =
    let
        negate : CNode -> CNode
        negate n =
            case n.constituent of
                Condition negated name ->
                    { n | constituent = Condition (not negated) name }

                Group negated ->
                    { n | constituent = Group (not negated) }

                _ ->
                    n
    in
    Tree.Zipper.fromTree tree
        |> Tree.Zipper.findNext (\n -> n.id == node.id)
        |> Maybe.map (Tree.Zipper.mapLabel negate)
        |> Maybe.map Tree.Zipper.toTree
        |> Maybe.withDefault tree


{-| Switches the value of an connective to the opposite value and updates the formula to match
-}
toggleconnective : Tree CNode -> CNode -> Tree CNode
toggleconnective tree node =
    let
        oppositeconnective : CNode -> CNode
        oppositeconnective n =
            case n.constituent of
                Connective And ->
                    { n | constituent = Connective Or }

                Connective Or ->
                    { n | constituent = Connective And }

                _ ->
                    n
    in
    Tree.Zipper.fromTree tree
        |> Tree.Zipper.findNext (\n -> n.id == node.id)
        |> Maybe.map (Tree.Zipper.mapLabel oppositeconnective)
        |> Maybe.map Tree.Zipper.toTree
        |> Maybe.withDefault tree


moveAfter : CNode -> CNode -> Tree CNode -> Tree CNode
moveAfter drag drop tree =
    -- - delete original
    -- - insert <or> after drop
    -- - insert drag after drop
    let
        highestId =
            Tree.foldr
                (\label acc ->
                    if label.id > acc then
                        label.id

                    else
                        acc
                )
                0
                tree

        -- try to find the subtree. If it cannot be found, just take the node
        getSubTree node =
            Tree.Zipper.fromTree tree
                |> Tree.Zipper.findNext (\n -> n.id == node.id)
                |> Maybe.map Tree.Zipper.tree
                -- Zipper.tree takes the tree that is currently in focus
                |> Maybe.withDefault (Tree.singleton node)
    in
    tree
        |> Tree.Zipper.fromTree
        -- move to drop node
        |> Tree.Zipper.findNext (\n -> n.id == drop.id)
        -- add drag node
        |> Maybe.map (Tree.Zipper.append (getSubTree drag |> Tree.mapLabel (\l -> { l | id = highestId + 1, parentId = drop.parentId })))
        -- add connective
        |> Maybe.map (Tree.Zipper.append (Tree.singleton { constituent = Connective Or, id = highestId + 2, parentId = drop.parentId }))
        -- remove maybe
        |> Maybe.withDefault (Tree.Zipper.fromTree tree)
        -- move back to root
        |> Tree.Zipper.root
        -- delete dragged node
        |> Tree.Zipper.mapTree (\t -> deleteNode t drag)
        -- make tree again
        |> Tree.Zipper.toTree



-- move a node inside a group


moveInside : CNode -> CNode -> Tree CNode -> Tree CNode
moveInside drag drop tree =
    let
        highestId =
            Tree.foldr
                (\label acc ->
                    if label.id > acc then
                        label.id

                    else
                        acc
                )
                0
                tree

        -- try to find the subtree. If it cannot be found, just take the node
        getSubTree node =
            Tree.Zipper.fromTree tree
                |> Tree.Zipper.findNext (\n -> n.id == node.id)
                |> Maybe.map Tree.Zipper.tree
                -- Zipper.tree takes the tree that is currently in focus
                |> Maybe.withDefault (Tree.singleton node)
    in
    case drop.constituent of
        Group negated ->
            Tree.Zipper.fromTree tree
                -- move to drop node
                |> Tree.Zipper.findNext (\n -> n.id == drop.id)
                -- add connective if there is another node after
                |> Maybe.map
                    (\dropNode ->
                        if (List.length <| Tree.Zipper.children dropNode) == 0 then
                            dropNode

                        else
                            Tree.Zipper.mapTree (Tree.prependChild (Tree.singleton { constituent = Connective Or, id = highestId + 1, parentId = drop.id })) dropNode
                    )
                -- prepend child to children of group
                |> Maybe.map (Tree.Zipper.mapTree (Tree.prependChild (getSubTree drag |> Tree.mapLabel (\l -> { l | id = highestId + 2, parentId = drop.id }))))
                -- remove maybe
                |> Maybe.withDefault (Tree.Zipper.fromTree tree)
                -- move back to root
                |> Tree.Zipper.root
                -- delete dragged node
                |> Tree.Zipper.mapTree (\t -> deleteNode t drag)
                -- make tree again
                |> Tree.Zipper.toTree

        _ ->
            -- return the unmodified tree if an attempt is made to insert
            -- something in anything other than a group
            tree
