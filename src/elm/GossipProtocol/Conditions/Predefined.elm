module GossipProtocol.Conditions.Predefined exposing (..)

{-| This file contains several predefined protocol conditions. Based on Van Ditmarsch (2018)
-}

import CallSequence.CallSequence exposing (containing)
import Dict exposing (Dict)
import GossipProtocol.Conditions.Constituents exposing (..)
import GossipProtocol.Creator exposing (CNode, Constituent(..), Junction(..), ProtocolConstituent(..))
import GossipProtocol.GossipProtocol exposing (ProtocolCondition)
import Html exposing (Html, p, sup, text)
import Tree exposing (Tree)


any : ProtocolCondition
any _ _ _ =
    True



-- ⊤


formulaAny : Tree CNode
formulaAny =
    Tree.tree { constituent = ProtocolRoot, id = 0, parentId = -1 }
        [ Tree.singleton { constituent = Condition False Verum, id = 1, parentId = 0 }
        ]


tok : ProtocolCondition
tok ( x, _ ) _ sequence =
    let
        sigma_x =
            containing sequence x
    in
    empty sigma_x || lastTo x sigma_x


formulaTok : Tree CNode
formulaTok =
    Tree.tree { constituent = ProtocolRoot, id = 0, parentId = -1 }
        [ Tree.singleton { constituent = Condition False Empty, id = 1, parentId = 0 }
        , Tree.singleton { constituent = Connective Or, id = 2, parentId = 0 }
        , Tree.singleton { constituent = Condition False LastTo, id = 3, parentId = 0 }
        ]


spi : ProtocolCondition
spi ( x, _ ) _ sequence =
    let
        sigma_x =
            containing sequence x
    in
    empty sigma_x || lastFrom x sigma_x


formulaSpi : Tree CNode
formulaSpi =
    Tree.tree { constituent = ProtocolRoot, id = 0, parentId = -1 }
        [ Tree.singleton { constituent = Condition False Empty, id = 1, parentId = 0 }
        , Tree.singleton { constituent = Connective Or, id = 2, parentId = 0 }
        , Tree.singleton { constituent = Condition False LastFrom, id = 3, parentId = 0 }
        ]


co : ProtocolCondition
co ( x, y ) _ sequence =
    let
        sigma_x =
            containing sequence x
    in
    not (hasCalled x y sigma_x) && not (wasCalledBy x y sigma_x)


formulaCo : Tree CNode
formulaCo =
    Tree.tree { constituent = ProtocolRoot, id = 0, parentId = -1 }
        [ Tree.singleton { constituent = Condition True HasCalled, id = 1, parentId = 0 }
        , Tree.singleton { constituent = Connective And, id = 2, parentId = 0 }
        , Tree.singleton { constituent = Condition True WasCalledBy, id = 3, parentId = 0 }
        ]


wco : ProtocolCondition
wco ( x, y ) _ sequence =
    let
        sigma_x =
            containing sequence x
    in
    not (hasCalled x y sigma_x)


formulaWco : Tree CNode
formulaWco =
    Tree.tree { constituent = ProtocolRoot, id = 0, parentId = -1 }
        [ Tree.singleton { constituent = Condition True HasCalled, id = 1, parentId = 0 }
        ]


lns : ProtocolCondition
lns ( x, y ) relations _ =
    not (knowsSecret x y relations)


formulaLns : Tree CNode
formulaLns =
    Tree.tree { constituent = ProtocolRoot, id = 0, parentId = -1 }
        [ Tree.singleton { constituent = Condition True KnowsSecret, id = 1, parentId = 0 }
        ]


name : Dict String String
name =
    Dict.fromList
        [ ( "any", "Any" )
        , ( "tok", "Token" )
        , ( "spi", "Spider" )
        , ( "co", "Call Once" )
        , ( "wco", "Weak Call Once" )
        , ( "lns", "Learn New Secrets" )
        ]


condition : Dict String (Tree CNode)
condition =
    Dict.fromList
        [ ( "any", formulaAny )
        , ( "tok", formulaTok )
        , ( "spi", formulaSpi )
        , ( "co", formulaCo )
        , ( "wco", formulaWco )
        , ( "lns", formulaLns )
        ]


explanation : Dict String (List (Html msg))
explanation =
    Dict.fromList
        [ ( "any"
          , [ p [] [ text "Until every agent knows all secrets, choose different agents x and y, such that x knows the number of y, and let x call y." ]
            , p [] [ text "Represented by the boolean formula ⊤." ]
            ]
          )
        , ( "tok"
          , [ p [] [ text "Until every agent knows all secrets, choose different agents x and y with x ≠ y, such that x knows y’s number and either x has not been in prior calls or the last call involving x was to x, and let x call y." ]
            , p [] [ text "Represented by the boolean formula σₓ = ϵ ∨ σₓ = t;zx." ]
            ]
          )
        , ( "spi"
          , [ p [] [ text "Until every agent knows all secrets, choose different agents x and y, such that x knows y’s number and either x has not been in prior calls or the last call involving x was from x, and let x call y." ]
            , p [] [ text "Represented by the boolean formula σₓ = ϵ ∨ σₓ = t;xz." ]
            ]
          )
        , ( "wco"
          , [ p [] [ text "Until every agent knows all secrets, choose different agents x and y, such that x knows the number of y and x did not call y before, and let x call y." ]
            , p [] [ text "Represented by the boolean formula xy ∉ σₓ ∧ yx ∉ σₓ." ]
            ]
          )
        , ( "co"
          , [ p [] [ text "Until every agent knows all secrets, choose different agents x and y, such that x knows the number of y and there was no prior call between x and y, and let x call y." ]
            , p [] [ text "Represented by the boolean formula xy ∉ σₓ." ]
            ]
          )
        , ( "lns"
          , [ p [] [ text "Until every agent knows all secrets, choose different agents x and y, such that x knows the number of y but not the secret of y, and let x call y." ]
            , p []
                [ text "Represented by the boolean formula ¬S"
                , sup [] [ text "σ" ]
                , text "xy."
                ]
            ]
          )
        ]
