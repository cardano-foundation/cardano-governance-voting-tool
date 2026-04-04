module ProposalRelationships exposing
    ( GovernancePurpose(..), ProposalRelInfo
    , actionLatestEnacted, isChainableAction, proposalRelationships
    )

{-| Module for computing governance proposal relationships.

Cardano Conway-era governance proposals that share the same "purpose" and
reference the same `latestEnacted` action are competing: only one can be
enacted, the rest expire. This module provides the types and logic to
detect and represent these relationships.

@docs GovernancePurpose, ProposalRelInfo
@docs actionLatestEnacted, isChainableAction, proposalRelationships

-}

import Api exposing (ActiveProposal)
import Cardano.Gov as Gov exposing (ActionId)
import Dict exposing (Dict)
import Set exposing (Set)


{-| The governance purpose of a chainable proposal.
Proposals sharing the same purpose and latestEnacted reference are competing.
-}
type GovernancePurpose
    = CommitteePurpose
    | ConstitutionPurpose
    | HardForkPurpose
    | PParamUpdatePurpose


{-| Relationship info for a single proposal, used to display
dependency and competition tags on proposal cards.
-}
type alias ProposalRelInfo =
    { number : Int
    , purpose : GovernancePurpose
    , latestEnacted : Maybe ActionId
    , follows : Maybe Int
    , competingWith : List Int
    , isDelaying : Bool
    }


{-| Check if an action type string corresponds to a chainable action
(one that participates in purpose chains and can conflict).
-}
isChainableAction : String -> Bool
isChainableAction actionType =
    case actionType of
        "ParameterChange" ->
            True

        "HardForkInitiation" ->
            True

        "NoConfidence" ->
            True

        "NewCommittee" ->
            True

        "NewConstitution" ->
            True

        _ ->
            False


{-| Extract the latestEnacted field from a Gov.Action, if applicable.
-}
actionLatestEnacted : Gov.Action -> Maybe ActionId
actionLatestEnacted action =
    case action of
        Gov.ParameterChange { latestEnacted } ->
            latestEnacted

        Gov.HardForkInitiation { latestEnacted } ->
            latestEnacted

        Gov.NoConfidence { latestEnacted } ->
            latestEnacted

        Gov.UpdateCommittee { latestEnacted } ->
            latestEnacted

        Gov.NewConstitution { latestEnacted } ->
            latestEnacted

        Gov.TreasuryWithdrawals _ ->
            Nothing

        Gov.Info ->
            Nothing


{-| Determine the governance purpose from an action type string.
-}
actionPurpose : String -> Maybe GovernancePurpose
actionPurpose actionType =
    case actionType of
        "ParameterChange" ->
            Just PParamUpdatePurpose

        "HardForkInitiation" ->
            Just HardForkPurpose

        "NoConfidence" ->
            Just CommitteePurpose

        "NewCommittee" ->
            Just CommitteePurpose

        "NewConstitution" ->
            Just ConstitutionPurpose

        _ ->
            Nothing


{-| Given a list of (bech32Id, ActiveProposal, Maybe ActionId) triples
for chainable proposals, compute the ProposalRelInfo for each.

Only proposals that are in a relationship (follows another, is followed by
another, or competes with another) get an entry. Numbering is time-based:
older proposals (by proposed epoch) get lower numbers.

-}
proposalRelationships :
    List ( String, ActiveProposal, Maybe ActionId )
    -> Dict String ProposalRelInfo
proposalRelationships chainableProposals =
    let
        -- Sort to match the display order in the view: expiry, then type, then id
        sorted =
            List.sortBy (\( bech32Id, p, _ ) -> ( p.epoch_validity.end, p.actionType, bech32Id )) chainableProposals

        -- Set of active proposal ActionId strings, for detecting "follows"
        activeActionIds : Set String
        activeActionIds =
            sorted
                |> List.map (\( _, p, _ ) -> Gov.actionIdToString p.id)
                |> Set.fromList

        -- For each proposal, compute whether it follows an active proposal
        followsLookup : Dict String String
        followsLookup =
            sorted
                |> List.filterMap
                    (\( bech32Id, _, latestEnacted ) ->
                        latestEnacted
                            |> Maybe.map Gov.actionIdToString
                            |> Maybe.andThen
                                (\aidStr ->
                                    if Set.member aidStr activeActionIds then
                                        Just ( bech32Id, aidStr )

                                    else
                                        Nothing
                                )
                    )
                |> Dict.fromList

        -- Set of ActionId strings that are followed by at least one proposal
        followedActionIds : Set String
        followedActionIds =
            Dict.values followsLookup |> Set.fromList

        -- Group proposals by (purpose, latestEnacted) to detect competing siblings.
        -- A group with >1 member means those proposals are competing.
        competingGroups : List (List String)
        competingGroups =
            sorted
                |> List.filterMap
                    (\( bech32Id, proposal, latestEnacted ) ->
                        actionPurpose proposal.actionType
                            |> Maybe.map
                                (\purpose ->
                                    ( ( purposeToString purpose
                                      , Maybe.map Gov.actionIdToString latestEnacted |> Maybe.withDefault "genesis"
                                      )
                                    , bech32Id
                                    )
                                )
                    )
                |> groupByFirst
                |> List.filter (\group -> List.length group > 1)

        -- Map each bech32Id to its competing siblings (excluding itself)
        competingLookup : Dict String (List String)
        competingLookup =
            competingGroups
                |> List.concatMap
                    (\group ->
                        List.map (\bech32Id -> ( bech32Id, List.filter (\other -> other /= bech32Id) group )) group
                    )
                |> Dict.fromList

        competingBech32Ids : Set String
        competingBech32Ids =
            Dict.keys competingLookup |> Set.fromList

        -- A proposal is "in a relationship" if it follows, is followed, or competes
        hasRelationship ( bech32Id, proposal, _ ) =
            Dict.member bech32Id followsLookup
                || Set.member (Gov.actionIdToString proposal.id) followedActionIds
                || Set.member bech32Id competingBech32Ids

        -- Filter to only proposals with relationships, preserving sorted order
        relatedProposals =
            List.filter hasRelationship sorted

        -- Assign numbers only to related proposals
        actionIdToNumber : Dict String Int
        actionIdToNumber =
            relatedProposals
                |> List.indexedMap
                    (\i ( _, proposal, _ ) ->
                        ( Gov.actionIdToString proposal.id, i + 1 )
                    )
                |> Dict.fromList

        bech32ToNumber : Dict String Int
        bech32ToNumber =
            relatedProposals
                |> List.indexedMap (\i ( bech32Id, _, _ ) -> ( bech32Id, i + 1 ))
                |> Dict.fromList

        toRelInfo : Int -> ( String, ActiveProposal, Maybe ActionId ) -> ( String, ProposalRelInfo )
        toRelInfo index ( bech32Id, proposal, latestEnacted ) =
            let
                purpose =
                    actionPurpose proposal.actionType
                        |> Maybe.withDefault PParamUpdatePurpose

                follows =
                    latestEnacted
                        |> Maybe.andThen (\aid -> Dict.get (Gov.actionIdToString aid) actionIdToNumber)

                competingWith =
                    Dict.get bech32Id competingLookup
                        |> Maybe.withDefault []
                        |> List.filterMap (\siblingId -> Dict.get siblingId bech32ToNumber)
                        |> List.sort

                isDelaying =
                    case proposal.actionType of
                        "ParameterChange" ->
                            False

                        _ ->
                            -- All other chainable types are delaying
                            True
            in
            ( bech32Id
            , { number = index + 1
              , purpose = purpose
              , latestEnacted = latestEnacted
              , follows = follows
              , competingWith = competingWith
              , isDelaying = isDelaying
              }
            )
    in
    relatedProposals
        |> List.indexedMap toRelInfo
        |> Dict.fromList


{-| Group a list of (key, value) pairs by key, returning list of value groups.
-}
groupByFirst : List ( comparable, a ) -> List (List a)
groupByFirst pairs =
    List.foldl
        (\( key, value ) acc ->
            Dict.update key
                (\existing ->
                    case existing of
                        Nothing ->
                            Just [ value ]

                        Just values ->
                            Just (value :: values)
                )
                acc
        )
        Dict.empty
        pairs
        |> Dict.values


purposeToString : GovernancePurpose -> String
purposeToString purpose =
    case purpose of
        CommitteePurpose ->
            "committee"

        ConstitutionPurpose ->
            "constitution"

        HardForkPurpose ->
            "hardfork"

        PParamUpdatePurpose ->
            "pparam"
