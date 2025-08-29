module Page.Cart exposing (Model, Msg, UpdateContext, ViewContext, VoteRecord, addVote, cartCount, contains, deleteVote, deserialize, get, getVoter, init, serialize, update, view)

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address as Address exposing (Address, CredentialHash)
import Cardano.Cip30 as Cip30
import Cardano.CoinSelection as CoinSelection
import Cardano.Gov as Gov exposing (ActionId, Anchor, CostModels, Id(..))
import Cardano.Script as Script
import Cardano.Transaction as Transaction exposing (Transaction)
import Cardano.TxIntent as TxIntent exposing (Fee(..), TxFinalized, TxIntent, VoteIntent)
import Cardano.Uplc as Uplc
import Cardano.Utils as Utils
import Cardano.Utxo as Utxo exposing (Output)
import Cardano.Witness as Witness exposing (Voter(..))
import Dict exposing (Dict)
import Dict.Any
import Helper exposing (cardContainer, cardContent, cardHeader, sectionTitle, viewButton, viewError)
import Html exposing (Html, div, text)
import Html.Attributes as HA
import Json.Decode as JD
import Json.Encode as JE
import Natural as N


{-| The Cart model has two states, preparing and ready.
By default, when adding votes to the cart, we will try to prepare
a transaction and estimate resources usage.
I think it’s reasonable to do at each vote added to the cart,
because most voters don’t use Plutus scripts, so it will be almost instant.
If for any reason, like no wallet connected, we fail to build the vote Tx,
we stay in the preparing state.
Otherwise, we are in the ready state.
-}
type Model
    = Preparing CartPreparation
    | Ready CartReady


init : Model
init =
    Preparing { votersIntents = Dict.empty, error = Nothing }


type alias CartPreparation =
    { votersIntents : Dict String CartVoter -- keys are bech32 gov IDs
    , error : Maybe String
    }


type alias CartVoter =
    { voter : Witness.Voter
    , voteRecords : Dict String VoteRecord -- keys are bech32 gov IDs (of the action ID)
    }


type alias VoteRecord =
    { proposalTitle : String
    , voteIntent : VoteIntent
    }



-- (intentionally no listAll; we render per-selected-voter only)


type alias CartReady =
    { votersIntents : Dict String CartVoter -- keys are bech32 gov IDs
    , maxResources : Resources
    , currentResources : Resources
    , txFinalized : TxFinalized
    }


type alias Resources =
    { txSize : Int
    , steps : Int
    , mem : Int
    }


{-| Check if a given proposal is present in the cart.
-}
contains : String -> ActionId -> Model -> Bool
contains voterId actionId model =
    get voterId actionId model
        |> Maybe.map (\_ -> True)
        |> Maybe.withDefault False


{-| Try to retrieve a vote from the cart.
-}
get : String -> ActionId -> Model -> Maybe VoteRecord
get voterId actionId model =
    let
        actionIdStr =
            Gov.idToBech32 <| GovActionId actionId
    in
    case model of
        Preparing { votersIntents } ->
            Dict.get voterId votersIntents
                |> Maybe.andThen (\{ voteRecords } -> Dict.get actionIdStr voteRecords)

        Ready { votersIntents } ->
            Dict.get voterId votersIntents
                |> Maybe.andThen (\{ voteRecords } -> Dict.get actionIdStr voteRecords)


{-| Retrieve all votes of a given voter from the cart.
-}
getVoter : String -> Model -> Dict String VoteRecord
getVoter voterId model =
    case model of
        Preparing { votersIntents } ->
            Dict.get voterId votersIntents
                |> Maybe.map .voteRecords
                |> Maybe.withDefault Dict.empty

        Ready { votersIntents } ->
            Dict.get voterId votersIntents
                |> Maybe.map .voteRecords
                |> Maybe.withDefault Dict.empty



-- UPDATE ############################################################


type Msg
    = BuildTx


type alias UpdateContext a msg =
    { a
        | wrapMsg : Msg -> msg
        , costModels : Maybe CostModels
        , loadedWallet : Maybe { wallet : Cip30.Wallet, utxos : Utxo.RefDict Output }
    }


update : UpdateContext a msg -> Msg -> Model -> ( Model, Cmd msg )
update ctx msg model =
    case ( ( ctx.loadedWallet, ctx.costModels ), msg, model ) of
        ( ( Just { wallet, utxos }, Just costModels ), BuildTx, Preparing ({ votersIntents } as cartPrep) ) ->
            let
                walletAddress =
                    Cip30.walletChangeAddress wallet
            in
            case buildTx costModels utxos walletAddress votersIntents of
                Ok ({ tx } as txFinalized) ->
                    let
                        txSize =
                            Bytes.width <| Transaction.serialize tx

                        { totalSteps, totalMem } =
                            Transaction.computeTotalExecUnits tx

                        txResources =
                            { txSize = txSize
                            , steps = N.toInt totalSteps
                            , mem = N.toInt totalMem
                            }

                        maxResources =
                            { txSize = Gov.defaultMaxTxSize
                            , steps = Gov.defaultMaxTxExUnits.steps
                            , mem = Gov.defaultMaxTxExUnits.mem
                            }
                    in
                    ( Ready
                        { votersIntents = votersIntents
                        , maxResources = maxResources
                        , currentResources = txResources
                        , txFinalized = txFinalized
                        }
                    , Cmd.none
                    )

                Err err ->
                    ( Preparing { cartPrep | error = Just err }, Cmd.none )

        ( ( _, Nothing ), BuildTx, Preparing cartPrep ) ->
            ( Preparing { cartPrep | error = Just "We failed to load the cost models, maybe try to refresh." }, Cmd.none )

        ( ( Nothing, _ ), BuildTx, Preparing cartPrep ) ->
            ( Preparing { cartPrep | error = Just "Please connect a wallet to build the transaction" }, Cmd.none )

        ( _, BuildTx, _ ) ->
            ( model, Cmd.none )



-- Add / Delete a vote


{-| Add a vote to the cart.
Reset the state to Preparing.
-}
addVote : Witness.Voter -> VoteRecord -> Model -> Model
addVote voter voteRecord model =
    let
        voterIdStr =
            Witness.toVoter voter
                |> Gov.voterToId
                |> Gov.idToBech32

        actionIdStr =
            Gov.idToBech32 <| GovActionId voteRecord.voteIntent.actionId

        updateVotersIntents : Dict String CartVoter -> Dict String CartVoter
        updateVotersIntents =
            Dict.update voterIdStr
                (\maybeCartVoter ->
                    case maybeCartVoter of
                        Nothing ->
                            Just <| CartVoter voter <| Dict.singleton actionIdStr voteRecord

                        Just { voteRecords } ->
                            Just <| CartVoter voter <| Dict.insert actionIdStr voteRecord voteRecords
                )
    in
    case model of
        Preparing { votersIntents } ->
            Preparing { votersIntents = updateVotersIntents votersIntents, error = Nothing }

        Ready { votersIntents } ->
            Preparing { votersIntents = updateVotersIntents votersIntents, error = Nothing }


{-| Delete a vote from the cart.
Reset the state to Preparing.
-}
deleteVote : String -> String -> Model -> Model
deleteVote voterIdStr actionIdStr model =
    let
        removeVoteFromCart intents =
            Dict.update voterIdStr (Maybe.andThen removeActionId) intents

        removeActionId : CartVoter -> Maybe CartVoter
        removeActionId { voter, voteRecords } =
            Dict.remove actionIdStr voteRecords
                |> (\newDict ->
                        if Dict.isEmpty newDict then
                            Nothing

                        else
                            Just { voter = voter, voteRecords = newDict }
                   )
    in
    case model of
        Preparing { votersIntents } ->
            Preparing { votersIntents = removeVoteFromCart votersIntents, error = Nothing }

        Ready { votersIntents } ->
            Preparing { votersIntents = removeVoteFromCart votersIntents, error = Nothing }



-- Serialization


serialize : Model -> JE.Value
serialize cart =
    case cart of
        Preparing { votersIntents } ->
            serializeVotersIntents votersIntents

        Ready { votersIntents } ->
            serializeVotersIntents votersIntents


{-| Serialization of all voters intents into a single JSON object.

There are many trade-offs between serializing all intents into a single object
or doing multiple objects such as one per voter or one per proposal.

The problem with one per voter, is that when reloading the app,
we cannot know in advance which voters to load, unless we have an api
to retrieve all items of a given store.
(which might be a good idea, but for now we only have read per-key in the store)

The problem with one per proposal, is that we have to duplicate the voter witness
for each and every proposal.
It might get error-prone as we essentially duplicate information
that is not supposed to be duplicated.

The problem with one single object for the whole cart,
is that we have the serialize the whole cart every time we update it.
And potentially riskier if something goes wrong and remove the whole cart.

There are counter-measures of course for each approach,
but for the sake of simplicity, let’s start with one single JSON object.

-}
serializeVotersIntents : Dict String CartVoter -> JE.Value
serializeVotersIntents votersIntents =
    JE.dict identity serializeCartVoter votersIntents


serializeCartVoter : CartVoter -> JE.Value
serializeCartVoter { voter, voteRecords } =
    JE.object
        [ ( "voter", serializeVoter voter )
        , ( "voteRecords", JE.dict identity serializeVoteRecord voteRecords )
        ]


{-| Total number of proposals currently in the cart.
-}
cartCount : Model -> Int
cartCount model =
    case model of
        Preparing { votersIntents } ->
            countAllVotersVotes votersIntents

        Ready { votersIntents } ->
            countAllVotersVotes votersIntents


serializeVoter : Witness.Voter -> JE.Value
serializeVoter voter =
    case voter of
        WithCommitteeHotCred cred ->
            JE.object
                [ ( "type", JE.string "withCommitteeHotCred" )
                , ( "cred", serializeCredentialWitness cred )
                ]

        WithDrepCred cred ->
            JE.object
                [ ( "type", JE.string "withDrepCred" )
                , ( "cred", serializeCredentialWitness cred )
                ]

        WithPoolCred credHash ->
            JE.object
                [ ( "type", JE.string "withPoolCred" )
                , ( "credHash", Bytes.jsonEncode credHash )
                ]


serializeVoteRecord : VoteRecord -> JE.Value
serializeVoteRecord { proposalTitle, voteIntent } =
    JE.object
        [ ( "proposalTitle", JE.string proposalTitle )
        , ( "voteIntent", serializeVoteIntent voteIntent )
        ]


serializeVoteIntent : VoteIntent -> JE.Value
serializeVoteIntent { actionId, vote, rationale } =
    JE.object
        [ ( "actionId", serializeActionId actionId )
        , ( "vote", serializeVote vote )
        , ( "rationale", Maybe.map serializeAnchor rationale |> Maybe.withDefault JE.null )
        ]


serializeActionId : ActionId -> JE.Value
serializeActionId { transactionId, govActionIndex } =
    JE.object
        [ ( "transactionId", Bytes.jsonEncode transactionId )
        , ( "govActionIndex", JE.int govActionIndex )
        ]


serializeVote : Gov.Vote -> JE.Value
serializeVote vote =
    case vote of
        Gov.VoteNo ->
            JE.int 0

        Gov.VoteYes ->
            JE.int 1

        Gov.VoteAbstain ->
            JE.int 2


serializeAnchor : Anchor -> JE.Value
serializeAnchor { url, dataHash } =
    JE.object
        [ ( "url", JE.string url )
        , ( "dataHash", Bytes.jsonEncode dataHash )
        ]


serializeCredentialWitness : Witness.Credential -> JE.Value
serializeCredentialWitness cred =
    -- type: key | nativeScriptByValue | nativeScriptByRef | plutusScriptByValue | plutusScriptByRef
    case cred of
        Witness.WithKey keyHash ->
            JE.object
                [ ( "type", JE.string "key" )
                , ( "keyHash", Bytes.jsonEncode keyHash )
                ]

        Witness.WithScript scriptHash scriptWitness ->
            case scriptWitness of
                Witness.Native { script, expectedSigners } ->
                    case script of
                        -- For NativeScript, we just serialize the script inline
                        Witness.ByValue nativeScript ->
                            JE.object
                                [ ( "type", JE.string "nativeScriptByValue" )
                                , ( "scriptHash", Bytes.jsonEncode scriptHash )
                                , ( "script", Script.jsonEncodeNativeScript nativeScript )
                                , ( "expectedSigners", JE.list Bytes.jsonEncode expectedSigners )
                                ]

                        Witness.ByReference ref ->
                            JE.object
                                [ ( "type", JE.string "nativeScriptByRef" )
                                , ( "scriptHash", Bytes.jsonEncode scriptHash )
                                , ( "ref", Utils.jsonEncodeCbor <| Utxo.encodeOutputReference ref )
                                , ( "expectedSigners", JE.list Bytes.jsonEncode expectedSigners )
                                ]

                Witness.Plutus _ ->
                    Debug.todo "Handle serialization of Plutus witnesses"



-- Deserialization


deserialize : JD.Decoder Model
deserialize =
    JD.map (\intents -> Preparing { votersIntents = intents, error = Nothing }) deserializeVotersIntents


deserializeVotersIntents : JD.Decoder (Dict String CartVoter)
deserializeVotersIntents =
    JD.dict deserializeCartVoter


deserializeCartVoter : JD.Decoder CartVoter
deserializeCartVoter =
    JD.map2 CartVoter
        (JD.field "voter" deserializeVoter)
        (JD.field "voteRecords" <| JD.dict deserializeVoteRecord)


deserializeVoter : JD.Decoder Voter
deserializeVoter =
    JD.field "type" JD.string
        |> JD.andThen
            (\voterType ->
                case voterType of
                    "withCommitteeHotCred" ->
                        JD.field "cred" deserializeCredentialWitness
                            |> JD.map WithCommitteeHotCred

                    "withDrepCred" ->
                        JD.field "cred" deserializeCredentialWitness
                            |> JD.map WithDrepCred

                    "withPoolCred" ->
                        JD.field "credHash" Bytes.jsonDecoder
                            |> JD.map WithPoolCred

                    _ ->
                        JD.fail <| "Unknown voter type: " ++ voterType
            )


deserializeCredentialWitness : JD.Decoder Witness.Credential
deserializeCredentialWitness =
    JD.field "type" JD.string
        |> JD.andThen
            (\witnessType ->
                case witnessType of
                    "key" ->
                        JD.map Witness.WithKey (JD.field "keyHash" Bytes.jsonDecoder)

                    "nativeScriptByValue" ->
                        JD.map3
                            (\scriptHash script signers ->
                                Witness.WithScript scriptHash
                                    (Witness.Native
                                        { script = Witness.ByValue script
                                        , expectedSigners = signers
                                        }
                                    )
                            )
                            (JD.field "scriptHash" Bytes.jsonDecoder)
                            (JD.field "script" Script.jsonDecodeNativeScript)
                            (JD.field "expectedSigners" (JD.list Bytes.jsonDecoder))

                    "nativeScriptByRef" ->
                        JD.map3
                            (\scriptHash ref signers ->
                                Witness.WithScript scriptHash
                                    (Witness.Native
                                        { script = Witness.ByReference ref
                                        , expectedSigners = signers
                                        }
                                    )
                            )
                            (JD.field "scriptHash" Bytes.jsonDecoder)
                            (JD.field "ref" <| Utils.jsonDecodeCbor Utxo.decodeOutputReference)
                            (JD.field "expectedSigners" (JD.list Bytes.jsonDecoder))

                    other ->
                        -- Deserialization for Plutus scripts is not yet implemented,
                        -- similar to the serialization logic.
                        JD.fail ("Unsupported credential witness type for deserialization: " ++ other)
            )


deserializeVoteRecord : JD.Decoder VoteRecord
deserializeVoteRecord =
    JD.map2 VoteRecord
        (JD.field "proposalTitle" JD.string)
        (JD.field "voteIntent" deserializeVoteIntent)


deserializeVoteIntent : JD.Decoder VoteIntent
deserializeVoteIntent =
    JD.map3 VoteIntent
        (JD.field "actionId" deserializeActionId)
        (JD.field "vote" deserializeVote)
        (JD.field "rationale" <| JD.maybe deserializeAnchor)


deserializeActionId : JD.Decoder ActionId
deserializeActionId =
    JD.map2 ActionId
        (JD.field "transactionId" Bytes.jsonDecoder)
        (JD.field "govActionIndex" JD.int)


deserializeVote : JD.Decoder Gov.Vote
deserializeVote =
    JD.int
        |> JD.andThen
            (\vote ->
                case vote of
                    0 ->
                        JD.succeed Gov.VoteNo

                    1 ->
                        JD.succeed Gov.VoteYes

                    2 ->
                        JD.succeed Gov.VoteAbstain

                    _ ->
                        JD.fail "Invalid vote value"
            )


deserializeAnchor : JD.Decoder Anchor
deserializeAnchor =
    JD.map2 Anchor
        (JD.field "url" JD.string)
        (JD.field "dataHash" Bytes.jsonDecoder)



-- Tx Building


buildTx : CostModels -> Utxo.RefDict Output -> Address -> Dict String CartVoter -> Result String TxFinalized
buildTx costModels localStateUtxos walletAddress votersIntents =
    let
        -- Use any address (enterprise / full) with the same payment cred
        -- as the one from the default wallet address to pay the fee
        walletOutputs =
            Dict.Any.values localStateUtxos

        potentialFeeSources =
            case Address.extractPubKeyHash walletAddress of
                Just paymentCred ->
                    walletOutputs
                        |> List.map (\output -> output.address)
                        |> List.filter (\addr -> Address.extractPubKeyHash addr == Just paymentCred)

                Nothing ->
                    []

        -- Helper function to gather free Ada for a given address
        -- Convert Natural amounts to Int (1 = 1 ada) for easy comparison
        freeAdaForAddress address =
            let
                freeAda output =
                    if output.address == address then
                        Utxo.freeAda output

                    else
                        N.zero
            in
            walletOutputs
                |> List.foldl (\output sum -> N.add sum <| freeAda output) N.zero
                -- divide by 1000000 to get ada amount from lovelace amount
                |> (\n -> n |> N.divBy (N.fromSafeInt 1000000))
                |> Maybe.withDefault N.zero
                |> N.toInt

        -- Pick the one with most free Ada as the payment source
        feeSource =
            List.sortBy freeAdaForAddress potentialFeeSources
                |> List.reverse
                |> List.head
                |> Maybe.withDefault walletAddress

        allVoteIntents : List TxIntent
        allVoteIntents =
            Dict.values votersIntents
                |> List.map
                    (\{ voter, voteRecords } ->
                        TxIntent.Vote voter <| List.map .voteIntent <| Dict.values voteRecords
                    )
    in
    allVoteIntents
        |> TxIntent.finalizeAdvanced
            { govState = TxIntent.emptyGovernanceState
            , localStateUtxos = localStateUtxos
            , coinSelectionAlgo = CoinSelection.largestFirst
            , evalScriptsCosts = Uplc.evalScriptsCosts Uplc.defaultVmConfig
            , costModels = costModels
            }
            (AutoFee { paymentSource = feeSource })
            []
        |> Result.mapError TxIntent.errorToString



-- VIEW ##############################################################


type alias ViewContext a msg =
    { a
        | wrapMsg : Msg -> msg
        , deleteVote : { voterIdStr : String, actionIdStr : String } -> msg
        , clearCart : msg
        , signingLink : Transaction -> List { keyName : String, keyHash : Bytes CredentialHash } -> List (Html msg) -> Html msg
    }


view : ViewContext a msg -> Model -> Html msg
view ctx model =
    case model of
        Preparing cartPreparation ->
            viewPreparingCart ctx cartPreparation

        Ready cartReady ->
            viewReadyCart ctx cartReady


viewPreparingCart : ViewContext a msg -> CartPreparation -> Html msg
viewPreparingCart ctx { votersIntents, error } =
    let
        hasVotes : Bool
        hasVotes =
            not (Dict.isEmpty votersIntents)

        pageAttrs : List (Html.Attribute msg)
        pageAttrs =
            [ HA.style "max-width" "1100px"
            , HA.style "margin" "0 auto"
            , HA.style "padding" "0 1rem"
            ]

        counts =
            decisionCounts votersIntents

        summaryBar : Html msg
        summaryBar =
            if hasVotes then
                div
                    [ HA.style "display" "flex"
                    , HA.style "align-items" "center"
                    , HA.style "justify-content" "space-between"
                    , HA.style "margin-bottom" "0.75rem"
                    ]
                    [ div
                        [ HA.style "display" "flex"
                        , HA.style "align-items" "center"
                        , HA.style "gap" "0.5rem"
                        , HA.style "flex-wrap" "wrap"
                        ]
                        [ viewTotalVotesChip (countAllVotersVotes votersIntents)
                        , viewDecisionStat Gov.VoteYes counts.yes
                        , viewDecisionStat Gov.VoteNo counts.no
                        , viewDecisionStat Gov.VoteAbstain counts.abstain
                        ]
                    , viewButton "Clear Cart" ctx.clearCart
                    ]

            else
                text ""

        actionsBar : Html msg
        actionsBar =
            div
                [ HA.style "display" "flex"
                , HA.style "gap" "0.75rem"
                , HA.style "margin-top" "0.5rem"
                , HA.style "margin-bottom" "1rem"
                ]
                [ viewButton "Build Transaction" (ctx.wrapMsg BuildTx) ]
    in
    if hasVotes then
        div pageAttrs <|
            List.concat
                [ [ viewCartHeader, summaryBar ]
                , List.map (viewVoterIntents ctx) (Dict.toList votersIntents)
                , [ actionsBar, viewError error ]
                ]

    else
        div pageAttrs
            [ viewCartHeader
            , summaryBar
            , viewEmptyCart
            , viewError error
            ]


viewVoterIntents : ViewContext a msg -> ( String, { voter : Witness.Voter, voteRecords : Dict String VoteRecord } ) -> Html msg
viewVoterIntents ctx ( voterIdStr, { voteRecords } ) =
    cardContainer []
        [ cardHeader
            [ HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            , HA.style "align-items" "flex-start"
            , HA.style "gap" "0.25rem"
            ]
            "Voter"
            ""
            [ Html.div
                [ HA.style "font-size" "0.875rem"
                , HA.style "color" "#4A5568"
                , HA.style "word-break" "break-all"
                , HA.style "overflow-wrap" "anywhere"
                ]
                [ text voterIdStr ]
            ]
        , cardContent []
            (Dict.toList voteRecords
                |> List.map (viewVoteRecord ctx voterIdStr)
            )
        ]


viewVoteRecord : ViewContext a msg -> String -> ( String, VoteRecord ) -> Html msg
viewVoteRecord ctx voterIdStr ( actionIdStr, { proposalTitle, voteIntent } ) =
    let
        { vote, rationale } =
            voteIntent

        -- Convert ipfs URL to a gateway link
        toWebUrl : String -> String
        toWebUrl url =
            if String.startsWith "ipfs://" url then
                "https://ipfs.io/ipfs/" ++ String.dropLeft 7 url

            else
                url

        rationaleView : Html msg
        rationaleView =
            case rationale of
                Nothing ->
                    Html.span [ HA.style "color" "#64748B" ] [ text "(no rationale)" ]

                Just { url } ->
                    Html.a
                        [ HA.href (toWebUrl url)
                        , HA.target "_blank"
                        , HA.style "color" "#2563EB"
                        , HA.style "text-decoration" "underline"
                        ]
                        [ text "Your rationale" ]

        row : List (Html.Attribute msg)
        row =
            [ HA.style "display" "flex"
            , HA.style "flex-wrap" "wrap"
            , HA.style "align-items" "flex-start"
            , HA.style "justify-content" "flex-start"
            , HA.style "gap" "0.75rem"
            , HA.style "padding" "0.5rem 0"
            , HA.style "border-bottom" "1px solid #EDF2F7"
            ]
    in
    div row
        [ div [ HA.style "flex" "1 1 0%", HA.style "min-width" "0" ]
            [ Html.div
                [ HA.style "font-weight" "600"
                , HA.style "color" "#1A202C"
                , HA.style "word-break" "break-word"
                , HA.style "overflow-wrap" "anywhere"
                ]
                [ text proposalTitle ]
            , Html.div [ HA.style "font-size" "0.875rem", HA.style "color" "#4A5568", HA.style "margin-top" "0.35rem", HA.style "display" "flex", HA.style "align-items" "center", HA.style "gap" "0.5rem", HA.style "flex-wrap" "wrap" ]
                [ Html.span [] [ text "Action ID:" ]
                , Html.code
                    [ HA.style "font-family" "monospace"
                    , HA.style "word-break" "break-all"
                    , HA.style "overflow-wrap" "anywhere"
                    ]
                    [ text actionIdStr ]
                , Html.span [] [ text "·" ]
                , rationaleView
                , Html.span [] [ text "·" ]
                , Html.span [ HA.style "color" "#64748B" ] [ text "Vote:" ]
                , viewDecisionBadge vote
                ]
            ]
        , div [ HA.style "align-self" "center", HA.style "margin-left" "auto", HA.style "flex-shrink" "0" ]
            [ Helper.trashButton (ctx.deleteVote { voterIdStr = voterIdStr, actionIdStr = actionIdStr }) ]
        ]


viewReadyCart : ViewContext a msg -> CartReady -> Html msg
viewReadyCart ctx { votersIntents, maxResources, currentResources, txFinalized } =
    let
        countedVotesCount : Int
        countedVotesCount =
            countAllVotersVotes votersIntents

        pageAttrs : List (Html.Attribute msg)
        pageAttrs =
            [ HA.style "max-width" "1100px"
            , HA.style "margin" "0 auto"
            , HA.style "padding" "0 1rem"
            ]

        counts =
            decisionCounts votersIntents

        summaryBar : Html msg
        summaryBar =
            if countedVotesCount > 0 then
                div
                    [ HA.style "display" "flex"
                    , HA.style "align-items" "center"
                    , HA.style "justify-content" "space-between"
                    , HA.style "margin-bottom" "0.75rem"
                    ]
                    [ div
                        [ HA.style "display" "flex"
                        , HA.style "align-items" "center"
                        , HA.style "gap" "0.5rem"
                        , HA.style "flex-wrap" "wrap"
                        ]
                        [ viewTotalVotesChip countedVotesCount
                        , viewDecisionStat Gov.VoteYes counts.yes
                        , viewDecisionStat Gov.VoteNo counts.no
                        , viewDecisionStat Gov.VoteAbstain counts.abstain
                        ]
                    , viewButton "Clear Cart" ctx.clearCart
                    ]

            else
                text ""

        votesSection : Html msg
        votesSection =
            if countedVotesCount == 0 then
                cardContainer []
                    [ cardHeader [] "No votes found" "The generated transaction doesn’t contain any vote." []
                    , cardContent [] []
                    ]

            else
                div [] (List.map (viewVoterIntents ctx) (Dict.toList votersIntents))
    in
    div pageAttrs
        [ viewCartHeader
        , summaryBar
        , votesSection
        , viewResourcesCard maxResources currentResources
        , viewSigningButton ctx txFinalized
        ]


viewCartHeader : Html msg
viewCartHeader =
    div [ HA.style "margin-bottom" "0.75rem" ]
        [ sectionTitle "Cart"
        , Html.p
            [ HA.style "color" "#64748B"
            , HA.style "font-size" "0.9375rem"
            , HA.style "margin-top" "0.25rem"
            ]
            []
        ]


{-| Empty cart placeholder card.
-}
viewEmptyCart : Html msg
viewEmptyCart =
    cardContainer []
        [ cardContent []
            [ div
                [ HA.style "display" "flex"
                , HA.style "flex-direction" "column"
                , HA.style "align-items" "center"
                , HA.style "justify-content" "center"
                , HA.style "text-align" "center"
                , HA.style "padding" "2rem 1rem"
                ]
                [ div
                    [ HA.style "width" "64px"
                    , HA.style "height" "64px"
                    , HA.style "border-radius" "9999px"
                    , HA.style "background-color" "#F1F5F9"
                    , HA.style "display" "flex"
                    , HA.style "align-items" "center"
                    , HA.style "justify-content" "center"
                    , HA.style "margin-bottom" "0.75rem"
                    ]
                    [ Html.span [ HA.style "font-size" "28px" ] [ text "🛒" ] ]
                , Html.div
                    [ HA.style "font-weight" "700"
                    , HA.style "font-size" "1.125rem"
                    , HA.style "color" "#111827"
                    , HA.style "margin-bottom" "0.25rem"
                    ]
                    [ text "Your cart is empty" ]
                , Html.p
                    [ HA.style "color" "#6B7280"
                    , HA.style "font-size" "0.95rem"
                    ]
                    [ text "Add votes from Vote Preparation page to see them here." ]
                ]
            ]
        ]


countAllVotersVotes : Dict String CartVoter -> Int
countAllVotersVotes votes =
    Dict.foldl (\_ cartVoter acc -> acc + Dict.size cartVoter.voteRecords) 0 votes


viewResources : Resources -> Resources -> { overall : Int, size : Int, steps : Int, mem : Int }
viewResources maxResources currentResources =
    let
        usagePercent : Int -> Int -> Int
        usagePercent used maxVal =
            ceiling (100 * toFloat used / toFloat maxVal)

        sizeUsage =
            usagePercent currentResources.txSize maxResources.txSize

        stepsUsage =
            usagePercent currentResources.steps maxResources.steps

        memUsage =
            usagePercent currentResources.mem maxResources.mem

        overallUsage =
            max sizeUsage (max stepsUsage memUsage)
    in
    { overall = overallUsage, size = sizeUsage, steps = stepsUsage, mem = memUsage }


viewResourcesCard : Resources -> Resources -> Html msg
viewResourcesCard maxResources currentResources =
    let
        usage =
            viewResources maxResources currentResources

        barColor : Int -> String
        barColor pct =
            if pct >= 90 then
                "#EF4444"

            else if pct >= 70 then
                "#F59E0B"

            else
                "#10B981"

        widthPct : Int -> Int
        widthPct pct =
            if pct > 100 then
                100

            else if pct < 0 then
                0

            else
                pct

        bar : Int -> Html msg
        bar pct =
            div
                [ HA.style "position" "relative"
                , HA.style "height" "0.75rem"
                , HA.style "background-color" "#EDF2F7"
                , HA.style "border-radius" "9999px"
                , HA.style "overflow" "hidden"
                ]
                [ div
                    [ HA.style "position" "absolute"
                    , HA.style "left" "0"
                    , HA.style "top" "0"
                    , HA.style "bottom" "0"
                    , HA.style "width" (String.fromInt (widthPct pct) ++ "%")
                    , HA.style "background-color" (barColor pct)
                    ]
                    []
                ]

        detailsText : String
        detailsText =
            "Tx size "
                ++ String.fromInt usage.size
                ++ "% · Steps "
                ++ String.fromInt usage.steps
                ++ "% · Mem "
                ++ String.fromInt usage.mem
                ++ "%"

        tooltipText : String
        tooltipText =
            String.fromInt usage.overall ++ "% of limits · " ++ detailsText
    in
    cardContainer []
        [ cardHeader
            [ HA.style "display" "flex"
            , HA.style "align-items" "flex-start"
            , HA.style "justify-content" "flex-start"
            , HA.style "gap" "0.5rem"
            , HA.style "flex-wrap" "wrap"
            ]
            "Max Cart Size"
            (String.fromInt usage.overall ++ "% of limits")
            [ Html.span
                [ HA.title tooltipText
                , HA.attribute "aria-label" tooltipText
                , HA.style "display" "inline-flex"
                , HA.style "align-items" "center"
                , HA.style "justify-content" "center"
                , HA.style "width" "1.1rem"
                , HA.style "height" "1.7rem"
                , HA.style "border-radius" "9999px"
                , HA.style "color" "#374151"
                , HA.style "font-size" "0.75rem"
                , HA.style "cursor" "pointer"
                ]
                [ text "ⓘ" ]
            ]
        , cardContent []
            [ bar usage.overall ]
        ]


decisionCounts : Dict String CartVoter -> { yes : Int, no : Int, abstain : Int }
decisionCounts votersIntents =
    let
        addRecord : VoteRecord -> { yes : Int, no : Int, abstain : Int } -> { yes : Int, no : Int, abstain : Int }
        addRecord { voteIntent } acc =
            case voteIntent.vote of
                Gov.VoteYes ->
                    { acc | yes = acc.yes + 1 }

                Gov.VoteNo ->
                    { acc | no = acc.no + 1 }

                Gov.VoteAbstain ->
                    { acc | abstain = acc.abstain + 1 }
    in
    Dict.values votersIntents
        |> List.concatMap (\{ voteRecords } -> Dict.values voteRecords)
        |> List.foldl addRecord { yes = 0, no = 0, abstain = 0 }


viewDecisionBadge : Gov.Vote -> Html msg
viewDecisionBadge v =
    let
        ( label, bg, fg ) =
            case v of
                Gov.VoteYes ->
                    ( "YES", "#10B981", "#FFFFFF" )

                Gov.VoteNo ->
                    ( "NO", "#EF4444", "#FFFFFF" )

                Gov.VoteAbstain ->
                    ( "ABSTAIN", "#6B7280", "#FFFFFF" )
    in
    Html.span
        [ HA.style "display" "inline-flex"
        , HA.style "align-items" "center"
        , HA.style "height" "1.5rem"
        , HA.style "padding" "0 0.5rem"
        , HA.style "border-radius" "9999px"
        , HA.style "background-color" bg
        , HA.style "color" fg
        , HA.style "font-weight" "600"
        , HA.style "font-size" "0.75rem"
        ]
        [ text label ]


viewTotalVotesChip : Int -> Html msg
viewTotalVotesChip total =
    Html.span
        [ HA.style "display" "inline-flex"
        , HA.style "align-items" "center"
        , HA.style "height" "1.5rem"
        , HA.style "padding" "0 0.6rem"
        , HA.style "border-radius" "9999px"
        , HA.style "background-color" "#EDF2F7"
        , HA.style "color" "#374151"
        , HA.style "font-weight" "600"
        , HA.style "font-size" "0.75rem"
        ]
        [ text <| String.fromInt total ++ " vote(s)" ]


viewDecisionStat : Gov.Vote -> Int -> Html msg
viewDecisionStat v count =
    let
        ( label, bg, fg ) =
            case v of
                Gov.VoteYes ->
                    ( "YES", "#10B981", "#FFFFFF" )

                Gov.VoteNo ->
                    ( "NO", "#EF4444", "#FFFFFF" )

                Gov.VoteAbstain ->
                    ( "ABSTAIN", "#6B7280", "#FFFFFF" )
    in
    Html.span
        [ HA.style "display" "inline-flex"
        , HA.style "align-items" "center"
        , HA.style "height" "1.5rem"
        , HA.style "padding" "0 0.6rem"
        , HA.style "border-radius" "9999px"
        , HA.style "background-color" bg
        , HA.style "color" fg
        , HA.style "font-weight" "700"
        , HA.style "font-size" "0.75rem"
        ]
        [ text <| label ++ " " ++ String.fromInt count ]


viewSigningButton : ViewContext a msg -> TxFinalized -> Html msg
viewSigningButton ctx { tx, expectedSignatures } =
    let
        keyNames : Dict String String
        keyNames =
            Dict.empty

        signingLinkView : Html msg
        signingLinkView =
            ctx.signingLink tx
                (expectedSignatures
                    |> List.map
                        (\keyHash ->
                            { keyHash = keyHash
                            , keyName =
                                Dict.get (Bytes.toHex keyHash) keyNames
                                    |> Maybe.withDefault "Key hash"
                            }
                        )
                )
                [ Helper.signingButton "Go to Signing Page" ]
    in
    div
        [ HA.style "display" "flex"
        , HA.style "gap" "0.75rem"
        , HA.style "align-items" "center"
        , HA.style "margin-top" "1rem"
        , HA.style "margin-bottom" "2rem"
        ]
        [ signingLinkView ]
