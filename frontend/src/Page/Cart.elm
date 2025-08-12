module Page.Cart exposing (Model, Msg, UpdateContext, ViewContext, VoteRecord, addVote, deserialize, init, serialize, update, view)

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
import Helper exposing (viewError)
import Html exposing (Html, div, text)
import Html.Attributes as HA
import Html.Events as HE
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



-- Add a vote


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
    div []
        [ div [] <|
            List.map viewVoterIntents (Dict.toList votersIntents)
        , Html.button [ HE.onClick <| ctx.wrapMsg BuildTx ] [ text "build Tx" ]
        , viewError error
        ]


viewVoterIntents : ( String, { voter : Witness.Voter, voteRecords : Dict String VoteRecord } ) -> Html msg
viewVoterIntents ( voterIdStr, { voter, voteRecords } ) =
    div []
        -- TODO: improve voter details
        [ Html.h4 [] [ text <| "Voter: " ++ voterIdStr ]
        , div [] <| List.map viewVoteRecord <| Dict.toList voteRecords
        ]


viewVoteRecord : ( String, VoteRecord ) -> Html msg
viewVoteRecord ( actionIdStr, { proposalTitle, voteIntent } ) =
    let
        { actionId, vote, rationale } =
            voteIntent

        viewRationale =
            case rationale of
                Nothing ->
                    "none"

                Just { url } ->
                    url
    in
    Html.p []
        [ text <| Debug.toString vote
        , text " | "
        , text proposalTitle
        , text " | action ID: "
        , text <| Gov.actionIdToString actionId
        , text " | rationale: "
        , text viewRationale
        ]


viewReadyCart : ViewContext a msg -> CartReady -> Html msg
viewReadyCart ctx { votersIntents, maxResources, currentResources, txFinalized } =
    let
        countedVotesCount =
            countAllVotersVotes votersIntents

        viewVotersIntents =
            if countedVotesCount == 0 then
                text "The generated Tx doesn’t contain any vote."

            else
                div [] <|
                    (Html.h3 [] [ text "Votes ready for submission" ]
                        :: List.map viewVoterIntents (Dict.toList votersIntents)
                    )
    in
    div []
        [ viewResources maxResources currentResources
        , viewVotersIntents
        , viewSigningButton ctx txFinalized
        ]


countAllVotersVotes : Dict String CartVoter -> Int
countAllVotersVotes votes =
    Dict.foldl (\_ cartVoter acc -> acc + Dict.size cartVoter.voteRecords) 0 votes


viewResources : Resources -> Resources -> Html msg
viewResources maxResources currentResources =
    let
        usagePercent used max =
            ceiling (100 * toFloat used / toFloat max)

        sizeUsage =
            usagePercent currentResources.txSize maxResources.txSize

        stepsUsage =
            usagePercent currentResources.steps maxResources.steps

        memUsage =
            usagePercent currentResources.mem maxResources.mem

        overallUsage =
            max sizeUsage (max stepsUsage memUsage)
    in
    div []
        [ Html.h3 [] [ text "Resource usage:" ]
        , Html.progress [ HA.max "100", HA.value (String.fromInt overallUsage) ] []
        , text <| " " ++ String.fromInt overallUsage ++ " %"
        ]


viewSigningButton : ViewContext a msg -> TxFinalized -> Html msg
viewSigningButton ctx { tx, expectedSignatures } =
    let
        keyNames : Dict String String
        keyNames =
            -- Debug.todo ""
            Dict.empty
    in
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
