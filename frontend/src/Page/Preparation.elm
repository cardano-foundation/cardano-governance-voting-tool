module Page.Preparation exposing (LoadedWallet, Model, Msg, MsgToParent(..), TaskCompleted, UpdateContext, ViewContext, handleTaskCompleted, init, update, view)

{-| This module handles the complete vote preparation workflow, from identifying
the voter to signing the transaction, which is handled by another page.

The workflow is split into the following sequential steps:

1.  Voter identification - Who is voting (DRep/SPO/CC)
2.  Proposal selection - What proposal to vote on
3.  IPFS storage configuration - How to store the rationale on IPFS
4.  Rationale creation - The reasoning behind the vote
5.  Rationale signing - Optional signatures from multiple authors
6.  Rationale storage - Storing rationale on IPFS
7.  Fee handling - How transaction fees will be paid
8.  Transaction building - Creating the vote transaction
9.  Transaction signing - Redirect to the signing page

Each step follows a common pattern using the Step type:

  - Preparing: Initial form state
  - Validating: Checking inputs
  - Done: Step is complete

The steps are sequential but allow going back to modify previous steps.

-}

import Api exposing (CcInfo, DrepInfo, PoolInfo)
import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address exposing (Credential(..), CredentialHash, NetworkId)
import Cardano.Cip30 as Cip30
import Cardano.Gov as Gov exposing (CostModels, Id(..))
import Cardano.Pool as Pool
import Cardano.Script as Script
import Cardano.Transaction as Transaction exposing (Transaction)
import Cardano.Utxo as Utxo exposing (Output, OutputReference)
import Cardano.Witness as Witness
import Cbor.Encode
import ConcurrentTask exposing (ConcurrentTask)
import ConcurrentTask.Extra
import ConcurrentTask.Http
import Dict exposing (Dict)
import Dict.Any
import Helper exposing (PreconfVoter)
import Html exposing (Html, div, text)
import Html.Attributes as HA
import Html.Lazy
import Http
import Json.Decode as JD
import List.Extra
import Natural
import Platform.Cmd as Cmd
import RemoteData exposing (RemoteData, WebData)
import ScriptInfo exposing (ScriptInfo)
import Storage



-- ###################################################################
-- MODEL
-- ###################################################################


type Model
    = Model InnerModel


{-| Main model containing the state for all preparation steps.
Each step uses the Step type to track its progress.
-}
type alias InnerModel =
    { someRefUtxos : Utxo.RefDict Output
    , voterStep : Step VoterPreparationForm Witness.Voter Witness.Voter
    }


{-| Represents the three possible states of any workflow step:

1.  Preparing - User is filling out form data
2.  Validating - Form data is being validated/processed
3.  Done - Step is complete with validated data

This allows a consistent pattern across all the preparation steps.

-}
type Step prep validating done
    = Preparing prep
    | Validating prep validating
    | Done prep done


init : Model
init =
    Model
        { someRefUtxos = Utxo.emptyRefDict
        , voterStep = Preparing initVoterForm
        }



-- Voter Step


type alias VoterPreparationForm =
    { govId : Maybe Gov.Id
    , scriptInfo : RemoteData ConcurrentTask.Http.Error ScriptInfo
    , drepInfo : WebData DrepInfo
    , ccInfo : WebData CcInfo
    , poolInfo : WebData PoolInfo
    , utxoRef : String
    , expectedSigners : Dict String { expected : Bool, key : Bytes CredentialHash }
    , error : Maybe String
    }


initVoterForm : VoterPreparationForm
initVoterForm =
    { govId = Nothing
    , scriptInfo = RemoteData.NotAsked
    , drepInfo = RemoteData.NotAsked
    , ccInfo = RemoteData.NotAsked
    , poolInfo = RemoteData.NotAsked
    , utxoRef = ""
    , expectedSigners = Dict.empty
    , error = Nothing
    }



-- ###################################################################
-- UPDATE
-- ###################################################################


{-| Messages that can be sent to the parent component to:

  - Cache loaded data for reuse
  - Execute concurrent tasks

-}
type MsgToParent
    = CacheScriptInfo ScriptInfo
    | CacheDrepInfo DrepInfo
    | CacheCcInfo CcInfo
    | CachePoolInfo PoolInfo
    | RunTask (ConcurrentTask String TaskCompleted)


{-| Results from asynchronous tasks:

  - Loading reference transaction bytes
  - Loading script info

-}
type TaskCompleted
    = GotRefUtxoTxBytes OutputReference (Result ConcurrentTask.Http.Error (Bytes Transaction))
    | GotScriptInfoTask (Result ConcurrentTask.Http.Error ScriptInfo)


type Msg
    = VoterGovIdChange String
    | GotDrepInfo (Result Http.Error DrepInfo)
    | GotCcInfo (Result Http.Error CcInfo)
    | GotPoolInfo (Result Http.Error PoolInfo)
    | UtxoRefChange String
    | ToggleExpectedSigner String Bool
    | ValidateVoterFormButtonClicked
    | ChangeVoterButtonClicked


{-| Configuration required by the update function.
Provides access to:

  - External data (script info, etc.)
  - Cardano network cost models (for script execution)
  - Wallet integration
  - In-browser storage connection
  - Governance metadata JSON LD format

-}
type alias UpdateContext msg =
    { wrapMsg : Msg -> msg
    , db : JD.Value
    , scriptsInfo : Dict String ScriptInfo
    , drepsInfo : Dict String DrepInfo
    , ccsInfo : Dict String CcInfo
    , poolsInfo : Dict String PoolInfo
    , loadedWallet : Maybe LoadedWallet
    , drepId : Maybe (Bytes CredentialHash)
    , costModels : Maybe CostModels
    , networkId : NetworkId
    }


type alias LoadedWallet =
    { wallet : Cip30.Wallet
    , utxos : Utxo.RefDict Output
    }


update : UpdateContext msg -> Msg -> Model -> ( Model, Cmd msg, Maybe MsgToParent )
update ctx msg (Model model) =
    let
        ( updatedModel, cmd, toParent ) =
            innerUpdate ctx msg model
    in
    ( Model updatedModel, cmd, toParent )


innerUpdate : UpdateContext msg -> Msg -> InnerModel -> ( InnerModel, Cmd msg, Maybe MsgToParent )
innerUpdate ctx msg model =
    case msg of
        --
        -- Voter Step
        --
        ToggleExpectedSigner keyHex expected ->
            let
                updatedExpectedSigners expectedSigners =
                    Dict.update keyHex
                        (Maybe.map (\entry -> { entry | expected = expected }))
                        expectedSigners
            in
            ( updateVoterForm (\form -> { form | expectedSigners = updatedExpectedSigners form.expectedSigners }) model
            , Cmd.none
            , Nothing
            )

        ValidateVoterFormButtonClicked ->
            case model.voterStep of
                Preparing form ->
                    let
                        ( newVoterStep, cmds, toParent ) =
                            confirmVoter ctx form model.someRefUtxos
                    in
                    ( { model | voterStep = newVoterStep }
                    , Cmd.map ctx.wrapMsg cmds
                    , toParent
                    )

                _ ->
                    ( model, Cmd.none, Nothing )

        VoterGovIdChange govIdStr ->
            case checkGovId ctx govIdStr of
                Err error ->
                    ( updateVoterForm (\_ -> { initVoterForm | error = Just error }) model
                    , Cmd.none
                    , Nothing
                    )

                Ok { govId, scriptInfo, expectedSigners, drepInfo, ccInfo, poolInfo, cmd, msgToParent } ->
                    ( updateVoterForm
                        (\_ ->
                            { initVoterForm
                                | govId = Just govId
                                , scriptInfo = scriptInfo
                                , expectedSigners = expectedSigners
                                , drepInfo = drepInfo
                                , ccInfo = ccInfo
                                , poolInfo = poolInfo
                            }
                        )
                        model
                    , Cmd.map ctx.wrapMsg cmd
                    , msgToParent
                    )

        GotDrepInfo result ->
            case result of
                Err error ->
                    ( updateVoterForm (\form -> { form | drepInfo = RemoteData.Failure error }) model
                    , Cmd.none
                    , Nothing
                    )

                Ok drepInfo ->
                    ( updateVoterForm (\form -> { form | drepInfo = RemoteData.Success drepInfo }) model
                    , Cmd.none
                    , Just <| CacheDrepInfo drepInfo
                    )

        GotCcInfo result ->
            case result of
                Err error ->
                    ( updateVoterForm (\form -> { form | ccInfo = RemoteData.Failure error }) model
                    , Cmd.none
                    , Nothing
                    )

                Ok ccInfo ->
                    ( updateVoterForm (\form -> { form | ccInfo = RemoteData.Success ccInfo }) model
                    , Cmd.none
                    , Just <| CacheCcInfo ccInfo
                    )

        GotPoolInfo result ->
            case result of
                Err error ->
                    ( updateVoterForm (\form -> { form | poolInfo = RemoteData.Failure error }) model
                    , Cmd.none
                    , Nothing
                    )

                Ok poolInfo ->
                    ( updateVoterForm (\form -> { form | poolInfo = RemoteData.Success poolInfo }) model
                    , Cmd.none
                    , Just <| CachePoolInfo poolInfo
                    )

        UtxoRefChange utxoRef ->
            ( updateVoterForm (\form -> { form | utxoRef = utxoRef }) model
            , Cmd.none
            , Nothing
            )

        ChangeVoterButtonClicked ->
            case model.voterStep of
                Done prep _ ->
                    ( { model | voterStep = Preparing prep }
                    , Cmd.none
                    , Nothing
                    )

                _ ->
                    ( model, Cmd.none, Nothing )


handleTaskCompleted : TaskCompleted -> Model -> ( Model, Cmd Msg, Maybe MsgToParent )
handleTaskCompleted task (Model model) =
    case task of
        GotRefUtxoTxBytes outputRef bytesResult ->
            case ( model.voterStep, bytesResult ) of
                ( Validating form voterWitness, Ok txBytes ) ->
                    case Transaction.deserialize txBytes of
                        Nothing ->
                            let
                                errorMsg =
                                    "Failed to decode Tx "
                                        ++ Bytes.toHex outputRef.transactionId
                                        ++ ": "
                                        ++ Bytes.toHex txBytes
                            in
                            ( Model { model | voterStep = Preparing { form | error = Just errorMsg } }
                            , Cmd.none
                            , Nothing
                            )

                        Just tx ->
                            case List.Extra.getAt outputRef.outputIndex tx.body.outputs of
                                Nothing ->
                                    let
                                        errorMsg =
                                            "The Tx retrieved (id: "
                                                ++ Bytes.toHex outputRef.transactionId
                                                ++ ") doesn’t seem to have an output at position "
                                                ++ String.fromInt outputRef.outputIndex
                                    in
                                    ( Model { model | voterStep = Preparing { form | error = Just errorMsg } }
                                    , Cmd.none
                                    , Nothing
                                    )

                                Just output ->
                                    ( Model
                                        { model
                                            | voterStep = Done { form | error = Nothing } voterWitness
                                            , someRefUtxos = Dict.Any.insert outputRef output model.someRefUtxos
                                        }
                                    , Cmd.none
                                    , Nothing
                                    )

                ( Validating form _, Err httpError ) ->
                    ( Model { model | voterStep = Preparing { form | error = Just (Debug.toString httpError) } }
                    , Cmd.none
                    , Nothing
                    )

                _ ->
                    ( Model model, Cmd.none, Nothing )

        GotScriptInfoTask scriptInfoResult ->
            case scriptInfoResult of
                Err error ->
                    ( Model <| updateVoterForm (\form -> { form | scriptInfo = RemoteData.Failure error }) model
                    , Cmd.none
                    , Nothing
                    )

                Ok scriptInfo ->
                    ( Model <|
                        updateVoterForm
                            (\form ->
                                { form
                                    | scriptInfo = RemoteData.Success scriptInfo
                                    , expectedSigners =
                                        case scriptInfo.script of
                                            Script.Native nativeScript ->
                                                Script.extractSigners nativeScript
                                                    |> Dict.map (\_ key -> { expected = True, key = key })

                                            Script.Plutus _ ->
                                                Dict.empty
                                }
                            )
                            model
                    , Cmd.none
                    , Just <| CacheScriptInfo scriptInfo
                    )



-- Voter Step


updateVoterForm : (VoterPreparationForm -> VoterPreparationForm) -> InnerModel -> InnerModel
updateVoterForm f ({ voterStep } as model) =
    case voterStep of
        Preparing form ->
            { model | voterStep = Preparing (f form) }

        _ ->
            model


type alias GovIdCheck =
    { govId : Gov.Id
    , scriptInfo : RemoteData ConcurrentTask.Http.Error ScriptInfo
    , expectedSigners : Dict String { expected : Bool, key : Bytes CredentialHash }
    , poolInfo : WebData PoolInfo
    , drepInfo : WebData DrepInfo
    , ccInfo : WebData CcInfo
    , cmd : Cmd Msg
    , msgToParent : Maybe MsgToParent
    }


{-| Validates if a governance ID is valid and loads associated information:

  - For scripts: loads script info and extracts expected signers
  - For DReps: loads voting power
  - For CCs: loads committee member info
  - For SPOs: loads stake pool info

Returns information needed for voting or an error message.

-}
checkGovId : UpdateContext msg -> String -> Result String GovIdCheck
checkGovId ctx str =
    case Gov.idFromBech32 str of
        Nothing ->
            Err <| "This doesn’t look like a valid CIP 129 governance Id: " ++ str

        Just govId ->
            let
                defaultCheck =
                    { govId = govId
                    , scriptInfo = RemoteData.NotAsked
                    , expectedSigners = Dict.empty
                    , poolInfo = RemoteData.NotAsked
                    , drepInfo = RemoteData.NotAsked
                    , ccInfo = RemoteData.NotAsked
                    , cmd = Cmd.none
                    , msgToParent = Nothing
                    }
            in
            case govId of
                Gov.CcColdCredId _ ->
                    Err <| "You are supposed to vote with your hot CC key, not the cold key: " ++ str

                Gov.GovActionId _ ->
                    Err "Please use one of the drep/pool/cc_hot CIP 129 governance Ids. Proposal to vote on will be selected later."

                -- Using a public key for the gov Id is the simplest case
                Gov.CcHotCredId ((VKeyHash keyHash) as cred) ->
                    let
                        ( ccInfo, fetchCcInfo ) =
                            case Dict.get (Bytes.toHex keyHash) ctx.ccsInfo of
                                Nothing ->
                                    ( RemoteData.Loading, Api.defaultApiProvider.getCcInfo ctx.networkId cred GotCcInfo )

                                Just info ->
                                    ( RemoteData.Success info, Cmd.none )
                    in
                    Ok { defaultCheck | ccInfo = ccInfo, cmd = fetchCcInfo }

                Gov.DrepId ((VKeyHash keyHash) as cred) ->
                    let
                        ( drepInfo, fetchDrepInfo ) =
                            case Dict.get (Bytes.toHex keyHash) ctx.drepsInfo of
                                Nothing ->
                                    ( RemoteData.Loading, Api.defaultApiProvider.getDrepInfo ctx.networkId cred GotDrepInfo )

                                Just info ->
                                    ( RemoteData.Success info, Cmd.none )
                    in
                    Ok { defaultCheck | drepInfo = drepInfo, cmd = fetchDrepInfo }

                Gov.PoolId poolId ->
                    let
                        ( poolInfo, fetchPoolInfo ) =
                            case Dict.get (Bytes.toHex poolId) ctx.poolsInfo of
                                Nothing ->
                                    ( RemoteData.Loading, Api.defaultApiProvider.getPoolLiveStake ctx.networkId poolId GotPoolInfo )

                                Just info ->
                                    ( RemoteData.Success info, Cmd.none )
                    in
                    Ok { defaultCheck | poolInfo = poolInfo, cmd = fetchPoolInfo }

                -- Using a script voter will require some extra information to be fetched
                Gov.CcHotCredId ((ScriptHash scriptHash) as cred) ->
                    let
                        ( scriptInfo, expectedSigners, fetchScriptInfoMsgToParent ) =
                            case Dict.get (Bytes.toHex scriptHash) ctx.scriptsInfo of
                                Nothing ->
                                    ( RemoteData.Loading
                                    , Dict.empty
                                    , Api.defaultApiProvider.getScriptInfo ctx.networkId scriptHash
                                        |> Storage.cacheWrap
                                            { db = ctx.db, storeName = "scriptInfo" }
                                            ScriptInfo.storageDecoder
                                            ScriptInfo.storageEncode
                                            { key = Bytes.toHex scriptHash }
                                        |> ConcurrentTask.onJsException (\{ message } -> ConcurrentTask.fail <| ConcurrentTask.Http.BadUrl <| "Uncaught JS exception: " ++ message)
                                        |> ConcurrentTask.Extra.toResult
                                        |> ConcurrentTask.map GotScriptInfoTask
                                        |> RunTask
                                        |> Just
                                    )

                                Just info ->
                                    ( RemoteData.Success info
                                    , case info.script of
                                        Script.Native nativeScript ->
                                            Script.extractSigners nativeScript
                                                |> Dict.map (\_ key -> { expected = True, key = key })

                                        Script.Plutus _ ->
                                            Dict.empty
                                    , Nothing
                                    )

                        ( ccInfo, fetchCcInfo ) =
                            case Dict.get (Bytes.toHex scriptHash) ctx.ccsInfo of
                                Nothing ->
                                    ( RemoteData.Loading, Api.defaultApiProvider.getCcInfo ctx.networkId cred GotCcInfo )

                                Just info ->
                                    ( RemoteData.Success info, Cmd.none )
                    in
                    Ok
                        { defaultCheck
                            | scriptInfo = scriptInfo
                            , expectedSigners = expectedSigners
                            , ccInfo = ccInfo
                            , cmd = fetchCcInfo
                            , msgToParent = fetchScriptInfoMsgToParent
                        }

                Gov.DrepId ((ScriptHash scriptHash) as cred) ->
                    let
                        ( scriptInfo, expectedSigners, fetchScriptInfoMsgToParent ) =
                            case Dict.get (Bytes.toHex scriptHash) ctx.scriptsInfo of
                                Nothing ->
                                    ( RemoteData.Loading
                                    , Dict.empty
                                    , Api.defaultApiProvider.getScriptInfo ctx.networkId scriptHash
                                        |> Storage.cacheWrap
                                            { db = ctx.db, storeName = "scriptInfo" }
                                            ScriptInfo.storageDecoder
                                            ScriptInfo.storageEncode
                                            { key = Bytes.toHex scriptHash }
                                        |> ConcurrentTask.onJsException (\{ message } -> ConcurrentTask.fail <| ConcurrentTask.Http.BadUrl <| "Uncaught JS exception: " ++ message)
                                        |> ConcurrentTask.Extra.toResult
                                        |> ConcurrentTask.map GotScriptInfoTask
                                        |> RunTask
                                        |> Just
                                    )

                                Just info ->
                                    ( RemoteData.Success info
                                    , case info.script of
                                        Script.Native nativeScript ->
                                            Script.extractSigners nativeScript
                                                |> Dict.map (\_ key -> { expected = True, key = key })

                                        Script.Plutus _ ->
                                            Dict.empty
                                    , Nothing
                                    )

                        ( drepInfo, fetchDrepInfo ) =
                            case Dict.get (Bytes.toHex scriptHash) ctx.drepsInfo of
                                Nothing ->
                                    ( RemoteData.Loading, Api.defaultApiProvider.getDrepInfo ctx.networkId cred GotDrepInfo )

                                Just info ->
                                    ( RemoteData.Success info, Cmd.none )
                    in
                    Ok
                        { defaultCheck
                            | scriptInfo = scriptInfo
                            , expectedSigners = expectedSigners
                            , drepInfo = drepInfo
                            , cmd = fetchDrepInfo
                            , msgToParent = fetchScriptInfoMsgToParent
                        }


confirmVoter : UpdateContext msg -> VoterPreparationForm -> Utxo.RefDict Output -> ( Step VoterPreparationForm Witness.Voter Witness.Voter, Cmd Msg, Maybe MsgToParent )
confirmVoter ctx form loadedRefUtxos =
    let
        justError errorMsg =
            ( Preparing { form | error = Just errorMsg }
            , Cmd.none
            , Nothing
            )
    in
    case form.govId of
        Nothing ->
            justError "You must provide a valid governance ID. It can be a bech32 pool ID, a CIP 129 DRep ID or CC hot ID."

        Just (CcColdCredId _) ->
            justError "CC cold keys are not used to vote, please provide your ID for the hot keys instead."

        Just (GovActionId _) ->
            justError "The proposal to vote on is selected later. For now please provide you voter ID. It can be a bech32 pool ID, a CIP 129 DRep ID or CC hot ID."

        Just (PoolId poolId) ->
            ( Done form <| Witness.WithPoolCred poolId
            , Cmd.none
            , Nothing
            )

        Just (DrepId (VKeyHash keyHash)) ->
            ( Done form <| Witness.WithDrepCred (Witness.WithKey keyHash)
            , Cmd.none
            , Nothing
            )

        Just (CcHotCredId (VKeyHash keyHash)) ->
            ( Done form <| Witness.WithCommitteeHotCred (Witness.WithKey keyHash)
            , Cmd.none
            , Nothing
            )

        Just (DrepId (ScriptHash _)) ->
            case form.scriptInfo of
                RemoteData.NotAsked ->
                    justError "Script info isn’t loading yet govId is a script. Please report the error."

                RemoteData.Loading ->
                    justError "Script info is still loading, please wait."

                RemoteData.Failure error ->
                    justError <| "There was an error loading the script info. Are you sure you registered? " ++ Debug.toString error

                RemoteData.Success scriptInfo ->
                    validateScriptVoter ctx form loadedRefUtxos Witness.WithDrepCred scriptInfo

        Just (CcHotCredId (ScriptHash _)) ->
            case form.scriptInfo of
                RemoteData.NotAsked ->
                    justError "Script info isn’t loading yet govId is a script. Please report the error."

                RemoteData.Loading ->
                    justError "Script info is still loading, please wait."

                RemoteData.Failure error ->
                    justError <| "There was an error loading the script info. Are you sure you registered? " ++ Debug.toString error

                RemoteData.Success scriptInfo ->
                    validateScriptVoter ctx form loadedRefUtxos Witness.WithCommitteeHotCred scriptInfo


validateScriptVoter : UpdateContext msg -> VoterPreparationForm -> Utxo.RefDict Output -> (Witness.Credential -> Witness.Voter) -> ScriptInfo -> ( Step VoterPreparationForm Witness.Voter Witness.Voter, Cmd Msg, Maybe MsgToParent )
validateScriptVoter ctx form loadedRefUtxos toVoter scriptInfo =
    let
        justError errorMsg =
            ( Preparing { form | error = Just errorMsg }
            , Cmd.none
            , Nothing
            )
    in
    case ( form.utxoRef, utxoRefFromStr form.utxoRef ) of
        -- When not using a reference UTxO, just proceed with an inline witness
        ( "", _ ) ->
            case scriptInfo.script of
                Script.Native nativeScript ->
                    -- If the native script isn’t encoded correctly we can’t vote
                    if scriptInfo.nativeCborEncodingMatchesHash == Just True then
                        let
                            witness =
                                { script = Witness.ByValue nativeScript
                                , expectedSigners = keepOnlyExpectedSigners form.expectedSigners
                                }
                        in
                        ( Done { form | error = Nothing } <| toVoter <| Witness.WithScript scriptInfo.scriptHash <| Witness.Native witness
                        , Cmd.none
                        , Nothing
                        )

                    else
                        -- If the native script isn’t encoded correctly we can’t vote
                        justError "For technical reasons you need to provide a reference UTxO for the script."

                Script.Plutus plutusScript ->
                    let
                        witness =
                            { script = ( Script.plutusVersion plutusScript, Witness.ByValue <| Script.cborWrappedBytes plutusScript )
                            , redeemerData = Debug.todo "Add forms for the redeemer Data"
                            , requiredSigners = Debug.todo "Add a required signers form for Plutus"
                            }
                    in
                    ( Done { form | error = Nothing } <| toVoter <| Witness.WithScript scriptInfo.scriptHash <| Witness.Plutus witness
                    , Cmd.none
                    , Nothing
                    )

        ( _, Err error ) ->
            justError error

        ( _, Ok outputRef ) ->
            -- We are using an output ref for the script, that we may have already loaded or not yet
            case scriptInfo.script of
                Script.Native _ ->
                    let
                        witness =
                            { script = Witness.ByReference outputRef
                            , expectedSigners = keepOnlyExpectedSigners form.expectedSigners
                            }

                        voter =
                            toVoter <| Witness.WithScript scriptInfo.scriptHash <| Witness.Native witness
                    in
                    if Dict.Any.member outputRef loadedRefUtxos then
                        ( Done { form | error = Nothing } voter
                        , Cmd.none
                        , Nothing
                        )

                    else
                        ( Validating form voter
                        , Cmd.none
                        , Api.defaultApiProvider.retrieveTx ctx.networkId outputRef.transactionId
                            |> Storage.cacheWrap
                                { db = ctx.db, storeName = "tx" }
                                Bytes.jsonDecoder
                                Bytes.jsonEncode
                                { key = Bytes.toHex outputRef.transactionId }
                            |> ConcurrentTask.onJsException (\{ message } -> ConcurrentTask.fail <| ConcurrentTask.Http.BadUrl <| "Uncaught JS exception: " ++ message)
                            |> ConcurrentTask.Extra.toResult
                            |> ConcurrentTask.map (GotRefUtxoTxBytes outputRef)
                            |> RunTask
                            |> Just
                        )

                Script.Plutus _ ->
                    Debug.todo "Handle Plutus script case"


keepOnlyExpectedSigners : Dict String { expected : Bool, key : Bytes CredentialHash } -> List (Bytes CredentialHash)
keepOnlyExpectedSigners signers =
    Dict.values signers
        |> List.filterMap
            (\{ expected, key } ->
                if expected then
                    Just key

                else
                    Nothing
            )


utxoRefFromStr : String -> Result String OutputReference
utxoRefFromStr str =
    case String.split "#" str of
        [ txIdHex, indexStr ] ->
            if String.length txIdHex == 64 then
                case ( Bytes.fromHex txIdHex, String.toInt indexStr ) of
                    ( Just txId, Just index ) ->
                        Ok { transactionId = txId, outputIndex = index }

                    ( Nothing, _ ) ->
                        Err <| "The Tx ID doesn’t look like valid hex: " ++ txIdHex

                    ( _, Nothing ) ->
                        Err <| "The output index doesn’t look like a valid integer: " ++ indexStr

            else
                Err "The Tx ID should be the hex of a 32 bytes identifier"

        _ ->
            Err "An output reference must have the shape: {txid}#0, for example: 10e7c91aca541c47c2a03debf6ebfc894ce553d0d0d3c01d053ebfca4e2893cb#0"



-- ###################################################################
-- VIEW
-- ###################################################################


{-| Configuration needed by the view:

  - Message wrapper for parent component
  - Wallet connectivity status
  - The current epoch
  - JSON-LD metadata context for rationale
  - Protocol parameters
  - The network ID
  - Link to transaction signing page

-}
type alias ViewContext msg =
    { wrapMsg : Msg -> msg
    , loadedWallet : Maybe LoadedWallet
    , drepId : Maybe (Bytes CredentialHash)
    , epoch : Maybe Int
    , costModels : Maybe CostModels
    , networkId : NetworkId
    , changeNetworkLink : NetworkId -> List (Html msg) -> Html msg
    , signingLink : Transaction -> List { keyName : String, keyHash : Bytes CredentialHash } -> List (Html msg) -> Html msg
    , voterPreconfig : List PreconfVoter
    }


view : ViewContext msg -> Model -> Html msg
view ctx (Model model) =
    div [ HA.style "max-width" "1536px", HA.style "margin" "0 auto" ]
        [ Helper.viewPageHeader
        , div
            [ HA.style "max-width" "840px"
            , HA.style "margin" "0 auto"
            , HA.style "padding" "0 1.5rem"
            , HA.style "position" "relative"
            ]
            [ -- Vertical timeline line with gradient
              div
                [ HA.style "position" "absolute"
                , HA.style "left" "2.5rem"
                , HA.style "top" "0"
                , HA.style "bottom" "0"
                , HA.style "width" "4px"
                , HA.style "background" "linear-gradient(180deg, #3b82f6, #10b981, #6366f1)"
                , HA.style "z-index" "1"
                ]
                []
            , Helper.viewStepWithCircle 1 "voter-step" (viewVoterIdentificationStep ctx model.voterStep)
            ]
        ]



--
-- Voter Identification Step
--


viewVoterIdentificationStep : ViewContext msg -> Step VoterPreparationForm Witness.Voter Witness.Voter -> Html msg
viewVoterIdentificationStep ctx step =
    case step of
        Preparing form ->
            let
                currentGovId =
                    Maybe.map Gov.idToBech32 form.govId |> Maybe.withDefault ""

                voterCard =
                    Helper.viewVoterCard VoterGovIdChange currentGovId

                voterPreconfig =
                    case ctx.drepId of
                        Nothing ->
                            ctx.voterPreconfig

                        Just id ->
                            { voterType = "DRep"
                            , description = "Vote using your CIP-95 wallet DRep ID"
                            , govId = Gov.idToBech32 <| Gov.DrepId <| VKeyHash id
                            }
                                :: ctx.voterPreconfig
            in
            Html.map ctx.wrapMsg <|
                div []
                    [ Helper.sectionTitle "Voter identification"
                    , Html.p [ HA.class "mb-4" ]
                        (if List.isEmpty voterPreconfig then
                            [ text "" ]

                         else
                            [ text "Select a predefined voter role or enter a custom governance ID" ]
                        )
                    , Helper.viewVoterGrid (List.map voterCard voterPreconfig)
                    , div [ HA.style "margin-bottom" "1.5rem" ]
                        [ viewCustomVoterCard form ]
                    , Html.Lazy.lazy viewValidGovIdForm form
                    , if form.govId /= Nothing then
                        Html.p [ HA.class "my-4" ] [ Helper.viewButton "Confirm Voter" ValidateVoterFormButtonClicked ]

                      else
                        text ""
                    , Helper.viewError form.error
                    ]

        Validating _ _ ->
            div []
                [ Helper.sectionTitle "Voter governance ID (drep/pool/cc_hot)"
                , Helper.boxContainer [ Html.p [] [ text "validating voter information ..." ] ]
                ]

        Done form voter ->
            Html.map ctx.wrapMsg <| viewIdentifiedVoter form voter


viewCustomVoterCard : VoterPreparationForm -> Html Msg
viewCustomVoterCard form =
    Helper.voterCustomCard
        { currentValue = Maybe.withDefault "" <| Maybe.map Gov.idToBech32 form.govId
        , onInputMsg = VoterGovIdChange
        }


viewValidGovIdForm : VoterPreparationForm -> Html Msg
viewValidGovIdForm form =
    case form.govId of
        Nothing ->
            text ""

        -- First the easy case: voting with a key
        Just (CcHotCredId (VKeyHash hash)) ->
            Helper.scriptInfoContainer
                [ Helper.viewVoterCredDetails "Voting as CC member with key:" (Bytes.toHex hash)
                , viewCcInfo form.ccInfo
                ]

        Just (DrepId (VKeyHash hash)) ->
            Helper.scriptInfoContainer
                [ Helper.viewVoterCredDetails "Voting as DRep with key:" (Bytes.toHex hash)
                , Helper.viewVoterDetailsItem "Voting power:" (Helper.votingPowerDisplay .votingPower form.drepInfo)
                ]

        Just (PoolId hash) ->
            Helper.scriptInfoContainer
                [ Helper.viewVoterCredDetails "Voting as SPO with pool ID:" (Bytes.toHex hash)
                , Helper.viewVoterDetailsItem "Live stake:" (Helper.votingPowerDisplay .stake form.poolInfo)
                ]

        -- Then the hard case: voting with a script
        Just (CcHotCredId (ScriptHash hash)) ->
            Helper.scriptInfoContainer
                [ Helper.viewVoterCredDetails "Voting as CC member with script:" (Bytes.toHex hash)
                , viewCcInfo form.ccInfo
                , viewScriptForm form
                ]

        Just (DrepId (ScriptHash hash)) ->
            Helper.scriptInfoContainer
                [ Helper.viewVoterCredDetails "Voting as DRep with script:" (Bytes.toHex hash)
                , Helper.viewVoterDetailsItem "Voting power:" (Helper.votingPowerDisplay .votingPower form.drepInfo)
                , viewScriptForm form
                ]

        Just govId ->
            Helper.scriptInfoContainer
                [ Html.p
                    [ HA.style "color" "#F59E0B"
                    , HA.style "font-style" "italic"
                    ]
                    [ text <| "Unexpected type of governance Id: " ++ Debug.toString govId ]
                ]


viewCcInfo : WebData CcInfo -> Html msg
viewCcInfo remoteCcInfo =
    case remoteCcInfo of
        RemoteData.Success { coldCred, hotCred, status, epochMandateEnd } ->
            Helper.viewCredInfo
                [ Helper.viewVoterInfoItem "Cold credential" (Gov.idToBech32 (CcColdCredId coldCred))
                , Helper.viewVoterInfoItem "Hot credential (used to vote)" (Gov.idToBech32 (CcHotCredId hotCred))
                , Helper.viewVoterInfoItem "Member status" status
                , Helper.viewVoterInfoItem "Mandate ending at epoch" (String.fromInt epochMandateEnd)
                ]

        RemoteData.NotAsked ->
            Html.p [] [ text "CC member info not querried" ]

        RemoteData.Loading ->
            Html.p [] [ text "CC member info loading ..." ]

        RemoteData.Failure error ->
            Html.p [] [ text <| "CC member info loading error: " ++ Debug.toString error ]


viewScriptForm : VoterPreparationForm -> Html Msg
viewScriptForm { scriptInfo, utxoRef, expectedSigners } =
    case scriptInfo of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            Html.p [] [ text "Loading info for script ..." ]

        RemoteData.Failure err ->
            Html.p [] [ text <| "Error while loading script info: " ++ Debug.toString err ]

        RemoteData.Success { scriptHash, script, nativeCborEncodingMatchesHash } ->
            let
                utxoRefForm =
                    Helper.viewUtxoRefForm utxoRef UtxoRefChange

                refScriptFeeSavings =
                    Transaction.estimateRefScriptFeeSavings script

                refScriptSuggestion =
                    refScriptSuggestionView refScriptFeeSavings utxoRefForm
            in
            case script of
                Script.Native _ ->
                    if nativeCborEncodingMatchesHash == Just True then
                        div []
                            [ Html.p [] [ text "Type of script: Native Script" ]
                            , refScriptSuggestion
                            , viewScriptSignersSection expectedSigners
                            ]

                    else
                        div []
                            [ Html.p [] [ text "Type of script: Native Script" ]
                            , Html.p [] [ text <| "IMPORTANT: for technical reasons, we need you to provide a reference UTxO containing your script of hash: " ++ Bytes.toHex scriptHash ]
                            , utxoRefForm
                            , viewScriptSignersSection expectedSigners
                            ]

                Script.Plutus plutusScript ->
                    div []
                        [ Html.p [] [ text <| "Plutus script version: " ++ Debug.toString (Script.plutusVersion plutusScript) ]
                        , Html.p [] [ text <| "Script size: " ++ (String.fromInt <| Bytes.width <| Script.cborWrappedBytes plutusScript) ++ " Bytes" ]
                        , refScriptSuggestion
                        , Html.p [] [ text "WIP: we are waiting for someone needing this to implement Plutus voters" ]
                        ]


refScriptSuggestionView : Int -> Html Msg -> Html Msg
refScriptSuggestionView refScriptFeeSavings utxoRefForm =
    if refScriptFeeSavings >= 5000 then
        div []
            [ Html.p [] [ text <| "By using a reference input for your script, you could save this much in Tx fees: " ++ Helper.prettyAdaLovelace (Natural.fromSafeInt refScriptFeeSavings) ]
            , utxoRefForm
            ]

    else if refScriptFeeSavings <= -5000 then
        Html.p [] [ text <| "Weirdly, using a reference input for your script would cost you more: " ++ Helper.prettyAdaLovelace (Natural.fromSafeInt -refScriptFeeSavings) ]

    else
        text ""


viewScriptSignersSection : Dict String { expected : Bool, key : Bytes CredentialHash } -> Html Msg
viewScriptSignersSection expectedSigners =
    let
        additionalBytesPerSignature =
            Transaction.encodeVKeyWitness
                { vkey = Bytes.dummy 32 "", signature = Bytes.dummy 64 "" }
                |> Cbor.Encode.encode
                |> Bytes.fromBytes
                |> Bytes.width
    in
    Helper.scriptSignerSection
        additionalBytesPerSignature
        Transaction.defaultTxFeeParams.feePerByte
        (List.map viewExpectedSignerCheckbox <| Dict.values expectedSigners)


viewExpectedSignerCheckbox : { expected : Bool, key : Bytes CredentialHash } -> Html Msg
viewExpectedSignerCheckbox { expected, key } =
    let
        keyHex =
            Bytes.toHex key
    in
    Helper.scriptSignerCheckbox keyHex expected (ToggleExpectedSigner keyHex)


viewIdentifiedVoter : VoterPreparationForm -> Witness.Voter -> Html Msg
viewIdentifiedVoter form voter =
    let
        govIdStr =
            Maybe.map Gov.idToBech32 form.govId
                |> Maybe.withDefault ""

        ( title, voterCred ) =
            getVoterDisplayInfo voter form govIdStr
    in
    Helper.viewIdentifiedVoterCard title
        [ case voterCred of
            Witness.WithKey cred ->
                div [ HA.style "display" "flex", HA.style "flex-direction" "column", HA.style "gap" "0.75rem" ]
                    [ Helper.viewVoterCredDetails "Using key with hash:" (Bytes.toHex cred) ]

            Witness.WithScript hash (Witness.Native { expectedSigners }) ->
                div [ HA.style "display" "flex", HA.style "flex-direction" "column", HA.style "gap" "0.75rem" ]
                    [ Helper.viewVoterCredDetails "Using native script with hash:" (Bytes.toHex hash)
                    , if List.isEmpty expectedSigners then
                        div
                            [ HA.style "color" "#718096"
                            , HA.style "font-style" "italic"
                            , HA.style "margin-top" "0.5rem"
                            ]
                            [ text "No expected signers." ]

                      else
                        div []
                            [ Html.p
                                [ HA.style "font-weight" "500"
                                , HA.style "color" "#4A5568"
                                , HA.style "margin-top" "0.75rem"
                                , HA.style "margin-bottom" "0.5rem"
                                ]
                                [ text "Expected signers:" ]
                            , div
                                [ HA.style "background-color" "#F9FAFB"
                                , HA.style "border" "1px solid #EDF2F7"
                                , HA.style "border-radius" "0.375rem"
                                , HA.style "padding" "0.75rem"
                                ]
                                [ Html.ul
                                    [ HA.style "list-style-type" "disc"
                                    , HA.style "padding-left" "1.25rem"
                                    , HA.style "display" "flex"
                                    , HA.style "flex-direction" "column"
                                    , HA.style "gap" "0.5rem"
                                    ]
                                    (List.map
                                        (\s ->
                                            Html.li []
                                                [ Html.span
                                                    [ HA.style "font-family" "monospace"
                                                    , HA.style "font-size" "0.875rem"
                                                    ]
                                                    [ text (Bytes.toHex s) ]
                                                ]
                                        )
                                        expectedSigners
                                    )
                                ]
                            ]
                    ]

            Witness.WithScript _ (Witness.Plutus _) ->
                div []
                    [ Html.span
                        [ HA.style "color" "#4A5568"
                        , HA.style "font-style" "italic"
                        ]
                        [ text "Using Plutus script (details not available)" ]
                    ]
        ]
        (Helper.viewButton "Change Voter" ChangeVoterButtonClicked)


getVoterDisplayInfo : Witness.Voter -> VoterPreparationForm -> String -> ( String, Witness.Credential )
getVoterDisplayInfo voter form govIdStr =
    case voter of
        Witness.WithCommitteeHotCred cred ->
            ( "Constitutional Committee Voter: " ++ govIdStr, cred )

        Witness.WithDrepCred cred ->
            let
                votingPowerStr =
                    case form.drepInfo of
                        RemoteData.Success { votingPower } ->
                            Helper.prettyAdaLovelace (Natural.fromSafeInt votingPower)

                        _ ->
                            "?"
            in
            ( "DRep Voter (voting power: " ++ votingPowerStr ++ "): " ++ govIdStr, cred )

        Witness.WithPoolCred hash ->
            let
                votingPower =
                    case form.poolInfo of
                        RemoteData.Success { stake } ->
                            Helper.prettyAdaLovelace (Natural.fromSafeInt stake)

                        _ ->
                            "?"
            in
            ( "SPO Voter (voting power: " ++ votingPower ++ "): " ++ Pool.toBech32 hash
            , Witness.WithKey hash
            )
