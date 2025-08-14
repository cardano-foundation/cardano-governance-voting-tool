module Page.Preparation exposing (InternalVote, JsonLdContexts, LoadedWallet, Model, Msg, MsgToParent(..), Rationale, Reference, ReferenceType(..), StorageConfig, TaskCompleted, UpdateContext, ViewContext, encodeStorageConfig, handleTaskCompleted, init, initStorageConfig, noInternalVote, pinPdfFile, pinRationaleFile, setLastStorageConfig, setLastVoter, storageConfigDecoder, update, view)

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

import Api exposing (ActiveProposal, CcInfo, DrepInfo, IpfsAnswer(..), PoolInfo)
import Blake2b exposing (blake2b256)
import Bytes as ElmBytes
import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address as Address exposing (Address, Credential(..), CredentialHash, NetworkId(..))
import Cardano.Cip30 as Cip30
import Cardano.Gov as Gov exposing (ActionId, Anchor, CostModels, Id(..), Vote)
import Cardano.Pool as Pool
import Cardano.Script as Script
import Cardano.Transaction as Transaction exposing (Transaction, VKeyWitness)
import Cardano.TxExamples exposing (prettyTx)
import Cardano.TxIntent exposing (TxFinalized)
import Cardano.Utxo as Utxo exposing (Output, OutputReference, TransactionId)
import Cardano.Witness as Witness
import Cbor.Encode
import ConcurrentTask exposing (ConcurrentTask)
import ConcurrentTask.Extra
import ConcurrentTask.Http
import Dict exposing (Dict)
import Dict.Any
import File exposing (File)
import File.Select
import Helper exposing (PreconfVoter)
import Html exposing (Html, div, text)
import Html.Attributes as HA
import Html.Events
import Html.Lazy
import Http
import Json.Decode as JD
import Json.Encode as JE
import List.Extra
import Markdown.Block
import Markdown.Parser as Md
import Natural
import Page.Cart as Cart exposing (VoteRecord)
import Platform.Cmd as Cmd
import ProposalMetadata exposing (AuthorWitness, ProposalMetadata)
import RemoteData exposing (RemoteData, WebData)
import ScriptInfo exposing (ScriptInfo)
import Set exposing (Set)
import Storage
import Task
import Url



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
    , pickProposalStep : Step {} {} ActiveProposal
    , storageConfigStep : Step StorageForm {} StorageConfig
    , rationaleCreationStep : Step RationaleForm Rationale Rationale
    , rationaleSignatureStep : Step RationaleSignatureForm {} RationaleSignature
    , permanentStorageStep : Step { error : Maybe String } {} Storage
    , buildTxStep : Step BuildTxPrep {} TxFinalized
    , signTxStep : Step { error : Maybe String } SigningTx SignedTx
    , visibleProposalCount : Int
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


init : { label : String, description : String } -> Model
init ipfsPreconfig =
    Model
        { someRefUtxos = Utxo.emptyRefDict
        , voterStep = Preparing initVoterForm
        , pickProposalStep = Preparing {}
        , storageConfigStep = Done (initStorageForm ipfsPreconfig) (UsePreconfigIpfs ipfsPreconfig)
        , rationaleCreationStep = Preparing initRationaleForm
        , rationaleSignatureStep = Preparing initRationaleSignatureForm
        , permanentStorageStep = Preparing { error = Nothing }
        , buildTxStep = Preparing { error = Nothing }
        , signTxStep = Preparing { error = Nothing }
        , visibleProposalCount = 10
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



-- Rationale Step


type alias RationaleForm =
    { summary : String
    , pdfAutogen : Bool
    , rationaleStatement : MarkdownForm
    , optionalFieldsAreVisible : Bool
    , precedentDiscussion : MarkdownForm
    , counterArgumentDiscussion : MarkdownForm
    , conclusion : MarkdownForm
    , internalVote : InternalVote
    , references : List Reference
    , error : Maybe String
    }


type alias MarkdownForm =
    String


type alias InternalVote =
    { constitutional : Int
    , unconstitutional : Int
    , abstain : Int
    , didNotVote : Int
    , against : Int
    }


noInternalVote : InternalVote
noInternalVote =
    { constitutional = 0
    , unconstitutional = 0
    , abstain = 0
    , didNotVote = 0
    , against = 0
    }


initRationaleForm : RationaleForm
initRationaleForm =
    { summary = ""
    , pdfAutogen = True
    , rationaleStatement = ""
    , optionalFieldsAreVisible = False
    , precedentDiscussion = ""
    , counterArgumentDiscussion = ""
    , conclusion = ""
    , internalVote = noInternalVote
    , references = []
    , error = Nothing
    }


type alias Rationale =
    { summary : String
    , rationaleStatement : String
    , precedentDiscussion : Maybe String
    , counterArgumentDiscussion : Maybe String
    , conclusion : Maybe String
    , internalVote : InternalVote
    , references : List Reference
    }


type alias Reference =
    { type_ : ReferenceType
    , label : String
    , uri : String
    }


type ReferenceType
    = OtherRefType
    | GovernanceMetadataRefType
    | RelevantArticlesRefType


allRefTypes : List ReferenceType
allRefTypes =
    [ RelevantArticlesRefType
    , GovernanceMetadataRefType
    , OtherRefType
    ]


refTypeToString : ReferenceType -> String
refTypeToString refType =
    case refType of
        RelevantArticlesRefType ->
            "relevant articles"

        GovernanceMetadataRefType ->
            "governance metadata"

        OtherRefType ->
            "other"


refTypeFromString : String -> ReferenceType
refTypeFromString str =
    List.filter (\refType -> refTypeToString refType == str) allRefTypes
        |> List.head
        |> Maybe.withDefault OtherRefType


initRefForm : Reference
initRefForm =
    { type_ = RelevantArticlesRefType
    , label = ""
    , uri = ""
    }



-- Rationale Signature Step


type alias RationaleSignatureForm =
    { authors : List AuthorWitness
    , rationale : Rationale
    , error : Maybe String
    }


initRationaleSignatureForm : RationaleSignatureForm
initRationaleSignatureForm =
    { authors = []
    , rationale = rationaleFromForm initRationaleForm
    , error = Nothing
    }


type alias RationaleSignature =
    { authors : List AuthorWitness
    , rationale : Rationale
    , signedJson : String
    , error : Maybe String
    }


initAuthorForm : AuthorWitness
initAuthorForm =
    { name = "John Doe"
    , witnessAlgorithm = "ed25519"
    , publicKey = ""
    , signature = Nothing
    }



-- Storage Step


type StorageMethod
    = PreconfigIPFS { label : String, description : String }
    | NmkrIPFS
    | BlockfrostIPFS
    | CustomIPFS


type alias StorageForm =
    { storageMethod : StorageMethod
    , nmkrUserId : String
    , nmkrApiToken : String
    , blockfrostProjectId : String
    , ipfsServer : String
    , headers : List ( String, String )
    , error : Maybe String
    }


initStorageForm : { label : String, description : String } -> StorageForm
initStorageForm ipfsPreconfig =
    { storageMethod = PreconfigIPFS ipfsPreconfig
    , nmkrUserId = ""
    , nmkrApiToken = ""
    , blockfrostProjectId = ""
    , ipfsServer = "https://ipfs-rpc.mycompany.org/api/v0"
    , headers = [ ( "Authorization", "Basic {token}" ) ]
    , error = Nothing
    }


type StorageConfig
    = UsePreconfigIpfs { label : String, description : String }
    | UseBlockfrostIpfs { label : String, description : String, projectId : String }
    | UseNmkrIpfs { label : String, description : String, userId : String, apiToken : String }
    | UseCustomIpfs { label : String, description : String, ipfsServer : String, headers : List ( String, String ) }


{-| Initialize the default storage config.
-}
initStorageConfig : { label : String, description : String } -> StorageConfig
initStorageConfig { label, description } =
    UsePreconfigIpfs { label = label, description = description }


encodeStorageConfig : StorageConfig -> JE.Value
encodeStorageConfig config =
    case config of
        UsePreconfigIpfs { label, description } ->
            JE.object
                [ ( "storageType", JE.string "UsePreconfigIpfs" )
                , ( "label", JE.string label )
                , ( "description", JE.string description )
                ]

        UseBlockfrostIpfs { label, description, projectId } ->
            JE.object
                [ ( "storageType", JE.string "UseBlockfrostIpfs" )
                , ( "label", JE.string label )
                , ( "description", JE.string description )
                , ( "projectId", JE.string projectId )
                ]

        UseNmkrIpfs { label, description, userId, apiToken } ->
            JE.object
                [ ( "storageType", JE.string "UseNmkrIpfs" )
                , ( "label", JE.string label )
                , ( "description", JE.string description )
                , ( "userId", JE.string userId )
                , ( "apiToken", JE.string apiToken )
                ]

        UseCustomIpfs { label, description, ipfsServer, headers } ->
            JE.object
                [ ( "storageType", JE.string "UseCustomIpfs" )
                , ( "label", JE.string label )
                , ( "description", JE.string description )
                , ( "ipfsServer", JE.string ipfsServer )
                , ( "headers", JE.list encodeHttpHeader headers )
                ]


encodeHttpHeader : ( String, String ) -> JE.Value
encodeHttpHeader ( name, value ) =
    JE.object [ ( "name", JE.string name ), ( "value", JE.string value ) ]


httpHeaderDecoder : JD.Decoder ( String, String )
httpHeaderDecoder =
    JD.map2 Tuple.pair
        (JD.field "name" JD.string)
        (JD.field "value" JD.string)


storageConfigDecoder : JD.Decoder StorageConfig
storageConfigDecoder =
    JD.field "storageType" JD.string
        |> JD.andThen
            (\storageType ->
                case storageType of
                    "UsePreconfigIpfs" ->
                        JD.map2 (\label description -> UsePreconfigIpfs { label = label, description = description })
                            (JD.field "label" JD.string)
                            (JD.field "description" JD.string)

                    "UseBlockfrostIpfs" ->
                        JD.map3 (\label description projectId -> UseBlockfrostIpfs { label = label, description = description, projectId = projectId })
                            (JD.field "label" JD.string)
                            (JD.field "description" JD.string)
                            (JD.field "projectId" JD.string)

                    "UseNmkrIpfs" ->
                        JD.map4 (\label description userId apiToken -> UseNmkrIpfs { label = label, description = description, userId = userId, apiToken = apiToken })
                            (JD.field "label" JD.string)
                            (JD.field "description" JD.string)
                            (JD.field "userId" JD.string)
                            (JD.field "apiToken" JD.string)

                    "UseCustomIpfs" ->
                        JD.map4 (\label description ipfsServer headers -> UseCustomIpfs { label = label, description = description, ipfsServer = ipfsServer, headers = headers })
                            (JD.field "label" JD.string)
                            (JD.field "description" JD.string)
                            (JD.field "ipfsServer" JD.string)
                            (JD.field "headers" (JD.list httpHeaderDecoder))

                    _ ->
                        JD.fail "Unknown storage type"
            )


type alias Storage =
    { jsonFile : IpfsFile
    }


type alias IpfsFile =
    { name : String
    , cid : String
    , size : String
    }



-- Build Tx Step


type alias BuildTxPrep =
    { error : Maybe String
    }



-- Sign Tx Step


type alias SigningTx =
    { tx : Transaction
    , expectedSignatures : List (Bytes CredentialHash)
    , vkeyWitnesses : List VKeyWitness
    }


type alias SignedTx =
    { signedTx : Transaction
    , txId : Bytes TransactionId
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
    | CacheVoterGovId Gov.Id
    | CacheStorageConfig StorageConfig
    | AddVoteToCart Witness.Voter Cart.VoteRecord
    | RunTask (ConcurrentTask String TaskCompleted)
    | BatchToParent MsgToParent MsgToParent


{-| Results from asynchronous tasks:

  - Loading reference transaction bytes
  - Loading script info

-}
type TaskCompleted
    = GotRefUtxoTxBytes OutputReference (Result ConcurrentTask.Http.Error (Bytes Transaction))
    | GotScriptInfoTask (Result ConcurrentTask.Http.Error ScriptInfo)


type Msg
    = NoMsg
      -- Voter Step
    | VoterGovIdChange String
    | GotDrepInfo (Result Http.Error DrepInfo)
    | GotCcInfo (Result Http.Error CcInfo)
    | GotPoolInfo (Result Http.Error PoolInfo)
    | UtxoRefChange String
    | ToggleExpectedSigner String Bool
    | ValidateVoterFormButtonClicked
    | ChangeVoterButtonClicked
      -- Pick Proposal Step
    | PickProposalButtonClicked String
    | ChangeProposalButtonClicked
    | ShowMoreProposals Int
      -- Storage Config Step
    | StorageMethodSelected StorageMethod
    | BlockfrostProjectIdChange String
    | NmkrUserIdChange String
    | NmkrApiTokenChange String
    | IpfsServerChange String
    | AddHeaderButtonClicked
    | DeleteHeaderButtonClicked Int
    | StorageHeaderFieldChange Int String
    | StorageHeaderValueChange Int String
    | ValidateStorageConfigButtonClicked
      -- Rationale
    | RationaleSummaryChange String
    | TogglePdfAutogen Bool
    | RationaleStatementChange String
    | ToggleOptionalFieldsVisibility Bool
    | PrecedentDiscussionChange String
    | CounterArgumentChange String
    | ConclusionChange String
    | InternalConstitutionalVoteChange String
    | InternalUnconstitutionalVoteChange String
    | InternalAbstainVoteChange String
    | InternalDidNotVoteChange String
    | InternalAgainstVoteChange String
    | AddRefButtonClicked
    | DeleteRefButtonClicked Int
    | ReferenceLabelChange Int String
    | ReferenceUriChange Int String
    | ReferenceTypeChange Int String
    | ValidateRationaleButtonClicked
    | GotUnsignedPdfFile (Result Http.Error ElmBytes.Bytes)
    | EditRationaleButtonClicked
      -- Rationale Signature
    | AddAuthorButtonClicked
    | DeleteAuthorButtonClicked Int
    | AuthorNameChange Int String
    | LoadJsonSignatureButtonClicked Int String
    | FileSelectedForJsonSignature Int String File
    | LoadedAuthorSignatureJsonRationale Int String String
    | SkipRationaleSignaturesButtonClicked
    | ValidateRationaleSignaturesButtonClicked
    | ChangeAuthorsButtonClicked
      -- Rationale Storage
    | PinJsonIpfsButtonClicked
    | GotIpfsAnswer (Result String IpfsAnswer)
    | AddOtherStorageButtonCLicked
      -- Build Tx Step
    | AddVoteToCartButtonClicked Vote
    | ChangeVoteButtonClicked


{-| Configuration required by the update function.
Provides access to:

  - External data (proposals, script info, etc.)
  - Cardano network cost models (for script execution)
  - Wallet integration
  - In-browser storage connection
  - Governance metadata JSON LD format

-}
type alias UpdateContext msg =
    { wrapMsg : Msg -> msg
    , db : JD.Value
    , proposals : WebData (Dict String ActiveProposal)
    , scriptsInfo : Dict String ScriptInfo
    , drepsInfo : Dict String DrepInfo
    , ccsInfo : Dict String CcInfo
    , poolsInfo : Dict String PoolInfo
    , loadedWallet : Maybe LoadedWallet
    , drepId : Maybe (Bytes CredentialHash)
    , jsonLdContexts : JsonLdContexts
    , jsonRationaleToFile : { fileContent : String, fileName : String } -> Cmd msg
    , pdfBytesToFile : { fileContentHex : String, fileName : String } -> Cmd msg
    , costModels : Maybe CostModels
    , networkId : NetworkId
    }


type alias JsonLdContexts =
    { ccCip136Context : JE.Value }


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
        ShowMoreProposals currentCount ->
            ( { model | visibleProposalCount = currentCount + 10 }
            , Cmd.none
            , Nothing
            )

        NoMsg ->
            ( model, Cmd.none, Nothing )

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
                        ( newVoterStep, toParent ) =
                            confirmVoter ctx form model.someRefUtxos
                                |> saveValidVoter
                    in
                    ( { model | voterStep = newVoterStep }
                    , Cmd.none
                    , toParent
                    )

                _ ->
                    ( model, Cmd.none, Nothing )

        VoterGovIdChange govIdStr ->
            case ( govIdStr, checkGovId ctx govIdStr ) of
                ( "", _ ) ->
                    ( updateVoterForm (\_ -> initVoterForm) model
                    , Cmd.none
                    , Nothing
                    )

                ( _, Err error ) ->
                    ( updateVoterForm (\_ -> { initVoterForm | error = Just error }) model
                    , Cmd.none
                    , Nothing
                    )

                ( _, Ok { govId, scriptInfo, expectedSigners, drepInfo, ccInfo, poolInfo, cmd, msgToParent } ) ->
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
                      -- TODO: also reset all dependents steps
                      -- |> resetProposal
                      -- |> resetRationaleCreation
                      -- |> resetRationaleSignature
                      -- |> resetStorage
                      -- |> resetTxBuilding
                      -- |> resetTxSigning
                    , Cmd.none
                    , Nothing
                    )

                _ ->
                    ( model, Cmd.none, Nothing )

        --
        -- Pick Proposal Step
        --
        PickProposalButtonClicked actionId ->
            case ( model.pickProposalStep, ctx.proposals ) of
                ( Preparing form, RemoteData.Success proposalsDict ) ->
                    case Dict.get actionId proposalsDict of
                        Just prop ->
                            ( { model | pickProposalStep = Done form prop }
                            , Cmd.none
                            , Nothing
                            )

                        Nothing ->
                            ( model, Cmd.none, Nothing )

                _ ->
                    ( model, Cmd.none, Nothing )

        ChangeProposalButtonClicked ->
            ( { model | pickProposalStep = Preparing {} }
            , Cmd.none
            , Nothing
            )

        --
        -- Storage Configuration Step
        --
        StorageMethodSelected method ->
            ( updateStorageConfigForm (\form -> { form | storageMethod = method }) model
            , Cmd.none
            , Nothing
            )

        BlockfrostProjectIdChange projectId ->
            ( updateStorageConfigForm (\form -> { form | blockfrostProjectId = projectId }) model
            , Cmd.none
            , Nothing
            )

        NmkrUserIdChange userId ->
            ( updateStorageConfigForm (\form -> { form | nmkrUserId = userId }) model
            , Cmd.none
            , Nothing
            )

        NmkrApiTokenChange token ->
            ( updateStorageConfigForm (\form -> { form | nmkrApiToken = token }) model
            , Cmd.none
            , Nothing
            )

        IpfsServerChange ipfsServer ->
            ( updateStorageConfigForm (\form -> { form | ipfsServer = ipfsServer }) model
            , Cmd.none
            , Nothing
            )

        AddHeaderButtonClicked ->
            ( updateStorageConfigForm (\form -> { form | headers = form.headers ++ [ ( "", "" ) ] }) model
            , Cmd.none
            , Nothing
            )

        DeleteHeaderButtonClicked n ->
            ( updateStorageConfigForm (\form -> { form | headers = List.Extra.removeAt n form.headers }) model
            , Cmd.none
            , Nothing
            )

        StorageHeaderFieldChange n field ->
            ( updateStorageConfigForm (\form -> { form | headers = List.Extra.updateAt n (\( _, v ) -> ( field, v )) form.headers }) model
            , Cmd.none
            , Nothing
            )

        StorageHeaderValueChange n value ->
            ( updateStorageConfigForm (\form -> { form | headers = List.Extra.updateAt n (\( f, _ ) -> ( f, value )) form.headers }) model
            , Cmd.none
            , Nothing
            )

        ValidateStorageConfigButtonClicked ->
            case model.storageConfigStep of
                Preparing form ->
                    case validateIpfsForm form of
                        Ok storageConfig ->
                            -- TODO: test some endpoint to check custom config validity,
                            -- and return a "Validating" step instead of a "Done" step.
                            ( { model | storageConfigStep = Done { form | error = Nothing } storageConfig }
                            , Cmd.none
                            , Just <| CacheStorageConfig storageConfig
                            )

                        Err error ->
                            ( { model | storageConfigStep = Preparing { form | error = Just error } }
                            , Cmd.none
                            , Nothing
                            )

                -- When the user clicks on "Change storage configuration"
                -- TODO: Should be another msg
                Done prep _ ->
                    ( { model | storageConfigStep = Preparing prep }
                    , Cmd.none
                    , Nothing
                    )

                _ ->
                    ( model, Cmd.none, Nothing )

        --
        -- Rationale Step
        --
        RationaleSummaryChange summary ->
            ( updateRationaleForm (\form -> { form | summary = summary }) model
            , Cmd.none
            , Nothing
            )

        TogglePdfAutogen pdfAutogen ->
            ( updateRationaleForm (\form -> { form | pdfAutogen = pdfAutogen }) model
            , Cmd.none
            , Nothing
            )

        RationaleStatementChange statement ->
            ( updateRationaleForm (\form -> { form | rationaleStatement = statement }) model
            , Cmd.none
            , Nothing
            )

        ToggleOptionalFieldsVisibility checked ->
            ( updateRationaleForm (\form -> { form | optionalFieldsAreVisible = checked }) model
            , Cmd.none
            , Nothing
            )

        PrecedentDiscussionChange precedentDiscussion ->
            ( updateRationaleForm (\form -> { form | precedentDiscussion = precedentDiscussion }) model
            , Cmd.none
            , Nothing
            )

        CounterArgumentChange argument ->
            ( updateRationaleForm (\form -> { form | counterArgumentDiscussion = argument }) model
            , Cmd.none
            , Nothing
            )

        ConclusionChange conclusion ->
            ( updateRationaleForm (\form -> { form | conclusion = conclusion }) model
            , Cmd.none
            , Nothing
            )

        InternalConstitutionalVoteChange constitutionalStr ->
            ( updateRationaleInternalVoteForm (\n internal -> { internal | constitutional = n }) constitutionalStr model
            , Cmd.none
            , Nothing
            )

        InternalUnconstitutionalVoteChange unconstitutionalStr ->
            ( updateRationaleInternalVoteForm (\n internal -> { internal | unconstitutional = n }) unconstitutionalStr model
            , Cmd.none
            , Nothing
            )

        InternalAbstainVoteChange abstainStr ->
            ( updateRationaleInternalVoteForm (\n internal -> { internal | abstain = n }) abstainStr model
            , Cmd.none
            , Nothing
            )

        InternalDidNotVoteChange didNotVoteStr ->
            ( updateRationaleInternalVoteForm (\n internal -> { internal | didNotVote = n }) didNotVoteStr model
            , Cmd.none
            , Nothing
            )

        InternalAgainstVoteChange againstVoteStr ->
            ( updateRationaleInternalVoteForm (\n internal -> { internal | against = n }) againstVoteStr model
            , Cmd.none
            , Nothing
            )

        AddRefButtonClicked ->
            ( updateRationaleForm (\form -> { form | references = form.references ++ [ initRefForm ] }) model
            , Cmd.none
            , Nothing
            )

        DeleteRefButtonClicked n ->
            ( updateRationaleForm (\form -> { form | references = List.Extra.removeAt n form.references }) model
            , Cmd.none
            , Nothing
            )

        ReferenceLabelChange n label ->
            ( updateRationaleForm (\form -> { form | references = List.Extra.updateAt n (\ref -> { ref | label = label }) form.references }) model
            , Cmd.none
            , Nothing
            )

        ReferenceUriChange n uri ->
            ( updateRationaleForm (\form -> { form | references = List.Extra.updateAt n (\ref -> { ref | uri = uri }) form.references }) model
            , Cmd.none
            , Nothing
            )

        ReferenceTypeChange n refTypeStr ->
            ( updateRationaleForm (\form -> { form | references = List.Extra.updateAt n (\ref -> { ref | type_ = refTypeFromString refTypeStr }) form.references }) model
            , Cmd.none
            , Nothing
            )

        ValidateRationaleButtonClicked ->
            case ( validateRationaleForm model.rationaleCreationStep, model.pickProposalStep ) of
                -- If validation fails, it will return back to the form editing step with an error message
                ( Preparing formWithError, _ ) ->
                    ( { model | rationaleCreationStep = Preparing formWithError }, Cmd.none, Nothing )

                -- If validation partially succeeds but needs more processing,
                -- it will return in the Preparing step and we now need to store the PDF on IPFS,
                -- and then edit the rationale to include the PDF link
                ( Validating form rationale, Done _ { id } ) ->
                    let
                        tempUnsignedRationaleForm =
                            rationaleSignatureFromForm ctx.jsonLdContexts id { authors = [], error = Nothing, rationale = rationale }

                        rawFileContent =
                            tempUnsignedRationaleForm.signedJson
                    in
                    ( { model | rationaleCreationStep = Validating form rationale }
                      -- Save rationale PDF to IPFS and update rationale with link
                    , Api.defaultApiProvider.convertToPdf rawFileContent GotUnsignedPdfFile
                        |> Cmd.map ctx.wrapMsg
                    , Nothing
                    )

                -- If validation fully succeeds, it will proceed to the Done step
                ( Done prep newRationale, Done _ { id } ) ->
                    let
                        -- Initialize with no rationale signature
                        form =
                            { authors = []
                            , rationale = newRationale
                            , error = Nothing
                            }

                        updatedModel =
                            { model
                                | rationaleCreationStep = Done prep newRationale
                                , rationaleSignatureStep =
                                    Done form (rationaleSignatureFromForm ctx.jsonLdContexts id form)
                            }
                    in
                    ( updatedModel, Cmd.none, Nothing )

                _ ->
                    ( model, Cmd.none, Nothing )

        -- Handling PDF auto-gen for the rationale
        GotUnsignedPdfFile result ->
            case ( model.rationaleCreationStep, result ) of
                ( Validating _ _, Ok pdfBytes ) ->
                    ( model
                    , ctx.pdfBytesToFile
                        { fileContentHex = Bytes.fromBytes pdfBytes |> Bytes.toHex
                        , fileName = "unsigned-rationale.pdf"
                        }
                    , Nothing
                    )

                ( Validating form _, Err error ) ->
                    ( { model | rationaleCreationStep = Preparing { form | error = Just <| "An error occurred while converting the rationale to PDF: " ++ Debug.toString error } }
                    , Cmd.none
                    , Nothing
                    )

                _ ->
                    -- Ignore if we are not validating the rationale
                    ( model, Cmd.none, Nothing )

        EditRationaleButtonClicked ->
            ( { model | rationaleCreationStep = editRationale model.rationaleCreationStep }
            , Cmd.none
            , Nothing
            )

        --
        -- Rationale Signature Step
        --
        AddAuthorButtonClicked ->
            ( updateAuthorsForm (\authors -> initAuthorForm :: authors) model
            , Cmd.none
            , Nothing
            )

        DeleteAuthorButtonClicked n ->
            ( updateAuthorsForm (\authors -> List.Extra.removeAt n authors) model
            , Cmd.none
            , Nothing
            )

        AuthorNameChange n name ->
            ( updateAuthorsForm (\authors -> List.Extra.updateAt n (\author -> { author | name = name }) authors) model
            , Cmd.none
            , Nothing
            )

        LoadJsonSignatureButtonClicked n authorName ->
            ( model
            , Cmd.map ctx.wrapMsg <|
                File.Select.file [ "application/json" ] <|
                    FileSelectedForJsonSignature n authorName
            , Nothing
            )

        FileSelectedForJsonSignature n authorName file ->
            ( model
            , Task.attempt (handleJsonSignatureFileRead n authorName) (File.toString file)
                |> Cmd.map ctx.wrapMsg
            , Nothing
            )

        LoadedAuthorSignatureJsonRationale n authorName jsonStr ->
            ( case JD.decodeString (authorWitnessExtractDecoder authorName) jsonStr of
                Err decodingError ->
                    { model
                        | rationaleSignatureStep =
                            signatureDecodingError decodingError model.rationaleSignatureStep
                    }

                Ok authorWitness ->
                    updateAuthorsForm (\authors -> List.Extra.updateAt n (\_ -> authorWitness) authors) model
            , Cmd.none
            , Nothing
            )

        SkipRationaleSignaturesButtonClicked ->
            ( { model | rationaleSignatureStep = skipRationaleSignature ctx.jsonLdContexts model.pickProposalStep model.rationaleSignatureStep }
            , Cmd.none
            , Nothing
            )

        ValidateRationaleSignaturesButtonClicked ->
            validateRationaleSignature ctx.jsonLdContexts model

        ChangeAuthorsButtonClicked ->
            ( { model | rationaleSignatureStep = Preparing <| rationaleSignatureToForm model.rationaleSignatureStep }
            , Cmd.none
            , Nothing
            )

        --
        -- Rationale Storage Step
        --
        PinJsonIpfsButtonClicked ->
            case ( model.storageConfigStep, model.permanentStorageStep ) of
                ( Done _ _, Preparing _ ) ->
                    sendPinRequest ctx model

                _ ->
                    ( model, Cmd.none, Nothing )

        GotIpfsAnswer (Err httpError) ->
            case ( model.rationaleCreationStep, model.permanentStorageStep ) of
                -- If we are validating the rationale form, it means the IPFS answer
                -- is most likely the PDF we got back for PDF auto-gen.
                ( Validating form _, _ ) ->
                    ( { model | rationaleCreationStep = Preparing { form | error = Just <| Debug.toString httpError } }
                    , Cmd.none
                    , Nothing
                    )

                -- Otherwise, if we are validating the permanent storage step, it means the IPFS answer
                -- is most likely for the signed JSON rationale.
                ( _, Validating form _ ) ->
                    ( { model | permanentStorageStep = Preparing { form | error = Just <| Debug.toString httpError } }
                    , Cmd.none
                    , Nothing
                    )

                -- Any other case is not supposed to happen.
                _ ->
                    ( model, Cmd.none, Nothing )

        GotIpfsAnswer (Ok ipfsAnswer) ->
            case ( model.rationaleCreationStep, model.permanentStorageStep ) of
                -- If we are validating the rationale form, it means the IPFS answer
                -- is most likely the PDF we got back for PDF auto-gen.
                ( Validating form rationale, _ ) ->
                    handlePdfIpfsAnswer ctx model form rationale ipfsAnswer

                -- Otherwise, if we are validating the permanent storage step, it means the IPFS answer
                -- is most likely for the signed JSON rationale.
                ( _, Validating _ _ ) ->
                    handleRationaleIpfsAnswer model ipfsAnswer

                _ ->
                    ( model, Cmd.none, Nothing )

        AddOtherStorageButtonCLicked ->
            case model.permanentStorageStep of
                Preparing _ ->
                    ( model, Cmd.none, Nothing )

                Validating _ _ ->
                    ( model, Cmd.none, Nothing )

                Done prep _ ->
                    ( { model | permanentStorageStep = Preparing prep }
                    , Cmd.none
                    , Nothing
                    )

        --
        -- Add Vote to Cart
        --
        AddVoteToCartButtonClicked vote ->
            case allPrepSteps ctx model of
                Err error ->
                    ( { model | buildTxStep = Preparing { error = Just error } }
                    , Cmd.none
                    , Nothing
                    )

                Ok { voter, actionId, proposalTitle, rationaleAnchor } ->
                    let
                        voteIntent =
                            { actionId = actionId
                            , vote = vote
                            , rationale = Just rationaleAnchor
                            }
                    in
                    -- TODO: update the model to mark proposals already in the cart
                    ( model
                    , Cmd.none
                    , Just <| AddVoteToCart voter <| Cart.VoteRecord proposalTitle voteIntent
                    )

        ChangeVoteButtonClicked ->
            ( { model | buildTxStep = Preparing { error = Nothing } }
            , Cmd.none
            , Nothing
            )


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


setLastVoter : Maybe Gov.Id -> Msg
setLastVoter maybeGovId =
    Maybe.map Gov.idToBech32 maybeGovId
        |> Maybe.withDefault ""
        |> VoterGovIdChange


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


saveValidVoter : ( Step VoterPreparationForm Witness.Voter Witness.Voter, Maybe MsgToParent ) -> ( Step VoterPreparationForm Witness.Voter Witness.Voter, Maybe MsgToParent )
saveValidVoter ( step, msgToParent ) =
    case step of
        Done form _ ->
            case ( form.govId, msgToParent ) of
                ( Just govId, Nothing ) ->
                    ( step, Just <| CacheVoterGovId govId )

                ( Just govId, Just msg ) ->
                    ( step, Just <| BatchToParent msg <| CacheVoterGovId govId )

                ( Nothing, _ ) ->
                    ( step, msgToParent )

        _ ->
            ( step, msgToParent )


confirmVoter : UpdateContext msg -> VoterPreparationForm -> Utxo.RefDict Output -> ( Step VoterPreparationForm Witness.Voter Witness.Voter, Maybe MsgToParent )
confirmVoter ctx form loadedRefUtxos =
    let
        justError errorMsg =
            ( Preparing { form | error = Just errorMsg }
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
            , Nothing
            )

        Just (DrepId (VKeyHash keyHash)) ->
            ( Done form <| Witness.WithDrepCred (Witness.WithKey keyHash)
            , Nothing
            )

        Just (CcHotCredId (VKeyHash keyHash)) ->
            ( Done form <| Witness.WithCommitteeHotCred (Witness.WithKey keyHash)
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


validateScriptVoter : UpdateContext msg -> VoterPreparationForm -> Utxo.RefDict Output -> (Witness.Credential -> Witness.Voter) -> ScriptInfo -> ( Step VoterPreparationForm Witness.Voter Witness.Voter, Maybe MsgToParent )
validateScriptVoter ctx form loadedRefUtxos toVoter scriptInfo =
    let
        justError errorMsg =
            ( Preparing { form | error = Just errorMsg }
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
                        , Nothing
                        )

                    else
                        ( Validating form voter
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
                    justError "This Plutus script cannot be supported automatically. Please open an issue on GitHub describing how your Plutus script works."


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



-- Storage Configuration Step


setLastStorageConfig : StorageConfig -> Model -> ( Model, Msg )
setLastStorageConfig storageConfig (Model innerModel) =
    let
        form =
            case storageConfig of
                UsePreconfigIpfs labelAndDescription ->
                    initStorageForm labelAndDescription

                UseBlockfrostIpfs { label, description, projectId } ->
                    let
                        empty =
                            initStorageForm { label = label, description = description }
                    in
                    { empty | blockfrostProjectId = projectId }

                UseNmkrIpfs { label, description, userId, apiToken } ->
                    let
                        empty =
                            initStorageForm { label = label, description = description }
                    in
                    { empty | nmkrUserId = userId, nmkrApiToken = apiToken }

                UseCustomIpfs { label, description, ipfsServer, headers } ->
                    let
                        empty =
                            initStorageForm { label = label, description = description }
                    in
                    { empty | ipfsServer = ipfsServer, headers = headers }
    in
    ( Model { innerModel | storageConfigStep = Preparing form }
    , ValidateStorageConfigButtonClicked
    )


updateStorageConfigForm : (StorageForm -> StorageForm) -> InnerModel -> InnerModel
updateStorageConfigForm formUpdate model =
    case model.storageConfigStep of
        Preparing form ->
            { model | storageConfigStep = Preparing <| formUpdate form }

        Validating _ _ ->
            model

        Done _ _ ->
            model


validateIpfsForm : StorageForm -> Result String StorageConfig
validateIpfsForm form =
    case form.storageMethod of
        -- For standard IPFS, no validation needed
        PreconfigIPFS { label, description } ->
            Ok (UsePreconfigIpfs { label = label, description = description })

        BlockfrostIPFS ->
            case String.trim form.blockfrostProjectId of
                "" ->
                    Err "Missing blockfrost project id"

                projectId ->
                    Ok <|
                        UseBlockfrostIpfs
                            { label = "Own Blockfrost IPFS"
                            , description = "Using Blockfrost IPFS server to store your files."
                            , projectId = projectId
                            }

        NmkrIPFS ->
            case String.trim form.nmkrUserId of
                "" ->
                    Err "Missing nmkr user id"

                userId ->
                    Ok <|
                        UseNmkrIpfs
                            { label = "Own NMKR IPFS"
                            , description = "Using NMKR IPFS server to store your files. Remark that using NMKR own gateway will be faster to access pinned files: https://c-ipfs-gw.nmkr.io/ipfs/{file-hash-here}"
                            , userId = userId
                            , apiToken = form.nmkrApiToken
                            }

        CustomIPFS ->
            let
                -- Check that the IPFS server url looks legit
                ipfsServerUrlSeemsLegit =
                    case Url.fromString form.ipfsServer of
                        Just _ ->
                            Ok ()

                        Nothing ->
                            Err ("This url seems incorrect, it must look like this: https://subdomain.domain.org, instead I got this: " ++ form.ipfsServer)

                -- Check that headers look valid
                nonEmptyHeadersResult headers =
                    if List.any (\( f, _ ) -> String.isEmpty f) headers then
                        Err "Empty header fields are forbidden."

                    else
                        Ok ()
            in
            ipfsServerUrlSeemsLegit
                |> Result.andThen (\_ -> nonEmptyHeadersResult form.headers)
                |> Result.map
                    (\_ ->
                        UseCustomIpfs
                            { label = "Custom IPFS Server"
                            , description = "Using a custom IPFS server configuration to store your files. The RPC should provide the /add?pin=true endpoint with answers equivalent to those described in the official kubo IPFS RPC docs: https://docs.ipfs.tech/reference/kubo/rpc/#api-v0-add"
                            , ipfsServer = form.ipfsServer
                            , headers = form.headers
                            }
                    )



-- Rationale Step


updateRationaleForm : (RationaleForm -> RationaleForm) -> InnerModel -> InnerModel
updateRationaleForm f ({ rationaleCreationStep } as model) =
    case rationaleCreationStep of
        Preparing form ->
            { model | rationaleCreationStep = Preparing (f form) }

        _ ->
            model


updateRationaleInternalVoteForm : (Int -> InternalVote -> InternalVote) -> String -> InnerModel -> InnerModel
updateRationaleInternalVoteForm updateF numberStr model =
    let
        rationaleUpdate : RationaleForm -> RationaleForm
        rationaleUpdate form =
            String.toInt numberStr
                |> Maybe.map (\n -> { form | internalVote = updateF n form.internalVote })
                |> Maybe.withDefault form
    in
    updateRationaleForm rationaleUpdate model


validateMarkdownHeadings : List Markdown.Block.Block -> Result String ()
validateMarkdownHeadings blocks =
    if List.any isH1Block blocks then
        Err "Please use heading level 2 (##) or higher. Level 1 headings (#) are reserved for the level 1 sections, such as summary, rationale statement, discussion, etc."

    else
        Ok ()


isH1Block : Markdown.Block.Block -> Bool
isH1Block block =
    case block of
        Markdown.Block.Heading Markdown.Block.H1 _ ->
            True

        _ ->
            False


validateRationaleForm : Step RationaleForm Rationale Rationale -> Step RationaleForm Rationale Rationale
validateRationaleForm step =
    case step of
        Preparing form ->
            let
                rationaleValidation =
                    validateRationaleSummary form.summary
                        |> Result.andThen (\_ -> validateRationaleStatement form.rationaleStatement)
                        |> Result.andThen (\_ -> validateRationaleDiscussion form.precedentDiscussion)
                        |> Result.andThen (\_ -> validateRationaleCounterArg form.counterArgumentDiscussion)
                        |> Result.andThen (\_ -> validateRationaleConclusion form.conclusion)
                        |> Result.andThen (\_ -> validateRationaleInternVote form.internalVote)
                        |> Result.andThen (\_ -> validateRationaleRefs form.references)
            in
            case ( rationaleValidation, form.pdfAutogen ) of
                -- Without PDF autogeneration, validation is considered complete
                ( Ok _, False ) ->
                    let
                        formWithoutError =
                            { form | error = Nothing }
                    in
                    Done formWithoutError (rationaleFromForm formWithoutError)

                -- With PDF autogeneration, we will need to store on IPFS the PDF,
                -- and then auto-edit the rationale statement and references sections.
                ( Ok _, True ) ->
                    let
                        formWithoutError =
                            { form | error = Nothing }
                    in
                    Validating formWithoutError (rationaleFromForm formWithoutError)

                ( Err err, _ ) ->
                    Preparing { form | error = Just err }

        _ ->
            step


validateRationaleSummary : String -> Result String ()
validateRationaleSummary summary =
    -- Limited to 300 characters
    -- Should NOT support markdown (implied by not providing MD editor)
    let
        summaryLength =
            String.length summary
    in
    if summaryLength > 300 then
        Err ("Summary is limited to 300 characters, and is currently " ++ String.fromInt summaryLength)

    else if String.isEmpty (String.trim summary) then
        Err "The summary field is mandatory"

    else
        Ok ()


validateRationaleStatement : MarkdownForm -> Result String ()
validateRationaleStatement statement =
    case String.trim statement of
        "" ->
            Err "The rationale statement field is mandatory"

        str ->
            checkValidMarkdown str
                |> Result.andThen validateMarkdownHeadings


checkValidMarkdown : String -> Result String (List Markdown.Block.Block)
checkValidMarkdown str =
    let
        errorToString deadEnds =
            List.map Md.deadEndToString deadEnds
                |> String.join "\n\n"
    in
    Md.parse str
        |> Result.mapError errorToString


validateRationaleDiscussion : MarkdownForm -> Result String ()
validateRationaleDiscussion discussion =
    checkValidMarkdown (String.trim discussion)
        |> Result.andThen validateMarkdownHeadings


validateRationaleCounterArg : MarkdownForm -> Result String ()
validateRationaleCounterArg counterArg =
    checkValidMarkdown (String.trim counterArg)
        |> Result.andThen validateMarkdownHeadings


validateRationaleConclusion : MarkdownForm -> Result String ()
validateRationaleConclusion _ =
    Ok ()


validateRationaleInternVote : InternalVote -> Result String ()
validateRationaleInternVote internVote =
    -- Check that all numbers are positive
    if internVote.constitutional < 0 then
        Err "Constitutional internal vote must be >= 0"

    else if internVote.unconstitutional < 0 then
        Err "Unconstitutional internal vote must be >= 0"

    else if internVote.abstain < 0 then
        Err "Abstain internal vote must be >= 0"

    else if internVote.didNotVote < 0 then
        Err "DidNotVote internal vote must be >= 0"

    else if internVote.against < 0 then
        Err "AgainstVote internal vote must be >= 0"

    else
        Ok ()


validateRationaleRefs : List Reference -> Result String ()
validateRationaleRefs references =
    let
        -- Check that there are no duplicate label in the references
        validateNoDuplicate =
            case findDuplicate (List.map .label references) of
                Just dup ->
                    Err ("There is a duplicate label in the references: " ++ dup)

                Nothing ->
                    Ok ()

        -- TODO: Check that URIs seem valid
        checkUris _ =
            Ok ()
    in
    validateNoDuplicate
        |> Result.andThen
            (\_ ->
                List.map checkUris references
                    |> reduceResults
                    |> Result.map (always ())
            )


{-| Converts a RationaleForm into a final Rationale by:

  - Trimming whitespace
  - Converting empty strings to Nothing for optional fields
  - Preserving internal vote counts
  - Keeping all references

-}
rationaleFromForm : RationaleForm -> Rationale
rationaleFromForm form =
    let
        cleanup str =
            case String.trim str of
                "" ->
                    Nothing

                trimmed ->
                    Just trimmed
    in
    { summary = String.trim form.summary
    , rationaleStatement = String.trim form.rationaleStatement
    , precedentDiscussion = cleanup form.precedentDiscussion
    , counterArgumentDiscussion = cleanup form.counterArgumentDiscussion
    , conclusion = cleanup form.conclusion
    , internalVote = form.internalVote
    , references = form.references
    }


pinPdfFile : JD.Value -> Model -> ( Model, Cmd Msg )
pinPdfFile fileAsValue (Model model) =
    case ( JD.decodeValue File.decoder fileAsValue, model.storageConfigStep, model.rationaleCreationStep ) of
        ( Err error, _, Validating form _ ) ->
            ( Model { model | rationaleCreationStep = Preparing { form | error = Just <| JD.errorToString error } }
            , Cmd.none
            )

        ( Ok file, Done _ storageConfig, Validating _ _ ) ->
            ( Model model
            , case storageConfig of
                UsePreconfigIpfs _ ->
                    Api.defaultApiProvider.ipfsAddFile
                        { file = file }
                        GotIpfsAnswer

                UseBlockfrostIpfs { projectId } ->
                    Api.defaultApiProvider.ipfsAddFileBlockfrost
                        { projectId = projectId
                        , file = file
                        }
                        GotIpfsAnswer

                UseNmkrIpfs { userId, apiToken } ->
                    Api.defaultApiProvider.ipfsAddFileNmkr
                        { userId = userId
                        , apiToken = apiToken
                        , file = file
                        }
                        GotIpfsAnswer

                UseCustomIpfs { ipfsServer, headers } ->
                    Api.defaultApiProvider.ipfsAddFileCustom
                        { rpc = ipfsServer
                        , headers = headers
                        , file = file
                        }
                        GotIpfsAnswer
            )

        -- Ignore if we aren't validating the rationale storage step
        _ ->
            ( Model model, Cmd.none )


editRationale : Step RationaleForm Rationale Rationale -> Step RationaleForm Rationale Rationale
editRationale step =
    case step of
        Preparing _ ->
            step

        Validating rationaleForm _ ->
            Preparing rationaleForm

        Done prep _ ->
            Preparing prep



-- Rationale Signature Step


encodeJsonLdRationale : Gov.ActionId -> Rationale -> JE.Value
encodeJsonLdRationale actionId rationale =
    JE.object <|
        List.filterMap identity
            [ Just ( "govActionId", JE.string <| Gov.idToBech32 <| GovActionId actionId )
            , Just ( "summary", JE.string rationale.summary )
            , Just ( "rationaleStatement", JE.string rationale.rationaleStatement )
            , Maybe.map (\s -> ( "precedentDiscussion", JE.string s )) rationale.precedentDiscussion
            , Maybe.map (\s -> ( "counterargumentDiscussion", JE.string s )) rationale.counterArgumentDiscussion
            , Maybe.map (\s -> ( "conclusion", JE.string s )) rationale.conclusion
            , if rationale.internalVote == noInternalVote then
                Nothing

              else
                Just ( "internalVote", encodeInternalVote rationale.internalVote )
            , if List.isEmpty rationale.references then
                Nothing

              else
                Just ( "references", JE.list encodeReference rationale.references )
            ]


encodeInternalVote : InternalVote -> JE.Value
encodeInternalVote { constitutional, unconstitutional, abstain, didNotVote, against } =
    JE.object
        [ ( "constitutional", JE.int constitutional )
        , ( "unconstitutional", JE.int unconstitutional )
        , ( "abstain", JE.int abstain )
        , ( "didNotVote", JE.int didNotVote )
        , ( "againstVote", JE.int against )
        ]


encodeReference : Reference -> JE.Value
encodeReference ref =
    JE.object
        [ ( "@type", encodeRefType ref.type_ )
        , ( "label", JE.string ref.label )
        , ( "uri", JE.string ref.uri )
        ]


encodeRefType : ReferenceType -> JE.Value
encodeRefType refType =
    case refType of
        OtherRefType ->
            JE.string "Other"

        GovernanceMetadataRefType ->
            JE.string "GovernanceMetadata"

        RelevantArticlesRefType ->
            JE.string "RelevantArticles"


updateAuthorsForm : (List AuthorWitness -> List AuthorWitness) -> InnerModel -> InnerModel
updateAuthorsForm f ({ rationaleSignatureStep } as model) =
    case rationaleSignatureStep of
        Preparing form ->
            { model
                | rationaleSignatureStep =
                    Preparing { form | authors = f form.authors, error = Nothing }
            }

        _ ->
            model


handleJsonSignatureFileRead : Int -> String -> Result x String -> Msg
handleJsonSignatureFileRead n authorName result =
    case result of
        Err _ ->
            NoMsg

        Ok json ->
            LoadedAuthorSignatureJsonRationale n authorName json


authorWitnessExtractDecoder : String -> JD.Decoder AuthorWitness
authorWitnessExtractDecoder authorName =
    JD.field "authors" (JD.list ProposalMetadata.authorWitnessDecoder)
        |> JD.andThen
            (\authors ->
                case List.head <| List.filter (\a -> a.name == authorName) authors of
                    Just author ->
                        JD.succeed author

                    Nothing ->
                        JD.fail <| "No witness found for author: " ++ authorName
            )


signatureDecodingError : JD.Error -> Step RationaleSignatureForm {} RationaleSignature -> Step RationaleSignatureForm {} RationaleSignature
signatureDecodingError decodingError rationaleSignatureStep =
    case rationaleSignatureStep of
        Preparing form ->
            Preparing { form | error = Just <| JD.errorToString decodingError }

        _ ->
            rationaleSignatureStep


skipRationaleSignature : JsonLdContexts -> Step {} {} ActiveProposal -> Step RationaleSignatureForm {} RationaleSignature -> Step RationaleSignatureForm {} RationaleSignature
skipRationaleSignature jsonLdContexts pickProposalStep step =
    case ( pickProposalStep, step ) of
        ( Done _ { id }, Preparing ({ authors } as form) ) ->
            case findDuplicate (List.map .name authors) of
                Just dup ->
                    Preparing { form | error = Just <| "There is a duplicate name in the authors list: " ++ dup }

                Nothing ->
                    { form | authors = List.map (\a -> { a | signature = Nothing }) authors }
                        |> rationaleSignatureFromForm jsonLdContexts id
                        |> Done form

        ( Done _ { id }, Validating ({ authors } as form) _ ) ->
            case findDuplicate (List.map .name authors) of
                Just dup ->
                    Preparing { form | error = Just <| "There is a duplicate name in the authors list: " ++ dup }

                Nothing ->
                    { form | authors = List.map (\a -> { a | signature = Nothing }) authors }
                        |> rationaleSignatureFromForm jsonLdContexts id
                        |> Done form

        _ ->
            step


validateRationaleSignature : JsonLdContexts -> InnerModel -> ( InnerModel, Cmd msg, Maybe MsgToParent )
validateRationaleSignature jsonLdContexts model =
    case ( model.rationaleSignatureStep, model.pickProposalStep ) of
        ( Preparing ({ authors } as form), Done _ activeProposal ) ->
            case validateAuthorsForm authors of
                Err error ->
                    ( { model | rationaleSignatureStep = Preparing { form | error = Just error } }
                    , Cmd.none
                    , Nothing
                    )

                Ok _ ->
                    -- TODO: change to Validating instead, and emit a command to check signatures
                    ( { model | rationaleSignatureStep = Done form <| rationaleSignatureFromForm jsonLdContexts activeProposal.id form }
                    , Cmd.none
                    , Nothing
                    )

        _ ->
            ( model, Cmd.none, Nothing )


rationaleSignatureFromForm : JsonLdContexts -> Gov.ActionId -> RationaleSignatureForm -> RationaleSignature
rationaleSignatureFromForm jsonLdContexts actionId form =
    { authors = form.authors
    , rationale = form.rationale
    , signedJson =
        createJsonRationale jsonLdContexts actionId form.rationale form.authors
            |> JE.encode 0
    , error = Nothing
    }


rationaleSignatureToForm : Step RationaleSignatureForm {} RationaleSignature -> RationaleSignatureForm
rationaleSignatureToForm step =
    case step of
        Preparing form ->
            form

        Validating form _ ->
            form

        Done form _ ->
            form


validateAuthorsForm : List AuthorWitness -> Result String ()
validateAuthorsForm authors =
    let
        -- Check that there are no duplicate names in the authors
        validateNoDuplicate =
            case findDuplicate (List.map .name authors) of
                Just dup ->
                    Err ("There is a duplicate name in the authors list: " ++ dup)

                Nothing ->
                    Ok ()

        -- Check that witnessAlgorithm are authorized by the CIP
        authorizedAlgorithms =
            Set.singleton "ed25519"

        checkWitnessAlgo algo =
            if Set.member algo authorizedAlgorithms then
                Ok ()

            else
                Err ("The witness algorithm (" ++ algo ++ ") is not in the authorized standard list: " ++ String.join ", " (Set.toList authorizedAlgorithms))
    in
    validateNoDuplicate
        |> Result.andThen
            (\_ ->
                List.map .witnessAlgorithm authors
                    |> List.map checkWitnessAlgo
                    |> reduceResults
                    |> Result.map (always ())
            )


findDuplicate : List comparable -> Maybe comparable
findDuplicate list =
    findDuplicateHelper list Set.empty


findDuplicateHelper : List comparable -> Set comparable -> Maybe comparable
findDuplicateHelper list seen =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if Set.member x seen then
                Just x

            else
                findDuplicateHelper xs (Set.insert x seen)


reduceResults : List (Result a b) -> Result a (List b)
reduceResults results =
    List.foldr
        (\result acc ->
            case ( result, acc ) of
                ( Err err, _ ) ->
                    Err err

                ( Ok value, Ok values ) ->
                    Ok (value :: values)

                ( _, Err err ) ->
                    Err err
        )
        (Ok [])
        results



-- Rationale Storage Step


sendPinRequest : UpdateContext msg -> InnerModel -> ( InnerModel, Cmd msg, Maybe MsgToParent )
sendPinRequest ctx model =
    case model.rationaleSignatureStep of
        Done _ ratSig ->
            ( { model | permanentStorageStep = Validating { error = Nothing } {} }
            , ctx.jsonRationaleToFile
                { fileContent = ratSig.signedJson
                , fileName = "rationale-signed.json"
                }
            , Nothing
            )

        _ ->
            ( { model | permanentStorageStep = Preparing { error = Just "Validate the rationale signature step first." } }
            , Cmd.none
            , Nothing
            )


pinRationaleFile : JD.Value -> Model -> ( Model, Cmd Msg )
pinRationaleFile fileAsValue (Model model) =
    case ( JD.decodeValue File.decoder fileAsValue, model.storageConfigStep, model.permanentStorageStep ) of
        ( Err error, _, Validating _ _ ) ->
            ( Model { model | permanentStorageStep = Preparing { error = Just <| JD.errorToString error } }
            , Cmd.none
            )

        ( Ok file, Done _ storageConfig, Validating _ _ ) ->
            ( Model model
            , case storageConfig of
                UsePreconfigIpfs _ ->
                    Api.defaultApiProvider.ipfsAddFile
                        { file = file }
                        GotIpfsAnswer

                UseBlockfrostIpfs { projectId } ->
                    Api.defaultApiProvider.ipfsAddFileBlockfrost
                        { projectId = projectId
                        , file = file
                        }
                        GotIpfsAnswer

                UseNmkrIpfs { userId, apiToken } ->
                    Api.defaultApiProvider.ipfsAddFileNmkr
                        { userId = userId
                        , apiToken = apiToken
                        , file = file
                        }
                        GotIpfsAnswer

                UseCustomIpfs { ipfsServer, headers } ->
                    Api.defaultApiProvider.ipfsAddFileCustom
                        { rpc = ipfsServer
                        , headers = headers
                        , file = file
                        }
                        GotIpfsAnswer
            )

        -- Ignore if we aren't validating the rationale storage step
        _ ->
            ( Model model, Cmd.none )


handlePdfIpfsAnswer : UpdateContext msg -> InnerModel -> RationaleForm -> Rationale -> IpfsAnswer -> ( InnerModel, Cmd msg, Maybe MsgToParent )
handlePdfIpfsAnswer ctx model form rationale ipfsAnswer =
    case ( model.pickProposalStep, ipfsAnswer ) of
        ( _, IpfsError error ) ->
            ( { model | rationaleCreationStep = Preparing { form | error = Just error } }
            , Cmd.none
            , Nothing
            )

        -- Edit the rationale to prepend a link to the PDF at the beginning of the rationale statement,
        -- and in the list of references.
        ( Done _ { id }, IpfsAddSuccessful file ) ->
            let
                pdfLink =
                    "https://ipfs.io/ipfs/" ++ file.cid

                updatedRationaleStatement =
                    "A [PDF version][pdf-link] of this rationale is also made available."
                        ++ "\n\n"
                        ++ ("[pdf-link]: " ++ pdfLink)
                        ++ "\n\n"
                        ++ rationale.rationaleStatement

                updatedReferences =
                    rationale.references
                        ++ [ { type_ = OtherRefType
                             , label = "Rationale PDF"
                             , uri = "ipfs://" ++ file.cid
                             }
                           ]

                updatedRationale =
                    { rationale
                        | rationaleStatement = updatedRationaleStatement
                        , references = updatedReferences
                    }

                -- Initialize rationale signature with no author
                rationaleSignatureForm =
                    { authors = []
                    , rationale = updatedRationale
                    , error = Nothing
                    }
            in
            ( { model
                | rationaleCreationStep = Done { form | error = Nothing } updatedRationale
                , rationaleSignatureStep =
                    Done rationaleSignatureForm (rationaleSignatureFromForm ctx.jsonLdContexts id rationaleSignatureForm)
              }
            , Cmd.none
            , Nothing
            )

        -- Do nothing if no proposal was picked yet
        ( _, IpfsAddSuccessful _ ) ->
            ( model, Cmd.none, Nothing )


handleRationaleIpfsAnswer : InnerModel -> IpfsAnswer -> ( InnerModel, Cmd msg, Maybe MsgToParent )
handleRationaleIpfsAnswer model ipfsAnswer =
    case ipfsAnswer of
        IpfsError error ->
            ( { model | permanentStorageStep = Preparing { error = Just error } }
            , Cmd.none
            , Nothing
            )

        IpfsAddSuccessful file ->
            ( { model | permanentStorageStep = Done { error = Nothing } { jsonFile = file } }
            , Cmd.none
            , Nothing
            )



-- Build Tx Step


{-| Requirements needed to build a valid voting transaction:

  - Voter witness (key or script)
  - Proposal being voted on
  - Rationale anchor (metadata hash)
  - UTXOs for fee payment and scripts
  - Protocol parameters for cost calculation

-}
type alias TxRequirements =
    { voter : Witness.Voter
    , actionId : ActionId
    , proposalTitle : String
    , rationaleAnchor : Anchor
    , localStateUtxos : Utxo.RefDict Output
    , walletAddress : Address
    , costModels : CostModels
    }


allPrepSteps : { a | loadedWallet : Maybe LoadedWallet, costModels : Maybe CostModels } -> InnerModel -> Result String TxRequirements
allPrepSteps { loadedWallet, costModels } m =
    case ( costModels, ( m.voterStep, m.pickProposalStep, m.rationaleSignatureStep ), ( m.permanentStorageStep, loadedWallet ) ) of
        ( Just theCostModels, ( Done _ voter, Done _ p, Done _ r ), ( Done _ s, Just { utxos, wallet } ) ) ->
            let
                proposalTitle =
                    case p.metadata of
                        RemoteData.Success metadata ->
                            metadata.body.title
                                |> Maybe.withDefault "??? Unknown Proposal Title"

                        _ ->
                            "??? Unknown Proposal Title"
            in
            Ok
                { voter = voter
                , actionId = p.id
                , proposalTitle = proposalTitle
                , rationaleAnchor =
                    { url = "ipfs://" ++ s.jsonFile.cid
                    , dataHash =
                        Bytes.fromText r.signedJson
                            |> Bytes.toU8
                            |> blake2b256 Nothing
                            |> Bytes.fromU8
                    }
                , localStateUtxos = Dict.Any.union m.someRefUtxos utxos
                , walletAddress = Cip30.walletChangeAddress wallet
                , costModels = theCostModels
                }

        ( Nothing, _, _ ) ->
            Err "Somehow cost models are missing, please report this issue"

        _ ->
            Err "Incomplete steps before Tx building"



-- ###################################################################
-- VIEW
-- ###################################################################


{-| Configuration needed by the view:

  - Message wrapper for parent component
  - Wallet connectivity status
  - The current epoch
  - Available proposals to vote on
  - JSON-LD metadata context for rationale
  - Protocol parameters
  - The network ID
  - Link to transaction signing page
  - IPFS pre-configuration description

-}
type alias ViewContext msg =
    { wrapMsg : Msg -> msg
    , walletsDiscovered : List Cip30.WalletDescriptor
    , loadedWallet : Maybe LoadedWallet
    , drepId : Maybe (Bytes CredentialHash)
    , epoch : Maybe Int
    , cart : Cart.Model
    , proposals : WebData (Dict String ActiveProposal)
    , jsonLdContexts : JsonLdContexts
    , costModels : Maybe CostModels
    , networkId : NetworkId
    , changeNetworkLink : NetworkId -> List (Html msg) -> Html msg
    , signingLink : Transaction -> List { keyName : String, keyHash : Bytes CredentialHash } -> List (Html msg) -> Html msg
    , ipfsPreconfig : { label : String, description : String }
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
            , Helper.viewStepWithCircle 2 "proposal-step" (viewProposalSelectionStep ctx model)
            , Helper.viewStepWithCircle 3 "storage-config-step" (viewStorageConfigStep ctx model.storageConfigStep)
            , Helper.viewStepWithCircle 4 "rationale-step" (viewRationaleStep ctx model.pickProposalStep model.storageConfigStep model.rationaleCreationStep)
            , Helper.viewStepWithCircle 5 "rationale-signature-step" (viewRationaleSignatureStep ctx model.pickProposalStep model.rationaleCreationStep model.rationaleSignatureStep)
            , Helper.viewStepWithCircle 6 "storage-step" (viewPermanentStorageStep ctx model.rationaleSignatureStep model.storageConfigStep model.permanentStorageStep)
            , Html.map ctx.wrapMsg <| Helper.viewStepWithCircle 7 "build-tx-step" (viewBuildTxStep ctx model)
            , Helper.viewStepWithCircle 8 "sign-tx-step" (viewSignTxStep ctx model.voterStep model.buildTxStep)
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



--
-- Proposal Selection Step
--


viewProposalSelectionStep : ViewContext msg -> InnerModel -> Html msg
viewProposalSelectionStep ctx model =
    case model.pickProposalStep of
        Preparing _ ->
            viewProposalSelectionForm ctx model

        Validating _ _ ->
            div []
                [ Helper.sectionTitle "Pick a Proposal"
                , Helper.loadingSpinner "Validating the picked proposal..."
                ]

        Done _ proposal ->
            viewSelectedProposal ctx proposal


viewProposalSelectionForm : ViewContext msg -> InnerModel -> Html msg
viewProposalSelectionForm ctx model =
    div []
        [ Helper.sectionTitle "Pick a Proposal"
        , case ctx.proposals of
            RemoteData.NotAsked ->
                text "Proposals are not loading, please report this error."

            RemoteData.Loading ->
                Helper.loadingSpinner "Loading proposals..."

            RemoteData.Failure httpError ->
                Html.pre []
                    [ text "Something went wrong while loading proposals."
                    , text <| Debug.toString httpError
                    ]

            RemoteData.Success proposalsDict ->
                case model.voterStep of
                    Done _ voter ->
                        viewProposalList ctx (Just voter) proposalsDict model.visibleProposalCount

                    _ ->
                        viewProposalList ctx Nothing proposalsDict model.visibleProposalCount
        ]


viewProposalList : ViewContext msg -> Maybe Witness.Voter -> Dict String ActiveProposal -> Int -> Html msg
viewProposalList ctx maybeVoter proposalsDict visibleCount =
    if Dict.isEmpty proposalsDict then
        div [ HA.style "text-align" "center", HA.style "padding" "2rem", HA.style "color" "#666" ]
            [ text "No active proposals found." ]

    else
        let
            currentEpoch =
                Maybe.withDefault 0 ctx.epoch

            proposalsDictValues =
                Dict.values proposalsDict

            maybeVoterId =
                Maybe.map (Witness.toVoter >> Gov.voterToId >> Gov.idToBech32) maybeVoter

            -- Remove proposals already in the cart from that list for this voter
            allProposals =
                case maybeVoterId of
                    Just voterId ->
                        proposalsDictValues
                            |> List.filter
                                (\p ->
                                    (p.epoch_validity.end > currentEpoch)
                                        && not (Cart.contains voterId p.id ctx.cart)
                                )

                    Nothing ->
                        proposalsDictValues
                            |> List.filter (\p -> p.epoch_validity.end > currentEpoch)

            totalProposalCount =
                List.length allProposals

            visibleProposals =
                List.sortBy (\proposal -> proposal.epoch_validity.end) allProposals
                    |> List.take visibleCount

            hasMore =
                totalProposalCount > visibleCount

            -- Filter proposals already in the cart for this voter
            proposalsInCart : List VoteRecord
            proposalsInCart =
                case maybeVoterId of
                    Nothing ->
                        []

                    Just voterId ->
                        proposalsDictValues
                            |> List.filterMap (\p -> Cart.get voterId p.id ctx.cart)
        in
        div []
            [ Helper.proposalListContainer
                "Select a proposal to vote on"
                totalProposalCount
                (List.map (viewProposalCardHelper ctx.wrapMsg ctx.networkId ctx.epoch) visibleProposals)
            , Helper.showMoreButton
                hasMore
                visibleCount
                totalProposalCount
                (ctx.wrapMsg (ShowMoreProposals visibleCount))
            , Helper.viewProposalsListInCart proposalsInCart
            ]


viewProposalCardHelper : (Msg -> msg) -> NetworkId -> Maybe Int -> ActiveProposal -> Html msg
viewProposalCardHelper wrapMsg networkId currentEpoch proposal =
    let
        idString =
            Gov.actionIdToString proposal.id

        hashIsValid =
            case proposal.metadata of
                RemoteData.Success meta ->
                    meta.computedHash == proposal.metadataHash

                _ ->
                    True

        title =
            case proposal.metadata of
                RemoteData.Success meta ->
                    meta.body.title |> Maybe.withDefault "Untitled Proposal"

                _ ->
                    "Loading..."

        abstract =
            case proposal.metadata of
                RemoteData.Success meta ->
                    meta.body.abstract |> Maybe.withDefault "No abstract available"

                RemoteData.Loading ->
                    "Loading proposal details..."

                RemoteData.Failure _ ->
                    "Error loading proposal details"

                RemoteData.NotAsked ->
                    "Proposal details not available"

        linkUrl =
            cardanoExplorerActionUrl networkId proposal.id

        linkHex =
            Helper.shortenedHex 5 (Bytes.toHex proposal.id.transactionId)
    in
    Helper.proposalCard
        { hashIsValid = hashIsValid
        , isRatifying = proposal.ratified == currentEpoch
        , title = title
        , abstract = abstract
        , actionType = proposal.actionType
        , linkUrl = linkUrl
        , linkHex = linkHex
        , index = proposal.id.govActionIndex
        }
        (wrapMsg (PickProposalButtonClicked idString))
        (Helper.viewActionTypeIcon proposal.actionType)


viewSelectedProposal : ViewContext msg -> ActiveProposal -> Html msg
viewSelectedProposal ctx { id, actionType, metadata, metadataUrl, metadataHash } =
    let
        { title, maybeMetadata } =
            getProposalContent metadata metadataUrl

        actionTypeDisplay =
            div
                [ HA.style "display" "inline-flex"
                , HA.style "align-items" "center"
                , HA.style "font-size" "0.875rem"
                , HA.style "background-color" "#F1F5F9"
                , HA.style "padding" "0.25rem 0.5rem"
                , HA.style "border-radius" "0.375rem"
                ]
                [ text actionType
                , Helper.viewActionTypeIcon actionType
                ]

        ( hashIsValid, abstractContent, authorsDetails ) =
            case maybeMetadata of
                Just { raw, computedHash, abstract, authors } ->
                    ( computedHash == metadataHash
                    , Helper.proposalDetailsItem "Abstract"
                        (div
                            [ HA.style "line-height" "1.6"
                            , HA.style "color" "#4A5568"
                            ]
                            [ Helper.renderMarkdownContent abstract ]
                        )
                    , viewAuthorsDetails raw authors
                    )

                Nothing ->
                    ( True, text "", viewAuthorsDetails "" [] )

        viewAuthorsDetails : String -> List AuthorWitness -> Html msg
        viewAuthorsDetails rawMetadata authors =
            Helper.proposalDetailsItem "Authors" <|
                if List.isEmpty authors then
                    text "No author with accompanying signature was found."

                else
                    div
                        [ HA.style "padding" "1rem"
                        , HA.style "border" "1px solid #E2E8F0"
                        , HA.style "border-radius" "0.5rem"
                        , HA.style "background-color" "#F9FAFB"
                        , HA.style "box-sizing" "border-box"
                        , HA.style "word-break" "break-word"
                        ]
                        [ Html.ul
                            [ HA.style "list-style-type" "disc"
                            , HA.style "margin-left" "1.5rem"
                            ]
                            (List.map viewOneAuthor authors)
                        , Html.p [ HA.style "margin-top" "1.5rem" ]
                            [ text "Verify authors signatures on: "
                            , Helper.externalLink
                                { url = "https://verifycardanomessage.cardanofoundation.org/method=cip100#" ++ Url.percentEncode rawMetadata
                                , label = "verifycardanomessage.cardanofoundation.org"
                                }
                            ]
                        , Html.p [ HA.style "margin-top" "1.5rem" ]
                            [ text "To verify the authors signatures locally, download the metadata and use cardano-signer as follows:" ]
                        , Html.pre
                            [ HA.style "background-color" "#F9FAFB"
                            , HA.style "padding" "0.75rem"
                            , HA.style "border-radius" "0.25rem"
                            , HA.style "margin-top" "0.5rem"
                            , HA.style "color" "#1F2937"
                            , HA.style "font-family" "monospace"
                            , HA.style "background-color" "#E2E8F0"
                            ]
                            [ text cardanoSignerExample ]
                        , Html.div [ HA.style "margin-top" "1rem" ]
                            [ Helper.downloadJSONButton "Download metadata.json"
                                { filename = "metadata.json"
                                , rawJson = rawMetadata
                                }
                            ]
                        ]

        viewOneAuthor : AuthorWitness -> Html msg
        viewOneAuthor { name, publicKey } =
            Html.li
                [ HA.style "margin-bottom" "0.75rem"
                , HA.style "line-height" "1.6"
                , HA.style "color" "#4A5568"
                ]
                [ Html.strong [] [ text "Name: " ]
                , text name
                , Html.br [] []
                , Html.strong [] [ text "Public key: " ]
                , text publicKey
                ]

        cardanoSignerExample =
            "cardano-signer.js verify --cip100 \\\n"
                ++ "   --data-file metadata.json \\\n"
                ++ "   --json-extended"
    in
    div []
        [ div [ HA.class "flex items-center mb-4" ]
            [ Helper.sectionTitle "Pick a Proposal" ]
        , Helper.selectedProposalCard
            [ Helper.proposalDetailsItem "Proposal ID" (cardanoExplorerActionLink ctx.networkId id)
            , Helper.proposalDetailsItem "Type" actionTypeDisplay
            , Helper.proposalDetailsItem "Title" (Html.span [ HA.style "font-weight" "500" ] [ text title ])
            , if hashIsValid then
                text ""

              else
                Html.span
                    [ HA.style "background-color" "#FEF2F2"
                    , HA.style "color" "#DC2626"
                    , HA.style "font-weight" "bold"
                    , HA.style "padding" "0.5rem"
                    , HA.style "border-radius" "0.25rem"
                    ]
                    [ Html.span [] [ text "⚠️" ]
                    , text "INVALID HASH"
                    ]
            , abstractContent
            , authorsDetails
            ]
        , Html.p
            [ HA.style "margin-top" "1rem" ]
            [ Html.map ctx.wrapMsg <|
                Helper.viewButton "Change Proposal" ChangeProposalButtonClicked
            ]
        ]



-- Helper function to generate Cardano Explorer URL


cardanoExplorerActionUrl : NetworkId -> ActionId -> String
cardanoExplorerActionUrl networkId id =
    let
        governanceActionId =
            Gov.idToBech32 (GovActionId id)

        baseUrl =
            case networkId of
                Mainnet ->
                    "https://explorer.cardano.org/governance-action/"

                Testnet ->
                    "https://explorer.cardano.org/preview/governance-action/"
    in
    baseUrl ++ governanceActionId


cardanoExplorerActionLink : NetworkId -> ActionId -> Html msg
cardanoExplorerActionLink networkId id =
    let
        url =
            cardanoExplorerActionUrl networkId id

        govId =
            Gov.idToBech32 (GovActionId id)
    in
    Helper.externalLink { url = url, label = strBothEnds 10 10 govId }


strBothEnds : Int -> Int -> String -> String
strBothEnds startLength endLength str =
    -- Helper function to only display the start and end of a long string
    let
        strLength =
            String.length str
    in
    if strLength <= startLength + endLength then
        str

    else
        String.slice 0 startLength str
            ++ "..."
            ++ String.slice (strLength - endLength) strLength str


getProposalContent : RemoteData String ProposalMetadata -> String -> { title : String, maybeMetadata : Maybe { raw : String, computedHash : String, abstract : String, authors : List AuthorWitness } }
getProposalContent metadata metadataUrl =
    case metadata of
        RemoteData.NotAsked ->
            { title = "not loading", maybeMetadata = Nothing }

        RemoteData.Loading ->
            { title = "loading ...", maybeMetadata = Nothing }

        RemoteData.Failure error ->
            { title = "ERROR for " ++ metadataUrl ++ ": " ++ Debug.toString error
            , maybeMetadata = Nothing
            }

        RemoteData.Success meta ->
            { title = meta.body.title |> Maybe.withDefault "unknown (unexpected metadata format)"
            , maybeMetadata =
                Just
                    { raw = meta.raw
                    , computedHash = meta.computedHash
                    , abstract = Maybe.withDefault "Unknown abstract (unexpected metadata format)" meta.body.abstract
                    , authors = meta.authors
                    }
            }



--
-- Storage Configuration Step
--


viewStorageConfigStep : ViewContext msg -> Step StorageForm {} StorageConfig -> Html msg
viewStorageConfigStep ctx step =
    case step of
        Preparing form ->
            Html.map ctx.wrapMsg <|
                div []
                    [ Helper.sectionTitle "Storage Configuration"
                    , Html.p [ HA.class "mb-4" ]
                        [ text "Only the hash of your rationale is stored on Cardano,"
                        , text " so it's recommended to also store the actual JSON file containing the rationale in a permanent storage solution."
                        , text " Here we provide an easy way to store it on IPFS."
                        ]
                    , Helper.storageConfigCard "IPFS Method"
                        [ Helper.viewGrid 240
                            [ Helper.storageMethodOption ctx.ipfsPreconfig.label (form.storageMethod == PreconfigIPFS ctx.ipfsPreconfig) (StorageMethodSelected <| PreconfigIPFS ctx.ipfsPreconfig)
                            , Helper.storageMethodOption "Own Blockfrost IPFS" (form.storageMethod == BlockfrostIPFS) (StorageMethodSelected BlockfrostIPFS)
                            , Helper.storageMethodOption "Own NMKR IPFS" (form.storageMethod == NmkrIPFS) (StorageMethodSelected NmkrIPFS)
                            , Helper.storageMethodOption "Custom IPFS server" (form.storageMethod == CustomIPFS) (StorageMethodSelected CustomIPFS)
                            ]
                        , case form.storageMethod of
                            BlockfrostIPFS ->
                                viewBlockfrostForm form

                            NmkrIPFS ->
                                viewNmkrForm form

                            CustomIPFS ->
                                viewCustomIpfsForm form

                            PreconfigIPFS _ ->
                                text ""
                        ]
                    , Html.p [ HA.class "mt-6" ] [ Helper.viewButton "Validate storage config" ValidateStorageConfigButtonClicked ]
                    , viewError form.error
                    ]

        Validating _ _ ->
            div []
                [ Helper.sectionTitle "Storage Configuration"
                , Helper.loadingSpinner "Validating storage configuration..."
                ]

        Done _ storageConfig ->
            div []
                [ Helper.sectionTitle "Storage Configuration"
                , Helper.storageConfigCard "Selected Storage Method"
                    [ viewStorageConfigInfo storageConfig ]
                , Html.p [ HA.style "margin-top" "1rem" ]
                    [ Html.map ctx.wrapMsg <| Helper.viewButton "Change storage configuration" ValidateStorageConfigButtonClicked ]
                ]


viewBlockfrostForm : StorageForm -> Html Msg
viewBlockfrostForm form =
    Helper.storageProviderForm
        [ Helper.storageHeaderInput
            { label = "Blockfrost project ID"
            , value = form.blockfrostProjectId
            , onInputMsg = BlockfrostProjectIdChange
            }
        ]


viewNmkrForm : StorageForm -> Html Msg
viewNmkrForm form =
    Helper.storageProviderForm
        [ div
            [ HA.style "display" "grid"
            , HA.style "grid-template-columns" "1fr 1fr"
            , HA.style "gap" "1rem"
            ]
            [ Helper.storageHeaderInput
                { label = "NMKR user ID"
                , value = form.nmkrUserId
                , onInputMsg = NmkrUserIdChange
                }
            , Helper.storageHeaderInput
                { label = "NMKR API token"
                , value = form.nmkrApiToken
                , onInputMsg = NmkrApiTokenChange
                }
            ]
        ]


viewCustomIpfsForm : StorageForm -> Html Msg
viewCustomIpfsForm form =
    div []
        [ Helper.storageProviderForm
            [ Helper.storageHeaderInput
                { label = "IPFS RPC server"
                , value = form.ipfsServer
                , onInputMsg = IpfsServerChange
                }
            ]
        , div
            [ HA.style "margin-top" "1.5rem"
            , HA.style "display" "flex"
            , HA.style "justify-content" "space-between"
            , HA.style "align-items" "center"
            ]
            [ Html.h4
                [ HA.style "font-weight" "600"
                , HA.style "font-size" "1rem"
                ]
                [ text "HTTP Headers" ]
            , Helper.addHeaderButton AddHeaderButtonClicked
            ]
        , if List.isEmpty form.headers then
            Html.p
                [ HA.style "text-align" "center"
                , HA.style "color" "#6B7280"
                , HA.style "font-style" "italic"
                , HA.style "padding" "1rem 0"
                ]
                [ text "No headers added yet." ]

          else
            div [ HA.style "margin-top" "1rem" ]
                (List.indexedMap
                    (\n ( field, value ) ->
                        Helper.storageHeaderForm
                            n
                            field
                            value
                            (DeleteHeaderButtonClicked n)
                            (StorageHeaderFieldChange n)
                            (StorageHeaderValueChange n)
                    )
                    form.headers
                )
        ]


viewStorageConfigInfo : StorageConfig -> Html msg
viewStorageConfigInfo config =
    case config of
        UsePreconfigIpfs { label, description } ->
            Helper.storageProviderCard
                [ Html.h4
                    [ HA.style "font-weight" "600"
                    , HA.style "margin-bottom" "0.5rem"
                    ]
                    [ text label ]
                , Html.p
                    [ HA.style "color" "#4A5568"
                    , HA.style "font-size" "0.875rem"
                    ]
                    [ text description ]
                ]

        UseBlockfrostIpfs { label, description, projectId } ->
            Helper.storageProviderCard
                [ Html.h4
                    [ HA.style "font-weight" "600"
                    , HA.style "margin-bottom" "0.5rem"
                    ]
                    [ text label ]
                , Html.p
                    [ HA.style "color" "#4A5568"
                    , HA.style "font-size" "0.875rem"
                    ]
                    [ text description ]
                , Helper.storageConfigItem "Project ID"
                    (Html.p
                        [ HA.style "font-family" "monospace"
                        , HA.style "word-break" "break-all"
                        ]
                        [ text (String.left 10 projectId ++ "..." ++ String.right 6 projectId) ]
                    )
                ]

        UseNmkrIpfs { label, description } ->
            Helper.storageProviderCard
                [ Html.h4
                    [ HA.style "font-weight" "600"
                    , HA.style "margin-bottom" "0.5rem"
                    ]
                    [ text label ]
                , Html.p
                    [ HA.style "color" "#4A5568"
                    , HA.style "font-size" "0.875rem"
                    ]
                    [ text description ]
                ]

        UseCustomIpfs { label, description, ipfsServer } ->
            Helper.storageProviderCard
                [ Html.h4
                    [ HA.style "font-weight" "600"
                    , HA.style "margin-bottom" "0.5rem"
                    ]
                    [ text label ]
                , Html.p
                    [ HA.style "color" "#4A5568"
                    , HA.style "font-size" "0.875rem"
                    ]
                    [ text description ]
                , Helper.storageConfigItem "IPFS Server"
                    (Html.a
                        [ HA.href ipfsServer
                        , HA.target "_blank"
                        , HA.style "font-family" "monospace"
                        , HA.style "word-break" "break-all"
                        , HA.style "color" "#3182CE"
                        ]
                        [ text ipfsServer ]
                    )
                ]



--
-- Rationale Step
--


viewRationaleStep :
    ViewContext msg
    -> Step {} {} ActiveProposal
    -> Step StorageForm {} StorageConfig
    -> Step RationaleForm Rationale Rationale
    -> Html msg
viewRationaleStep ctx pickProposalStep storageConfigStep step =
    Html.map ctx.wrapMsg <|
        case ( ( ctx.walletsDiscovered, pickProposalStep ), storageConfigStep, step ) of
            ( ( [], _ ), _, _ ) ->
                div []
                    [ Helper.sectionTitle "Vote Rationale"
                    , Helper.stepNotAvailableCard [ text "No Cardano wallet detected, you will not be able to create a transaction. Please make sure you are using the browser where your Cardano wallet is installed." ]
                    ]

            ( ( _, Done _ _ ), Done _ _, Preparing form ) ->
                viewRationaleForm form

            ( ( _, Done _ _ ), Done _ _, Validating _ _ ) ->
                div []
                    [ Helper.sectionTitle "Vote Rationale"
                    , Helper.formContainer
                        [ Html.p [ HA.class "text-gray-600" ] [ text "Auto-generation of PDF in progress ..." ]
                        , Html.p [ HA.class "mt-4" ] [ Helper.viewButton "Edit rationale" EditRationaleButtonClicked ]
                        ]
                    ]

            ( ( _, Done _ _ ), Done _ _, Done _ rationale ) ->
                viewCompletedRationale rationale

            ( _, Done _ _, _ ) ->
                div []
                    [ Helper.sectionTitle "Vote Rationale"
                    , Helper.stepNotAvailableCard [ text "Please pick a proposal first." ]
                    ]

            ( ( _, Done _ _ ), _, _ ) ->
                div []
                    [ Helper.sectionTitle "Vote Rationale"
                    , Helper.stepNotAvailableCard [ text "Please validate the IPFS config step first." ]
                    ]

            _ ->
                div []
                    [ Helper.sectionTitle "Vote Rationale"
                    , Helper.stepNotAvailableCard [ text "Please pick a proposal and validate the IPFS config step first." ]
                    ]


viewRationaleForm : RationaleForm -> Html Msg
viewRationaleForm form =
    div []
        [ Helper.sectionTitle "Vote Rationale"
        , div [ HA.style "display" "grid", HA.style "gap" "1.5rem", HA.style "position" "relative" ]
            [ Helper.rationaleCard
                "Summary"
                "Clearly state your stance, summarize your rationale with your main argument. Limited to 300 characters."
                (Helper.rationaleTextArea RationaleSummaryChange (Just 300) form.summary)
            , Helper.rationaleCard
                "Rationale Statement"
                "Fully describe your rationale, with your arguments in full details. Use markdown with heading level 2 (##) or higher."
                (viewStatementInput form.pdfAutogen form.rationaleStatement)
            , Helper.rationaleCard
                "Optional Fields"
                ""
                (Helper.checkbox { id = "optional-fields", label = " Show optional fields" } form.optionalFieldsAreVisible ToggleOptionalFieldsVisibility)
            , if form.optionalFieldsAreVisible then
                div []
                    [ Helper.rationaleCard
                        "Precedent Discussion"
                        "Optional: Discuss what you feel is relevant precedent."
                        (Helper.rationaleMarkdownInput form.precedentDiscussion PrecedentDiscussionChange)
                    , Helper.rationaleCard
                        "Counter Argument Discussion"
                        "Optional: Discuss significant counter arguments to your position."
                        (Helper.rationaleMarkdownInput form.counterArgumentDiscussion CounterArgumentChange)
                    , Helper.rationaleCard
                        "Conclusion"
                        "Optional: Final thoughts on your position."
                        (Helper.rationaleTextArea ConclusionChange Nothing form.conclusion)
                    , Helper.rationaleCard
                        "Internal Vote"
                        "If you vote as a group, you can report the group internal votes."
                        (viewInternalVoteInput form.internalVote)
                    , Helper.referenceCard
                        (List.indexedMap viewOneRefForm form.references)
                        AddRefButtonClicked
                    ]

              else
                text ""
            ]
        , Html.p [ HA.class "mt-6" ] [ Helper.viewButton "Confirm rationale" ValidateRationaleButtonClicked ]
        , viewError form.error
        ]


viewStatementInput : Bool -> String -> Html Msg
viewStatementInput hasAutoGen form =
    div []
        [ Helper.pdfAutogenCheckbox hasAutoGen TogglePdfAutogen
        , div
            [ HA.style "margin-bottom" "0.5rem"
            , HA.style "color" "#4A5568"
            , HA.style "font-size" "0.75rem"
            ]
            [ text "Markdown formatting is supported. Use ## or deeper heading levels. # headings are reserved for section titles." ]
        , Html.textarea
            [ HA.value form
            , Html.Events.onInput RationaleStatementChange
            , HA.style "width" "100%"
            , HA.style "padding" "0.75rem"
            , HA.style "border" "1px solid #E2E8F0"
            , HA.style "border-radius" "0.375rem"
            , HA.style "min-height" "200px"
            , HA.style "resize" "vertical"
            , HA.style "font-family" "monospace"
            , HA.style "font-size" "0.875rem"
            , HA.style "line-height" "1.5"
            ]
            []
        ]


viewInternalVoteInput : InternalVote -> Html Msg
viewInternalVoteInput { constitutional, unconstitutional, abstain, didNotVote, against } =
    div
        [ HA.style "display" "grid"
        , HA.style "grid-template-columns" "repeat(auto-fit, minmax(140px, 1fr))"
        , HA.style "gap" "1rem"
        ]
        [ Helper.voteNumberInput "Constitutional" constitutional InternalConstitutionalVoteChange
        , Helper.voteNumberInput "Unconstitutional" unconstitutional InternalUnconstitutionalVoteChange
        , Helper.voteNumberInput "Abstain" abstain InternalAbstainVoteChange
        , Helper.voteNumberInput "Did not vote" didNotVote InternalDidNotVoteChange
        , Helper.voteNumberInput "Against voting" against InternalAgainstVoteChange
        ]


viewOneRefForm : Int -> Reference -> Html Msg
viewOneRefForm n reference =
    Helper.referenceForm
        n
        (refTypeToString reference.type_)
        reference.label
        reference.uri
        (DeleteRefButtonClicked n)
        (ReferenceTypeChange n)
        (ReferenceLabelChange n)
        (ReferenceUriChange n)


viewCompletedRationale : Rationale -> Html Msg
viewCompletedRationale rationale =
    let
        statementSection =
            div [ HA.style "border-top" "1px solid #EDF2F7", HA.style "padding-top" "1.5rem" ]
                [ Html.h4
                    [ HA.style "font-size" "1rem"
                    , HA.style "font-weight" "600"
                    , HA.style "margin-bottom" "1rem"
                    , HA.style "color" "#1A202C"
                    ]
                    [ text "Rationale Statement" ]
                , div
                    [ HA.style "border" "1px solid #E2E8F0"
                    , HA.style "border-radius" "0.5rem"
                    , HA.style "padding" "1.25rem"
                    , HA.style "background-color" "#F9FAFB"
                    ]
                    [ Helper.renderMarkdownContent rationale.rationaleStatement ]
                ]

        precedentSection =
            Helper.optionalSection "Precedent Discussion" rationale.precedentDiscussion

        counterArgumentSection =
            Helper.optionalSection "Counter Argument Discussion" rationale.counterArgumentDiscussion

        conclusionSection =
            Helper.optionalSection "Conclusion" rationale.conclusion

        internalVoteSection =
            if rationale.internalVote /= noInternalVote then
                div [ HA.style "border-top" "1px solid #EDF2F7", HA.style "padding-top" "1.5rem" ]
                    [ Html.h4
                        [ HA.style "font-size" "1rem"
                        , HA.style "font-weight" "600"
                        , HA.style "margin-bottom" "1rem"
                        , HA.style "color" "#1A202C"
                        ]
                        [ text "Internal Vote" ]
                    , Helper.formattedInternalVote rationale.internalVote
                    ]

            else
                text ""

        referencesSection =
            if not (List.isEmpty rationale.references) then
                div [ HA.style "border-top" "1px solid #EDF2F7", HA.style "padding-top" "1.5rem" ]
                    [ Html.h4
                        [ HA.style "font-size" "1rem"
                        , HA.style "font-weight" "600"
                        , HA.style "margin-bottom" "1rem"
                        , HA.style "color" "#1A202C"
                        ]
                        [ text "References" ]
                    , Helper.formattedReferences refTypeToString rationale.references
                    ]

            else
                text ""
    in
    Helper.rationaleCompletedCard
        rationale.summary
        [ statementSection
        , precedentSection
        , counterArgumentSection
        , conclusionSection
        , internalVoteSection
        , referencesSection
        ]
        EditRationaleButtonClicked



--
-- Rationale Signature Step
--


viewRationaleSignatureStep :
    ViewContext msg
    -> Step {} {} ActiveProposal
    -> Step RationaleForm Rationale Rationale
    -> Step RationaleSignatureForm {} RationaleSignature
    -> Html msg
viewRationaleSignatureStep ctx pickProposalStep rationaleCreationStep step =
    div []
        [ Helper.sectionTitle "Rationale Signature" -- Always show the title
        , case ( pickProposalStep, rationaleCreationStep, step ) of
            ( _, Preparing _, _ ) ->
                Helper.stepNotAvailableCard [ text "Please complete the rationale creation step first." ]

            ( _, Validating _ _, _ ) ->
                Helper.stepNotAvailableCard [ text "Please wait for the rationale creation to complete." ]

            ( Done _ _, Done _ _, Preparing form ) ->
                Html.map ctx.wrapMsg <| viewRationaleSignatureForm form

            ( _, Done _ _, Preparing _ ) ->
                Helper.stepNotAvailableCard [ text "Please select a proposal first." ]

            ( _, Done _ _, Validating _ _ ) ->
                Helper.formContainer
                    [ Html.p [ HA.class "text-gray-600" ] [ text "Validating signatures..." ] ]

            ( _, Done _ _, Done _ ratSig ) ->
                viewCompletedRationaleSignature ctx ratSig
        ]


viewCompletedRationaleSignature : ViewContext msg -> RationaleSignature -> Html msg
viewCompletedRationaleSignature ctx ratSig =
    div []
        [ Helper.stepCard
            [ if List.isEmpty ratSig.authors then
                Html.p
                    [ HA.style "color" "#4A5568"
                    , HA.style "font-size" "0.9375rem"
                    ]
                    [ text "No author was added to this rationale." ]

              else
                div []
                    [ Html.p
                        [ HA.style "color" "#4A5568"
                        , HA.style "font-size" "0.9375rem"
                        , HA.style "margin-bottom" "1rem"
                        ]
                        [ text "This rationale has the following authors:" ]
                    , Html.ul
                        [ HA.style "list-style-type" "none"
                        , HA.style "padding" "0"
                        , HA.style "display" "flex"
                        , HA.style "flex-direction" "column"
                        , HA.style "gap" "0.75rem"
                        ]
                        (List.map viewSignerCard ratSig.authors)
                    ]
            , Html.p [ HA.style "margin-top" "1rem" ] [ Helper.downloadJSONButton "Download JSON rationale" { filename = "rationale.json", rawJson = ratSig.signedJson } ]
            ]
        , Html.map ctx.wrapMsg <| Helper.viewButton "Change authors" ChangeAuthorsButtonClicked
        ]


viewSignerCard : AuthorWitness -> Html msg
viewSignerCard { name, witnessAlgorithm, publicKey, signature } =
    Helper.signerCard name signature witnessAlgorithm publicKey (Maybe.withDefault "" signature)


viewRationaleSignatureForm : RationaleSignatureForm -> Html Msg
viewRationaleSignatureForm { authors } =
    let
        cardanoSignerExample =
            "cardano-signer.js sign --cip100 \\\n"
                ++ "   --data-file rationale.json \\\n"
                ++ "   --secret-key dummy.skey \\\n"
                ++ "   --author-name \"The great Name\" \\\n"
                ++ "   --out-file rationale-signed.json"
    in
    div []
        [ Helper.authorsCard
            [ div []
                [ Html.h3
                    [ HA.style "font-weight" "600"
                    , HA.style "font-size" "1.125rem"
                    , HA.style "color" "#1A202C"
                    , HA.style "line-height" "1.4"
                    ]
                    [ text "Authors" ]
                , Html.p
                    [ HA.style "font-size" "0.875rem"
                    , HA.style "color" "#4A5568"
                    ]
                    [ text "Add individual authors that contributed to this rationale" ]
                ]
            , Helper.addAuthorButton AddAuthorButtonClicked
            ]
            (div []
                [ Html.p
                    [ HA.style "margin-bottom" "1.5rem"
                    , HA.style "color" "#4A5568"
                    ]
                    [ text "Each author needs to sign the rationale document. You can download the JSON file, sign it with cardano-signer, and then upload the signature." ]
                , Helper.codeSnippetBox cardanoSignerExample
                , if List.isEmpty authors then
                    Helper.noAuthorsPlaceholder

                  else
                    div
                        [ HA.style "display" "flex"
                        , HA.style "flex-direction" "column"
                        , HA.style "gap" "1rem"
                        ]
                        (List.indexedMap viewOneAuthorForm authors)
                , Helper.formButtonsRow
                    [ Helper.secondaryButton "Skip Signatures" SkipRationaleSignaturesButtonClicked
                    , Helper.primaryButton "Confirm Signatures" ValidateRationaleSignaturesButtonClicked
                    ]
                ]
            )
        ]


viewOneAuthorForm : Int -> AuthorWitness -> Html Msg
viewOneAuthorForm n author =
    Helper.authorForm n
        (DeleteAuthorButtonClicked n)
        [ Helper.labeledField "Author name"
            (Html.input
                [ HA.type_ "text"
                , HA.value author.name
                , Html.Events.onInput (AuthorNameChange n)
                , HA.style "width" "100%"
                , HA.style "padding" "0.75rem"
                , HA.style "border" "1px solid #E2E8F0"
                , HA.style "border-radius" "0.375rem"
                , HA.style "background-color" "white"
                ]
                []
            )
        , case author.signature of
            Nothing ->
                Helper.labeledField "Signature"
                    (Helper.loadSignatureButton (LoadJsonSignatureButtonClicked n author.name))

            Just sig ->
                div [ HA.style "display" "grid", HA.style "gap" "1rem" ]
                    [ Helper.labeledField "Signature algorithm"
                        (Helper.readOnlyField author.witnessAlgorithm)
                    , Helper.labeledField "Public key"
                        (Helper.readOnlyField author.publicKey)
                    , Helper.signatureField sig (LoadJsonSignatureButtonClicked n author.name)
                    ]
        ]


{-| Creates a JSON-LD document for the rationale that follows CIP-0136.
Includes:

  - Core rationale content
  - Authors and their signatures if present
  - Standard context and hash algorithm

-}
createJsonRationale : JsonLdContexts -> Gov.ActionId -> Rationale -> List AuthorWitness -> JE.Value
createJsonRationale jsonLdContexts actionId rationale authors =
    JE.object <|
        List.filterMap identity
            [ Just ( "@context", jsonLdContexts.ccCip136Context )
            , Just ( "hashAlgorithm", JE.string "blake2b-256" )
            , Just ( "body", encodeJsonLdRationale actionId rationale )
            , if List.isEmpty authors then
                Nothing

              else
                Just <| ( "authors", JE.list encodeAuthorWitness authors )
            ]


encodeAuthorWitness : AuthorWitness -> JE.Value
encodeAuthorWitness { name, witnessAlgorithm, publicKey, signature } =
    case signature of
        Nothing ->
            JE.object [ ( "name", JE.string name ) ]

        Just sig ->
            JE.object
                [ ( "name", JE.string name )
                , ( "witnessAlgorithm", JE.string witnessAlgorithm )
                , ( "publicKey", JE.string publicKey )
                , ( "signature", JE.string sig )
                ]



--
-- Storage Step
--


viewPermanentStorageStep :
    ViewContext msg
    -> Step RationaleSignatureForm {} RationaleSignature
    -> Step StorageForm {} StorageConfig
    -> Step { error : Maybe String } {} Storage
    -> Html msg
viewPermanentStorageStep ctx rationaleSigStep storageConfigStep step =
    Html.map ctx.wrapMsg <|
        div []
            [ Helper.sectionTitle "Rationale Storage"
            , case ( rationaleSigStep, storageConfigStep, step ) of
                ( Done _ _, Done _ _, Preparing form ) ->
                    Helper.storageUploadCard PinJsonIpfsButtonClicked (viewError form.error)

                ( Done _ _, Done _ _, Validating _ _ ) ->
                    Helper.uploadingSpinner "Uploading rationale to IPFS..."

                ( Done _ r, Done _ _, Done _ storage ) ->
                    viewCompletedStorage r storage

                _ ->
                    Helper.storageNotAvailableCard
            ]


viewCompletedStorage : RationaleSignature -> Storage -> Html Msg
viewCompletedStorage r storage =
    let
        link =
            "https://ipfs.io/ipfs/" ++ storage.jsonFile.cid

        dataHash =
            Bytes.fromText r.signedJson
                |> Bytes.toU8
                |> blake2b256 Nothing
                |> Bytes.fromU8

        infoItems =
            List.concat
                [ Helper.fileInfoItem "File name:"
                    (Html.span
                        [ HA.style "font-family" "monospace"
                        , HA.style "overflow-wrap" "break-word"
                        ]
                        [ text storage.jsonFile.name ]
                    )
                , Helper.fileInfoItem "CID:"
                    (Html.span
                        [ HA.style "font-family" "monospace"
                        , HA.style "overflow-wrap" "break-word"
                        ]
                        [ text storage.jsonFile.cid ]
                    )
                , Helper.fileInfoItem "File Hash:"
                    (Html.span
                        [ HA.style "font-family" "monospace"
                        , HA.style "overflow-wrap" "break-word"
                        ]
                        [ text (Bytes.toHex dataHash) ]
                    )
                , Helper.fileInfoItem "Size:"
                    (Html.span
                        [ HA.style "font-family" "monospace"
                        ]
                        [ text (storage.jsonFile.size ++ " bytes") ]
                    )
                , Helper.fileInfoItem "IPFS Link:"
                    (Helper.externalLinkDisplay link link)
                ]
    in
    div []
        [ Helper.storageSuccessCard
            [ Html.p
                [ HA.style "color" "#4A5568"
                , HA.style "margin-bottom" "1.5rem"
                , HA.style "font-size" "0.9375rem"
                ]
                [ text "Your file has been uploaded to IPFS. File pinning is ongoing and may take a few hours to complete. We recommend saving a local copy of your JSON file in case you need to re-upload it in the future." ]
            , Html.p [ HA.style "margin" "1rem 0rem" ] [ Helper.downloadJSONButton "Download JSON rationale" { filename = "rationale.json", rawJson = r.signedJson } ]
            , Helper.storageInfoGrid infoItems
            ]
        , Helper.viewButton "Add another storage location" AddOtherStorageButtonCLicked
        ]



--
-- Tx Building Step
--


viewBuildTxStep : ViewContext msg -> InnerModel -> Html Msg
viewBuildTxStep ctx model =
    div []
        [ Helper.sectionTitle "Tx Building"
        , case ( allPrepSteps ctx model, model.buildTxStep ) of
            ( Err _, _ ) ->
                viewMissingStepsMessage ctx model

            ( Ok _, Preparing { error } ) ->
                Helper.stepCard
                    [ Html.p
                        [ HA.style "color" "#4A5568"
                        , HA.style "font-size" "0.9375rem"
                        , HA.style "margin-bottom" "1.5rem"
                        ]
                        [ text "Select how you want to vote on this proposal:" ]
                    , div
                        [ HA.style "display" "flex"
                        , HA.style "flex-wrap" "wrap"
                        , HA.style "gap" "1rem"
                        ]
                        [ Helper.voteButton "Vote YES" "#10B981" (AddVoteToCartButtonClicked Gov.VoteYes)
                        , Helper.voteButton "Vote NO" "#EF4444" (AddVoteToCartButtonClicked Gov.VoteNo)
                        , Helper.voteButton "ABSTAIN" "#6B7280" (AddVoteToCartButtonClicked Gov.VoteAbstain)
                        ]
                    , viewError error
                    ]

            ( Ok _, Validating _ _ ) ->
                Helper.loadingSpinner "Building transaction..."

            ( Ok _, Done _ { tx } ) ->
                div []
                    [ Helper.txResultCard
                        "Transaction Built"
                        "Your vote transaction has been created successfully"
                        [ Html.p
                            [ HA.style "color" "#4A5568"
                            , HA.style "font-size" "0.9375rem"
                            , HA.style "margin-bottom" "1rem"
                            ]
                            [ text "Transaction details (₳ displayed as lovelaces):" ]
                        , Helper.txDetailsContainer
                            [ Helper.txPreContainer <| prettyTx tx ]
                        ]
                    , Helper.viewButton "Change vote" ChangeVoteButtonClicked
                    ]
        ]


viewMissingStepsMessage : ViewContext msg -> InnerModel -> Html Msg
viewMissingStepsMessage ctx model =
    Helper.stepNotAvailableCard
        [ Html.p
            [ HA.style "color" "#4A5568"
            , HA.style "font-size" "0.9375rem"
            , HA.style "margin-bottom" "1rem"
            ]
            [ text "Please complete the following steps before building the transaction:" ]
        , Helper.missingStepsList
            [ Helper.missingStepItem "Voter identification" (isStepIncomplete model.voterStep) (Just "voter-step")
            , Helper.missingStepItem "Proposal selection" (isStepIncomplete model.pickProposalStep) (Just "proposal-step")
            , Helper.missingStepItem "Rationale creation" (isStepIncomplete model.rationaleCreationStep) (Just "rationale-step")
            , Helper.missingStepItem "Rationale storage" (isStepIncomplete model.permanentStorageStep) (Just "storage-step")
            , Helper.missingStepItem "Connect wallet" (ctx.loadedWallet == Nothing) Nothing
            , Helper.missingStepItem "Protocol parameters" (ctx.costModels == Nothing) Nothing
            ]
        ]


isStepIncomplete : Step a b c -> Bool
isStepIncomplete step =
    case step of
        Done _ _ ->
            False

        _ ->
            True



--
-- Tx Signing Step
--


viewSignTxStep : ViewContext msg -> Step a b Witness.Voter -> Step BuildTxPrep {} TxFinalized -> Html msg
viewSignTxStep ctx voterStep buildTxStep =
    case ( buildTxStep, voterStep ) of
        ( Done _ { tx, expectedSignatures }, Done _ voterWitness ) ->
            let
                voterId =
                    case voterWitness of
                        Witness.WithCommitteeHotCred (Witness.WithKey hotkey) ->
                            [ ( Bytes.toHex hotkey, "Committee hot key" ) ]

                        Witness.WithCommitteeHotCred (Witness.WithScript scriptHash witness) ->
                            ( Bytes.toHex scriptHash, "Committee governance script" )
                                :: extractMultisigKeys witness

                        Witness.WithDrepCred (Witness.WithKey credKey) ->
                            [ ( Bytes.toHex credKey, "DRep key" ) ]

                        Witness.WithDrepCred (Witness.WithScript scriptHash witness) ->
                            ( Bytes.toHex scriptHash, "DRep governance script" )
                                :: extractMultisigKeys witness

                        Witness.WithPoolCred poolKey ->
                            [ ( Bytes.toHex poolKey, "SPO key" ) ]

                extractMultisigKeys : Witness.Script -> List ( String, String )
                extractMultisigKeys witness =
                    case witness of
                        Witness.Native { expectedSigners } ->
                            List.map (\signer -> ( Bytes.toHex signer, "Multisig signer" )) expectedSigners

                        Witness.Plutus _ ->
                            []

                walletSpendingCredential =
                    case ctx.loadedWallet of
                        Just { wallet } ->
                            Address.extractPubKeyHash (Cip30.walletChangeAddress wallet)
                                |> Maybe.map (\hash -> [ ( Bytes.toHex hash, "Wallet spending key" ) ])
                                |> Maybe.withDefault []

                        Nothing ->
                            []

                walletStakeCredential =
                    case ctx.loadedWallet of
                        Just { wallet } ->
                            Address.extractStakeKeyHash (Cip30.walletChangeAddress wallet)
                                |> Maybe.map (\hash -> [ ( Bytes.toHex hash, "Wallet stake key" ) ])
                                |> Maybe.withDefault []

                        Nothing ->
                            []

                keyNames : Dict String String
                keyNames =
                    Dict.fromList (voterId ++ walletSpendingCredential ++ walletStakeCredential)
            in
            div []
                [ Helper.sectionTitle "Tx Signing"
                , Helper.signingStepCard
                    "Finalize Your Vote"
                    "The following keys will need to sign the transaction:"
                    [ Helper.infoBox []
                        [ Html.ul
                            [ HA.style "list-style-type" "none"
                            , HA.style "padding" "0"
                            , HA.style "margin" "0"
                            , HA.style "display" "flex"
                            , HA.style "flex-direction" "column"
                            , HA.style "gap" "0.75rem"
                            ]
                            (List.map
                                (\hash ->
                                    let
                                        hashHex =
                                            Bytes.toHex hash

                                        keyName =
                                            Dict.get hashHex keyNames
                                                |> Maybe.withDefault "Unknown key"
                                    in
                                    Helper.keyListItem keyName hashHex
                                )
                                expectedSignatures
                            )
                        ]
                    , Html.p
                        [ HA.style "color" "#4A5568"
                        , HA.style "font-size" "0.9375rem"
                        , HA.style "margin-bottom" "1.5rem"
                        ]
                        [ text "Click the button below to proceed to the signing page where you can sign and submit your voting transaction." ]
                    ]
                , ctx.signingLink tx
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
                ]

        _ ->
            div []
                [ Helper.sectionTitle "Tx Signing"
                , Helper.signingStepCard
                    "Step Not Available"
                    "Please complete the transaction building step first."
                    []
                ]



--
-- Helpers
--


viewError : Maybe String -> Html msg
viewError =
    Helper.viewError
