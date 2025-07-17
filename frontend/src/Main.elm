port module Main exposing (Flags, Model, Msg, Page, TaskCompleted, main)

{-| Main application module for the Cardano Governance Voting web app.


# Architecture Overview

This app follows a single-page application (SPA) architecture with client-side routing.
The main components are:

  - Wallet integration via CIP-30 ports for connecting to Cardano wallets
  - Multiple pages for different governance actions (vote preparation, signing, DRep registration)
  - Concurrent task handling for asynchronous operations
  - In-browser caching of proposal metadata
  - PDF generation capabilities


# Key Design Decisions

  - Uses ports for all wallet interactions to maintain type safety while talking to JS
  - Implements client-side routing to enable sharing direct links to specific actions
  - Caches proposal metadata locally to improve performance and reduce API load
  - Uses a "task port"-like pattern from elm-concurrent-task for advanced task composition
  - Maintains separation between pages while sharing common wallet state


# Data Flow

1.  App initializes with protocol parameters and wallet discovery
2.  User connects wallet to enable transaction signing
3.  Pages can request wallet operations via ports
4.  Task system handles advanced operations like metadata fetching with caching


# State Management

The main model contains:

  - Global application state (wallet connection, protocol params)
  - Page-specific state
  - Cached governance data (proposals, DReps, etc)
  - Task pool for elm-concurrent-task operations
  - Error tracking

Each page maintains its own state and can communicate up through MsgToParent patterns.

-}

import Api exposing (ActiveProposal, CcInfo, DrepInfo, PoolInfo, ProtocolParams)
import AppUrl exposing (AppUrl)
import Browser
import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address as Address exposing (CredentialHash, NetworkId(..))
import Cardano.Cip30 as Cip30 exposing (WalletDescriptor)
import Cardano.Cip95 as Cip95
import Cardano.Gov as Gov
import Cardano.Transaction as Transaction exposing (Transaction)
import Cardano.TxIntent
import Cardano.Utxo as Utxo exposing (Output, TransactionId)
import ConcurrentTask exposing (ConcurrentTask)
import ConcurrentTask.Extra
import Dict exposing (Dict)
import Footer
import Header
import Helper exposing (PreconfVoter)
import Html exposing (Html, div, text)
import Html.Attributes as HA
import Html.Events exposing (preventDefaultOn)
import Http
import Json.Decode as JD exposing (Decoder, Value)
import Page.Disclaimer
import Page.MultisigRegistration
import Page.Pdf
import Page.Preparation exposing (JsonLdContexts)
import Page.Signing
import Platform.Cmd as Cmd
import ProposalMetadata exposing (ProposalMetadata)
import RemoteData exposing (WebData)
import ScriptInfo exposing (ScriptInfo)
import Storage
import Url


type alias Flags =
    { url : String
    , jsonLdContexts : JsonLdContexts
    , db : Value
    , networkId : Int
    , ipfsPreconfig : { label : String, description : String }
    , voterPreconfig : List PreconfVoter
    }


main : Program Flags Model Msg
main =
    -- The main entry point of our app
    -- More info about that in the Browser package docs:
    -- https://package.elm-lang.org/packages/elm/browser/latest/
    Browser.element
        { init = init
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ fromWallet WalletMsg
                    , onUrlChange (locationHrefToRoute >> UrlChanged)
                    , gotRationaleAsFile GotRationaleAsFile
                    , gotPdfAsFile GotPdfAsFile
                    , ConcurrentTask.onProgress
                        { send = sendTask
                        , receive = receiveTask
                        , onProgress = OnTaskProgress
                        }
                        model.taskPool
                    ]
        , view = view
        }


port toWallet : Value -> Cmd msg


port fromWallet : (Value -> msg) -> Sub msg


port onUrlChange : (String -> msg) -> Sub msg


port pushUrl : String -> Cmd msg


port jsonRationaleToFile : { fileContent : String, fileName : String } -> Cmd msg


port gotRationaleAsFile : (Value -> msg) -> Sub msg


port pdfBytesToFile : { fileContentHex : String, fileName : String } -> Cmd msg


port gotPdfAsFile : (Value -> msg) -> Sub msg



-- Task port thingy


port sendTask : Value -> Cmd msg


port receiveTask : (Value -> msg) -> Sub msg



-- #########################################################
-- MODEL
-- #########################################################


type alias Model =
    { page : Page
    , appUrl : AppUrl
    , mobileMenuIsOpen : Bool
    , walletDropdownIsOpen : Bool
    , networkDropdownIsOpen : Bool
    , walletsDiscovered : List WalletDescriptor
    , wallet : Maybe Cip30.Wallet
    , walletUtxos : Maybe (Utxo.RefDict Output)
    , walletDrepId : Maybe (Bytes CredentialHash)
    , protocolParams : Maybe ProtocolParams
    , epoch : WebData Int
    , proposals : WebData (Dict String ActiveProposal)
    , scriptsInfo : Dict String ScriptInfo
    , drepsInfo : Dict String DrepInfo
    , ccsInfo : Dict String CcInfo
    , poolsInfo : Dict String PoolInfo
    , jsonLdContexts : JsonLdContexts
    , taskPool : ConcurrentTask.Pool Msg String TaskCompleted
    , db : Value
    , networkId : NetworkId
    , ipfsPreconfig : { label : String, description : String }
    , voterPreconfig : List PreconfVoter
    , errors : List String
    }


type Page
    = LandingPage
    | PreparationPage Page.Preparation.Model
    | SigningPage Page.Signing.Model
    | MultisigRegistrationPage Page.MultisigRegistration.Model
    | PdfPage Page.Pdf.Model
    | DisclaimerPage


type TaskCompleted
    = GotProposalMetadataTask String (Result String ProposalMetadata)
    | PreparationTaskCompleted Page.Preparation.TaskCompleted


init : Flags -> ( Model, Cmd Msg )
init { url, jsonLdContexts, db, networkId, ipfsPreconfig, voterPreconfig } =
    let
        networkIdTyped =
            Address.networkIdFromInt networkId |> Maybe.withDefault Testnet

        config =
            ModelConfig jsonLdContexts db networkIdTyped ipfsPreconfig voterPreconfig
    in
    initHelper (locationHrefToRoute url) config


initHelper : Route -> ModelConfig -> ( Model, Cmd Msg )
initHelper route config =
    let
        ( model, cmd ) =
            handleUrlChange route (initialModel config)
    in
    ( model
    , Cmd.batch
        [ cmd
        , toWallet (Cip30.encodeRequest Cip30.discoverWallets)
        , Api.defaultApiProvider.loadProtocolParams model.networkId GotProtocolParams
        ]
    )


type alias ModelConfig =
    { jsonLdContexts : JsonLdContexts
    , db : Value
    , networkId : NetworkId
    , ipfsPreconfig : { label : String, description : String }
    , voterPreconfig : List PreconfVoter
    }


initialModel : ModelConfig -> Model
initialModel { jsonLdContexts, db, networkId, ipfsPreconfig, voterPreconfig } =
    { page = LandingPage
    , appUrl = routeToAppUrl RouteLanding
    , mobileMenuIsOpen = False
    , walletDropdownIsOpen = False
    , networkDropdownIsOpen = False
    , walletsDiscovered = []
    , wallet = Nothing
    , walletUtxos = Nothing
    , walletDrepId = Nothing
    , protocolParams = Nothing
    , epoch = RemoteData.NotAsked
    , proposals = RemoteData.NotAsked
    , scriptsInfo = Dict.empty
    , drepsInfo = Dict.empty
    , ccsInfo = Dict.empty
    , poolsInfo = Dict.empty
    , jsonLdContexts = jsonLdContexts
    , taskPool = ConcurrentTask.pool
    , db = db
    , networkId = networkId
    , ipfsPreconfig = ipfsPreconfig
    , voterPreconfig = voterPreconfig
    , errors = []
    }



-- #########################################################
-- UPDATE
-- #########################################################


type Msg
    = NoMsg
    | UrlChanged Route
    | WalletMsg Value
    | GotProtocolParams (Result Http.Error ProtocolParams)
    | GotEpoch (Result Http.Error Int)
    | GotProposals (Result Http.Error (List ActiveProposal))
      -- Header
    | ToggleMobileMenu
    | ToggleWalletDropdown
    | ToggleNetworkDropdown
    | ConnectWalletClicked { id : String, supportedExtensions : List Int }
    | DisconnectWalletClicked
    | NetworkChanged NetworkId
      -- Preparation page
    | PreparationPageMsg Page.Preparation.Msg
    | GotPdfAsFile Value
    | GotRationaleAsFile Value
      -- Signing page
    | SigningPageMsg Page.Signing.Msg
      -- Multisig DRep registration page
    | MultisigPageMsg Page.MultisigRegistration.Msg
      -- PDF page
    | PdfPageMsg Page.Pdf.Msg
      -- Task port
    | OnTaskProgress ( ConcurrentTask.Pool Msg String TaskCompleted, Cmd Msg )
    | OnTaskComplete (ConcurrentTask.Response String TaskCompleted)


type Route
    = RouteLanding
    | RoutePreparation { networkId : NetworkId }
    | RouteSigning { networkId : NetworkId, expectedSigners : List { keyName : String, keyHash : Bytes CredentialHash }, tx : Maybe Transaction }
    | RouteMultisigRegistration
    | RoutePdf
    | RouteDisclaimer
    | Route404


link : Route -> List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
link route attrs children =
    Html.a
        (preventDefaultOn "click" (linkClickDecoder route)
            :: HA.href (AppUrl.toString <| routeToAppUrl <| route)
            :: attrs
        )
        children


linkClickDecoder : Route -> Decoder ( Msg, Bool )
linkClickDecoder route =
    -- Custom decoder on link clicks to not overwrite expected behaviors for click with modifiers.
    -- For example, Ctrl+Click should open in a new tab, and Shift+Click in a new window.
    JD.map4
        (\ctrl meta shift wheel ->
            if ctrl || meta || shift || wheel /= 0 then
                ( NoMsg, False )

            else
                ( UrlChanged route, True )
        )
        (JD.field "ctrlKey" JD.bool)
        (JD.field "metaKey" JD.bool)
        (JD.field "shiftKey" JD.bool)
        (JD.field "button" JD.int)


locationHrefToRoute : String -> Route
locationHrefToRoute locationHref =
    case Url.fromString locationHref |> Maybe.map AppUrl.fromUrl of
        Nothing ->
            Route404

        Just { path, queryParameters, fragment } ->
            let
                networkId =
                    Dict.get "networkId" queryParameters
                        |> Maybe.andThen List.head
                        |> Maybe.andThen networkIdFromString
                        |> Maybe.withDefault Testnet
            in
            case path of
                [] ->
                    RouteLanding

                [ "page", "preparation" ] ->
                    RoutePreparation { networkId = networkId }

                [ "page", "signing" ] ->
                    RouteSigning
                        { networkId = networkId
                        , expectedSigners =
                            Dict.get "signer" queryParameters
                                |> Maybe.withDefault []
                                |> List.filterMap
                                    (\stringKeySigner ->
                                        case String.split ";" stringKeySigner of
                                            [ keyName, keyBytes ] ->
                                                Just { keyName = keyName, keyHash = Bytes.fromHexUnchecked keyBytes }

                                            _ ->
                                                Nothing
                                    )
                        , tx =
                            Maybe.andThen Bytes.fromHex fragment
                                |> Maybe.andThen Transaction.deserialize
                        }

                [ "page", "registration" ] ->
                    RouteMultisigRegistration

                [ "page", "pdf" ] ->
                    RoutePdf

                [ "page", "disclaimer" ] ->
                    RouteDisclaimer

                _ ->
                    Route404


routeToAppUrl : Route -> AppUrl
routeToAppUrl route =
    case route of
        Route404 ->
            AppUrl.fromPath [ "404" ]

        RouteLanding ->
            AppUrl.fromPath []

        RoutePreparation { networkId } ->
            { path = [ "page", "preparation" ]
            , queryParameters = Dict.singleton "networkId" [ networkIdToString networkId ]
            , fragment = Nothing
            }

        RouteSigning { networkId, expectedSigners, tx } ->
            { path = [ "page", "signing" ]
            , queryParameters =
                Dict.fromList
                    [ ( "networkId", [ networkIdToString networkId ] )
                    , ( "signer", List.map (\{ keyName, keyHash } -> keyName ++ ";" ++ Bytes.toHex keyHash) expectedSigners )
                    ]
            , fragment = Maybe.map (Bytes.toHex << Transaction.serialize) tx
            }

        RouteMultisigRegistration ->
            AppUrl.fromPath [ "page", "registration" ]

        RoutePdf ->
            AppUrl.fromPath [ "page", "pdf" ]

        RouteDisclaimer ->
            AppUrl.fromPath [ "page", "disclaimer" ]


networkIdToString : NetworkId -> String
networkIdToString networkId =
    case networkId of
        Mainnet ->
            "Mainnet"

        Testnet ->
            "Preview"


networkIdFromString : String -> Maybe NetworkId
networkIdFromString str =
    case str of
        "Mainnet" ->
            Just Mainnet

        "Preview" ->
            Just Testnet

        _ ->
            Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ToggleNetworkDropdown, _ ) ->
            ( { model
                | networkDropdownIsOpen = not model.networkDropdownIsOpen
                , walletDropdownIsOpen = False -- Close wallet dropdown when toggling network
              }
            , Cmd.none
            )

        ( NetworkChanged newNet, _ ) ->
            let
                updatedModel =
                    { model
                        | networkId = newNet
                        , networkDropdownIsOpen = False -- Close dropdown after selection
                        , proposals = RemoteData.NotAsked
                    }
            in
            case model.page of
                PreparationPage _ ->
                    handleUrlChange (RoutePreparation { networkId = newNet }) updatedModel

                SigningPage _ ->
                    handleUrlChange (RouteSigning { networkId = newNet, tx = Nothing, expectedSigners = [] }) updatedModel

                _ ->
                    ( updatedModel, Cmd.none )

        ( NoMsg, _ ) ->
            ( model, Cmd.none )

        ( UrlChanged route, _ ) ->
            let
                oldUrl =
                    model.appUrl

                newUrl =
                    routeToAppUrl route
            in
            -- If the new URL is exactly the same, ignore
            if newUrl == oldUrl then
                ( model, Cmd.none )

            else
                handleUrlChange route model

        ( GotProtocolParams result, _ ) ->
            case result of
                Ok params ->
                    ( { model | protocolParams = Just params }, Cmd.none )

                Err err ->
                    ( { model | errors = Debug.toString err :: model.errors }, Cmd.none )

        ( WalletMsg value, _ ) ->
            case JD.decodeValue walletResponseDecoder value of
                Ok response ->
                    handleWalletResponse response model

                Err decodingError ->
                    ( { model | errors = JD.errorToString decodingError :: model.errors }
                    , Cmd.none
                    )

        ( PreparationPageMsg pageMsg, { page } ) ->
            case page of
                PreparationPage pageModel ->
                    let
                        loadedWallet =
                            case ( model.wallet, model.walletUtxos ) of
                                ( Just wallet, Just utxos ) ->
                                    Just { wallet = wallet, utxos = utxos }

                                _ ->
                                    Nothing

                        ctx =
                            { wrapMsg = PreparationPageMsg
                            , db = model.db
                            , proposals = model.proposals
                            , scriptsInfo = model.scriptsInfo
                            , drepsInfo = model.drepsInfo
                            , ccsInfo = model.ccsInfo
                            , poolsInfo = model.poolsInfo
                            , loadedWallet = loadedWallet
                            , drepId = model.walletDrepId
                            , jsonLdContexts = model.jsonLdContexts
                            , jsonRationaleToFile = jsonRationaleToFile
                            , pdfBytesToFile = pdfBytesToFile
                            , costModels = Maybe.map .costModels model.protocolParams
                            , networkId = model.networkId
                            }

                        ( newPageModel, cmds, msgToParent ) =
                            Page.Preparation.update ctx pageMsg pageModel
                    in
                    updateModelWithPrepToParentMsg msgToParent { model | page = PreparationPage newPageModel }
                        |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, cmds ])

                _ ->
                    ( model, Cmd.none )

        ( GotPdfAsFile file, { page } ) ->
            case page of
                PreparationPage pageModel ->
                    Page.Preparation.pinPdfFile file pageModel
                        |> Tuple.mapFirst (\newPageModel -> { model | page = PreparationPage newPageModel })
                        |> Tuple.mapSecond (Cmd.map PreparationPageMsg)

                _ ->
                    ( model, Cmd.none )

        ( GotRationaleAsFile file, { page } ) ->
            case page of
                PreparationPage pageModel ->
                    Page.Preparation.pinRationaleFile file pageModel
                        |> Tuple.mapFirst (\newPageModel -> { model | page = PreparationPage newPageModel })
                        |> Tuple.mapSecond (Cmd.map PreparationPageMsg)

                _ ->
                    ( model, Cmd.none )

        ( SigningPageMsg pageMsg, { page } ) ->
            case page of
                SigningPage pageModel ->
                    let
                        ( walletSignTx, walletSubmitTx ) =
                            case model.wallet of
                                Nothing ->
                                    ( \_ -> Cmd.none
                                    , \_ -> Cmd.none
                                    )

                                Just wallet ->
                                    ( \tx -> toWallet (Cip30.encodeRequest (Cip30.signTx wallet { partialSign = True } tx))
                                    , \tx -> toWallet (Cip30.encodeRequest (Cip30.submitTx wallet tx))
                                    )

                        ctx =
                            { wrapMsg = SigningPageMsg
                            , wallet = model.wallet
                            , walletSignTx = walletSignTx
                            , walletSubmitTx = walletSubmitTx
                            }
                    in
                    Page.Signing.update ctx pageMsg pageModel
                        |> Tuple.mapFirst (\newPageModel -> { model | page = SigningPage newPageModel })

                _ ->
                    ( model, Cmd.none )

        ( MultisigPageMsg pageMsg, { page } ) ->
            case page of
                MultisigRegistrationPage pageModel ->
                    let
                        ctx =
                            { wrapMsg = MultisigPageMsg
                            , wallet =
                                case ( model.wallet, model.walletUtxos ) of
                                    ( Just wallet, Just utxos ) ->
                                        Just { wallet = wallet, utxos = utxos }

                                    _ ->
                                        Nothing
                            , costModels = Maybe.map .costModels model.protocolParams
                            }
                    in
                    Page.MultisigRegistration.update ctx pageMsg pageModel
                        |> Tuple.mapFirst (\newPageModel -> { model | page = MultisigRegistrationPage newPageModel })

                _ ->
                    ( model, Cmd.none )

        ( PdfPageMsg pageMsg, { page } ) ->
            case page of
                PdfPage pageModel ->
                    let
                        ctx =
                            { wrapMsg = PdfPageMsg
                            }
                    in
                    Page.Pdf.update ctx pageMsg pageModel
                        |> Tuple.mapFirst (\newPageModel -> { model | page = PdfPage newPageModel })

                _ ->
                    ( model, Cmd.none )

        ( GotEpoch result, _ ) ->
            case result of
                Err httpError ->
                    ( { model | epoch = RemoteData.Failure httpError }
                    , Cmd.none
                    )

                Ok epoch ->
                    ( { model | epoch = RemoteData.Success epoch }
                    , Api.defaultApiProvider.loadGovProposals model.networkId epoch GotProposals
                    )

        ( GotProposals result, _ ) ->
            case result of
                Err httpError ->
                    ( { model | proposals = RemoteData.Failure httpError }
                    , Cmd.none
                    )

                Ok activeProposals ->
                    let
                        currentEpoch =
                            RemoteData.withDefault 0 model.epoch

                        proposalsList =
                            List.map (\p -> ( Gov.actionIdToString p.id, p )) activeProposals
                                -- deduplicate proposals
                                |> Dict.fromList
                                |> Dict.toList
                                -- only keep those that aren’t expired when we receive them
                                |> List.filter (\( _, p ) -> p.epoch_validity.end >= currentEpoch)

                        completeReadProposalMetadataTask : ActiveProposal -> ConcurrentTask x TaskCompleted
                        completeReadProposalMetadataTask { id, metadataHash, metadataUrl } =
                            Api.defaultApiProvider.loadProposalMetadata metadataUrl
                                |> Storage.cacheWrap
                                    { db = model.db, storeName = "proposalMetadata" }
                                    ProposalMetadata.decoder
                                    ProposalMetadata.encode
                                    { key = metadataHash }
                                |> ConcurrentTask.Extra.toResult
                                |> ConcurrentTask.map (GotProposalMetadataTask <| Gov.actionIdToString id)

                        ( newPool, cmds ) =
                            ConcurrentTask.Extra.attemptEach { pool = model.taskPool, send = sendTask, onComplete = OnTaskComplete }
                                (List.map completeReadProposalMetadataTask activeProposals)
                    in
                    ( { model
                        | taskPool = newPool
                        , proposals = RemoteData.Success <| Dict.fromList proposalsList
                      }
                    , Cmd.batch cmds
                    )

        ( OnTaskProgress ( taskPool, cmd ), _ ) ->
            ( { model | taskPool = taskPool }, cmd )

        ( OnTaskComplete taskCompleted, _ ) ->
            handleCompletedTask taskCompleted model

        ( ToggleMobileMenu, _ ) ->
            ( { model | mobileMenuIsOpen = not model.mobileMenuIsOpen }, Cmd.none )

        ( ToggleWalletDropdown, _ ) ->
            ( { model
                | walletDropdownIsOpen = not model.walletDropdownIsOpen
                , networkDropdownIsOpen = False
              }
            , Cmd.none
            )

        ( ConnectWalletClicked { id, supportedExtensions }, _ ) ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.enableWallet { id = id, extensions = List.filter (\ext -> ext == 95) supportedExtensions, watchInterval = Just 5 })) )

        ( DisconnectWalletClicked, _ ) ->
            ( { model | wallet = Nothing, walletUtxos = Nothing, walletDrepId = Nothing }
            , Cmd.none
            )


handleUrlChange : Route -> Model -> ( Model, Cmd Msg )
handleUrlChange route model =
    let
        appUrl =
            routeToAppUrl route

        pushUrlCmd =
            if routeToAppUrl route == model.appUrl then
                Cmd.none

            else
                pushUrl <| AppUrl.toString appUrl
    in
    case route of
        Route404 ->
            Debug.todo "Handle 404 page"

        RouteLanding ->
            ( { model
                | errors = []
                , page = LandingPage
                , appUrl = appUrl
              }
            , pushUrlCmd
            )

        RoutePreparation { networkId } ->
            let
                newModel =
                    { model
                        | errors = []
                        , page = PreparationPage <| Page.Preparation.init model.ipfsPreconfig
                        , appUrl = appUrl
                    }
            in
            if networkId /= model.networkId then
                initHelper route
                    { jsonLdContexts = model.jsonLdContexts
                    , db = model.db
                    , networkId = networkId
                    , ipfsPreconfig = model.ipfsPreconfig
                    , voterPreconfig = model.voterPreconfig
                    }

            else if RemoteData.isSuccess model.proposals then
                ( newModel, pushUrlCmd )

            else
                ( { newModel | proposals = RemoteData.Loading }
                , Cmd.batch
                    [ pushUrlCmd
                    , Api.defaultApiProvider.queryEpoch model.networkId GotEpoch
                    ]
                )

        RouteSigning { networkId, expectedSigners, tx } ->
            if networkId /= model.networkId then
                initHelper route
                    { jsonLdContexts = model.jsonLdContexts
                    , db = model.db
                    , networkId = networkId
                    , ipfsPreconfig = model.ipfsPreconfig
                    , voterPreconfig = model.voterPreconfig
                    }

            else
                ( { model
                    | errors = []
                    , page = SigningPage <| Page.Signing.initialModel expectedSigners tx
                    , appUrl = appUrl
                  }
                , pushUrlCmd
                )

        RouteMultisigRegistration ->
            ( { model
                | errors = []
                , page = MultisigRegistrationPage Page.MultisigRegistration.initialModel
                , appUrl = appUrl
              }
            , pushUrlCmd
            )

        RoutePdf ->
            ( { model
                | errors = []
                , page = PdfPage Page.Pdf.initialModel
                , appUrl = appUrl
              }
            , pushUrlCmd
            )

        RouteDisclaimer ->
            ( { model
                | errors = []
                , page = DisclaimerPage
                , appUrl = appUrl
              }
            , pushUrlCmd
            )


type ApiResponse
    = Cip30ApiResponse Cip30.ApiResponse
    | Cip95ApiResponse Cip95.ApiResponse


walletResponseDecoder : Decoder (Cip30.Response ApiResponse)
walletResponseDecoder =
    Cip30.responseDecoder <|
        Dict.fromList
            [ ( 30, \method -> JD.map Cip30ApiResponse (Cip30.apiDecoder method) )
            , ( 95, \method -> JD.map Cip95ApiResponse (Cip95.apiDecoder method) )
            ]


handleWalletResponse : Cip30.Response ApiResponse -> Model -> ( Model, Cmd Msg )
handleWalletResponse response model =
    case response of
        -- We just discovered available wallets
        Cip30.AvailableWallets wallets ->
            ( { model | walletsDiscovered = wallets }
            , Cmd.none
            )

        -- We just connected to the wallet, let’s ask for all that is still missing
        Cip30.EnabledWallet wallet ->
            ( { model | wallet = Just wallet, walletUtxos = Nothing }
            , Cmd.batch
                -- Retrieve UTXOs from the main wallet
                [ Cip30.getUtxos wallet { amount = Nothing, paginate = Nothing }
                    |> Cip30.encodeRequest
                    |> toWallet

                -- Retrieve the DRep ID of the wallet if it supports CIP-95
                , if List.member 95 (Cip30.walletDescriptor wallet).supportedExtensions then
                    Cip95.getPubDRepKey wallet
                        |> Cip30.encodeRequest
                        |> toWallet

                  else
                    Cmd.none
                ]
            )

        -- We just received the utxos
        Cip30.ApiResponse _ (Cip30ApiResponse (Cip30.WalletUtxos utxos)) ->
            ( { model | walletUtxos = Just (Utxo.refDictFromList utxos) }
            , Cmd.none
            )

        -- We just received the DRep ID of the wallet
        Cip30.ApiResponse _ (Cip95ApiResponse (Cip95.DrepKey drepKey)) ->
            ( { model | walletDrepId = Just <| Bytes.blake2b224 drepKey }
            , Cmd.none
            )

        -- The wallet just signed a Tx
        Cip30.ApiResponse _ (Cip30ApiResponse (Cip30.SignedTx vkeyWitnesses)) ->
            case model.page of
                SigningPage pageModel ->
                    ( { model | page = SigningPage <| Page.Signing.addWalletSignatures vkeyWitnesses pageModel }
                    , Cmd.none
                    )

                -- No other page expects to receive a Tx signature
                _ ->
                    ( model, Cmd.none )

        -- The wallet just submitted a Tx
        Cip30.ApiResponse _ (Cip30ApiResponse (Cip30.SubmittedTx txId)) ->
            case model.page of
                SigningPage pageModel ->
                    ( { model
                        | page = SigningPage <| Page.Signing.recordSubmittedTx txId pageModel

                        -- Update the wallet’s UTxOs
                        , walletUtxos =
                            Maybe.map2 updateWalletUtxosWithTx
                                (Page.Signing.getTxInfo pageModel)
                                model.walletUtxos
                      }
                    , Cmd.none
                    )

                -- No other page expects to submit a Tx
                _ ->
                    ( model, Cmd.none )

        Cip30.ApiResponse _ _ ->
            ( { model | errors = "TODO: unhandled CIP30 response yet" :: model.errors }
            , Cmd.none
            )

        -- Received an error message from the wallet
        Cip30.ApiError { info } ->
            ( { model
                -- TODO: ideally, each port to wallet should know
                -- how to redirect to a new message in fromWallet.
                -- That would need a bit of thinking to figure out a good way.
                -- For now, I mostly observed errors at Tx signing/submission
                | page = resetSigningStep info model.page
              }
            , Cmd.none
            )

        -- Unknown type of message received from the wallet
        Cip30.UnhandledResponseType error ->
            ( { model | errors = error :: model.errors }
            , Cmd.none
            )


{-| Update the known state of the wallet’s UTxOs
knowing the given transaction was just submitted to the network.
-}
updateWalletUtxosWithTx : { tx : Transaction, txId : Bytes TransactionId } -> Utxo.RefDict Output -> Utxo.RefDict Output
updateWalletUtxosWithTx { tx, txId } utxos =
    (Cardano.TxIntent.updateLocalState txId tx utxos).updatedState


updateModelWithPrepToParentMsg : Maybe Page.Preparation.MsgToParent -> Model -> ( Model, Cmd Msg )
updateModelWithPrepToParentMsg msgToParent model =
    case msgToParent of
        Nothing ->
            ( model, Cmd.none )

        Just (Page.Preparation.CacheScriptInfo scriptInfo) ->
            ( { model | scriptsInfo = Dict.insert (Bytes.toHex scriptInfo.scriptHash) scriptInfo model.scriptsInfo }
            , Cmd.none
            )

        Just (Page.Preparation.CacheDrepInfo drepInfo) ->
            ( { model | drepsInfo = Dict.insert (Bytes.toHex <| Address.extractCredentialHash drepInfo.credential) drepInfo model.drepsInfo }
            , Cmd.none
            )

        Just (Page.Preparation.CacheCcInfo ccInfo) ->
            ( { model | ccsInfo = Dict.insert (Bytes.toHex <| Address.extractCredentialHash ccInfo.hotCred) ccInfo model.ccsInfo }
            , Cmd.none
            )

        Just (Page.Preparation.CachePoolInfo poolInfo) ->
            ( { model | poolsInfo = Dict.insert (Bytes.toHex poolInfo.pool) poolInfo model.poolsInfo }
            , Cmd.none
            )

        Just (Page.Preparation.RunTask task) ->
            ConcurrentTask.attempt { pool = model.taskPool, send = sendTask, onComplete = OnTaskComplete }
                (ConcurrentTask.map PreparationTaskCompleted task)
                |> Tuple.mapFirst (\newTaskPool -> { model | taskPool = newTaskPool })


{-| Helper function to reset the signing step of the Preparation.
-}
resetSigningStep : String -> Page -> Page
resetSigningStep error page =
    case page of
        SigningPage pageModel ->
            SigningPage <| Page.Signing.resetSubmission error pageModel

        _ ->
            page


handleCompletedTask : ConcurrentTask.Response String TaskCompleted -> Model -> ( Model, Cmd Msg )
handleCompletedTask response model =
    case ( response, model.page ) of
        ( ConcurrentTask.Error error, _ ) ->
            ( { model | errors = error :: model.errors }, Cmd.none )

        ( ConcurrentTask.UnexpectedError error, _ ) ->
            ( { model | errors = Debug.toString error :: model.errors }, Cmd.none )

        ( ConcurrentTask.Success (GotProposalMetadataTask id result), _ ) ->
            let
                updateMetadata maybeProposal =
                    case ( maybeProposal, result ) of
                        ( Nothing, _ ) ->
                            Nothing

                        ( Just p, Ok metadata ) ->
                            Just { p | metadata = RemoteData.Success metadata }

                        ( Just p, Err error ) ->
                            Just { p | metadata = RemoteData.Failure error }
            in
            ( { model | proposals = RemoteData.map (\ps -> Dict.update id updateMetadata ps) model.proposals }
            , Cmd.none
            )

        ( ConcurrentTask.Success (PreparationTaskCompleted taskCompleted), PreparationPage pageModel ) ->
            let
                ( newPageModel, cmds, msgToParent ) =
                    Page.Preparation.handleTaskCompleted taskCompleted pageModel
            in
            updateModelWithPrepToParentMsg msgToParent { model | page = PreparationPage newPageModel }
                |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, Cmd.map PreparationPageMsg cmds ])

        ( ConcurrentTask.Success (PreparationTaskCompleted _), _ ) ->
            ( model, Cmd.none )



-- #########################################################
-- VIEW
-- #########################################################


view : Model -> Html Msg
view model =
    let
        backgroundStyle =
            if model.page == LandingPage then
                HA.style "background" "transparent"

            else
                HA.style "background" "#E2E8F0"
    in
    div
        [ HA.style "min-height" "100vh"
        , HA.style "position" "relative"
        , backgroundStyle
        ]
        [ -- Gradient circles (only on landing page)
          if model.page == LandingPage then
            viewGradientBackgrounds

          else
            text ""
        , viewHeader model
        , viewContent model
        , viewErrors model.errors
        , Footer.view
            { copyright = "© 2025 Cardano Stiftung"
            , githubLink = "https://github.com/cardano-foundation/cardano-governance-voting-tool"
            , disclaimerLink = link RouteDisclaimer
            }
        ]


viewGradientBackgrounds : Html Msg
viewGradientBackgrounds =
    div
        [ HA.style "position" "absolute"
        , HA.style "right" "0"
        , HA.style "top" "0"
        , HA.style "z-index" "-1"
        ]
        [ -- Blue gradient circle
          div
            [ HA.style "width" "75vw"
            , HA.style "height" "75vh"
            , HA.style "border-radius" "75rem"
            , HA.style "background" "linear-gradient(270deg, #00e0ff, #0084ff 100%)"
            , HA.style "filter" "blur(128px)"
            , HA.style "transform-origin" "center center"
            , HA.style "position" "absolute"
            , HA.style "right" "0vh"
            , HA.style "top" "-30vh"
            ]
            []

        -- Red-Yellow gradient circle
        , div
            [ HA.style "width" "22rem"
            , HA.style "height" "22rem"
            , HA.style "border-radius" "22rem"
            , HA.style "background" "linear-gradient(90deg, #d1085c -0.01%, #ffad0f 55.09%)"
            , HA.style "filter" "blur(4rem)"
            , HA.style "transform-origin" "center center"
            , HA.style "position" "absolute"
            , HA.style "right" "0vh"
            , HA.style "top" "5vh"
            ]
            []
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    let
        navigationItems =
            [ { label = "Home"
              , link = link RouteLanding
              , isActive = model.page == LandingPage
              }
            , { label = "Vote Preparation"
              , link = link <| RoutePreparation { networkId = model.networkId }
              , isActive =
                    case model.page of
                        PreparationPage _ ->
                            True

                        _ ->
                            False
              }
            , { label = "PDFs"
              , link = link RoutePdf
              , isActive =
                    case model.page of
                        PdfPage _ ->
                            True

                        _ ->
                            False
              }
            ]

        walletConnectorState =
            { walletDropdownIsOpen = model.walletDropdownIsOpen
            , walletsDiscovered = model.walletsDiscovered
            , wallet = model.wallet
            }

        walletConnectorMsgs =
            { toggleWalletDropdown = ToggleWalletDropdown
            , connectWalletClicked = ConnectWalletClicked
            , disconnectWalletClicked = DisconnectWalletClicked
            }
    in
    Header.view
        { mobileMenuIsOpen = model.mobileMenuIsOpen
        , toggleMobileMenu = ToggleMobileMenu
        , networkDropdownIsOpen = model.networkDropdownIsOpen
        , toggleNetworkDropdown = ToggleNetworkDropdown
        , walletConnector = walletConnectorState
        , walletConnectorMsgs = walletConnectorMsgs
        , logoLink = link RouteLanding
        , navigationItems = navigationItems
        , networkId = model.networkId
        , onNetworkChange = NetworkChanged
        }


viewContent : Model -> Html Msg
viewContent model =
    case model.page of
        LandingPage ->
            viewLandingPage model.networkId

        DisclaimerPage ->
            Page.Disclaimer.view

        PreparationPage prepModel ->
            let
                loadedWallet =
                    case ( model.wallet, model.walletUtxos ) of
                        ( Just wallet, Just utxos ) ->
                            Just { wallet = wallet, utxos = utxos }

                        _ ->
                            Nothing
            in
            Page.Preparation.view
                { wrapMsg = PreparationPageMsg
                , walletsDiscovered = model.walletsDiscovered
                , loadedWallet = loadedWallet
                , drepId = model.walletDrepId
                , epoch = RemoteData.toMaybe model.epoch
                , proposals = model.proposals
                , jsonLdContexts = model.jsonLdContexts
                , costModels = Maybe.map .costModels model.protocolParams
                , networkId = model.networkId
                , changeNetworkLink =
                    \networkId ->
                        link (RoutePreparation { networkId = networkId }) []
                , signingLink =
                    \tx expectedSigners ->
                        link (RouteSigning { networkId = model.networkId, tx = Just tx, expectedSigners = expectedSigners }) []
                , ipfsPreconfig = model.ipfsPreconfig
                , voterPreconfig = model.voterPreconfig
                }
                prepModel

        SigningPage signingModel ->
            Page.Signing.view
                { wrapMsg = SigningPageMsg
                , wallet = model.wallet
                , networkId = model.networkId
                }
                signingModel

        MultisigRegistrationPage pageModel ->
            Page.MultisigRegistration.view
                { wrapMsg = MultisigPageMsg
                , wallet = model.wallet
                , signingLink =
                    \tx expectedSigners ->
                        link (RouteSigning { networkId = model.networkId, tx = Just tx, expectedSigners = expectedSigners }) []
                }
                pageModel

        PdfPage pageModel ->
            Page.Pdf.view
                { wrapMsg = PdfPageMsg
                }
                pageModel


viewLandingPage : NetworkId -> Html Msg
viewLandingPage networkId =
    div [ HA.class "container mx-auto px-4" ]
        [ div [ HA.style "max-width" "800px", HA.style "margin" "0 auto" ]
            [ Html.h2
                [ HA.style "font-size" "min(4.5rem, 10vw)"
                , HA.style "line-height" "1.2"
                , HA.style "margin-top" "3rem"
                , HA.style "margin-bottom" "1.5rem"
                , HA.style "font-weight" "600"
                , HA.style "color" "#272727"
                ]
                [ text "Cardano Governance Voting Tool" ]
            , Html.p
                [ HA.style "font-size" "min(1.5rem, 5vw)"
                , HA.style "line-height" "1.5"
                , HA.style "margin-bottom" "2rem"
                , HA.style "color" "#333"
                ]
                [ text "A simple tool to help every Cardano stakeholder participate in on-chain governance with confidence." ]
            , Html.p
                [ HA.style "line-height" "1.6"
                , HA.style "margin-bottom" "2.5rem"
                , HA.style "color" "#555"
                ]
                [ text "Create, sign, and submit governance votes with proper rationale documentation. Generate formatted PDFs for transparency and record-keeping." ]
            , Html.p [ HA.style "margin-bottom" "4rem" ]
                [ link (RoutePreparation { networkId = networkId })
                    [ HA.class "inline-block" ]
                    [ Helper.viewButton "Start Voting Process" NoMsg ]
                ]
            , div
                [ HA.style "margin-bottom" "4rem"
                ]
                [ Html.h3
                    [ HA.style "font-size" "1.5rem"
                    , HA.style "font-weight" "600"
                    , HA.style "margin-bottom" "1.5rem"
                    , HA.style "color" "#272727"
                    ]
                    [ text "Cardano Governance Ecosystem" ]
                , Html.p
                    [ HA.style "font-size" "1.1rem"
                    , HA.style "margin-bottom" "1.5rem"
                    , HA.style "color" "#444"
                    ]
                    [ text "While this tool focuses specifically on voting, you may also find value in other platforms that support different aspects of Cardano governance. Below is a non-exhaustive list of other projects, each with its own purpose and features:" ]
                , div
                    [ HA.style "display" "grid"
                    , HA.style "grid-template-columns" "repeat(auto-fill, minmax(240px, 1fr))"
                    , HA.style "gap" "1rem"
                    ]
                    (List.map viewGovernanceTool governanceTools)
                ]
            ]
        ]


governanceTools : List { name : String, url : String, description : String }
governanceTools =
    [ { name = "gov.tools"
      , url = "https://gov.tools"
      , description = "The original governance platform for Cardano. Register as a DRep, delegate your voting power, explore proposals, and cast your votes."
      }
    , { name = "tempo.vote"
      , url = "https://tempo.vote"
      , description = "An alternative comprehensive platform aiming to support all aspects of Cardano governance."
      }
    , { name = "governancespace.com"
      , url = "https://governancespace.com"
      , description = "Another alternative comprehensive platform for Cardano governance, still WIP."
      }
    , { name = "forum.cardano.org"
      , url = "https://forum.cardano.org"
      , description = "A community forum for discussions on all things Cardano, including governance topics."
      }
    , { name = "1694.io"
      , url = "https://1694.io"
      , description = "A knowledge hub dedicated to Cardano's governance processes."
      }
    , { name = "changwatch.com"
      , url = "https://changwatch.com"
      , description = "A governance dashboard providing insights and tracking."
      }
    , { name = "cgov.app"
      , url = "https://cgov.app"
      , description = "An information and analytics platform focused on Cardano governance."
      }
    , { name = "DRep-Collective"
      , url = "https://github.com/DRep-Collective"
      , description = "A community initiative created to provide greater visibility and availability of governance DReps across the broader Cardano block-chain community."
      }
    ]


viewGovernanceTool : { name : String, url : String, description : String } -> Html Msg
viewGovernanceTool tool =
    div
        [ HA.style "background-color" "rgba(255, 255, 255, 0.9)"
        , HA.style "border-radius" "8px"
        , HA.style "padding" "1.25rem"
        , HA.style "transition" "transform 0.2s, box-shadow 0.2s"
        , HA.style "box-shadow" "0 2px 4px rgba(0, 0, 0, 0.05)"
        , HA.style "height" "100%"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        ]
        [ Html.h4
            [ HA.style "font-weight" "600"
            , HA.style "font-size" "1.1rem"
            , HA.style "margin-bottom" "0.75rem"
            ]
            [ text tool.name ]
        , Html.p
            [ HA.style "font-size" "0.9rem"
            , HA.style "line-height" "1.5"
            , HA.style "color" "#555"
            , HA.style "flex-grow" "1"
            , HA.style "margin-bottom" "1rem"
            ]
            [ text tool.description ]
        , Html.a
            [ HA.href tool.url
            , HA.target "_blank"
            , HA.rel "noopener noreferrer"
            , HA.style "font-size" "0.9rem"
            , HA.style "text-decoration" "none"
            , HA.style "font-weight" "500"
            , HA.style "display" "inline-flex"
            , HA.style "align-items" "center"
            , HA.style "color" "#0084ff"
            ]
            [ text ("Visit " ++ tool.name ++ " ")
            , Html.span
                [ HA.style "margin-left" "4px" ]
                [ text "→" ]
            ]
        ]



-- Helpers


viewErrors : List String -> Html Msg
viewErrors errors =
    if List.isEmpty errors then
        text ""

    else
        div [ HA.class "errors" ]
            [ Html.h3 [] [ text "Errors" ]
            , Html.ul [] (List.map (\err -> Html.li [] [ Html.pre [] [ text err ] ]) errors)
            ]
