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
import Cardano.Data as Data
import Cardano.Gov as Gov
import Cardano.Transaction as Transaction exposing (Transaction)
import Cardano.TxIntent
import Cardano.Utxo as Utxo exposing (Output, TransactionId)
import Cmd.Extra
import ConcurrentTask exposing (ConcurrentTask)
import ConcurrentTask.Http
import Dict exposing (Dict)
import Dict.Any
import Footer
import Header
import Helper exposing (PreconfAuthor, PreconfVoter)
import Html exposing (Html, div, text)
import Html.Attributes as HA
import Html.Events exposing (preventDefaultOn)
import Http
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE
import List.Extra
import Natural as N
import Page.Cart
import Page.Disclaimer
import Page.MultisigRegistration
import Page.Pdf
import Page.Preparation exposing (JsonLdContexts, StorageConfig)
import Page.Signing
import Platform.Cmd as Cmd
import ProposalMetadata exposing (ProposalMetadata)
import ProposalRelationships exposing (ProposalRelInfo, isChainableAction)
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
    , authorPreconfig : List PreconfAuthor
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
                    , onBroadcast CartBroadcastReceived
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


port broadcast : Value -> Cmd msg


port onBroadcast : (Value -> msg) -> Sub msg



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
    , lastConnectedWalletId : Maybe String
    , walletUtxos : Maybe (Utxo.RefDict Output)
    , walletDrepId : Maybe (Bytes CredentialHash)
    , protocolParams : Maybe ProtocolParams
    , constitutionUri : Maybe String
    , epoch : WebData Int
    , proposals : WebData (Dict String ActiveProposal)
    , pendingProposalId : Maybe String
    , proposalRelationships : Dict String ProposalRelInfo
    , proposalGovActions : Dict String Gov.Action
    , scriptsInfo : Dict String ScriptInfo
    , drepsInfo : Dict String DrepInfo
    , ccsInfo : Dict String CcInfo
    , poolsInfo : Dict String PoolInfo
    , jsonLdContexts : JsonLdContexts
    , taskPool : ConcurrentTask.Pool Msg
    , db : Value
    , networkId : NetworkId
    , ipfsPreconfig : { label : String, description : String }
    , voterPreconfig : List PreconfVoter
    , authorPreconfig : List PreconfAuthor
    , cart : Page.Cart.Model
    , errors : List String
    }


type Page
    = LandingPage
    | PreparationPage Page.Preparation.Model
    | SigningPage Page.Signing.Model
    | CartPage
    | MultisigRegistrationPage Page.MultisigRegistration.Model
    | PdfPage Page.Pdf.Model
    | DisclaimerPage


type TaskCompleted
    = Ignore
    | GotLastConnectedWalletId (Result String String)
    | GotLastVoter (Maybe Gov.Id)
    | GotLastStorageConfig Page.Preparation.StorageConfig
    | GotProposalMetadataTask String (Result String ProposalMetadata)
    | GotCart Page.Cart.Model
    | GotHlabsIncentive Page.Cart.HlabsIncentive
    | GotProposalGovActions (Result String (Dict String Gov.Action))
    | PreparationTaskCompleted Page.Preparation.TaskCompleted


init : Flags -> ( Model, Cmd Msg )
init { url, jsonLdContexts, db, networkId, ipfsPreconfig, voterPreconfig, authorPreconfig } =
    let
        networkIdTyped =
            Address.networkIdFromInt networkId |> Maybe.withDefault Testnet

        config =
            ModelConfig jsonLdContexts db networkIdTyped ipfsPreconfig voterPreconfig authorPreconfig
    in
    initHelper (locationHrefToRoute url) config


initHelper : Route -> ModelConfig -> ( Model, Cmd Msg )
initHelper route config =
    let
        ( model, cmd ) =
            handleUrlChange route (initialModel config)

        loadLastConnectedWalletId =
            Storage.read { db = config.db, storeName = "app" }
                JD.string
                { key = "walletId" }
                |> ConcurrentTask.toResult
                |> ConcurrentTask.map GotLastConnectedWalletId

        loadCart =
            Storage.read { db = config.db, storeName = "app" }
                Page.Cart.deserialize
                { key = "cart:" ++ networkIdToString config.networkId }
                |> ConcurrentTask.map GotCart
                |> ConcurrentTask.onError (\_ -> ConcurrentTask.succeed Ignore)

        ( updatedTaskPool, tasksCmds ) =
            ConcurrentTask.attemptEach
                { pool = model.taskPool, send = sendTask, onComplete = OnTaskComplete }
                [ loadLastConnectedWalletId
                , loadCart
                ]
    in
    ( { model | taskPool = updatedTaskPool }
    , Cmd.batch
        [ cmd
        , Api.defaultApiProvider.loadProtocolParams model.networkId GotProtocolParams
        , Api.defaultApiProvider.queryConstitution model.networkId GotConstitution
        , tasksCmds
        ]
    )


type alias ModelConfig =
    { jsonLdContexts : JsonLdContexts
    , db : Value
    , networkId : NetworkId
    , ipfsPreconfig : { label : String, description : String }
    , voterPreconfig : List PreconfVoter
    , authorPreconfig : List PreconfAuthor
    }


initialModel : ModelConfig -> Model
initialModel { jsonLdContexts, db, networkId, ipfsPreconfig, voterPreconfig, authorPreconfig } =
    { page = LandingPage
    , appUrl = routeToAppUrl RouteLanding
    , mobileMenuIsOpen = False
    , walletDropdownIsOpen = False
    , networkDropdownIsOpen = False
    , walletsDiscovered = []
    , wallet = Nothing
    , lastConnectedWalletId = Nothing
    , walletUtxos = Nothing
    , walletDrepId = Nothing
    , protocolParams = Nothing
    , constitutionUri = Nothing
    , epoch = RemoteData.NotAsked
    , proposals = RemoteData.NotAsked
    , pendingProposalId = Nothing
    , proposalRelationships = Dict.empty
    , proposalGovActions = Dict.empty
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
    , authorPreconfig = authorPreconfig
    , cart = Page.Cart.init
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
    | GotConstitution (Result Http.Error String)
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
      -- Cart page
    | CartPageMsg Page.Cart.Msg
    | DeleteVote { voterIdStr : String, actionIdStr : String }
    | ClearCart
    | RemoveFeePayer
    | SaveImportedCart
    | CartBroadcastReceived Value
      -- Multisig DRep registration page
    | MultisigPageMsg Page.MultisigRegistration.Msg
      -- PDF page
    | PdfPageMsg Page.Pdf.Msg
      -- Task port
    | OnTaskProgress ( ConcurrentTask.Pool Msg, Cmd Msg )
    | OnTaskComplete (ConcurrentTask.Response String TaskCompleted)


type Route
    = RouteLanding
    | RoutePreparation { networkId : NetworkId, proposalId : Maybe String }
    | RouteSigning { networkId : NetworkId, expectedSigners : List { keyName : String, keyHash : Bytes CredentialHash }, tx : Maybe Transaction }
    | RouteCart { networkId : NetworkId }
    | RouteMultisigRegistration
    | RoutePdf
    | RouteDisclaimer
    | Route404


{-| Broadcast the cart to other tabs, scoped to the current network.
-}
broadcastCart : NetworkId -> Page.Cart.Model -> Cmd Msg
broadcastCart net cart =
    broadcast <|
        JE.object
            [ ( "networkId", JE.string (networkIdToString net) )
            , ( "cart", Page.Cart.serialize cart )
            ]


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
                    RoutePreparation
                        { networkId = networkId
                        , proposalId =
                            Dict.get "proposalId" queryParameters
                                |> Maybe.andThen List.head
                                -- Validate it's a proper bech32 gov_action ID
                                |> Maybe.andThen (\pid -> Helper.actionIdFromBech32 pid |> Maybe.map (\_ -> pid))
                        }

                [ "page", "cart" ] ->
                    RouteCart { networkId = networkId }

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

        RoutePreparation { networkId, proposalId } ->
            { path = [ "page", "preparation" ]
            , queryParameters =
                case proposalId of
                    Nothing ->
                        Dict.singleton "networkId" [ networkIdToString networkId ]

                    Just pid ->
                        Dict.fromList
                            [ ( "networkId", [ networkIdToString networkId ] )
                            , ( "proposalId", [ pid ] )
                            ]
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

        RouteCart { networkId } ->
            { path = [ "page", "cart" ]
            , queryParameters = Dict.singleton "networkId" [ networkIdToString networkId ]
            , fragment = Nothing
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
                loadCart =
                    Storage.read { db = model.db, storeName = "app" }
                        Page.Cart.deserialize
                        { key = "cart:" ++ networkIdToString newNet }
                        |> ConcurrentTask.map GotCart
                        |> ConcurrentTask.onError (\_ -> ConcurrentTask.succeed Ignore)

                ( updatedTaskPool, tasksCmds ) =
                    ConcurrentTask.attempt { pool = model.taskPool, send = sendTask, onComplete = OnTaskComplete } loadCart

                updatedModel =
                    { model
                        | networkId = newNet
                        , networkDropdownIsOpen = False -- Close dropdown after selection
                        , proposals = RemoteData.NotAsked
                        , cart = Page.Cart.init
                        , taskPool = updatedTaskPool
                    }
            in
            case model.page of
                PreparationPage _ ->
                    handleUrlChange (RoutePreparation { networkId = newNet, proposalId = model.pendingProposalId }) updatedModel
                        |> Cmd.Extra.add tasksCmds

                SigningPage _ ->
                    handleUrlChange (RouteSigning { networkId = newNet, tx = Nothing, expectedSigners = [] }) updatedModel
                        |> Cmd.Extra.add tasksCmds

                CartPage ->
                    handleUrlChange (RouteCart { networkId = newNet }) updatedModel
                        |> Cmd.Extra.add tasksCmds

                _ ->
                    ( updatedModel, tasksCmds )

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

        ( GotConstitution result, _ ) ->
            case result of
                Ok uri ->
                    ( { model | constitutionUri = Just uri }, Cmd.none )

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
                            , constitutionUri = model.constitutionUri
                            , networkId = model.networkId
                            , authorPreconfig = model.authorPreconfig
                            , ipfsPreconfig = model.ipfsPreconfig
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

                        ( newModel, pageCmds ) =
                            Page.Signing.update ctx pageMsg pageModel
                                |> Tuple.mapFirst (\newPageModel -> { model | page = SigningPage newPageModel })

                        ( finalModel, urlCmd ) =
                            updateSigningPageUrl newModel
                    in
                    ( finalModel, Cmd.batch [ pageCmds, urlCmd ] )

                _ ->
                    ( model, Cmd.none )

        ( CartPageMsg pageMsg, { cart } ) ->
            let
                loadedWallet =
                    case ( model.wallet, model.walletUtxos ) of
                        ( Just wallet, Just utxos ) ->
                            Just { wallet = wallet, utxos = utxos }

                        _ ->
                            Nothing

                ctx =
                    { wrapMsg = CartPageMsg
                    , costModels = Maybe.map .costModels model.protocolParams
                    , loadedWallet = loadedWallet
                    , saveImportedCart = SaveImportedCart
                    }

                ( updatedCart, cmds ) =
                    Page.Cart.update ctx pageMsg cart
            in
            ( { model | cart = updatedCart }, cmds )

        ( DeleteVote { voterIdStr, actionIdStr }, { cart, networkId } ) ->
            let
                updatedCart =
                    Page.Cart.deleteVote voterIdStr actionIdStr cart

                writeCartToDb =
                    Storage.write { db = model.db, storeName = "app" } Page.Cart.serialize { key = "cart:" ++ networkIdToString networkId } updatedCart
                        |> ConcurrentTask.map (always Ignore)
            in
            ConcurrentTask.attempt { pool = model.taskPool, send = sendTask, onComplete = OnTaskComplete } writeCartToDb
                |> Tuple.mapFirst (\newTaskPool -> { model | taskPool = newTaskPool, cart = updatedCart })
                |> Cmd.Extra.add (broadcastCart networkId updatedCart)

        ( ClearCart, _ ) ->
            saveCart Page.Cart.init model

        ( RemoveFeePayer, { cart } ) ->
            -- No need to save cart here, we are just resetting the Tx builder
            ( { model | cart = Page.Cart.removeFeePayer cart }, Cmd.none )

        ( SaveImportedCart, _ ) ->
            saveCart model.cart model

        ( CartBroadcastReceived value, _ ) ->
            let
                networkIdDecoder : JD.Decoder NetworkId
                networkIdDecoder =
                    JD.field "networkId" JD.string
                        |> JD.andThen
                            (\str ->
                                case networkIdFromString str of
                                    Just n ->
                                        JD.succeed n

                                    Nothing ->
                                        JD.fail ("Unknown network id: " ++ str)
                            )

                cartDecoder : JD.Decoder Page.Cart.Model
                cartDecoder =
                    JD.field "cart" Page.Cart.deserialize

                decoder : JD.Decoder ( NetworkId, Page.Cart.Model )
                decoder =
                    JD.map2 Tuple.pair networkIdDecoder cartDecoder
            in
            case JD.decodeValue decoder value of
                Ok ( net, cart ) ->
                    if net == model.networkId then
                        ( { model | cart = cart }, Cmd.none )

                    else
                        ( model, Cmd.none )

                Err err ->
                    ( { model | errors = [ JD.errorToString err ] }, Cmd.none )

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

                        isDropped p =
                            -- has expired
                            (p.epoch_validity.end <= currentEpoch)
                                -- or was enacted (1 epoch after marked ratified by Koios)
                                || (Maybe.withDefault False <| Maybe.map (\ratifiedEpoch -> currentEpoch > ratifiedEpoch) p.ratified)
                                -- or was dropped because a conflicting proposal was enacted
                                || (p.dropped /= Nothing)

                        proposalsList =
                            List.map (\p -> ( Helper.actionIdToBech32 p.id, p )) activeProposals
                                -- deduplicate proposals
                                |> Dict.fromList
                                |> Dict.toList
                                -- only keep if not dropped (enacted or expired)
                                |> List.filter (\( _, p ) -> not <| isDropped p)

                        completeReadProposalMetadataTask : ActiveProposal -> ConcurrentTask x TaskCompleted
                        completeReadProposalMetadataTask { id, metadataHash, metadataUrl } =
                            Api.defaultApiProvider.loadProposalMetadata metadataUrl
                                |> Storage.cacheWrap
                                    { db = model.db, storeName = "proposalMetadata" }
                                    ProposalMetadata.decoder
                                    ProposalMetadata.encode
                                    { key = metadataHash }
                                |> ConcurrentTask.toResult
                                |> ConcurrentTask.map (GotProposalMetadataTask <| Helper.actionIdToBech32 id)

                        -- Fetch tx CBORs for chainable proposals to extract latestEnacted
                        chainableProposals =
                            List.filter (\( _, p ) -> isChainableAction p.actionType) proposalsList

                        -- Deduplicate tx hashes (multiple proposals can be in the same tx)
                        uniqueTxIds =
                            chainableProposals
                                |> List.map (\( _, p ) -> p.id.transactionId)
                                |> List.Extra.uniqueBy Bytes.toHex

                        fetchGovActionsTask : ConcurrentTask x TaskCompleted
                        fetchGovActionsTask =
                            Api.taskRetrieveTxBatch model.networkId uniqueTxIds
                                |> ConcurrentTask.map
                                    (\txCborDict ->
                                        let
                                            extractAction ( bech32Id, proposal ) =
                                                Dict.get (Bytes.toHex proposal.id.transactionId) txCborDict
                                                    |> Maybe.andThen Transaction.deserialize
                                                    |> Maybe.andThen (\tx -> List.Extra.getAt proposal.id.govActionIndex tx.body.proposalProcedures)
                                                    |> Maybe.map (\procedure -> ( bech32Id, procedure.govAction ))

                                            govActions =
                                                List.filterMap extractAction chainableProposals
                                                    |> Dict.fromList
                                        in
                                        GotProposalGovActions (Ok govActions)
                                    )
                                |> ConcurrentTask.onError
                                    (\_ ->
                                        ConcurrentTask.succeed <|
                                            GotProposalGovActions (Err "Failed to fetch proposals transaction CBORs")
                                    )

                        govActionTasks =
                            if List.isEmpty chainableProposals then
                                []

                            else
                                [ fetchGovActionsTask ]

                        ( newPool, cmds ) =
                            ConcurrentTask.attemptEach { pool = model.taskPool, send = sendTask, onComplete = OnTaskComplete }
                                (List.map completeReadProposalMetadataTask activeProposals
                                    ++ govActionTasks
                                )

                        updatedModel =
                            { model
                                | taskPool = newPool
                                , proposals = RemoteData.Success <| Dict.fromList proposalsList
                            }

                        baseCmds =
                            -- Let's also redo a wallet discovery,
                            -- just to make sure all wallets have had the time to load,
                            -- which should be the case by now.
                            -- This is to prevent a situation where the browser extensions
                            -- were not ready yet the first time around.
                            Cmd.batch [ toWallet (Cip30.encodeRequest Cip30.discoverWallets), cmds ]
                    in
                    handlePendingProposalSelection updatedModel baseCmds

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
            ( { model | wallet = Nothing, lastConnectedWalletId = Nothing, walletUtxos = Nothing, walletDrepId = Nothing }
            , Cmd.none
            )


handleUrlChange : Route -> Model -> ( Model, Cmd Msg )
handleUrlChange route model =
    let
        appUrl =
            routeToAppUrl route

        pushUrlCmd =
            if appUrl == model.appUrl then
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

        RoutePreparation { networkId, proposalId } ->
            let
                newModel =
                    { model
                        | errors = []
                        , page = PreparationPage Page.Preparation.init
                        , appUrl = appUrl
                        , pendingProposalId = proposalId
                    }

                reloadLatestVoterTask : ConcurrentTask String (Maybe Gov.Id)
                reloadLatestVoterTask =
                    Storage.read { db = model.db, storeName = "app" } govIdDecoder { key = "lastVoter" }
                        |> ConcurrentTask.onError (\_ -> ConcurrentTask.succeed Nothing)

                govIdDecoder =
                    JD.string |> JD.map Gov.idFromBech32

                reloadLatestStorageConfigTask : ConcurrentTask String StorageConfig
                reloadLatestStorageConfigTask =
                    Storage.read { db = model.db, storeName = "app" } Page.Preparation.storageConfigDecoder { key = "lastStorageConfig" }
                        |> ConcurrentTask.onError (\_ -> ConcurrentTask.succeed Page.Preparation.initStorageConfig)

                ( newTaskPool, taskCmds ) =
                    ConcurrentTask.attemptEach { pool = model.taskPool, send = sendTask, onComplete = OnTaskComplete }
                        [ ConcurrentTask.map GotLastVoter reloadLatestVoterTask
                        , ConcurrentTask.map GotLastStorageConfig reloadLatestStorageConfigTask
                        ]
            in
            if networkId /= model.networkId then
                initHelper route
                    { jsonLdContexts = model.jsonLdContexts
                    , db = model.db
                    , networkId = networkId
                    , ipfsPreconfig = model.ipfsPreconfig
                    , voterPreconfig = model.voterPreconfig
                    , authorPreconfig = model.authorPreconfig
                    }

            else if RemoteData.isSuccess model.proposals then
                -- If proposals are already loaded, handle pending proposal selection
                handlePendingProposalSelection
                    { newModel | taskPool = newTaskPool }
                    (Cmd.batch [ pushUrlCmd, taskCmds ])

            else
                ( { newModel
                    | proposals = RemoteData.Loading
                    , taskPool = newTaskPool
                  }
                , Cmd.batch
                    [ pushUrlCmd
                    , Api.defaultApiProvider.queryEpoch model.networkId GotEpoch
                    , taskCmds
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
                    , authorPreconfig = model.authorPreconfig
                    }

            else
                ( { model
                    | errors = []
                    , page = SigningPage <| Page.Signing.initialModel expectedSigners tx
                    , appUrl = appUrl
                  }
                , pushUrlCmd
                )

        RouteCart { networkId } ->
            if networkId /= model.networkId then
                initHelper route
                    { jsonLdContexts = model.jsonLdContexts
                    , db = model.db
                    , networkId = networkId
                    , ipfsPreconfig = model.ipfsPreconfig
                    , voterPreconfig = model.voterPreconfig
                    , authorPreconfig = model.authorPreconfig
                    }

            else
                let
                    updatedModel =
                        { model
                            | errors = []
                            , page = CartPage
                            , appUrl = appUrl
                        }
                in
                case Page.Cart.findHlabsIncentiveDatumHash updatedModel.cart of
                    Just datumHash ->
                        let
                            task =
                                hlabsIncentiveLookup model.networkId datumHash
                        in
                        ConcurrentTask.attempt { pool = updatedModel.taskPool, send = sendTask, onComplete = OnTaskComplete } task
                            |> Tuple.mapFirst
                                (\newTaskPool ->
                                    { updatedModel
                                        | taskPool = newTaskPool
                                        , cart = Page.Cart.setHlabsIncentive Page.Cart.Checking updatedModel.cart
                                    }
                                )
                            |> Cmd.Extra.add pushUrlCmd

                    Nothing ->
                        ( updatedModel, pushUrlCmd )

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


{-| Handle pending proposal selection after proposals have been loaded.
If a proposalId was provided in the route, check if it exists:

  - If it doesn't exist, clear the proposalId from the route.
  - If it exists, keep the pending state; the proposal will be auto-selected
    once its metadata has loaded (in the GotProposalMetadataTask handler).

-}
handlePendingProposalSelection : Model -> Cmd Msg -> ( Model, Cmd Msg )
handlePendingProposalSelection model baseCmds =
    case model.pendingProposalId of
        Nothing ->
            ( model, baseCmds )

        Just proposalId ->
            case model.proposals of
                RemoteData.Success proposalsDict ->
                    if Dict.member proposalId proposalsDict then
                        -- Proposal exists, keep pending state until metadata loads
                        ( model, baseCmds )

                    else
                        -- Proposal doesn't exist, clear it from the route
                        let
                            routeWithoutProposal =
                                RoutePreparation { networkId = model.networkId, proposalId = Nothing }

                            newAppUrl =
                                routeToAppUrl routeWithoutProposal
                        in
                        ( { model | pendingProposalId = Nothing, appUrl = newAppUrl }
                        , Cmd.batch [ baseCmds, pushUrl <| AppUrl.toString newAppUrl ]
                        )

                _ ->
                    -- Proposals not loaded yet, keep the pending state
                    ( model, baseCmds )


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
            let
                -- If the wallet isn’t connected already,
                -- and if the last connected wallet is enabled in the discovered list,
                -- then try to connect to it.
                shouldAutoReconnect : Maybe WalletDescriptor
                shouldAutoReconnect =
                    if model.wallet == Nothing then
                        List.Extra.find (\{ id, isEnabled } -> isEnabled && Just id == model.lastConnectedWalletId) wallets

                    else
                        Nothing
            in
            case shouldAutoReconnect of
                Just { id, supportedExtensions } ->
                    ( { model | walletsDiscovered = wallets }
                    , toWallet (Cip30.encodeRequest (Cip30.enableWallet { id = id, extensions = List.filter (\ext -> ext == 95) supportedExtensions, watchInterval = Just 5 }))
                    )

                Nothing ->
                    ( { model | walletsDiscovered = wallets }
                    , Cmd.none
                    )

        -- We just connected to the wallet, let’s ask for all that is still missing
        Cip30.EnabledWallet wallet ->
            let
                saveConnectedWalletId =
                    Storage.write { db = model.db, storeName = "app" } JE.string { key = "walletId" } (Cip30.walletDescriptor wallet).id
                        |> ConcurrentTask.map (always Ignore)

                ( updatedTaskPool, saveConnectedWalletIdCmd ) =
                    ConcurrentTask.attempt { pool = model.taskPool, send = sendTask, onComplete = OnTaskComplete } saveConnectedWalletId
            in
            ( { model
                | wallet = Just wallet
                , walletUtxos = Nothing
                , lastConnectedWalletId = Just (Cip30.walletDescriptor wallet).id
                , taskPool = updatedTaskPool
                , page =
                    case model.page of
                        SigningPage pageModel ->
                            SigningPage (Page.Signing.clearError pageModel)

                        other ->
                            other
              }
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

                -- Save the connected wallet ID to reconnect it automatically next time
                , saveConnectedWalletIdCmd
                ]
            )

        -- We just received the utxos
        Cip30.ApiResponse _ (Cip30ApiResponse (Cip30.WalletUtxos utxos)) ->
            case model.wallet of
                Nothing ->
                    -- This should never happen in practice
                    ( model, Cmd.none )

                Just wallet ->
                    ( { model | walletUtxos = Just (Utxo.refDictFromList utxos) }
                      -- Also ask for collateral UTxOs after receiving the normal UTxOs
                    , Cip30.getCollateral wallet { amount = N.fromSafeInt 1000000 }
                        |> Cip30.encodeRequest
                        |> toWallet
                    )

        -- We just received the collateral utxos
        Cip30.ApiResponse _ (Cip30ApiResponse (Cip30.Collateral collateralUtxos)) ->
            case model.walletUtxos of
                Nothing ->
                    ( { model | walletUtxos = Just (Utxo.refDictFromList collateralUtxos) }
                    , Cmd.none
                    )

                Just utxos ->
                    ( { model | walletUtxos = Just <| Dict.Any.union utxos (Utxo.refDictFromList collateralUtxos) }
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
                    let
                        updatedModel =
                            { model | page = SigningPage <| Page.Signing.addWalletSignatures vkeyWitnesses pageModel }
                    in
                    updateSigningPageUrl updatedModel

                -- No other page expects to receive a Tx signature
                _ ->
                    ( model, Cmd.none )

        -- The wallet just submitted a Tx
        Cip30.ApiResponse _ (Cip30ApiResponse (Cip30.SubmittedTx txId)) ->
            case model.page of
                SigningPage pageModel ->
                    let
                        emptyCart =
                            Page.Cart.init

                        writeCartToDb =
                            Storage.write { db = model.db, storeName = "app" } Page.Cart.serialize { key = "cart:" ++ networkIdToString model.networkId } emptyCart
                                |> ConcurrentTask.map (always Ignore)

                        ( updatedTaskPool, taskCmds ) =
                            ConcurrentTask.attempt { pool = model.taskPool, send = sendTask, onComplete = OnTaskComplete } writeCartToDb
                    in
                    ( { model
                        | page = SigningPage <| Page.Signing.recordSubmittedTx txId pageModel

                        -- Update the wallet’s UTxOs
                        , walletUtxos =
                            Maybe.map2 updateWalletUtxosWithTx
                                (Page.Signing.getTxInfo pageModel)
                                model.walletUtxos

                        -- Reset the cart
                        , cart = emptyCart
                        , taskPool = updatedTaskPool
                      }
                    , Cmd.batch
                        [ taskCmds
                        , broadcastCart model.networkId emptyCart
                        ]
                    )

                -- No other page expects to submit a Tx
                _ ->
                    ( model, Cmd.none )

        Cip30.ApiResponse _ _ ->
            ( { model | errors = "Unhandled CIP30 response yet" :: model.errors }
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

        Just (Page.Preparation.CacheVoterGovId voterGovId) ->
            let
                encodeGovId id =
                    JE.string <| Gov.idToBech32 id

                writeGovIdToDb =
                    Storage.write { db = model.db, storeName = "app" } encodeGovId { key = "lastVoter" } voterGovId
                        |> ConcurrentTask.map (always Ignore)
            in
            ConcurrentTask.attempt { pool = model.taskPool, send = sendTask, onComplete = OnTaskComplete } writeGovIdToDb
                |> Tuple.mapFirst (\newTaskPool -> { model | taskPool = newTaskPool })

        Just (Page.Preparation.CacheStorageConfig storageConfig) ->
            let
                writeStorageConfigToDb =
                    Storage.write { db = model.db, storeName = "app" } Page.Preparation.encodeStorageConfig { key = "lastStorageConfig" } storageConfig
                        |> ConcurrentTask.map (always Ignore)
            in
            ConcurrentTask.attempt { pool = model.taskPool, send = sendTask, onComplete = OnTaskComplete } writeStorageConfigToDb
                |> Tuple.mapFirst (\newTaskPool -> { model | taskPool = newTaskPool })

        Just (Page.Preparation.AddVoteToCart voter voteRecord) ->
            let
                updatedCart =
                    Page.Cart.addVote voter voteRecord model.cart

                writeCartToDb =
                    Storage.write { db = model.db, storeName = "app" } Page.Cart.serialize { key = "cart:" ++ networkIdToString model.networkId } updatedCart
                        |> ConcurrentTask.map (always Ignore)
            in
            ConcurrentTask.attempt { pool = model.taskPool, send = sendTask, onComplete = OnTaskComplete } writeCartToDb
                |> Tuple.mapFirst (\newTaskPool -> { model | taskPool = newTaskPool, cart = updatedCart })
                |> Cmd.Extra.add (broadcastCart model.networkId updatedCart)

        Just Page.Preparation.GoToCart ->
            handleUrlChange (RouteCart { networkId = model.networkId }) model

        Just (Page.Preparation.ProposalChanged maybeProposalId) ->
            let
                newAppUrl =
                    routeToAppUrl (RoutePreparation { networkId = model.networkId, proposalId = maybeProposalId })
            in
            ( { model | appUrl = newAppUrl }
            , pushUrl <| AppUrl.toString newAppUrl
            )

        Just (Page.Preparation.RunTask task) ->
            ConcurrentTask.attempt { pool = model.taskPool, send = sendTask, onComplete = OnTaskComplete }
                (ConcurrentTask.map PreparationTaskCompleted task)
                |> Tuple.mapFirst (\newTaskPool -> { model | taskPool = newTaskPool })

        Just (Page.Preparation.BatchToParent msg1 msg2) ->
            updateModelWithPrepToParentMsg (Just msg1) model
                |> (\( newModel, cmd1 ) ->
                        updateModelWithPrepToParentMsg (Just msg2) newModel
                            |> Tuple.mapSecond (\cmd2 -> Cmd.batch [ cmd1, cmd2 ])
                   )


saveCart : Page.Cart.Model -> Model -> ( Model, Cmd Msg )
saveCart cart model =
    let
        writeCartToDb =
            Storage.write { db = model.db, storeName = "app" } Page.Cart.serialize { key = "cart:" ++ networkIdToString model.networkId } cart
                |> ConcurrentTask.map (always Ignore)
    in
    ConcurrentTask.attempt { pool = model.taskPool, send = sendTask, onComplete = OnTaskComplete } writeCartToDb
        |> Tuple.mapFirst (\newTaskPool -> { model | taskPool = newTaskPool, cart = cart })
        |> Cmd.Extra.add (broadcastCart model.networkId cart)


{-| Update the URL fragment to include the latest gathered signatures.
Called after wallet signing or file upload adds new signatures.
-}
updateSigningPageUrl : Model -> ( Model, Cmd Msg )
updateSigningPageUrl model =
    case model.page of
        SigningPage pageModel ->
            case Page.Signing.getSignedTx pageModel of
                Just signedTx ->
                    let
                        newAppUrl =
                            { path = model.appUrl.path
                            , queryParameters = model.appUrl.queryParameters
                            , fragment = Just (Bytes.toHex (Transaction.serialize signedTx))
                            }
                    in
                    if newAppUrl == model.appUrl then
                        ( model, Cmd.none )

                    else
                        ( { model | appUrl = newAppUrl }
                        , pushUrl (AppUrl.toString newAppUrl)
                        )

                Nothing ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


{-| Helper function to reset the signing step of the Preparation.
-}
resetSigningStep : String -> Page -> Page
resetSigningStep error page =
    case page of
        SigningPage pageModel ->
            SigningPage <| Page.Signing.resetSubmission error pageModel

        _ ->
            page


{-| Build a ConcurrentTask that looks up the HLabs incentive UTxO for a given datum hash.
Chain: datum\_info -> retrieve tx -> find output -> utxo\_info -> result.
-}
hlabsIncentiveLookup : NetworkId -> Bytes a -> ConcurrentTask String TaskCompleted
hlabsIncentiveLookup networkId datumHash =
    Api.taskGetDatumInfo networkId datumHash
        |> httpErrToString
        |> ConcurrentTask.andThen
            (\{ creationTxHash } ->
                retrieveTx networkId creationTxHash
                    |> ConcurrentTask.andThen
                        (\tx ->
                            case findHlabsIncentiveOutput datumHash creationTxHash 0 tx.body.outputs of
                                Nothing ->
                                    ConcurrentTask.fail "No matching output found at script address"

                                Just ( outputRef, output ) ->
                                    Api.taskGetUtxoInfo networkId creationTxHash outputRef.outputIndex
                                        |> httpErrToString
                                        |> ConcurrentTask.andThen
                                            (\isUnspent ->
                                                if not isUnspent then
                                                    ConcurrentTask.succeed Page.Cart.AlreadySpent

                                                else
                                                    retrieveRefScriptOutput networkId
                                                        |> ConcurrentTask.map
                                                            (\refScriptOutput ->
                                                                Page.Cart.Found
                                                                    { outputRef = outputRef
                                                                    , output = output
                                                                    , lovelace = output.amount.lovelace
                                                                    , refScriptOutput = refScriptOutput
                                                                    , enabled = True
                                                                    }
                                                            )
                                            )
                        )
            )
        |> ConcurrentTask.toResult
        |> ConcurrentTask.map
            (\result ->
                case result of
                    Ok incentive ->
                        GotHlabsIncentive incentive

                    Err _ ->
                        GotHlabsIncentive Page.Cart.NotFound
            )


httpErrToString : ConcurrentTask ConcurrentTask.Http.Error a -> ConcurrentTask String a
httpErrToString =
    ConcurrentTask.mapError (\_ -> "HTTP request failed")


retrieveTx : NetworkId -> Bytes TransactionId -> ConcurrentTask String Transaction
retrieveTx networkId txId =
    Api.defaultApiProvider.retrieveTx networkId txId
        |> httpErrToString
        |> ConcurrentTask.andThen
            (\txBytes ->
                case Transaction.deserialize txBytes of
                    Nothing ->
                        ConcurrentTask.fail "Failed to deserialize transaction"

                    Just tx ->
                        ConcurrentTask.succeed tx
            )


retrieveRefScriptOutput : NetworkId -> ConcurrentTask String Output
retrieveRefScriptOutput networkId =
    retrieveTx networkId Page.Cart.hlabsReferenceScriptRef.transactionId
        |> ConcurrentTask.andThen
            (\tx ->
                case List.Extra.getAt Page.Cart.hlabsReferenceScriptRef.outputIndex tx.body.outputs of
                    Nothing ->
                        ConcurrentTask.fail "Reference script output not found"

                    Just output ->
                        ConcurrentTask.succeed output
            )


{-| Recursively search transaction outputs for the HLabs incentive UTxO.
Matches outputs at the HLabs script address whose inline datum hashes to the expected hash.
-}
findHlabsIncentiveOutput : Bytes a -> Bytes TransactionId -> Int -> List Output -> Maybe ( Utxo.OutputReference, Output )
findHlabsIncentiveOutput datumHash txId index outputs =
    case outputs of
        [] ->
            Nothing

        output :: rest ->
            if output.address == Page.Cart.hlabsIncentiveScriptAddress && outputMatchesHlabsDatum datumHash output then
                Just ( { transactionId = txId, outputIndex = index }, output )

            else
                findHlabsIncentiveOutput datumHash txId (index + 1) rest


outputMatchesHlabsDatum : Bytes a -> Output -> Bool
outputMatchesHlabsDatum datumHash output =
    case output.datumOption of
        Just (Utxo.DatumValue { rawBytes }) ->
            Bytes.toHex (Data.rawDatumHash rawBytes) == Bytes.toHex datumHash

        _ ->
            False


handleCompletedTask : ConcurrentTask.Response String TaskCompleted -> Model -> ( Model, Cmd Msg )
handleCompletedTask response model =
    case ( response, model.page ) of
        ( ConcurrentTask.Error error, _ ) ->
            ( { model | errors = error :: model.errors }, Cmd.none )

        ( ConcurrentTask.UnexpectedError error, _ ) ->
            ( { model | errors = Debug.toString error :: model.errors }, Cmd.none )

        ( ConcurrentTask.Success Ignore, _ ) ->
            ( model, Cmd.none )

        ( ConcurrentTask.Success (GotLastConnectedWalletId walletIdResult), _ ) ->
            -- Discover wallets after having loaded the last connected wallet ID
            case walletIdResult of
                Ok walletId ->
                    ( { model | lastConnectedWalletId = Just walletId }
                    , toWallet (Cip30.encodeRequest Cip30.discoverWallets)
                    )

                Err _ ->
                    ( model
                    , toWallet (Cip30.encodeRequest Cip30.discoverWallets)
                    )

        ( ConcurrentTask.Success (GotLastVoter maybeGovId), PreparationPage pageModel ) ->
            let
                ( newPageModel, pageCmd ) =
                    Page.Preparation.setLastVoter maybeGovId pageModel
            in
            ( { model | page = PreparationPage newPageModel }
            , Cmd.map PreparationPageMsg pageCmd
            )

        ( ConcurrentTask.Success (GotLastVoter _), _ ) ->
            ( model, Cmd.none )

        ( ConcurrentTask.Success (GotLastStorageConfig storageConfig), PreparationPage pageModel ) ->
            let
                ( newPageModel, pageMsg ) =
                    Page.Preparation.setLastStorageConfig storageConfig pageModel
            in
            ( { model | page = PreparationPage newPageModel }
            , Cmd.Extra.perform <| PreparationPageMsg pageMsg
            )

        ( ConcurrentTask.Success (GotLastStorageConfig _), _ ) ->
            ( model, Cmd.none )

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

                updatedModel =
                    { model | proposals = RemoteData.map (\ps -> Dict.update id updateMetadata ps) model.proposals }
            in
            -- If this is the pending proposal, auto-select it now that metadata is loaded
            if model.pendingProposalId == Just id then
                ( { updatedModel | pendingProposalId = Nothing }
                , Cmd.Extra.perform <| PreparationPageMsg (Page.Preparation.pickProposalMsg id)
                )

            else
                ( updatedModel, Cmd.none )

        ( ConcurrentTask.Success (GotProposalGovActions result), _ ) ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok govActions ->
                    let
                        newRelationships =
                            recomputeProposalRelationships model.proposals govActions
                    in
                    ( { model
                        | proposalGovActions = govActions
                        , proposalRelationships = newRelationships
                      }
                    , Cmd.none
                    )

        ( ConcurrentTask.Success (GotCart cart), _ ) ->
            let
                updatedModel =
                    { model | cart = cart }
            in
            case ( model.page, Page.Cart.findHlabsIncentiveDatumHash cart ) of
                ( CartPage, Just datumHash ) ->
                    ConcurrentTask.attempt { pool = updatedModel.taskPool, send = sendTask, onComplete = OnTaskComplete } (hlabsIncentiveLookup model.networkId datumHash)
                        |> Tuple.mapFirst
                            (\newTaskPool ->
                                { updatedModel
                                    | taskPool = newTaskPool
                                    , cart = Page.Cart.setHlabsIncentive Page.Cart.Checking cart
                                }
                            )

                _ ->
                    ( updatedModel, Cmd.none )

        ( ConcurrentTask.Success (GotHlabsIncentive incentive), _ ) ->
            ( { model | cart = Page.Cart.setHlabsIncentive incentive model.cart }, Cmd.none )

        ( ConcurrentTask.Success (PreparationTaskCompleted taskCompleted), PreparationPage pageModel ) ->
            let
                ( newPageModel, cmds, msgToParent ) =
                    Page.Preparation.handleTaskCompleted taskCompleted pageModel
            in
            updateModelWithPrepToParentMsg msgToParent { model | page = PreparationPage newPageModel }
                |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, Cmd.map PreparationPageMsg cmds ])

        ( ConcurrentTask.Success (PreparationTaskCompleted _), _ ) ->
            ( model, Cmd.none )


{-| Recompute proposal relationships from the current proposals and decoded gov actions.
-}
recomputeProposalRelationships : WebData (Dict String ActiveProposal) -> Dict String Gov.Action -> Dict String ProposalRelInfo
recomputeProposalRelationships proposalsData govActions =
    case proposalsData of
        RemoteData.Success proposals ->
            let
                -- Build the list of chainable proposals with their decoded latestEnacted
                chainableWithActions : List ( String, ActiveProposal, Maybe Gov.ActionId )
                chainableWithActions =
                    Dict.toList proposals
                        |> List.filter (\( _, p ) -> isChainableAction p.actionType)
                        |> List.filterMap
                            (\( bech32Id, proposal ) ->
                                case Dict.get bech32Id govActions of
                                    Just action ->
                                        Just ( bech32Id, proposal, ProposalRelationships.actionLatestEnacted action )

                                    Nothing ->
                                        -- Gov action not yet decoded, skip for now
                                        Nothing
                            )
            in
            ProposalRelationships.proposalRelationships chainableWithActions

        _ ->
            Dict.empty



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
            [ { label = "Vote Preparation"
              , link = link <| RoutePreparation { networkId = model.networkId, proposalId = Nothing }
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
        , cartCount = Page.Cart.cartCount model.cart
        , cartLink = link <| RouteCart { networkId = model.networkId }
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
                , cart = model.cart
                , proposals = model.proposals
                , jsonLdContexts = model.jsonLdContexts
                , costModels = Maybe.map .costModels model.protocolParams
                , constitutionUri = model.constitutionUri
                , networkId = model.networkId
                , changeNetworkLink =
                    \networkId ->
                        link (RoutePreparation { networkId = networkId, proposalId = Nothing }) []
                , signingLink =
                    \tx expectedSigners ->
                        link (RouteSigning { networkId = model.networkId, tx = Just tx, expectedSigners = expectedSigners }) []
                , ipfsPreconfig = model.ipfsPreconfig
                , voterPreconfig = model.voterPreconfig
                , proposalRelationships = model.proposalRelationships
                }
                prepModel

        SigningPage signingModel ->
            Page.Signing.view
                { wrapMsg = SigningPageMsg
                , wallet = model.wallet
                , networkId = model.networkId
                }
                signingModel

        CartPage ->
            Page.Cart.view
                { wrapMsg = CartPageMsg
                , deleteVote = DeleteVote
                , clearCart = ClearCart
                , removeFeePayer = RemoveFeePayer
                , signingLink =
                    \tx expectedSigners ->
                        link (RouteSigning { networkId = model.networkId, tx = Just tx, expectedSigners = expectedSigners }) []
                }
                model.cart

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
                [ link (RoutePreparation { networkId = networkId, proposalId = Nothing })
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
    , { name = "CGOV"
      , url = "https://app.cgov.io/"
      , description = "A Cardano governance platform for monitoring, tracking, and participating in on-chain governance."
      }
    , { name = "tempo.vote"
      , url = "https://tempo.vote"
      , description = "An alternative comprehensive platform aiming to support all aspects of Cardano governance."
      }
    , { name = "CardanoCube Governance"
      , url = "https://www.cardanocube.com/governance"
      , description = "Governance activity feed & browse DReps and proposals."
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
