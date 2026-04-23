port module Main exposing (main)

{-| Minimal Cardano governance app: initializes Cardano-related code
and displays current proposals with their metadata.
-}

import Api exposing (ActiveProposal)
import Browser
import Cardano.Address exposing (NetworkId(..))
import Cardano.Cip30 as Cip30 exposing (WalletDescriptor)
import Cardano.Gov as Gov
import ConcurrentTask
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, h3, p, span, text)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD exposing (Value)
import ProposalMetadata exposing (ProposalMetadata)
import RemoteData exposing (RemoteData(..), WebData)
import Storage



-- PORTS


port toWallet : Value -> Cmd msg


port fromWallet : (Value -> msg) -> Sub msg


port sendTask : Value -> Cmd msg


port receiveTask : (Value -> msg) -> Sub msg



-- MODEL


type alias Flags =
    { url : String
    , db : Value
    , networkId : Int
    }


type alias Model =
    { networkId : NetworkId
    , db : Value
    , protocolParams : Maybe Api.ProtocolParams
    , epoch : WebData Int
    , proposals : WebData (Dict String ActiveProposal)
    , walletsDiscovered : List WalletDescriptor
    , wallet : Maybe Cip30.Wallet
    , taskPool : ConcurrentTask.Pool Msg
    , errors : List String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        networkId =
            if flags.networkId == 1 then
                Mainnet

            else
                Testnet

        model =
            { networkId = networkId
            , db = flags.db
            , protocolParams = Nothing
            , epoch = NotAsked
            , proposals = NotAsked
            , walletsDiscovered = []
            , wallet = Nothing
            , taskPool = ConcurrentTask.pool
            , errors = []
            }
    in
    ( { model | epoch = Loading }
    , Cmd.batch
        [ Api.loadProtocolParams networkId GotProtocolParams
        , Api.queryEpoch networkId GotEpoch
        , toWallet (Cip30.encodeRequest Cip30.discoverWallets)
        ]
    )



-- MSG


type Msg
    = NoMsg
    | WalletMsg Value
    | GotProtocolParams (Result Http.Error Api.ProtocolParams)
    | GotEpoch (Result Http.Error Int)
    | GotProposals (Result Http.Error (List ActiveProposal))
    | ConnectWalletClicked { id : String, supportedExtensions : List Int }
    | DisconnectWalletClicked
    | OnTaskProgress ( ConcurrentTask.Pool Msg, Cmd Msg )
    | OnTaskComplete (ConcurrentTask.Response String TaskCompleted)


type TaskCompleted
    = Ignore
    | GotProposalMetadata String (Result String ProposalMetadata)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoMsg ->
            ( model, Cmd.none )

        GotProtocolParams result ->
            case result of
                Ok params ->
                    ( { model | protocolParams = Just params }, Cmd.none )

                Err _ ->
                    ( { model | errors = "Failed to load protocol params" :: model.errors }, Cmd.none )

        GotEpoch result ->
            case result of
                Err _ ->
                    ( { model | epoch = Failure Http.NetworkError }, Cmd.none )

                Ok epoch ->
                    ( { model | epoch = Success epoch, proposals = Loading }
                    , Api.loadGovProposals model.networkId epoch GotProposals
                    )

        GotProposals result ->
            case result of
                Err err ->
                    ( { model | proposals = Failure err }, Cmd.none )

                Ok activeProposals ->
                    let
                        currentEpoch =
                            RemoteData.withDefault 0 model.epoch

                        isDropped p =
                            (p.epoch_validity.end <= currentEpoch)
                                || (Maybe.withDefault False <| Maybe.map (\r -> currentEpoch > r) p.ratified)

                        proposalsList =
                            List.map (\p -> ( actionIdToBech32 p.id, p )) activeProposals
                                |> Dict.fromList
                                |> Dict.toList
                                |> List.filter (\( _, p ) -> not (isDropped p))

                        -- Load metadata for each proposal (with caching)
                        metadataTasks =
                            List.map
                                (\p ->
                                    Api.taskLoadProposalMetadata p.metadataUrl
                                        |> Storage.cacheWrap
                                            { db = model.db, storeName = "proposalMetadata" }
                                            ProposalMetadata.decoder
                                            ProposalMetadata.encode
                                            { key = p.metadataHash }
                                        |> ConcurrentTask.toResult
                                        |> ConcurrentTask.map (GotProposalMetadata (actionIdToBech32 p.id))
                                )
                                activeProposals

                        ( newPool, cmds ) =
                            ConcurrentTask.attemptEach
                                { pool = model.taskPool, send = sendTask, onComplete = OnTaskComplete }
                                metadataTasks
                    in
                    ( { model
                        | proposals = Success (Dict.fromList proposalsList)
                        , taskPool = newPool
                      }
                    , cmds
                    )

        WalletMsg value ->
            case JD.decodeValue walletResponseDecoder value of
                Ok response ->
                    handleWalletResponse response model

                Err err ->
                    ( { model | errors = JD.errorToString err :: model.errors }, Cmd.none )

        ConnectWalletClicked { id, supportedExtensions } ->
            ( model
            , toWallet
                (Cip30.encodeRequest
                    (Cip30.enableWallet
                        { id = id
                        , extensions = List.filter (\ext -> ext == 95) supportedExtensions
                        , watchInterval = Nothing
                        }
                    )
                )
            )

        DisconnectWalletClicked ->
            ( { model | wallet = Nothing }, Cmd.none )

        OnTaskProgress ( taskPool, cmd ) ->
            ( { model | taskPool = taskPool }, cmd )

        OnTaskComplete response ->
            case response of
                ConcurrentTask.Success (GotProposalMetadata id result) ->
                    let
                        updateMetadata maybeProposal =
                            case ( maybeProposal, result ) of
                                ( Just p, Ok metadata ) ->
                                    Just { p | metadata = Success metadata }

                                ( Just p, Err error ) ->
                                    Just { p | metadata = Failure error }

                                ( Nothing, _ ) ->
                                    Nothing
                    in
                    ( { model | proposals = RemoteData.map (Dict.update id updateMetadata) model.proposals }
                    , Cmd.none
                    )

                ConcurrentTask.Success Ignore ->
                    ( model, Cmd.none )

                ConcurrentTask.Error err ->
                    ( { model | errors = err :: model.errors }, Cmd.none )

                ConcurrentTask.UnexpectedError _ ->
                    ( model, Cmd.none )


walletResponseDecoder : JD.Decoder (Cip30.Response ())
walletResponseDecoder =
    Cip30.responseDecoder Dict.empty


handleWalletResponse : Cip30.Response () -> Model -> ( Model, Cmd Msg )
handleWalletResponse response model =
    case response of
        Cip30.AvailableWallets wallets ->
            ( { model | walletsDiscovered = wallets }, Cmd.none )

        Cip30.EnabledWallet wallet ->
            ( { model | wallet = Just wallet }, Cmd.none )

        Cip30.ApiError { info } ->
            ( { model | errors = info :: model.errors }, Cmd.none )

        Cip30.UnhandledResponseType err ->
            ( { model | errors = err :: model.errors }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ fromWallet WalletMsg
        , ConcurrentTask.onProgress
            { send = sendTask
            , receive = receiveTask
            , onProgress = OnTaskProgress
            }
            model.taskPool
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Cardano Governance Proposals" ]
        , viewNetworkInfo model.networkId
        , viewWalletBar model
        , viewStatus model
        , viewProposals model
        , viewErrors model.errors
        ]


viewNetworkInfo : NetworkId -> Html Msg
viewNetworkInfo networkId =
    p [ HA.class "meta" ]
        [ text <|
            "Network: "
                ++ (case networkId of
                        Mainnet ->
                            "Mainnet"

                        Testnet ->
                            "Preview (Testnet)"
                   )
        ]


viewWalletBar : Model -> Html Msg
viewWalletBar model =
    div [ HA.class "wallet-bar" ]
        (case model.wallet of
            Just wallet ->
                [ span [ HA.class "connected" ]
                    [ text ("Connected: " ++ (Cip30.walletDescriptor wallet).name) ]
                , button [ onClick DisconnectWalletClicked ] [ text "Disconnect" ]
                ]

            Nothing ->
                if List.isEmpty model.walletsDiscovered then
                    [ span [ HA.class "meta" ] [ text "No wallets detected" ] ]

                else
                    List.map
                        (\w ->
                            button
                                [ onClick (ConnectWalletClicked { id = w.id, supportedExtensions = w.supportedExtensions }) ]
                                [ text ("Connect " ++ w.name) ]
                        )
                        model.walletsDiscovered
        )


viewStatus : Model -> Html Msg
viewStatus model =
    div []
        [ case model.protocolParams of
            Nothing ->
                p [ HA.class "loading" ] [ text "Loading protocol parameters..." ]

            Just _ ->
                text ""
        , case model.epoch of
            Loading ->
                p [ HA.class "loading" ] [ text "Loading epoch..." ]

            Success epoch ->
                p [ HA.class "meta" ] [ text ("Current epoch: " ++ String.fromInt epoch) ]

            Failure _ ->
                p [ HA.class "error" ] [ text "Failed to load epoch" ]

            NotAsked ->
                text ""
        ]


viewProposals : Model -> Html Msg
viewProposals model =
    case model.proposals of
        NotAsked ->
            text ""

        Loading ->
            p [ HA.class "loading" ] [ text "Loading proposals..." ]

        Failure _ ->
            p [ HA.class "error" ] [ text "Failed to load proposals" ]

        Success proposals ->
            let
                sortedProposals =
                    Dict.toList proposals
                        |> List.sortBy (\( _, p ) -> negate p.epoch_validity.end)
            in
            div []
                [ p [ HA.class "meta" ]
                    [ text (String.fromInt (Dict.size proposals) ++ " active proposals") ]
                , div [ HA.class "proposals" ]
                    (List.map viewProposal sortedProposals)
                ]


viewProposal : ( String, ActiveProposal ) -> Html Msg
viewProposal ( bech32Id, proposal ) =
    div [ HA.class "proposal" ]
        [ div []
            [ span [ HA.class "badge" ] [ text proposal.actionType ]
            , span [ HA.class "meta", HA.style "margin-left" "0.5rem" ]
                [ text ("expires epoch " ++ String.fromInt proposal.epoch_validity.end) ]
            ]
        , case proposal.metadata of
            Success metadata ->
                div []
                    [ h3 []
                        [ text (Maybe.withDefault "(no title)" metadata.body.title) ]
                    , case metadata.body.abstract of
                        Just abstract ->
                            p [] [ text abstract ]

                        Nothing ->
                            text ""
                    , viewHashValidity proposal.metadataHash metadata.computedHash
                    , if not (List.isEmpty metadata.authors) then
                        p [ HA.class "meta" ]
                            [ text ("Authors: " ++ String.join ", " (List.map .name metadata.authors)) ]

                      else
                        text ""
                    ]

            Loading ->
                p [ HA.class "loading" ] [ text "Loading metadata..." ]

            Failure err ->
                p [ HA.class "error" ] [ text ("Metadata error: " ++ err) ]

            NotAsked ->
                text ""
        , p [ HA.class "meta" ]
            [ text bech32Id ]
        ]


viewHashValidity : String -> String -> Html Msg
viewHashValidity onchainHash computedHash =
    if onchainHash == computedHash then
        p [ HA.class "meta hash-match" ] [ text "Hash verified" ]

    else
        p [ HA.class "meta hash-mismatch" ] [ text "Hash mismatch!" ]


viewErrors : List String -> Html Msg
viewErrors errors =
    if List.isEmpty errors then
        text ""

    else
        div []
            (List.map (\err -> p [ HA.class "error" ] [ text err ]) errors)



-- HELPERS


actionIdToBech32 : Gov.ActionId -> String
actionIdToBech32 actionId =
    Gov.idToBech32 (Gov.GovActionId actionId)



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
