port module Main exposing (main)

{-| Minimal Cardano governance app: initializes Cardano-related code
and displays current proposals with their metadata.
Includes CIP-179 survey display and creation.
-}

import Api exposing (ActiveProposal)
import Browser
import Bytes.Comparable as Bytes
import Cardano.Address exposing (Credential(..), NetworkId(..))
import Cardano.Cip30 as Cip30 exposing (WalletDescriptor)
import Cardano.Gov as Gov
import Cardano.Metadatum as Metadatum
import Cardano.Transaction as Transaction exposing (Transaction)
import Cardano.TxIntent as TxIntent exposing (SpendSource(..), TxIntent(..), TxOtherInfo(..))
import Cardano.Utxo as Utxo exposing (Output)
import Cardano.Value as Value
import ConcurrentTask
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, h3, nav, p, pre, span, text)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Http
import Integer
import Json.Decode as JD exposing (Decoder, Value)
import Natural as N
import ProposalMetadata exposing (ProposalMetadata)
import RemoteData exposing (RemoteData(..), WebData)
import Storage
import Survey



-- PORTS


port toWallet : Value -> Cmd msg


port fromWallet : (Value -> msg) -> Sub msg


port sendTask : Value -> Cmd msg


port receiveTask : (Value -> msg) -> Sub msg



-- MODEL


type Tab
    = ProposalsTab
    | SurveysTab
    | CreateSurveyTab


type SubmissionStatus
    = NotSubmitting
    | WaitingForSignature { tx : Transaction, survey : Survey.SurveyDefinition }
    | WaitingForSubmission { tx : Transaction, survey : Survey.SurveyDefinition }
    | Submitted { txId : String, survey : Survey.SurveyDefinition }
    | SubmissionError String


type alias OnchainSurvey =
    { txHash : String
    , index : Int
    , definition : Survey.SurveyDefinition
    }


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
    , activeTab : Tab
    , surveyForm : Survey.SurveyForm
    , surveyFormError : Maybe String
    , createdSurveys : List Survey.SurveyDefinition
    , onchainSurveys : WebData (List OnchainSurvey)
    , walletUtxos : Maybe (Utxo.RefDict Output)
    , submissionStatus : SubmissionStatus
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
            , activeTab = SurveysTab
            , surveyForm = Survey.emptyForm
            , surveyFormError = Nothing
            , createdSurveys = []
            , onchainSurveys = NotAsked
            , walletUtxos = Nothing
            , submissionStatus = NotSubmitting
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
    | TabClicked Tab
    | SurveyFormMsg Survey.FormMsg
    | GotSurveyTxHashes (Result Http.Error (List String))
    | GotSurveyMetadata (Result Http.Error (List Api.SurveyTxMetadata))


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
                    ( { model | epoch = Success epoch, proposals = Loading, onchainSurveys = Loading }
                    , Cmd.batch
                        -- Deactivate temporarily proposals
                        -- [ Api.loadGovProposals model.networkId epoch GotProposals
                        [ Api.loadSurveyTxHashes model.networkId GotSurveyTxHashes
                        ]
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
            ( { model | wallet = Nothing, walletUtxos = Nothing, submissionStatus = NotSubmitting }, Cmd.none )

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

        TabClicked tab ->
            ( { model | activeTab = tab }, Cmd.none )

        SurveyFormMsg formMsg ->
            case formMsg of
                Survey.SubmitSurvey ->
                    submitSurvey model

                _ ->
                    ( { model
                        | surveyForm = Survey.updateForm formMsg model.surveyForm
                        , submissionStatus =
                            case model.submissionStatus of
                                SubmissionError _ ->
                                    NotSubmitting

                                _ ->
                                    model.submissionStatus
                      }
                    , Cmd.none
                    )

        GotSurveyTxHashes result ->
            case result of
                Err err ->
                    ( { model | onchainSurveys = Failure err }, Cmd.none )

                Ok txHashes ->
                    if List.isEmpty txHashes then
                        ( { model | onchainSurveys = Success [] }, Cmd.none )

                    else
                        ( model, Api.loadSurveyMetadata model.networkId txHashes GotSurveyMetadata )

        GotSurveyMetadata result ->
            case result of
                Err err ->
                    ( { model | onchainSurveys = Failure err }, Cmd.none )

                Ok txMetaList ->
                    let
                        currentEpoch =
                            RemoteData.withDefault 0 model.epoch

                        surveys =
                            List.concatMap
                                (\txMeta ->
                                    case Survey.fromMetadatum txMeta.metadatum of
                                        Ok defs ->
                                            List.indexedMap
                                                (\i def ->
                                                    { txHash = txMeta.txHash
                                                    , index = i
                                                    , definition = def
                                                    }
                                                )
                                                defs

                                        Err _ ->
                                            []
                                )
                                txMetaList

                        validSurveys =
                            List.filter (\s -> s.definition.endEpoch >= currentEpoch) surveys
                    in
                    ( { model | onchainSurveys = Success validSurveys }, Cmd.none )


submitSurvey : Model -> ( Model, Cmd Msg )
submitSurvey model =
    case Survey.formToDefinition model.surveyForm of
        Err err ->
            ( { model | surveyFormError = Just err }, Cmd.none )

        Ok def ->
            case ( model.wallet, model.walletUtxos ) of
                ( Nothing, _ ) ->
                    ( { model | surveyFormError = Just "Please connect a wallet first" }, Cmd.none )

                ( _, Nothing ) ->
                    ( { model | surveyFormError = Just "Wallet UTxOs not loaded yet" }, Cmd.none )

                ( Just wallet, Just utxos ) ->
                    let
                        changeAddr =
                            Cip30.walletChangeAddress wallet

                        surveyMetadatum =
                            Survey.toMetadatum def

                        -- CIP-179: owner key hash must be in required_signers
                        requiredSignerInfo =
                            case def.owner of
                                VKeyHash hash ->
                                    [ TxRequiredSigner hash ]

                                ScriptHash _ ->
                                    []

                        txResult =
                            TxIntent.finalize utxos
                                (TxMetadata { tag = N.fromSafeInt Survey.metadataLabel, metadata = surveyMetadatum }
                                    :: requiredSignerInfo
                                )
                                [ Spend (FromWallet { address = changeAddr, value = Value.onlyLovelace N.zero, guaranteedUtxos = [] }) ]
                    in
                    case txResult of
                        Err err ->
                            ( { model | surveyFormError = Just (TxIntent.errorToString err) }, Cmd.none )

                        Ok { tx } ->
                            ( { model
                                | submissionStatus = WaitingForSignature { tx = tx, survey = def }
                                , surveyFormError = Nothing
                              }
                            , toWallet (Cip30.encodeRequest (Cip30.signTx wallet { partialSign = False } tx))
                            )


walletResponseDecoder : Decoder (Cip30.Response Cip30.ApiResponse)
walletResponseDecoder =
    Cip30.responseDecoder
        (Dict.fromList [ ( 30, Cip30.apiDecoder ) ])


handleWalletResponse : Cip30.Response Cip30.ApiResponse -> Model -> ( Model, Cmd Msg )
handleWalletResponse response model =
    case response of
        Cip30.AvailableWallets wallets ->
            ( { model | walletsDiscovered = wallets }, Cmd.none )

        Cip30.EnabledWallet wallet ->
            ( { model | wallet = Just wallet }
            , toWallet
                (Cip30.encodeRequest
                    (Cip30.getUtxos wallet { amount = Nothing, paginate = Nothing })
                )
            )

        Cip30.ApiResponse _ apiResponse ->
            handleApiResponse apiResponse model

        Cip30.ApiError { info } ->
            let
                submissionUpdate =
                    case model.submissionStatus of
                        WaitingForSignature _ ->
                            SubmissionError ("Wallet error: " ++ info)

                        WaitingForSubmission _ ->
                            SubmissionError ("Submission error: " ++ info)

                        other ->
                            other
            in
            ( { model
                | errors = info :: model.errors
                , submissionStatus = submissionUpdate
              }
            , Cmd.none
            )

        Cip30.UnhandledResponseType err ->
            ( { model | errors = err :: model.errors }, Cmd.none )


handleApiResponse : Cip30.ApiResponse -> Model -> ( Model, Cmd Msg )
handleApiResponse apiResponse model =
    case apiResponse of
        Cip30.WalletUtxos utxos ->
            ( { model | walletUtxos = Just (Utxo.refDictFromList utxos) }, Cmd.none )

        Cip30.ChangeAddress addr ->
            ( { model | wallet = Maybe.map (Cip30.updateChangeAddress addr) model.wallet }, Cmd.none )

        Cip30.SignedTx vkeyWitnesses ->
            case model.submissionStatus of
                WaitingForSignature { tx, survey } ->
                    let
                        signedTx =
                            Transaction.updateSignatures (\_ -> Just vkeyWitnesses) tx
                    in
                    case model.wallet of
                        Just wallet ->
                            ( { model | submissionStatus = WaitingForSubmission { tx = signedTx, survey = survey } }
                            , toWallet (Cip30.encodeRequest (Cip30.submitTx wallet signedTx))
                            )

                        Nothing ->
                            ( { model | submissionStatus = SubmissionError "Wallet disconnected" }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Cip30.SubmittedTx txId ->
            case model.submissionStatus of
                WaitingForSubmission { survey } ->
                    ( { model
                        | submissionStatus = Submitted { txId = Bytes.toHex txId, survey = survey }
                        , createdSurveys = survey :: model.createdSurveys
                        , surveyForm = Survey.emptyForm
                        , surveyFormError = Nothing
                        , activeTab = SurveysTab
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

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
        [ h1 [] [ text "Cardano Governance" ]
        , viewNetworkInfo model.networkId
        , viewWalletBar model
        , viewStatus model
        , viewTabs model.activeTab
        , case model.activeTab of
            ProposalsTab ->
                viewProposals model

            SurveysTab ->
                viewSurveysTab model

            CreateSurveyTab ->
                viewCreateSurveyTab model
        , viewErrors model.errors
        ]


viewTabs : Tab -> Html Msg
viewTabs activeTab =
    nav [ HA.class "tabs" ]
        -- Disable proposals tab temporarily
        -- [ tabButton ProposalsTab "Proposals" activeTab
        [ tabButton SurveysTab "Surveys" activeTab
        , tabButton CreateSurveyTab "Create Survey" activeTab
        ]


tabButton : Tab -> String -> Tab -> Html Msg
tabButton tab label activeTab =
    button
        [ HA.class
            (if tab == activeTab then
                "tab active"

             else
                "tab"
            )
        , onClick (TabClicked tab)
        ]
        [ text label ]


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



-- PROPOSALS TAB


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



-- SURVEYS TAB


viewSurveysTab : Model -> Html Msg
viewSurveysTab model =
    div []
        [ case model.onchainSurveys of
            NotAsked ->
                text ""

            Loading ->
                p [ HA.class "loading" ] [ text "Loading on-chain surveys..." ]

            Failure _ ->
                p [ HA.class "error" ] [ text "Failed to load surveys from chain" ]

            Success surveys ->
                if List.isEmpty surveys then
                    div [ HA.class "empty-state" ]
                        [ p [] [ text "No active CIP-179 surveys found on-chain." ]
                        , p [ HA.class "meta" ]
                            [ text "Create one in the "
                            , button
                                [ HA.class "link-btn"
                                , onClick (TabClicked CreateSurveyTab)
                                ]
                                [ text "Create Survey" ]
                            , text " tab."
                            ]
                        ]

                else
                    div []
                        [ p [ HA.class "meta" ]
                            [ text (String.fromInt (List.length surveys) ++ " active survey(s) on-chain") ]
                        , div [ HA.class "proposals" ]
                            (List.map viewOnchainSurvey surveys)
                        ]
        ]


viewOnchainSurvey : OnchainSurvey -> Html Msg
viewOnchainSurvey survey =
    div []
        [ Survey.viewSurvey survey.definition
        , p [ HA.class "meta" ]
            [ text ("Tx: " ++ survey.txHash ++ " [" ++ String.fromInt survey.index ++ "]") ]
        ]



-- CREATE SURVEY TAB


viewCreateSurveyTab : Model -> Html Msg
viewCreateSurveyTab model =
    div []
        [ Survey.viewSurveyForm model.surveyForm model.surveyFormError (submitButtonLabel model) SurveyFormMsg
        , viewSubmissionStatus model.submissionStatus
        , case Survey.formToDefinition model.surveyForm of
            Ok def ->
                div [ HA.class "metadatum-preview" ]
                    [ h3 [] [ text ("Preview: Metadatum (label " ++ String.fromInt Survey.metadataLabel ++ ")") ]
                    , pre [] [ text (metadatumToString (Survey.toMetadatum def)) ]
                    ]

            Err _ ->
                text ""
        ]


submitButtonLabel : Model -> String
submitButtonLabel model =
    case model.submissionStatus of
        WaitingForSignature _ ->
            "Awaiting signature..."

        WaitingForSubmission _ ->
            "Submitting..."

        _ ->
            case model.wallet of
                Nothing ->
                    "Connect wallet to submit"

                Just _ ->
                    "Submit Survey On-Chain"


viewSubmissionStatus : SubmissionStatus -> Html Msg
viewSubmissionStatus status =
    case status of
        NotSubmitting ->
            text ""

        WaitingForSignature _ ->
            p [ HA.class "loading" ] [ text "Waiting for wallet signature..." ]

        WaitingForSubmission _ ->
            p [ HA.class "loading" ] [ text "Submitting transaction..." ]

        Submitted { txId } ->
            p [ HA.class "meta hash-match" ] [ text ("Transaction submitted! TxId: " ++ txId) ]

        SubmissionError err ->
            p [ HA.class "error" ] [ text ("Submission failed: " ++ err) ]



-- ERRORS


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


metadatumToString : Metadatum.Metadatum -> String
metadatumToString m =
    metadatumToStringHelper 0 m


metadatumToStringHelper : Int -> Metadatum.Metadatum -> String
metadatumToStringHelper indent m =
    let
        pad =
            String.repeat (indent * 2) " "
    in
    case m of
        Metadatum.Int i ->
            String.fromInt (Integer.toInt i)

        Metadatum.String s ->
            "\"" ++ s ++ "\""

        Metadatum.Bytes b ->
            "h'" ++ Bytes.toHex (Bytes.toAny b) ++ "'"

        Metadatum.List items ->
            if List.isEmpty items then
                "[]"

            else
                "[\n"
                    ++ String.join ",\n"
                        (List.map
                            (\item -> pad ++ "  " ++ metadatumToStringHelper (indent + 1) item)
                            items
                        )
                    ++ "\n"
                    ++ pad
                    ++ "]"

        Metadatum.Map pairs ->
            if List.isEmpty pairs then
                "{}"

            else
                "{\n"
                    ++ String.join ",\n"
                        (List.map
                            (\( k, v ) ->
                                pad
                                    ++ "  "
                                    ++ metadatumToStringHelper (indent + 1) k
                                    ++ ": "
                                    ++ metadatumToStringHelper (indent + 1) v
                            )
                            pairs
                        )
                    ++ "\n"
                    ++ pad
                    ++ "}"



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
