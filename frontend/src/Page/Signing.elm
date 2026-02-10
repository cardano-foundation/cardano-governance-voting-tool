module Page.Signing exposing (Model, Msg, UpdateContext, ViewContext, addWalletSignatures, clearError, getSignedTx, getTxInfo, initialModel, recordSubmittedTx, resetSubmission, update, view)

{-| This module handles the signing process for Cardano transactions, particularly
focusing on complex scenarios like Native or Plutus script multi-signatures.

Key design features:

  - Supports both connected wallet signing and file-based signature collection
  - Tracks expected signers
  - Allows transaction submission even with partial signatures (useful for M-of-N schemes)
  - Handles signature deduplication and verification against expected signers

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address exposing (CredentialHash, NetworkId(..))
import Cardano.Cip30 as Cip30
import Cardano.Transaction as Transaction exposing (Transaction, VKeyWitness)
import Cardano.TxExamples exposing (prettyTx)
import Cardano.Utxo exposing (TransactionId)
import Dict exposing (Dict)
import File exposing (File)
import File.Select
import Helper
import Html exposing (Html, div, text)
import Html.Attributes as HA
import Json.Decode as JD
import Json.Encode as JE
import Task
import Url



-- ###################################################################
-- MODEL
-- ###################################################################


{-| Represents the two possible states of the signing page:

  - MissingTx: No transaction loaded yet
  - LoadedTx: Active transaction being processed for signatures

-}
type Model
    = MissingTx
    | LoadedTx LoadedTxModel


{-| Core model for an active signing session.
Tracks the transaction, expected signers, collected signatures,
submission status and any errors that occur.
-}
type alias LoadedTxModel =
    { tx : Transaction
    , txId : Bytes TransactionId
    , expectedSigners : Dict String { keyName : String, keyHash : Bytes CredentialHash }
    , vkeyWitnesses : Dict String VKeyWitness
    , txSubmitted : Maybe (Bytes TransactionId)
    , error : Maybe String
    }


initialModel : List { keyName : String, keyHash : Bytes CredentialHash } -> Maybe Transaction -> Model
initialModel expectedSigners maybeTx =
    case maybeTx of
        Just tx ->
            let
                expectedSignersDict =
                    expectedSigners
                        |> List.map (\{ keyName, keyHash } -> ( Bytes.toHex keyHash, { keyName = keyName, keyHash = keyHash } ))
                        |> Dict.fromList

                existingWitnesses =
                    tx.witnessSet.vkeywitness
                        |> Maybe.withDefault []
                        |> List.filter (\w -> Dict.member (Bytes.toHex (Transaction.hashVKey w.vkey)) expectedSignersDict)
                        |> List.map (\w -> ( Bytes.toHex (Transaction.hashVKey w.vkey), w ))
                        |> Dict.fromList
            in
            LoadedTx
                { tx = tx
                , txId = Transaction.computeTxId tx
                , expectedSigners = expectedSignersDict
                , vkeyWitnesses = existingWitnesses
                , txSubmitted = Nothing
                , error = Nothing
                }

        Nothing ->
            MissingTx


getTxInfo : Model -> Maybe { tx : Transaction, txId : Bytes TransactionId }
getTxInfo model =
    case model of
        LoadedTx loadedTxModel ->
            Just { tx = loadedTxModel.tx, txId = loadedTxModel.txId }

        _ ->
            Nothing


{-| Returns the Tx with currently gathered vkeyWitnesses baked in.
Returns the original Tx unchanged if no signatures have been gathered.
-}
getSignedTx : Model -> Maybe Transaction
getSignedTx model =
    case model of
        LoadedTx { tx, vkeyWitnesses } ->
            if Dict.isEmpty vkeyWitnesses then
                Just tx

            else
                Just (Transaction.updateSignatures (\_ -> Just (Dict.values vkeyWitnesses)) tx)

        MissingTx ->
            Nothing



-- ###################################################################
-- UPDATE
-- ###################################################################


type Msg
    = NoMsg
    | SignTxButtonClicked
    | LoadSignedTxButtonClicked
    | SignedTxFileSelected File
    | LoadedSignedTxJson String
    | SubmitTxButtonClicked


type alias UpdateContext msg =
    { wrapMsg : Msg -> msg
    , wallet : Maybe Cip30.Wallet
    , walletSignTx : Transaction -> Cmd msg
    , walletSubmitTx : Transaction -> Cmd msg
    }


update : UpdateContext msg -> Msg -> Model -> ( Model, Cmd msg )
update ctx msg model =
    case ( msg, model ) of
        ( SignTxButtonClicked, LoadedTx loadedTxModel ) ->
            ( model
            , ctx.walletSignTx loadedTxModel.tx
            )

        ( SignTxButtonClicked, _ ) ->
            ( model, Cmd.none )

        ( LoadSignedTxButtonClicked, LoadedTx _ ) ->
            ( model
            , Cmd.map ctx.wrapMsg <|
                File.Select.file [] SignedTxFileSelected
            )

        ( LoadSignedTxButtonClicked, _ ) ->
            ( model, Cmd.none )

        ( SignedTxFileSelected file, LoadedTx _ ) ->
            ( model
            , Task.attempt handleSignedTxFileRead (File.toString file)
                |> Cmd.map ctx.wrapMsg
            )

        ( SignedTxFileSelected _, _ ) ->
            ( model, Cmd.none )

        ( LoadedSignedTxJson txJsonStr, LoadedTx loadedTxModel ) ->
            case extractVkeyWitnesses loadedTxModel.txId txJsonStr of
                Ok sigs ->
                    ( addWalletSignatures sigs model, Cmd.none )

                Err error ->
                    ( LoadedTx { loadedTxModel | error = Just error }, Cmd.none )

        ( LoadedSignedTxJson _, _ ) ->
            ( model, Cmd.none )

        ( SubmitTxButtonClicked, LoadedTx loadedTxModel ) ->
            case ctx.wallet of
                Nothing ->
                    ( LoadedTx { loadedTxModel | error = Just "Please connect a wallet to submit the transaction via the CIP-30 wallet API." }
                    , Cmd.none
                    )

                Just _ ->
                    let
                        signedTx =
                            Transaction.updateSignatures (\_ -> Just <| Dict.values loadedTxModel.vkeyWitnesses) loadedTxModel.tx
                    in
                    ( LoadedTx { loadedTxModel | error = Nothing }
                    , ctx.walletSubmitTx signedTx
                    )

        ( SubmitTxButtonClicked, _ ) ->
            ( model, Cmd.none )

        ( NoMsg, _ ) ->
            ( model, Cmd.none )


handleSignedTxFileRead : Result x String -> Msg
handleSignedTxFileRead result =
    case result of
        Err _ ->
            NoMsg

        Ok signedTxJson ->
            LoadedSignedTxJson signedTxJson


{-| Extracts VKey witnesses from a serialized transaction JSON, performing several validations:

1.  Ensures the transaction can be properly decoded
2.  Verifies the transaction ID matches the expected one
3.  Returns the list of witnesses if all checks pass

-}
extractVkeyWitnesses : Bytes TransactionId -> String -> Result String (List VKeyWitness)
extractVkeyWitnesses txId rawJson =
    let
        txJsonDecoder =
            JD.field "cborHex" JD.string
                |> JD.andThen
                    (\txHex ->
                        case Bytes.fromHex txHex |> Maybe.andThen Transaction.deserialize of
                            Just tx ->
                                JD.succeed tx

                            Nothing ->
                                JD.fail <| "Unable to decode Tx hex: " ++ txHex
                    )
    in
    case JD.decodeString txJsonDecoder rawJson of
        Ok tx ->
            let
                decodedTxId =
                    Transaction.computeTxId tx
            in
            if decodedTxId /= txId then
                Err <| "The wrong Tx was uploaded. It has a Tx ID of " ++ Bytes.toHex decodedTxId ++ " instead of " ++ Bytes.toHex txId

            else
                Ok <| Maybe.withDefault [] tx.witnessSet.vkeywitness

        Err error ->
            Err <| "Error while decoding the Tx: " ++ JD.errorToString error


{-| Adds new signatures to the model, with important filtering logic:

  - Only accepts signatures from expected signers
  - Handles cases where wallets might provide more signatures than needed
  - Updates existing signatures for the same key hash

-}
addWalletSignatures : List VKeyWitness -> Model -> Model
addWalletSignatures newVkeyWitnesses model =
    case model of
        LoadedTx loadedTxModel ->
            let
                -- Filter out unexpected signatures.
                -- Because sometimes, a wallet will provide more signatures than strictly needed.
                -- For example for some 1-of-n native multisig, a wallet might provide multiple signatures
                -- even if we only want 1, and only paid fees for one.
                expectedVkeyWitnesses =
                    List.filter keyHashIsExpected newVkeyWitnesses

                keyHashIsExpected vkeyWitness =
                    Dict.member (Bytes.toHex <| Transaction.hashVKey vkeyWitness.vkey) loadedTxModel.expectedSigners

                updatedWitnesses =
                    List.foldl (\w acc -> Dict.insert (keyHashHex w.vkey) w acc)
                        loadedTxModel.vkeyWitnesses
                        expectedVkeyWitnesses

                keyHashHex vkey =
                    Transaction.hashVKey vkey
                        |> Bytes.toHex
            in
            LoadedTx { loadedTxModel | vkeyWitnesses = updatedWitnesses }

        _ ->
            model


clearError : Model -> Model
clearError model =
    case model of
        LoadedTx loadedTxModel ->
            LoadedTx { loadedTxModel | error = Nothing }

        MissingTx ->
            MissingTx


recordSubmittedTx : Bytes TransactionId -> Model -> Model
recordSubmittedTx txId model =
    case model of
        LoadedTx loadedTxModel ->
            LoadedTx { loadedTxModel | txSubmitted = Just txId }

        _ ->
            model


resetSubmission : String -> Model -> Model
resetSubmission error model =
    case model of
        MissingTx ->
            MissingTx

        LoadedTx loadedTxModel ->
            LoadedTx { loadedTxModel | error = Just error, txSubmitted = Nothing }



-- ###################################################################
-- VIEW
-- ###################################################################


type alias ViewContext msg =
    { wrapMsg : Msg -> msg
    , wallet : Maybe Cip30.Wallet
    , networkId : NetworkId
    }


view : ViewContext msg -> Model -> Html msg
view ctx model =
    div
        [ HA.style "max-width" "1024px"
        , HA.style "margin" "0 auto"
        , HA.style "padding" "0rem 2rem"
        ]
        [ Html.h2
            [ HA.style "font-weight" "600"
            , HA.style "font-size" "1.875rem"
            , HA.style "color" "#1A202C"
            , HA.style "margin" "1.5rem 0"
            ]
            [ text "Signing the Transaction" ]
        , div
            [ HA.style "background-color" "#F9FAFB"
            , HA.style "border" "1px solid #E2E8F0"
            , HA.style "border-radius" "0.5rem"
            , HA.style "padding" "1.25rem"
            , HA.style "margin-bottom" "2rem"
            ]
            [ Html.p
                [ HA.style "color" "#4A5568"
                , HA.style "font-size" "1rem"
                ]
                [ text "This page facilitates complex signature processes, such as Native or Plutus scripts multi-sig." ]
            ]
        , case model of
            MissingTx ->
                div
                    [ HA.style "border" "1px solid #E2E8F0"
                    , HA.style "border-radius" "0.75rem"
                    , HA.style "box-shadow" "0 2px 4px rgba(0,0,0,0.06)"
                    , HA.style "background-color" "#FFFFFF"
                    , HA.style "padding" "1.5rem"
                    , HA.style "margin-bottom" "1.5rem"
                    ]
                    [ Html.p
                        [ HA.style "color" "#718096"
                        , HA.style "font-style" "italic"
                        ]
                        [ text "No transaction loaded. Please go back to the preparation page and create a transaction first." ]
                    ]

            LoadedTx { tx, txId, expectedSigners, vkeyWitnesses, txSubmitted, error } ->
                let
                    sectionTitle title =
                        Html.h3
                            [ HA.style "font-weight" "600"
                            , HA.style "font-size" "1.25rem"
                            , HA.style "color" "#2D3748"
                            , HA.style "margin" "1.5rem 0 1rem 0"
                            ]
                            [ text title ]

                    cardContainer content =
                        Helper.cardContainer []
                            [ Helper.cardHeader [] "Transaction Details" "" []
                            , Helper.cardContent [] content
                            ]

                    gatheredSignaturesSection =
                        div
                            [ HA.style "border" "1px solid #E2E8F0"
                            , HA.style "border-radius" "0.75rem"
                            , HA.style "box-shadow" "0 2px 4px rgba(0,0,0,0.06)"
                            , HA.style "background-color" "#FFFFFF"
                            , HA.style "overflow" "hidden"
                            , HA.style "margin-bottom" "1.5rem"
                            ]
                            [ div
                                [ HA.style "padding" "1.25rem" ]
                                [ div [ HA.style "margin-bottom" "1rem" ] (viewExpectedSignatures expectedSigners vkeyWitnesses)
                                , Html.p
                                    [ HA.style "color" "#718096"
                                    , HA.style "font-style" "italic"
                                    , HA.style "font-size" "0.875rem"
                                    ]
                                    [ text "At least one of these signatures is to pay the transaction fees." ]
                                ]
                            ]

                    signSection =
                        let
                            downloadButton label description fileName someTx =
                                Html.a
                                    [ HA.href <| "data:application/json;charset=utf-8," ++ Url.percentEncode (txUnsignedJson description someTx)
                                    , HA.download fileName
                                    , HA.class "inline-block"
                                    ]
                                    [ Helper.viewButton label (ctx.wrapMsg NoMsg) ]

                            txUnsignedJson description someTx =
                                JE.encode 2 <|
                                    JE.object
                                        [ ( "type", JE.string "Tx ConwayEra" )
                                        , ( "description", JE.string description )
                                        , ( "cborHex", JE.string <| Bytes.toHex <| Transaction.serialize someTx )
                                        ]
                        in
                        Helper.cardContainer []
                            [ Helper.cardHeader [] "Sign Transaction" "" []
                            , Helper.cardContent []
                                [ if ctx.wallet == Nothing then
                                    Html.p
                                        [ HA.style "color" "#F59E0B"
                                        , HA.style "margin-bottom" "1rem"
                                        , HA.style "font-weight" "500"
                                        ]
                                        [ text "If you want to sign with your web wallet, you need to connect it (see the page top)." ]

                                  else if Dict.isEmpty vkeyWitnesses then
                                    Html.p [ HA.style "margin-bottom" "1rem" ]
                                        [ Helper.viewButton "Sign with connected wallet" (ctx.wrapMsg SignTxButtonClicked) ]

                                  else
                                    let
                                        signedTx =
                                            Transaction.updateSignatures (\_ -> Just <| Dict.values vkeyWitnesses) tx
                                    in
                                    Html.div
                                        [ HA.style "display" "flex"
                                        , HA.style "align-items" "center"
                                        , HA.style "gap" "1rem"
                                        , HA.style "margin-bottom" "1rem"
                                        ]
                                        [ Helper.viewButton "Sign with connected wallet" (ctx.wrapMsg SignTxButtonClicked)
                                        , downloadButton "Download partially signed Tx" "signed" "tx-signed.json" signedTx
                                        ]
                                , Html.div [ HA.style "margin-bottom" "1rem" ]
                                    [ Html.p
                                        [ HA.style "color" "#4A5568"
                                        , HA.style "font-size" "0.9375rem"
                                        , HA.style "line-height" "1.5"
                                        , HA.style "margin-bottom" "1rem"
                                        ]
                                        [ text "If additional signatures are required, ask the relevant parties to partially sign the transaction and use the button below to load their signatures. You can share this page URL with them—it contains all transaction details they need for signing on their device." ]
                                    ]
                                , Html.div
                                    [ HA.style "background-color" "#F9FAFB"
                                    , HA.style "border" "1px solid #E2E8F0"
                                    , HA.style "border-radius" "0.5rem"
                                    , HA.style "padding" "1rem"
                                    , HA.style "margin-bottom" "1.5rem"
                                    ]
                                    [ Html.p
                                        [ HA.style "color" "#4A5568"
                                        , HA.style "font-size" "0.9375rem"
                                        , HA.style "margin-bottom" "0.5rem"
                                        ]
                                        [ text "Verify this transaction before signing using "
                                        , Html.a
                                            [ HA.href <| "https://council-toolkit.gov.tools/tx#" ++ (Bytes.toHex <| Transaction.serialize tx)
                                            , HA.target "_blank"
                                            , HA.rel "noopener noreferrer"
                                            , HA.style "color" "#3182CE"
                                            , HA.style "text-decoration" "underline"
                                            , HA.style "font-family" "monospace"
                                            ]
                                            [ text "council-toolkit.gov.tools" ]
                                        ]
                                    ]
                                , Html.div
                                    [ HA.style "display" "flex"
                                    , HA.style "align-items" "center"
                                    , HA.style "gap" "1rem"
                                    ]
                                    [ downloadButton "Download unsigned Tx" "unsigned" "tx-unsigned.json" tx
                                    , Helper.viewButton "Load signed Tx file" (ctx.wrapMsg LoadSignedTxButtonClicked)
                                    ]
                                ]
                            ]

                    submissionSection hasAllSignatures =
                        div
                            [ HA.style "border" "1px solid #E2E8F0"
                            , HA.style "border-radius" "0.75rem"
                            , HA.style "box-shadow" "0 2px 4px rgba(0,0,0,0.06)"
                            , HA.style "background-color" "#FFFFFF"
                            , HA.style "overflow" "hidden"
                            , HA.style "margin-bottom" "1.5rem"
                            ]
                            [ div
                                [ HA.style "padding" "1.25rem" ]
                                [ Html.p
                                    [ HA.style "color" "#4A5568"
                                    , HA.style "margin-bottom" "1.5rem"
                                    ]
                                    [ if hasAllSignatures then
                                        text "All required signatures have been collected."

                                      else if Dict.isEmpty expectedSigners then
                                        text "Expected signers are unknown. Submit when you believe the transaction is ready."

                                      else
                                        text "Not all expected signatures are gathered yet, but you can still submit if ready."
                                    ]
                                , if hasAllSignatures then
                                    Helper.viewButton "Submit Transaction" (ctx.wrapMsg SubmitTxButtonClicked)

                                  else if Dict.isEmpty expectedSigners then
                                    Helper.viewButton "Submit Transaction" (ctx.wrapMsg SubmitTxButtonClicked)

                                  else
                                    Helper.viewButton "Submit Transaction Anyway" (ctx.wrapMsg SubmitTxButtonClicked)
                                ]
                            ]

                    successSection =
                        div
                            [ HA.style "border" "1px solid #D1FAE5"
                            , HA.style "border-radius" "0.75rem"
                            , HA.style "box-shadow" "0 2px 4px rgba(0,0,0,0.06)"
                            , HA.style "background-color" "#ECFDF5"
                            , HA.style "overflow" "hidden"
                            , HA.style "margin-bottom" "1.5rem"
                            ]
                            [ div
                                [ HA.style "padding" "1.5rem" ]
                                [ div
                                    [ HA.style "display" "flex"
                                    , HA.style "align-items" "center"
                                    , HA.style "gap" "0.75rem"
                                    , HA.style "margin-bottom" "1rem"
                                    ]
                                    [ div
                                        [ HA.style "background-color" "#15803D"
                                        , HA.style "width" "2.5rem"
                                        , HA.style "height" "2.5rem"
                                        , HA.style "border-radius" "9999px"
                                        , HA.style "display" "flex"
                                        , HA.style "align-items" "center"
                                        , HA.style "justify-content" "center"
                                        , HA.style "color" "white"
                                        , HA.style "font-size" "1.25rem"
                                        , HA.style "font-weight" "bold"
                                        ]
                                        [ text "✓" ]
                                    , Html.p
                                        [ HA.style "color" "#15803D"
                                        , HA.style "font-weight" "600"
                                        , HA.style "font-size" "1.25rem"
                                        ]
                                        [ text "Transaction submitted successfully!" ]
                                    ]
                                , Html.p
                                    [ HA.style "margin-bottom" "1.5rem"
                                    , HA.style "color" "#065F46"
                                    ]
                                    [ text "Your vote has been recorded on the Cardano blockchain." ]
                                , div
                                    [ HA.style "background-color" "white"
                                    , HA.style "border" "1px solid #D1FAE5"
                                    , HA.style "border-radius" "0.5rem"
                                    , HA.style "padding" "1rem"
                                    , HA.style "margin-bottom" "1.5rem"
                                    ]
                                    [ Html.p
                                        [ HA.style "font-size" "0.875rem"
                                        , HA.style "color" "#065F46"
                                        , HA.style "margin-bottom" "0.5rem"
                                        ]
                                        [ text "Transaction ID:" ]
                                    , Html.p
                                        [ HA.style "font-family" "monospace"
                                        , HA.style "font-weight" "500"
                                        , HA.style "background-color" "#F0FDF4"
                                        , HA.style "padding" "0.5rem"
                                        , HA.style "border-radius" "0.25rem"
                                        , HA.style "word-break" "break-all"
                                        ]
                                        [ text <| Bytes.toHex txId ]
                                    ]
                                , Html.p
                                    [ HA.style "color" "#4b5563"
                                    , HA.style "margin-bottom" "1rem"
                                    ]
                                    [ text "Track your transaction:" ]
                                , div
                                    [ HA.style "display" "flex"
                                    , HA.style "gap" "1rem"
                                    , HA.style "flex-wrap" "wrap"
                                    ]
                                    [ let
                                        cardanoExplorerTxUrl =
                                            case ctx.networkId of
                                                Mainnet ->
                                                    "https://explorer.cardano.org/transaction?id=" ++ Bytes.toHex txId

                                                Testnet ->
                                                    "https://explorer.cardano.org/preview/transaction?id=" ++ Bytes.toHex txId
                                      in
                                      Helper.externalLinkButton { url = cardanoExplorerTxUrl, label = "View on Cardano Explorer" }
                                    ]
                                ]
                            ]
                in
                div []
                    [ cardContainer
                        [ Html.div
                            [ HA.style "display" "flex"
                            , HA.style "flex-direction" "column"
                            , HA.style "gap" "0.75rem"
                            ]
                            [ Html.div
                                [ HA.style "display" "flex"
                                , HA.style "flex-direction" "column"
                                ]
                                [ Html.span
                                    [ HA.style "font-weight" "500"
                                    , HA.style "color" "#4A5568"
                                    , HA.style "margin-bottom" "0.5rem"
                                    ]
                                    [ text "Transaction ID:" ]
                                , Html.div
                                    [ HA.style "font-family" "monospace"
                                    , HA.style "background-color" "#F1F5F9"
                                    , HA.style "padding" "0.5rem"
                                    , HA.style "border-radius" "0.25rem"
                                    , HA.style "font-size" "0.875rem"
                                    , HA.style "word-break" "break-all"
                                    ]
                                    [ text <| Bytes.toHex txId ]
                                ]
                            ]
                        , Html.p
                            [ HA.style "margin" "1.25rem 0 0.75rem 0"
                            , HA.style "font-weight" "500"
                            ]
                            [ text "Transaction details: (₳ amounts are in lovelaces)" ]
                        , div
                            [ HA.style "position" "relative" ]
                            [ Html.pre
                                [ HA.style "padding" "1rem"
                                , HA.style "border-radius" "0.375rem"
                                , HA.style "border" "1px solid #E2E8F0"
                                , HA.style "background-color" "#F8FAFC"
                                , HA.style "overflow-x" "auto"
                                , HA.style "overflow-y" "auto"
                                , HA.style "margin-top" "0.5rem"
                                , HA.style "font-size" "0.875rem"
                                , HA.style "white-space" "pre-wrap"
                                , HA.style "word-break" "break-all"
                                , HA.style "word-wrap" "break-word"
                                , HA.style "max-height" "300px"
                                , HA.style "font-family" "ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace"
                                ]
                                [ text <| prettyTx tx ]
                            ]
                        ]
                    , if Dict.isEmpty expectedSigners then
                        div []
                            [ sectionTitle "Gathered Signatures"
                            , gatheredSignaturesSection
                            , signSection
                            , sectionTitle "Transaction Submission"
                            , submissionSection True
                            ]

                      else if Dict.isEmpty (Dict.diff expectedSigners (Dict.map (\k _ -> vkeyWitnesses |> Dict.get k) expectedSigners |> Dict.filter (\_ v -> v /= Nothing))) then
                        div []
                            [ sectionTitle "Expected Signatures"
                            , gatheredSignaturesSection
                            , sectionTitle "Transaction Submission"
                            , submissionSection True
                            ]

                      else
                        div []
                            [ sectionTitle "Expected Signatures"
                            , gatheredSignaturesSection
                            , signSection
                            , sectionTitle "Transaction Submission"
                            , submissionSection False
                            ]
                    , case txSubmitted of
                        Nothing ->
                            text ""

                        Just _ ->
                            successSection
                    , viewError error
                    ]
        ]


viewExpectedSignatures : Dict String { keyName : String, keyHash : Bytes CredentialHash } -> Dict String VKeyWitness -> List (Html msg)
viewExpectedSignatures expectedSigners vkeyWitnesses =
    let
        viewExpectedSigner : String -> { keyName : String, keyHash : Bytes CredentialHash } -> Html msg
        viewExpectedSigner keyHash { keyName } =
            case Dict.get keyHash vkeyWitnesses of
                Just witness ->
                    Html.div
                        [ HA.style "background-color" "#F0FDF4"
                        , HA.style "border" "1px solid #D1FAE5"
                        , HA.style "border-radius" "0.5rem"
                        , HA.style "padding" "1rem"
                        , HA.style "margin-bottom" "0.75rem"
                        , HA.style "display" "flex"
                        , HA.style "align-items" "flex-start"
                        ]
                        [ Html.div
                            [ HA.style "color" "#16A34A"
                            , HA.style "font-weight" "bold"
                            , HA.style "font-size" "1.25rem"
                            , HA.style "margin-right" "0.75rem"
                            , HA.style "line-height" "1"
                            ]
                            [ text "✓" ]
                        , Html.div
                            [ HA.style "font-family" "monospace"
                            , HA.style "font-size" "0.875rem"
                            ]
                            [ Html.div
                                [ HA.style "font-weight" "500"
                                , HA.style "margin-bottom" "0.25rem"
                                , HA.style "color" "#065F46"
                                ]
                                [ text <| keyName ++ ": " ++ Helper.shortenedHex 8 keyHash ]
                            , Html.div
                                [ HA.style "color" "#059669"
                                , HA.style "margin-bottom" "0.25rem"
                                ]
                                [ text <| "VKey: " ++ Helper.shortenedHex 8 (Bytes.toHex witness.vkey) ]
                            , Html.div
                                [ HA.style "color" "#059669" ]
                                [ text <| "Signature: " ++ Helper.shortenedHex 8 (Bytes.toHex witness.signature) ]
                            ]
                        ]

                Nothing ->
                    Html.div
                        [ HA.style "background-color" "#F1F5F9"
                        , HA.style "border" "1px solid #E2E8F0"
                        , HA.style "border-radius" "0.5rem"
                        , HA.style "padding" "1rem"
                        , HA.style "margin-bottom" "0.75rem"
                        , HA.style "display" "flex"
                        , HA.style "align-items" "center"
                        ]
                        [ Html.div
                            [ HA.style "color" "#94A3B8"
                            , HA.style "margin-right" "0.75rem"
                            , HA.style "font-size" "1.25rem"
                            , HA.style "line-height" "1"
                            ]
                            [ text "□" ]
                        , Html.div
                            [ HA.style "font-family" "monospace"
                            , HA.style "font-size" "0.875rem"
                            , HA.style "color" "#64748B"
                            ]
                            [ text <| keyName ++ ": " ++ Helper.shortenedHex 8 keyHash ]
                        ]
    in
    Dict.map viewExpectedSigner expectedSigners
        |> Dict.values


viewError : Maybe String -> Html msg
viewError error =
    case error of
        Nothing ->
            text ""

        Just err ->
            Html.div
                [ HA.style "background-color" "#FEF2F2"
                , HA.style "border" "1px solid #FEE2E2"
                , HA.style "border-radius" "0.5rem"
                , HA.style "padding" "1.25rem"
                , HA.style "margin-top" "1.5rem"
                ]
                [ Html.div
                    [ HA.style "display" "flex"
                    , HA.style "align-items" "flex-start"
                    , HA.style "margin-bottom" "0.75rem"
                    ]
                    [ Html.div
                        [ HA.style "color" "#DC2626"
                        , HA.style "font-weight" "bold"
                        , HA.style "margin-right" "0.75rem"
                        , HA.style "font-size" "1.25rem"
                        ]
                        [ text "!" ]
                    , Html.p
                        [ HA.style "color" "#DC2626"
                        , HA.style "font-weight" "600"
                        ]
                        [ text "Error" ]
                    ]
                , Html.pre
                    [ HA.style "background-color" "#FFFFFF"
                    , HA.style "border" "1px solid #FEE2E2"
                    , HA.style "border-radius" "0.25rem"
                    , HA.style "padding" "0.75rem"
                    , HA.style "font-size" "0.875rem"
                    , HA.style "white-space" "pre-wrap"
                    , HA.style "overflow-x" "auto"
                    , HA.style "font-family" "monospace"
                    , HA.style "color" "#991B1B"
                    ]
                    [ text err ]
                ]
