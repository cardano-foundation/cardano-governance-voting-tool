module Api exposing (ActiveProposal, ApiProvider, AuthorVerification, CcInfo, Cip100VerificationResponse, DrepInfo, IpfsAnswer(..), IpfsFile, OnchainVote, PoolInfo, ProtocolParams, defaultApiProvider)

{-| Module gathering all the remote HTTP calls made by the app.
-}

import Bytes as ElmBytes
import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address exposing (Credential(..), CredentialHash, NetworkId(..))
import Cardano.Gov as Gov exposing (ActionId, CostModels)
import Cardano.Pool as Pool
import Cardano.Transaction exposing (Transaction)
import Cardano.Utxo exposing (TransactionId)
import ConcurrentTask exposing (ConcurrentTask)
import ConcurrentTask.Http
import ConcurrentTask.Process
import File exposing (File)
import Helper
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import List.Extra
import Natural exposing (Natural)
import Platform exposing (Task)
import ProposalMetadata exposing (ProposalMetadata)
import RemoteData exposing (RemoteData)
import ScriptInfo exposing (ScriptInfo)
import Task
import Url


{-| Free Tier Koios API token.
Expiration date: 2026-02-17.
-}
koiosApiToken : String
koiosApiToken =
    "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJhZGRyIjoic3Rha2UxdTl5dG5rMnB4dTlobHJnM2Rqc2t2ODhraG1uOTBsZGowMHE0Z3o4YTJycXN1NGc0NHB6Mm0iLCJleHAiOjE3NzEzMjA0MzQsInRpZXIiOjEsInByb2pJRCI6Imdvdi12b3RpbmcifQ.8PiNPWnc9j5mjL4e7pbGA3cXEB0GepXnxWjXn0mYocY"


{-| Overview of all the requests that the app may do.

We could get rid of this type entirely and instead just expose individual functions.
However I think it is a nice overview for anyone looking at the code.
A default implementation of all these requests is also provided in this module.

Initially, requests were exclusively built using the elm/http package.
However, we later introduced the elm-concurrent-task package, enable more composable requests,
with caching (in a browser DB) behavior possible.
So this is how there is a mix of regular `(...) -> Cmd msg` and `ConcurrentTask ...`.

-}
type alias ApiProvider msg =
    { loadProtocolParams : NetworkId -> (Result Http.Error ProtocolParams -> msg) -> Cmd msg
    , queryEpoch : NetworkId -> (Result Http.Error Int -> msg) -> Cmd msg
    , queryConstitution : NetworkId -> (Result Http.Error String -> msg) -> Cmd msg
    , loadGovProposals : NetworkId -> Int -> (Result Http.Error (List ActiveProposal) -> msg) -> Cmd msg
    , loadProposalMetadata : String -> ConcurrentTask String ProposalMetadata
    , retrieveTx : NetworkId -> Bytes TransactionId -> ConcurrentTask ConcurrentTask.Http.Error (Bytes Transaction)
    , getScriptInfo : NetworkId -> Bytes CredentialHash -> ConcurrentTask ConcurrentTask.Http.Error ScriptInfo
    , getDrepInfo : NetworkId -> Credential -> (Result Http.Error DrepInfo -> msg) -> Cmd msg
    , getCcInfo : NetworkId -> Credential -> (Result Http.Error CcInfo -> msg) -> Cmd msg
    , getPoolLiveStake : NetworkId -> Bytes Pool.Id -> (Result Http.Error PoolInfo -> msg) -> Cmd msg
    , getVotes : NetworkId -> Gov.Id -> (Result Http.Error (List OnchainVote) -> msg) -> Cmd msg
    , ipfsAddFileCustom : { rpc : String, headers : List ( String, String ), file : File } -> (Result String IpfsAnswer -> msg) -> Cmd msg
    , ipfsAddFileNmkr : { userId : String, apiToken : String, file : File } -> (Result String IpfsAnswer -> msg) -> Cmd msg
    , ipfsAddFileBlockfrost : { projectId : String, file : File } -> (Result String IpfsAnswer -> msg) -> Cmd msg
    , ipfsAddFile : { file : File } -> (Result String IpfsAnswer -> msg) -> Cmd msg
    , convertToPdf : String -> (Result Http.Error ElmBytes.Bytes -> msg) -> Cmd msg
    , getFromIpfsGateway : (Result Http.Error String -> msg) -> String -> String -> Cmd msg
    , verifyCip100Metadata : String -> (Result Http.Error Cip100VerificationResponse -> msg) -> Cmd msg
    }



-- Protocol Parameters


{-| Relevant protocol parameters for this app.
-}
type alias ProtocolParams =
    { costModels : CostModels
    , drepDeposit : Natural
    }


protocolParamsDecoder : Decoder ProtocolParams
protocolParamsDecoder =
    JD.map4
        (\v1 v2 v3 drepDeposit ->
            { costModels = CostModels (Just v1) (Just v2) (Just v3)
            , drepDeposit = drepDeposit
            }
        )
        (JD.at [ "result", "plutusCostModels", "plutus:v1" ] <| JD.list JD.int)
        (JD.at [ "result", "plutusCostModels", "plutus:v2" ] <| JD.list JD.int)
        (JD.at [ "result", "plutusCostModels", "plutus:v3" ] <| JD.list JD.int)
        (JD.at [ "result", "delegateRepresentativeDeposit", "ada", "lovelace" ] <| JD.map Natural.fromSafeInt JD.int)



-- Epoch


ogmiosEpochDecoder : Decoder Int
ogmiosEpochDecoder =
    JD.field "result" JD.int


{-| Decoder for the Cardano constitution URI from Ogmios.
The constitution metadata contains a URL field that points to the constitution document.
-}
ogmiosConstitutionDecoder : Decoder String
ogmiosConstitutionDecoder =
    JD.at [ "result", "metadata", "url" ] JD.string



-- Governance Proposals


{-| Information for currently active proposals.

Proposals metadata are not stored onchain, and need extra remote calls to be fetched.

-}
type alias ActiveProposal =
    { id : ActionId
    , actionType : String
    , metadataUrl : String
    , metadataHash : String
    , epoch_validity : { start : Int, end : Int }
    , ratified : Maybe Int
    , metadata : RemoteData String ProposalMetadata
    }


koiosGovProposalsDecoder : Decoder (List ActiveProposal)
koiosGovProposalsDecoder =
    JD.list <|
        JD.map7 ActiveProposal
            (JD.map2
                (\id index ->
                    { transactionId = Bytes.fromHexUnchecked id
                    , govActionIndex = index
                    }
                )
                (JD.field "proposal_tx_hash" JD.string)
                (JD.field "proposal_index" JD.int)
            )
            (JD.field "proposal_type" JD.string)
            (JD.field "meta_url" JD.string)
            (JD.field "meta_hash" JD.string)
            (JD.map2
                (\start end -> { start = start, end = end })
                (JD.field "proposed_epoch" JD.int)
                -- TODO or is it expiration +-1 ?
                (JD.field "expiration" JD.int)
            )
            (JD.field "ratified_epoch" <| JD.maybe JD.int)
            (JD.succeed RemoteData.Loading)



-- Retrieve Txs


koiosFirstTxBytesDecoder : Decoder { txId : Bytes TransactionId, txCbor : Bytes a }
koiosFirstTxBytesDecoder =
    let
        oneTxBytesDecoder =
            JD.map2 (\txId cbor -> { txId = txId, txCbor = cbor })
                (JD.field "tx_hash" Bytes.jsonDecoder)
                (JD.field "cbor" Bytes.jsonDecoder)
    in
    JD.list oneTxBytesDecoder
        |> JD.andThen
            (\txs ->
                case txs of
                    [] ->
                        JD.fail "The Tx was not found. Maybe the request was done for the wrong network?"

                    [ tx ] ->
                        JD.succeed tx

                    _ ->
                        JD.fail "The server unexpectedly returned more than 1 transaction."
            )



-- DRep Info


{-| DRep can be "Abstain", "NoConfidence" or an actual DRep.
-}
type AnyDrepInfo
    = Abstain
    | NoConfidence
    | Registered DrepInfo


{-| DRep information needed for this app.

For technichal reasons, voting power is in `Int` and not `Natural`,
which may cause imprecisions if a DRep accumulates more than 2^53 Lovelace of voting power,
which is around ₳9B.

-}
type alias DrepInfo =
    { credential : Credential

    -- TODO: eventually need to change to Natural,
    -- but it’s not possible with regular http requests,
    -- because Elm will call JSON.parse() which will silently
    -- lose precision on integers > 2^53
    , votingPower : Int
    }


ogmiosSpecificDrepInfoDecoder : Credential -> Decoder DrepInfo
ogmiosSpecificDrepInfoDecoder cred =
    let
        extractRegistered anyDrep =
            case anyDrep of
                Registered info ->
                    Just info

                _ ->
                    Nothing
    in
    JD.field "result" (JD.list ogmiosAnyDrepInfoDecoder)
        |> JD.map (List.filterMap extractRegistered)
        |> JD.map (List.Extra.find (\drep -> drep.credential == cred))
        |> JD.andThen
            (\drepFound ->
                case drepFound of
                    Nothing ->
                        JD.fail <| "DRep not in the response: " ++ Debug.toString cred

                    Just drepInfo ->
                        JD.succeed drepInfo
            )


ogmiosAnyDrepInfoDecoder : Decoder AnyDrepInfo
ogmiosAnyDrepInfoDecoder =
    JD.field "type" JD.string
        |> JD.andThen
            (\drepType ->
                case drepType of
                    "abstain" ->
                        JD.succeed Abstain

                    "noConfidence" ->
                        JD.succeed NoConfidence

                    "registered" ->
                        JD.map Registered ogmiosRegisteredDrepInfoDecoder

                    _ ->
                        JD.fail <| "Unknown DRep type: " ++ drepType
            )


ogmiosRegisteredDrepInfoDecoder : Decoder DrepInfo
ogmiosRegisteredDrepInfoDecoder =
    JD.map2 (\cred stake -> { credential = cred, votingPower = stake })
        ogmiosCredDecoder
        (JD.at [ "stake", "ada", "lovelace" ] JD.int)


ogmiosCredDecoder : Decoder Credential
ogmiosCredDecoder =
    JD.map2 Tuple.pair (JD.field "from" JD.string) (JD.field "id" JD.string)
        |> JD.andThen
            (\( from, id ) ->
                case ( from, Bytes.fromHex id ) of
                    ( "verificationKey", Just hash ) ->
                        JD.succeed <| VKeyHash hash

                    ( "script", Just hash ) ->
                        JD.succeed <| ScriptHash hash

                    _ ->
                        JD.fail <| "Invalid credential: " ++ from ++ ", " ++ id
            )



-- CC Info


{-| Relevant information for this app, about a Constitutional Committee (CC) member.
-}
type alias CcInfo =
    { coldCred : Credential
    , hotCred : Credential
    , status : String
    , epochMandateEnd : Int
    }


ogmiosSpecificCcInfoDecoder : Credential -> Decoder CcInfo
ogmiosSpecificCcInfoDecoder hotCred =
    JD.at [ "result", "members" ] (JD.list ogmiosCcInfoDecoder)
        |> JD.map (List.Extra.find (\info -> info.hotCred == hotCred))
        |> JD.andThen
            (\maybeInfo ->
                case maybeInfo of
                    Nothing ->
                        JD.fail <| "Hot cred not present in CC members: " ++ Debug.toString hotCred

                    Just info ->
                        JD.succeed info
            )


ogmiosCcInfoDecoder : Decoder CcInfo
ogmiosCcInfoDecoder =
    JD.map4 CcInfo
        -- Cold credential at the top, then Hot credential inside the "delegate" field
        ogmiosCredDecoder
        (JD.field "delegate" ogmiosCredDecoder)
        (JD.field "status" JD.string)
        (JD.at [ "mandate", "epoch" ] JD.int)



-- Pool Live Stake


{-| Pool stake information as of last epoch snapshot.
-}
type alias PoolInfo =
    { pool : Bytes Pool.Id
    , stake : Int
    }


koiosPoolSnapshotDecoder : Bytes Pool.Id -> Decoder PoolInfo
koiosPoolSnapshotDecoder poolId =
    JD.list poolSnapshotDecoder
        |> JD.andThen
            (\snapshots ->
                List.sortBy .epoch snapshots
                    |> List.reverse
                    |> List.head
                    |> Maybe.map (\{ stake } -> JD.succeed { pool = poolId, stake = stake })
                    |> Maybe.withDefault (JD.fail ("Stake not found for pool: " ++ Bytes.toHex poolId))
            )


poolSnapshotDecoder : Decoder { epoch : Int, stake : Int }
poolSnapshotDecoder =
    JD.map2
        (\epoch stake -> { epoch = epoch, stake = stake })
        (JD.field "epoch_no" JD.int)
        (JD.field "pool_stake" JD.string
            |> JD.andThen
                (\intStr ->
                    case String.toInt intStr of
                        Just n ->
                            JD.succeed n

                        Nothing ->
                            JD.fail <| "Invalid stake number: " ++ intStr
                )
        )



-- On-chain votes


type alias OnchainVote =
    { voterId : String
    , proposalId : String
    , blockHeight : Int
    , vote : Gov.Vote
    }


onchainVotesDecoder : Decoder (List OnchainVote)
onchainVotesDecoder =
    JD.list <|
        JD.map4 OnchainVote
            (JD.field "voter_id" JD.string)
            (JD.field "proposal_id" JD.string)
            (JD.field "block_height" JD.int)
            (JD.field "vote" <| JD.map voteFromString JD.string)


voteFromString : String -> Gov.Vote
voteFromString str =
    case String.toLower str of
        "yes" ->
            Gov.VoteYes

        "no" ->
            Gov.VoteNo

        _ ->
            Gov.VoteAbstain



-- IPFS


{-| Response from an IPFS server to a request to store a file.
-}
type IpfsAnswer
    = IpfsError String
    | IpfsAddSuccessful IpfsFile


{-| Relevant IPFS file information.
-}
type alias IpfsFile =
    { name : String
    , cid : String
    , size : String
    }


responseToIpfsAnswer : Http.Response String -> Result String IpfsAnswer
responseToIpfsAnswer response =
    case response of
        Http.GoodStatus_ _ body ->
            JD.decodeString ipfsAnswerDecoder body
                |> Result.mapError JD.errorToString

        Http.BadStatus_ meta body ->
            case JD.decodeString ipfsAnswerDecoder body of
                Ok answer ->
                    Ok answer

                Err _ ->
                    Err <| "Bad status (" ++ String.fromInt meta.statusCode ++ "): " ++ meta.statusText

        Http.NetworkError_ ->
            Err "Network error. Maybe you lost your connection, or an IPFS gateway did not respond in time, or some other network error occured."

        Http.Timeout_ ->
            Err "The Pin request timed out."

        Http.BadUrl_ str ->
            Err <| "Incorrect URL: " ++ str


ipfsAnswerDecoder : JD.Decoder IpfsAnswer
ipfsAnswerDecoder =
    JD.oneOf
        -- Error
        [ JD.map3 (\err msg code -> IpfsError <| String.fromInt code ++ " (" ++ err ++ "): " ++ msg)
            (JD.field "error" JD.string)
            (JD.field "message" JD.string)
            (JD.field "status_code" JD.int)
        , JD.map IpfsError
            (JD.field "errorMessage" JD.string)
        , JD.map IpfsError
            (JD.field "detail" JD.string)
        , JD.map (\json -> IpfsError <| JE.encode 2 json)
            (JD.field "detail" JD.value)

        -- Blockfrost format
        , JD.map3 (\name hash size -> IpfsAddSuccessful <| IpfsFile name hash size)
            (JD.field "name" JD.string)
            (JD.field "ipfs_hash" JD.string)
            (JD.field "size" JD.string)

        -- CF format
        , JD.map3 (\name hash size -> IpfsAddSuccessful <| IpfsFile name hash size)
            (JD.field "Name" JD.string)
            (JD.field "Hash" JD.string)
            (JD.field "Size" JD.string)

        -- NMKR format
        , JD.map (\cid -> IpfsAddSuccessful <| IpfsFile "unkown" cid "unknown")
            JD.string
        ]



-- CIP100 signature verification


{-| Response from the CIP-100 verification API.
-}
type alias Cip100VerificationResponse =
    { authors : List AuthorVerification
    }


{-| Verification result for a single author.
-}
type alias AuthorVerification =
    { authorName : String
    , isValid : Bool
    , errorMessage : Maybe String
    }


{-| Decoder for CIP-100 verification response.
The API returns a nested structure with data.authors.
-}
cip100VerificationDecoder : JD.Decoder Cip100VerificationResponse
cip100VerificationDecoder =
    JD.map Cip100VerificationResponse
        (JD.at [ "data", "authors" ] (JD.list authorVerificationDecoder))


authorVerificationDecoder : JD.Decoder AuthorVerification
authorVerificationDecoder =
    JD.map3 AuthorVerification
        (JD.field "name" JD.string)
        (JD.field "valid" JD.bool)
        (JD.maybe (JD.field "error" JD.string))



-- Default API Provider


{-| Default implementation for each remote request that this app requires.
Most of them are relying on Koios infrastructure.
-}
defaultApiProvider : ApiProvider msg
defaultApiProvider =
    let
        koiosUrl networkId =
            case networkId of
                Testnet ->
                    "https://preview.koios.rest/api/v1"

                Mainnet ->
                    "https://api.koios.rest/api/v1"
    in
    -- Get protocol parameters via Koios
    { loadProtocolParams =
        \networkId toMsg ->
            Http.request
                { method = "POST"
                , url = koiosUrl networkId ++ "/ogmios"
                , headers = [ Http.header "Authorization" <| "Bearer " ++ koiosApiToken ]
                , body =
                    Http.jsonBody
                        (JE.object
                            [ ( "jsonrpc", JE.string "2.0" )
                            , ( "method", JE.string "queryLedgerState/protocolParameters" )
                            ]
                        )
                , expect = Http.expectJson toMsg protocolParamsDecoder
                , timeout = Nothing
                , tracker = Nothing
                }

    -- Query epoch via Koios
    , queryEpoch =
        \networkId toMsg ->
            Http.request
                { method = "POST"
                , url = koiosUrl networkId ++ "/ogmios"
                , headers = [ Http.header "Authorization" <| "Bearer " ++ koiosApiToken ]
                , body =
                    Http.jsonBody
                        (JE.object
                            [ ( "jsonrpc", JE.string "2.0" )
                            , ( "method", JE.string "queryLedgerState/epoch" )
                            ]
                        )
                , expect = Http.expectJson toMsg ogmiosEpochDecoder
                , timeout = Nothing
                , tracker = Nothing
                }

    -- Query constitution URI via Koios
    , queryConstitution =
        \networkId toMsg ->
            Http.request
                { method = "POST"
                , url = koiosUrl networkId ++ "/ogmios"
                , headers = [ Http.header "Authorization" <| "Bearer " ++ koiosApiToken ]
                , body =
                    Http.jsonBody
                        (JE.object
                            [ ( "jsonrpc", JE.string "2.0" )
                            , ( "method", JE.string "queryLedgerState/constitution" )
                            ]
                        )
                , expect = Http.expectJson toMsg ogmiosConstitutionDecoder
                , timeout = Nothing
                , tracker = Nothing
                }

    -- Get governance proposals via Koios
    , loadGovProposals =
        \networkId currentEpoch toMsg ->
            let
                selected_rows =
                    [ "proposal_tx_hash"
                    , "proposal_index"
                    , "proposal_type"
                    , "meta_url"
                    , "meta_hash"
                    , "proposed_epoch"
                    , "expiration"
                    , "ratified_epoch"
                    ]
                        |> String.join ","
            in
            Http.request
                { method = "GET"
                , url = koiosUrl networkId ++ "/proposal_list?select=" ++ selected_rows ++ "&expiration=gt." ++ String.fromInt currentEpoch
                , headers = [ Http.header "Authorization" <| "Bearer " ++ koiosApiToken ]
                , body = Http.emptyBody
                , expect = Http.expectJson toMsg koiosGovProposalsDecoder
                , timeout = Nothing
                , tracker = Nothing
                }

    -- Load the metadata associated with a governance proposal
    , loadProposalMetadata = taskLoadProposalMetadata

    -- Retrieve transactions via Koios by proxying with the server (to avoid CORS errors)
    , retrieveTx = taskRetrieveTx

    -- Retrieve script info via Koios by proxying with the server (to avoid CORS errors)
    , getScriptInfo = taskGetScriptInfo

    -- Retrieve DRep info via Koios using an Ogmios endpoint
    , getDrepInfo =
        \networkId cred toMsg ->
            Http.request
                { method = "POST"
                , url = koiosUrl networkId ++ "/ogmios"
                , headers = [ Http.header "Authorization" <| "Bearer " ++ koiosApiToken ]
                , body =
                    Http.jsonBody
                        (JE.object
                            [ ( "jsonrpc", JE.string "2.0" )
                            , ( "method", JE.string "queryLedgerState/delegateRepresentatives" )
                            , ( "params"
                              , case cred of
                                    VKeyHash hash ->
                                        JE.object [ ( "keys", JE.list JE.string <| [ Bytes.toHex hash ] ) ]

                                    ScriptHash hash ->
                                        JE.object [ ( "scripts", JE.list JE.string <| [ Bytes.toHex hash ] ) ]
                              )
                            ]
                        )
                , expect = Http.expectJson toMsg (ogmiosSpecificDrepInfoDecoder cred)
                , timeout = Nothing
                , tracker = Nothing
                }

    -- Retrieve CC member info
    , getCcInfo =
        \networkId cred toMsg ->
            Http.request
                { method = "POST"
                , url = koiosUrl networkId ++ "/ogmios"
                , headers = [ Http.header "Authorization" <| "Bearer " ++ koiosApiToken ]
                , body =
                    Http.jsonBody
                        (JE.object
                            [ ( "jsonrpc", JE.string "2.0" )
                            , ( "method", JE.string "queryLedgerState/constitutionalCommittee" )
                            ]
                        )
                , expect = Http.expectJson toMsg (ogmiosSpecificCcInfoDecoder cred)
                , timeout = Nothing
                , tracker = Nothing
                }

    -- Retrieve Pool live stake
    , getPoolLiveStake =
        \networkId poolId toMsg ->
            let
                poolIdBech32 =
                    Pool.toBech32 poolId
            in
            Http.request
                { method = "GET"
                , url = koiosUrl networkId ++ "/pool_stake_snapshot?_pool_bech32=" ++ poolIdBech32
                , headers = [ Http.header "Authorization" <| "Bearer " ++ koiosApiToken ]
                , body = Http.emptyBody
                , expect = Http.expectJson toMsg (koiosPoolSnapshotDecoder poolId)
                , timeout = Nothing
                , tracker = Nothing
                }

    -- Load past votes for a given voter
    , getVotes =
        \networkId govId toMsg ->
            let
                selected_rows =
                    [ "voter_id"
                    , "proposal_id"
                    , "block_height"
                    , "vote"
                    ]
                        |> String.join ","
            in
            Http.request
                { method = "GET"

                -- TODO: improve efficiency by filtering out old epochs
                , url = koiosUrl networkId ++ "/vote_list?select=" ++ selected_rows ++ "&voter_id=eq." ++ Gov.idToBech32 govId
                , headers = [ Http.header "Authorization" <| "Bearer " ++ koiosApiToken ]
                , body = Http.emptyBody
                , expect = Http.expectJson toMsg onchainVotesDecoder
                , timeout = Nothing
                , tracker = Nothing
                }

    -- Make a request to an IPFS RPC
    , ipfsAddFileCustom =
        \{ rpc, headers, file } toMsg ->
            Http.request
                { method = "POST"
                , headers = List.map (\( k, v ) -> Http.header k v) headers
                , url = rpc ++ "/add?pin=true"
                , body = Http.multipartBody [ Http.filePart "file" file ]
                , expect = Http.expectStringResponse toMsg responseToIpfsAnswer
                , timeout = Nothing
                , tracker = Nothing
                }

    -- Make a request to NMKR IPFS server
    , ipfsAddFileNmkr =
        \{ userId, apiToken, file } toMsg ->
            File.toUrl file
                |> Task.andThen
                    (\fileAsBase64Url ->
                        let
                            -- Remove the uri prefix for NMKR which doesn’t accept it
                            prefixSize =
                                String.length <| "data:" ++ File.mime file ++ ";base64,"

                            fileAsBase64 =
                                String.dropLeft prefixSize fileAsBase64Url
                        in
                        Http.task
                            { method = "POST"
                            , url = "/proxy/json"
                            , headers = []
                            , body =
                                Http.jsonBody
                                    (JE.object
                                        [ ( "url", JE.string <| "https://studio-api.nmkr.io/v2/UploadToIpfs/" ++ userId )
                                        , ( "method", JE.string "POST" )
                                        , ( "headers", JE.object [ ( "Authorization", JE.string ("Bearer " ++ apiToken) ) ] )
                                        , ( "body"
                                          , JE.object
                                                [ ( "mimetype", JE.string <| File.mime file )
                                                , ( "name", JE.string <| File.name file )
                                                , ( "fileFromBase64", JE.string fileAsBase64 )
                                                ]
                                          )
                                        ]
                                    )
                            , resolver = Http.stringResolver responseToIpfsAnswer
                            , timeout = Nothing
                            }
                    )
                |> Task.attempt toMsg

    -- Make an add+pin request to Blockfrost IPFS servers
    , ipfsAddFileBlockfrost =
        let
            addRequest : String -> File -> Task String IpfsAnswer
            addRequest projectId file =
                Http.task
                    { method = "POST"
                    , headers = [ Http.header "project_id" projectId ]
                    , url = "https://ipfs.blockfrost.io/api/v0/ipfs/add"
                    , body = Http.multipartBody [ Http.filePart "file" file ]
                    , resolver = Http.stringResolver <| responseToIpfsAnswer
                    , timeout = Nothing
                    }

            pinRequest : String -> IpfsAnswer -> Task String IpfsAnswer
            pinRequest projectId ipfsAnswer =
                case ipfsAnswer of
                    IpfsError _ ->
                        Task.succeed ipfsAnswer

                    IpfsAddSuccessful { cid } ->
                        Http.task
                            { method = "POST"
                            , headers = [ Http.header "project_id" projectId ]
                            , url = "https://ipfs.blockfrost.io/api/v0/ipfs/pin/add/" ++ cid
                            , body =
                                Http.jsonBody <|
                                    JE.object
                                        [ ( "ipfs_hash", JE.string cid )
                                        , ( "state", JE.string "queued" )
                                        , ( "filecoin", JE.bool False )
                                        ]
                            , resolver =
                                Http.stringResolver
                                    (\pinResponse ->
                                        case pinResponse of
                                            Http.GoodStatus_ _ _ ->
                                                -- Return the original /add answer
                                                Ok ipfsAnswer

                                            Http.BadStatus_ meta _ ->
                                                Err <| "Pinning failed (" ++ String.fromInt meta.statusCode ++ "): " ++ meta.statusText

                                            Http.NetworkError_ ->
                                                Err "Network error during IPFS pinning."

                                            Http.Timeout_ ->
                                                Err "The pin request timed out."

                                            Http.BadUrl_ str ->
                                                Err <| "Incorrect URL for pinning: " ++ str
                                    )
                            , timeout = Nothing
                            }
        in
        \{ projectId, file } toMsg ->
            addRequest projectId file
                |> Task.andThen (pinRequest projectId)
                |> Task.attempt toMsg

    -- Make a request to the pre-configured IPFS RPC via the server
    , ipfsAddFile =
        \{ file } toMsg ->
            Http.request
                { method = "POST"
                , headers = []
                , url = "/ipfs-pin/file"
                , body = Http.multipartBody [ Http.filePart "file" file ]
                , expect = Http.expectStringResponse toMsg responseToIpfsAnswer
                , timeout = Nothing
                , tracker = Nothing
                }

    -- Convert the JSON LD file into a pretty PDF on the server
    , convertToPdf =
        \jsonFileContent toMsg ->
            Http.post
                { url = "/pretty-gov-pdf"
                , body = Http.stringBody "application/json" jsonFileContent
                , expect = Http.expectBytesResponse toMsg bytesResponseToResult
                }
    , getFromIpfsGateway =
        \toMsg gateway cid ->
            Http.get
                { url = gateway ++ "/" ++ cid
                , expect = Http.expectString toMsg
                }
    , verifyCip100Metadata =
        \metadataUrl toMsg ->
            let
                -- Convert IPFS URLs to HTTPS gateway URLs
                httpsUrl =
                    Helper.ipfsToHttpsUrl metadataUrl

                verificationApiUrl =
                    "https://verifycardanomessage.cardanofoundation.org/api/verify-cip100?url=" ++ Url.percentEncode httpsUrl

                proxyRequestBody =
                    JE.object
                        [ ( "url", JE.string verificationApiUrl )
                        , ( "method", JE.string "GET" )
                        , ( "headers", JE.object [] )
                        ]
            in
            Http.post
                { url = "/proxy/json"
                , body = Http.jsonBody proxyRequestBody
                , expect = Http.expectJson toMsg cip100VerificationDecoder
                }
    }


bytesResponseToResult : Http.Response ElmBytes.Bytes -> Result Http.Error ElmBytes.Bytes
bytesResponseToResult response =
    case response of
        Http.BadUrl_ str ->
            Err <| Http.BadUrl str

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ meta _ ->
            Err <| Http.BadStatus meta.statusCode

        Http.GoodStatus_ _ bytes ->
            Ok bytes



-- Task port requests


{-| IPFS gateways to try when resolving ipfs:// URLs.
The first gateway is tried immediately, and subsequent gateways are tried
with a staggered delay (2s between each) to avoid wasting bandwidth
while still providing fast fallback when a gateway is unresponsive.
-}
ipfsGateways : List String
ipfsGateways =
    [ "https://ipfs.io/ipfs/"
    , "https://ipfs.blockfrost.dev/ipfs/"
    , "https://dweb.link/ipfs/"
    , "https://c-ipfs-gw.nmkr.io/ipfs/"
    , "https://cloudflare-ipfs.com/ipfs/"
    , "https://gateway.pinata.cloud/ipfs/"
    ]


{-| Task to retrieve a proposal metadata.

If the metadata URL is pointing to an IPFS resource ("ipfs://"),
requests are staggered across multiple IPFS gateways: the first gateway
is tried immediately, and each subsequent gateway is tried with an additional
2-second delay. The first successful response wins (via `race`).

For non-IPFS URLs, a single direct request is made.

If a request fails with a NetworkError (potentially CORS), it is retried
by proxying through this app server.

-}
taskLoadProposalMetadata : String -> ConcurrentTask String ProposalMetadata
taskLoadProposalMetadata url =
    if String.startsWith "ipfs://" url then
        let
            cid =
                String.dropLeft 7 url
        in
        fetchFromIpfsGateways cid

    else
        fetchMetadataFromUrl url
            |> ConcurrentTask.map ProposalMetadata.fromRaw
            |> httpErrorToString


{-| Try fetching from multiple IPFS gateways with staggered delays.
The first gateway fires immediately, then every 2 seconds a new gateway
is tried. The first successful response wins via `ConcurrentTask.race`.
-}
fetchFromIpfsGateways : String -> ConcurrentTask String ProposalMetadata
fetchFromIpfsGateways cid =
    case ipfsGateways of
        [] ->
            ConcurrentTask.fail "No IPFS gateways configured"

        first :: rest ->
            let
                staggeredTask : Int -> String -> ConcurrentTask ConcurrentTask.Http.Error String
                staggeredTask index gateway =
                    ConcurrentTask.Process.sleep (index * 2000)
                        |> ConcurrentTask.andThenDo (fetchMetadataFromUrl (gateway ++ cid))
            in
            ConcurrentTask.race
                (fetchMetadataFromUrl (first ++ cid))
                (List.indexedMap (\i gw -> staggeredTask (i + 1) gw) rest)
                |> ConcurrentTask.map ProposalMetadata.fromRaw
                |> httpErrorToString


{-| Fetch metadata from a URL, with CORS fallback via the server proxy.
-}
fetchMetadataFromUrl : String -> ConcurrentTask ConcurrentTask.Http.Error String
fetchMetadataFromUrl adjustedUrl =
    ConcurrentTask.Http.get
        { url = adjustedUrl
        , headers = []
        , expect = ConcurrentTask.Http.expectString
        , timeout = Nothing
        }
        |> ConcurrentTask.onError
            (\httpError ->
                case httpError of
                    -- NetworkError is potentially a CORS issue, so we proxy through the server
                    ConcurrentTask.Http.NetworkError ->
                        ConcurrentTask.Http.post
                            { url = "/proxy/json"
                            , headers = []
                            , body =
                                ConcurrentTask.Http.jsonBody
                                    (JE.object
                                        [ ( "url", JE.string adjustedUrl )
                                        , ( "method", JE.string "GET" )
                                        ]
                                    )
                            , expect = ConcurrentTask.Http.expectString
                            , timeout = Nothing
                            }

                    _ ->
                        ConcurrentTask.fromResult (Err httpError)
            )


{-| Convert HTTP errors to user-friendly strings.
-}
httpErrorToString : ConcurrentTask ConcurrentTask.Http.Error a -> ConcurrentTask String a
httpErrorToString =
    ConcurrentTask.onError
        (\httpError ->
            case httpError of
                ConcurrentTask.Http.NetworkError ->
                    ConcurrentTask.fail "Network error. Maybe you lost your connection, or the request was blocked by CORS on the server."

                _ ->
                    ConcurrentTask.fail (Debug.toString httpError)
        )


{-| Task to retrieve the raw CBOR of a given Tx.
It uses the Koios API, proxied through the app server (because CORS).
-}
taskRetrieveTx : NetworkId -> Bytes TransactionId -> ConcurrentTask ConcurrentTask.Http.Error (Bytes a)
taskRetrieveTx networkId txId =
    let
        thisTxDecoder : Decoder (Bytes a)
        thisTxDecoder =
            koiosFirstTxBytesDecoder
                |> JD.andThen
                    (\tx ->
                        if tx.txId == txId then
                            JD.succeed tx.txCbor

                        else
                            JD.fail <| "The retrieved Tx (" ++ Bytes.toHex tx.txId ++ ") does not correspond to the expected one (" ++ Bytes.toHex txId ++ ")"
                    )

        koiosUrl =
            case networkId of
                Testnet ->
                    "https://preview.koios.rest/api/v1"

                Mainnet ->
                    "https://api.koios.rest/api/v1"
    in
    -- TODO: change to authenticated free tier Koios request
    ConcurrentTask.Http.post
        { url = "/proxy/json"
        , headers = []
        , body =
            ConcurrentTask.Http.jsonBody
                (JE.object
                    [ ( "url", JE.string <| koiosUrl ++ "/tx_cbor" )
                    , ( "method", JE.string "POST" )
                    , ( "body", JE.object [ ( "_tx_hashes", JE.list (JE.string << Bytes.toHex) [ txId ] ) ] )
                    ]
                )
        , expect = ConcurrentTask.Http.expectJson thisTxDecoder
        , timeout = Nothing
        }


{-| Task to retrieve relevant script info for the app.
It uses the Koios API, proxied through the app server (because CORS).
-}
taskGetScriptInfo : NetworkId -> Bytes CredentialHash -> ConcurrentTask ConcurrentTask.Http.Error ScriptInfo
taskGetScriptInfo networkId scriptHash =
    let
        koiosUrl =
            case networkId of
                Testnet ->
                    "https://preview.koios.rest/api/v1"

                Mainnet ->
                    "https://api.koios.rest/api/v1"
    in
    -- TODO: change to authenticated free tier Koios request
    ConcurrentTask.Http.post
        { url = "/proxy/json"
        , headers = []
        , body =
            ConcurrentTask.Http.jsonBody
                (JE.object
                    [ ( "url", JE.string <| koiosUrl ++ "/script_info" )
                    , ( "method", JE.string "POST" )
                    , ( "body", JE.object [ ( "_script_hashes", JE.list (JE.string << Bytes.toHex) [ scriptHash ] ) ] )
                    ]
                )
        , expect = ConcurrentTask.Http.expectJson ScriptInfo.koiosFirstScriptInfoDecoder
        , timeout = Nothing
        }
