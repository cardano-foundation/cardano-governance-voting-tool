module Api exposing (ActiveProposal, ProtocolParams, loadProtocolParams, queryEpoch, loadGovProposals, taskLoadProposalMetadata)

{-| Minimal API module for fetching Cardano governance data from Koios.
-}

import Bytes.Comparable as Bytes
import Cardano.Address exposing (NetworkId(..))
import Cardano.Gov as Gov exposing (ActionId, CostModels)
import ConcurrentTask exposing (ConcurrentTask)
import ConcurrentTask.Http
import ConcurrentTask.Process
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import Natural
import ProposalMetadata exposing (ProposalMetadata)
import RemoteData exposing (RemoteData)


{-| Free Tier Koios API token.
-}
koiosApiToken : String
koiosApiToken =
    "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJhZGRyIjoic3Rha2UxdXljY3J5MzZwcXB0aGV4cmw5eW4zZDN6azJrbGR3N3lhdG0wM2gwcHU1eXdjMHFqMzYyNzQiLCJleHAiOjE4MDI5NDcwMTIsInRpZXIiOjEsInByb2pJRCI6Ind5Wk1Sb0ZmYnBKdmNuYncifQ.JMJNKGGXo_yDBottzKUB34D1afR6-2j3vtxw70k1Les"


koiosUrl : NetworkId -> String
koiosUrl networkId =
    case networkId of
        Testnet ->
            "https://preview.koios.rest/api/v1"

        Mainnet ->
            "https://api.koios.rest/api/v1"



-- Protocol Parameters


type alias ProtocolParams =
    { costModels : CostModels
    }


loadProtocolParams : NetworkId -> (Result Http.Error ProtocolParams -> msg) -> Cmd msg
loadProtocolParams networkId toMsg =
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
        , expect =
            Http.expectJson toMsg
                (JD.map3
                    (\v1 v2 v3 -> { costModels = CostModels (Just v1) (Just v2) (Just v3) })
                    (JD.at [ "result", "plutusCostModels", "plutus:v1" ] <| JD.list JD.int)
                    (JD.at [ "result", "plutusCostModels", "plutus:v2" ] <| JD.list JD.int)
                    (JD.at [ "result", "plutusCostModels", "plutus:v3" ] <| JD.list JD.int)
                )
        , timeout = Nothing
        , tracker = Nothing
        }



-- Epoch


queryEpoch : NetworkId -> (Result Http.Error Int -> msg) -> Cmd msg
queryEpoch networkId toMsg =
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
        , expect = Http.expectJson toMsg (JD.field "result" JD.int)
        , timeout = Nothing
        , tracker = Nothing
        }



-- Governance Proposals


type alias ActiveProposal =
    { id : ActionId
    , actionType : String
    , metadataUrl : String
    , metadataHash : String
    , epoch_validity : { start : Int, end : Int }
    , ratified : Maybe Int
    , metadata : RemoteData String ProposalMetadata
    }


loadGovProposals : NetworkId -> Int -> (Result Http.Error (List ActiveProposal) -> msg) -> Cmd msg
loadGovProposals networkId currentEpoch toMsg =
    let
        selectedRows =
            [ "proposal_tx_hash", "proposal_index", "proposal_type"
            , "meta_url", "meta_hash", "proposed_epoch", "expiration", "ratified_epoch"
            ]
                |> String.join ","
    in
    Http.request
        { method = "GET"
        , url = koiosUrl networkId ++ "/proposal_list?select=" ++ selectedRows ++ "&expiration=gt." ++ String.fromInt currentEpoch
        , headers = [ Http.header "Authorization" <| "Bearer " ++ koiosApiToken ]
        , body = Http.emptyBody
        , expect =
            Http.expectJson toMsg
                (JD.list <|
                    JD.map7 ActiveProposal
                        (JD.map2
                            (\txHash index -> { transactionId = Bytes.fromHexUnchecked txHash, govActionIndex = index })
                            (JD.field "proposal_tx_hash" JD.string)
                            (JD.field "proposal_index" JD.int)
                        )
                        (JD.field "proposal_type" JD.string)
                        (JD.field "meta_url" JD.string)
                        (JD.field "meta_hash" JD.string)
                        (JD.map2
                            (\start end -> { start = start, end = end })
                            (JD.field "proposed_epoch" JD.int)
                            (JD.field "expiration" JD.int)
                        )
                        (JD.field "ratified_epoch" <| JD.maybe JD.int)
                        (JD.succeed RemoteData.Loading)
                )
        , timeout = Nothing
        , tracker = Nothing
        }



-- Proposal Metadata (via ConcurrentTask for caching)


ipfsGateways : List String
ipfsGateways =
    [ "https://ipfs.io/ipfs/"
    , "https://ipfs.blockfrost.dev/ipfs/"
    , "https://dweb.link/ipfs/"
    , "https://c-ipfs-gw.nmkr.io/ipfs/"
    , "https://cloudflare-ipfs.com/ipfs/"
    , "https://gateway.pinata.cloud/ipfs/"
    ]


taskLoadProposalMetadata : String -> ConcurrentTask String ProposalMetadata
taskLoadProposalMetadata url =
    taskFetchFromUrl url
        |> ConcurrentTask.map ProposalMetadata.fromRaw


taskFetchFromUrl : String -> ConcurrentTask String String
taskFetchFromUrl url =
    if String.startsWith "ipfs://" url then
        let
            cid =
                String.dropLeft 7 url

            staggeredTask index gateway =
                ConcurrentTask.Process.sleep (index * 2000)
                    |> ConcurrentTask.andThenDo (fetchFromUrl (gateway ++ cid))
        in
        case ipfsGateways of
            [] ->
                ConcurrentTask.fail "No IPFS gateways configured"

            first :: rest ->
                ConcurrentTask.race
                    (fetchFromUrl (first ++ cid))
                    (List.indexedMap (\i gw -> staggeredTask (i + 1) gw) rest)
                    |> ConcurrentTask.onError (\_ -> ConcurrentTask.fail "All IPFS gateways failed")

    else
        fetchFromUrl url
            |> ConcurrentTask.onError (\_ -> ConcurrentTask.fail "HTTP request failed")


fetchFromUrl : String -> ConcurrentTask ConcurrentTask.Http.Error String
fetchFromUrl url =
    ConcurrentTask.Http.get
        { url = url
        , headers = []
        , expect = ConcurrentTask.Http.expectString
        , timeout = Nothing
        }
