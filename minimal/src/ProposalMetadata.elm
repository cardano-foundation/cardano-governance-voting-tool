module ProposalMetadata exposing (AuthorWitness, Body, ProposalMetadata, authorWitnessDecoder, decoder, encode, fromRaw, justAuthorName)

{-| Helper module to handle proposals metadata following [CIP-108](https://cips.cardano.org/cip/CIP-0108).
-}

import Bytes.Comparable as Bytes
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE


{-| Proposal metadata, following [CIP-108](https://cips.cardano.org/cip/CIP-0108).
We keep the raw metadata in order to be able to display it
even if the metadata itself doesn’t follow CIP-108.
-}
type alias ProposalMetadata =
    { raw : String
    , computedHash : String
    , body : Body
    , authors : List AuthorWitness
    }


{-| Author witness for the proposal metadata.
-}
type alias AuthorWitness =
    { name : String
    , witnessAlgorithm : String
    , publicKey : String
    , signature : Maybe String
    }


justAuthorName : String -> AuthorWitness
justAuthorName name =
    { name = name
    , witnessAlgorithm = ""
    , publicKey = ""
    , signature = Nothing
    }


{-| Body of the CIP-108 metadata JSON object.
All fields are optional here to better handle mistakes when creating the metadata.
-}
type alias Body =
    { title : Maybe String
    , abstract : Maybe String
    }


noBody : Body
noBody =
    { title = Nothing
    , abstract = Nothing
    }


{-| JSON encoder, simply using the raw metadata.
-}
encode : ProposalMetadata -> Value
encode { raw } =
    JE.string raw


{-| JSON decoder for proposal metadata.
-}
decoder : Decoder ProposalMetadata
decoder =
    JD.map fromRaw JD.string


{-| Extract proposal metadata, trying to decode its CIP-108 structure.
-}
fromRaw : String -> ProposalMetadata
fromRaw raw =
    let
        computedHash =
            Bytes.fromText raw
                |> Bytes.blake2b256
                |> Bytes.toHex

        authorsDecoder =
            JD.field "authors" (JD.list authorWitnessDecoder)
                |> JD.maybe
                |> JD.map (Maybe.withDefault [])
    in
    Result.map2 (ProposalMetadata raw computedHash)
        (JD.decodeString (JD.field "body" bodyDecoder) raw)
        (JD.decodeString authorsDecoder raw)
        |> Result.withDefault (ProposalMetadata raw computedHash noBody [])


bodyDecoder : Decoder Body
bodyDecoder =
    JD.map2 Body
        (JD.maybe <| JD.field "title" JD.string)
        (JD.maybe <| JD.field "abstract" JD.string)


{-| JSON decoder for author witness.
Follows the standard CIP-100 format with nested witness object:
{ "name": "...", "witness": { "witnessAlgorithm": "...", "publicKey": "...", "signature": "..." } }
-}
authorWitnessDecoder : JD.Decoder AuthorWitness
authorWitnessDecoder =
    JD.map2 (\name w -> AuthorWitness name w.witnessAlgorithm w.publicKey w.signature)
        (JD.field "name" JD.string)
        (JD.field "witness" witnessDecoder
            |> JD.maybe
            |> JD.map (Maybe.withDefault noWitness)
        )


witnessDecoder : JD.Decoder { witnessAlgorithm : String, publicKey : String, signature : Maybe String }
witnessDecoder =
    JD.map3 (\wa pk sig -> { witnessAlgorithm = wa, publicKey = pk, signature = sig })
        (JD.field "witnessAlgorithm" JD.string)
        (JD.field "publicKey" JD.string)
        (JD.field "signature" JD.string |> JD.map Just)


noWitness : { witnessAlgorithm : String, publicKey : String, signature : Maybe String }
noWitness =
    { witnessAlgorithm = ""
    , publicKey = ""
    , signature = Nothing
    }
