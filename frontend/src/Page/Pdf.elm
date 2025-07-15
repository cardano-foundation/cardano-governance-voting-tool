module Page.Pdf exposing (Model, Msg, UpdateContext, ViewContext, initialModel, update, view)

{-| This module handles the conversion of governance metadata JSON-LD files to PDF format.
It specifically supports CIP-136 vote rationale documents, with potential for extension
to other governance metadata types in the future.

Key Features

  - Loads and validates JSON-LD files according to CIP standards
  - Converts valid governance metadata to formatted PDF documents
  - Provides immediate download of generated PDFs

-}

import Api
import Bytes as ElmBytes
import File exposing (File)
import File.Download
import File.Select
import Helper
import Html exposing (Html, div, text)
import Html.Attributes as HA
import Http
import Json.Decode as JD exposing (Decoder)
import Page.Preparation as Preparation exposing (InternalVote, Rationale, Reference, ReferenceType(..))
import Platform.Cmd as Cmd
import Task



-- ###################################################################
-- MODEL
-- ###################################################################


type Model
    = Model InnerModel


{-| The application state, tracking:

  - The loaded JSON file content and its decoded representation
  - The generated PDF bytes (if conversion has occurred)
  - Any error messages that need to be displayed

-}
type alias InnerModel =
    { fileContent : Maybe { raw : String, name : String, decoded : GovMetadataFile }
    , pdfBytes : Maybe ElmBytes.Bytes
    , error : Maybe String
    }


initialModel : Model
initialModel =
    Model innerInitialModel


innerInitialModel : InnerModel
innerInitialModel =
    { fileContent = Nothing
    , pdfBytes = Nothing
    , error = Nothing
    }


{-| Represents the different types of governance metadata files that can be processed.
Currently only supports CIP-136 Vote Rationales, but designed to be extensible for
future governance metadata types.
-}
type GovMetadataFile
    = VoteRationale Rationale -- CIP-136



-- ###################################################################
-- UPDATE
-- ###################################################################


type Msg
    = NoMsg
    | LoadJsonButtonClicked
    | JsonFileSelected File
    | LoadedJson String String
    | ConvertToPdfButtonClicked String
    | GotPdfFile (Result Http.Error ElmBytes.Bytes)


type alias UpdateContext msg =
    { wrapMsg : Msg -> msg
    }


update : UpdateContext msg -> Msg -> Model -> ( Model, Cmd msg )
update ctx msg (Model model) =
    innerUpdate ctx msg model
        |> Tuple.mapFirst Model


innerUpdate : UpdateContext msg -> Msg -> InnerModel -> ( InnerModel, Cmd msg )
innerUpdate ctx msg model =
    case msg of
        NoMsg ->
            ( model, Cmd.none )

        LoadJsonButtonClicked ->
            ( model
            , File.Select.file [] JsonFileSelected
                |> Cmd.map ctx.wrapMsg
            )

        JsonFileSelected file ->
            ( model
            , Task.attempt (handleJsonFileJustRead <| File.name file) (File.toString file)
                |> Cmd.map ctx.wrapMsg
            )

        LoadedJson filename fileContent ->
            case checkJsonMetadataType fileContent of
                Err error ->
                    ( { innerInitialModel | error = Just <| "Unrecognized JSON LD metadata: " ++ error }
                    , Cmd.none
                    )

                Ok govMetadataFile ->
                    ( { model | fileContent = Just { raw = fileContent, name = filename, decoded = govMetadataFile } }
                    , Cmd.none
                    )

        ConvertToPdfButtonClicked rawFileContent ->
            ( model
            , Api.defaultApiProvider.convertToPdf rawFileContent GotPdfFile
                |> Cmd.map ctx.wrapMsg
            )

        GotPdfFile result ->
            case result of
                Err httpError ->
                    ( { model | error = Just <| "An error happened when trying to convert the JSON LD to a PDF: " ++ Debug.toString httpError }
                    , Cmd.none
                    )

                Ok elmBytes ->
                    ( { model | pdfBytes = Just elmBytes, error = Nothing }
                    , File.Download.bytes "metadata.pdf" "application/pdf" elmBytes
                    )


handleJsonFileJustRead : String -> Result x String -> Msg
handleJsonFileJustRead filename result =
    case result of
        Err _ ->
            NoMsg

        Ok jsonFileContent ->
            LoadedJson filename jsonFileContent


{-| Handles JSON validation and decoding for different metadata types.
Currently implements validation for CIP-136 vote rationales, with a structure
that allows for easy addition of other metadata types.
-}
checkJsonMetadataType : String -> Result String GovMetadataFile
checkJsonMetadataType fileContent =
    -- Attempt to decode the file content into
    -- one of the valid shapes for GovMetadataFile
    let
        -- Decoder for the whole structure where body contains a Rationale
        govMetadataDecoder : Decoder GovMetadataFile
        govMetadataDecoder =
            JD.field "body" decodeRationale
                |> JD.map VoteRationale
    in
    case JD.decodeString govMetadataDecoder fileContent of
        Ok metadata ->
            Ok metadata

        Err error ->
            Err (JD.errorToString error)


{-| Decodes a Rationale according to the CIP-136 specification.
Includes required fields (summary, rationaleStatement) and optional fields
(precedentDiscussion, counterargumentDiscussion, conclusion, internalVote, references).
-}
decodeRationale : Decoder Rationale
decodeRationale =
    JD.map7 Rationale
        (JD.field "summary" JD.string)
        (JD.field "rationaleStatement" JD.string)
        (JD.maybe (JD.field "precedentDiscussion" JD.string))
        (JD.maybe (JD.field "counterargumentDiscussion" JD.string))
        (JD.maybe (JD.field "conclusion" JD.string))
        (JD.oneOf
            [ JD.field "internalVote" decodeInternalVote
            , JD.succeed Preparation.noInternalVote
            ]
        )
        (JD.oneOf
            [ JD.field "references" (JD.list decodeReference)
            , JD.succeed []
            ]
        )


decodeInternalVote : Decoder InternalVote
decodeInternalVote =
    JD.map5 InternalVote
        (JD.field "constitutional" JD.int)
        (JD.field "unconstitutional" JD.int)
        (JD.field "abstain" JD.int)
        (JD.field "didNotVote" JD.int)
        (JD.field "against" JD.int)


decodeReference : Decoder Reference
decodeReference =
    JD.map3 Reference
        (JD.field "@type" decodeRefType)
        (JD.field "label" JD.string)
        (JD.field "uri" JD.string)


decodeRefType : Decoder ReferenceType
decodeRefType =
    JD.string
        |> JD.andThen
            (\str ->
                case str of
                    "Other" ->
                        JD.succeed OtherRefType

                    "GovernanceMetadata" ->
                        JD.succeed GovernanceMetadataRefType

                    "RelevantArticles" ->
                        JD.succeed RelevantArticlesRefType

                    _ ->
                        JD.fail "Invalid reference type"
            )



-- ###################################################################
-- VIEW
-- ###################################################################


type alias ViewContext msg =
    { wrapMsg : Msg -> msg
    }


view : ViewContext msg -> Model -> Html msg
view ctx (Model model) =
    let
        extLink href content =
            Html.a
                [ HA.href href
                , HA.target "_blank"
                , HA.rel "noopener noreferrer"
                , HA.style "color" "#0084FF"
                , HA.style "text-decoration" "underline"
                , HA.style "font-weight" "500"
                ]
                [ text content ]

        heroSection =
            div
                [ HA.style "position" "relative"
                , HA.style "overflow" "hidden"
                , HA.style "padding-top" "6rem"
                , HA.style "padding-bottom" "6rem"
                , HA.style "margin-bottom" "2rem"
                ]
                [ div
                    [ HA.style "position" "relative"
                    , HA.style "z-index" "10"
                    , HA.style "max-width" "840px"
                    , HA.style "margin" "0 auto"
                    , HA.style "padding" "0 1.5rem"
                    ]
                    [ Html.h1
                        [ HA.style "font-size" "3.5rem"
                        , HA.style "font-weight" "600"
                        , HA.style "line-height" "1.1"
                        , HA.style "margin-bottom" "1.5rem"
                        ]
                        [ text "Generate Pretty PDFs for Governance Metadata" ]
                    , Html.p
                        [ HA.style "font-size" "1.25rem"
                        , HA.style "line-height" "1.6"
                        , HA.style "max-width" "640px"
                        , HA.style "margin-bottom" "2rem"
                        ]
                        [ text "This page helps you generate well-formatted PDF documents from governance metadata JSON files. It aims to support metadata documents following the "
                        , extLink "https://github.com/cardano-foundation/CIPs/tree/master/CIP-0100" "CIP-100 standard"
                        , text ". For now, rationales following the "
                        , extLink "https://github.com/cardano-foundation/CIPs/tree/master/CIP-0136" "CIP-136 standard"
                        , text " are supported."
                        , div [ HA.style "height" "1rem" ] []
                        , text "Simpler CIP-100 DRep votes format is not directly supported yet. Pull request is welcome!"
                        , text " Meanwhile, we still provide this template for you to convert your CIP-100 DRep vote rationales to pretty PDFs: "
                        , extLink "https://typst.app/project/rfmOnSMMOVlIY52dkYmFNL" "Typst CIP-100 template."
                        ]
                    , Html.div
                        [ HA.style "display" "flex"
                        , HA.style "gap" "1rem"
                        , HA.style "flex-wrap" "wrap"
                        ]
                        [ Helper.viewButton "Load JSON-LD File" LoadJsonButtonClicked ]
                    ]
                , div
                    [ HA.style "position" "absolute"
                    , HA.style "z-index" "1"
                    , HA.style "top" "-13rem"
                    , HA.style "right" "0"
                    , HA.style "left" "0"
                    , HA.style "overflow" "hidden"
                    , HA.style "transform" "translateZ(0)"
                    , HA.style "filter" "blur(64px)"
                    ]
                    [ div
                        [ HA.style "position" "relative"
                        , HA.style "width" "100%"
                        , HA.style "padding-bottom" "58.7%"
                        , HA.style "background" "linear-gradient(90deg, #00E0FF, #0084FF)"
                        , HA.style "opacity" "0.8"
                        , HA.style "clip-path" "polygon(19% 5%, 36% 8%, 55% 15%, 76% 5%, 100% 16%, 100% 100%, 0 100%, 0 14%)"
                        ]
                        []
                    ]
                ]

        fileStatusSection =
            case model.fileContent of
                Nothing ->
                    Helper.formContainer
                        [ Html.h3 [ HA.style "font-size" "1.25rem", HA.style "font-weight" "500", HA.style "margin-bottom" "0.5rem" ]
                            [ text "File Status" ]
                        , Html.p [ HA.style "color" "#666666" ]
                            [ text "No file loaded yet" ]
                        ]

                Just { name, decoded } ->
                    Helper.formContainer
                        [ Html.h3 [ HA.style "font-size" "1.25rem", HA.style "font-weight" "500", HA.style "margin-bottom" "0.5rem" ]
                            [ text "File Status" ]
                        , Html.div [ HA.style "margin-bottom" "0.5rem" ]
                            [ Html.span [ HA.style "font-weight" "500", HA.style "margin-right" "0.5rem" ]
                                [ text "Loaded file:" ]
                            , text name
                            ]
                        , case decoded of
                            VoteRationale rationale ->
                                Html.div []
                                    [ Html.div
                                        [ HA.style "padding" "1rem"
                                        , HA.style "background-color" "#f0f7ff"
                                        , HA.style "border" "1px solid #bedcff"
                                        , HA.style "border-radius" "0.375rem"
                                        , HA.style "margin-top" "0.5rem"
                                        ]
                                        [ Html.h4 [ HA.style "font-weight" "500", HA.style "margin-bottom" "0.5rem" ]
                                            [ text "Vote Rationale" ]
                                        , Html.div [ HA.style "display" "flex", HA.style "flex-direction" "column", HA.style "gap" "0.25rem" ]
                                            [ Html.div []
                                                [ Html.span [ HA.style "font-weight" "500", HA.style "margin-right" "0.5rem" ]
                                                    [ text "Summary:" ]
                                                , text
                                                    (String.left 80 rationale.summary
                                                        ++ (if String.length rationale.summary > 80 then
                                                                "..."

                                                            else
                                                                ""
                                                           )
                                                    )
                                                ]
                                            , if not (List.isEmpty rationale.references) then
                                                Html.div []
                                                    [ Html.span [ HA.style "font-weight" "500", HA.style "margin-right" "0.5rem" ]
                                                        [ text "References:" ]
                                                    , text (String.fromInt (List.length rationale.references) ++ " included")
                                                    ]

                                              else
                                                text ""
                                            ]
                                        ]
                                    ]
                        ]

        pdfConversionSection =
            case model.fileContent of
                Just { raw, decoded } ->
                    case decoded of
                        VoteRationale _ ->
                            Helper.formContainer
                                [ Html.h3 [ HA.style "font-size" "1.25rem", HA.style "font-weight" "500", HA.style "margin-bottom" "0.5rem" ]
                                    [ text "PDF Conversion" ]
                                , Html.p [ HA.style "margin-bottom" "1rem" ]
                                    [ text "Convert the loaded JSON-LD metadata file to a nicely formatted PDF document." ]
                                , Html.div [ HA.style "display" "flex", HA.style "align-items" "center" ]
                                    [ Helper.viewButton "Generate PDF" (ConvertToPdfButtonClicked raw)
                                    , if model.pdfBytes /= Nothing then
                                        Html.span [ HA.style "margin-left" "1rem", HA.style "color" "#059669" ]
                                            [ text "✓ PDF generated and downloaded" ]

                                      else
                                        text ""
                                    ]
                                ]

                _ ->
                    text ""

        errorSection =
            case model.error of
                Nothing ->
                    text ""

                Just err ->
                    Html.div
                        [ HA.style "margin-top" "1rem"
                        , HA.style "margin-bottom" "1rem"
                        , HA.style "padding" "1rem"
                        , HA.style "background-color" "#fef2f2"
                        , HA.style "border" "1px solid #fecaca"
                        , HA.style "border-radius" "0.375rem"
                        ]
                        [ Html.p [ HA.style "color" "#b91c1c", HA.style "font-weight" "500", HA.style "margin-bottom" "0.5rem" ]
                            [ text "Error:" ]
                        , Html.pre
                            [ HA.style "font-size" "0.875rem"
                            , HA.style "white-space" "pre-wrap"
                            , HA.style "background-color" "white"
                            , HA.style "padding" "0.5rem"
                            , HA.style "border" "1px solid #fee2e2"
                            , HA.style "border-radius" "0.25rem"
                            ]
                            [ text err ]
                        ]
    in
    Html.map ctx.wrapMsg <|
        div [ HA.style "max-width" "1536px", HA.style "margin" "0 auto" ]
            [ heroSection
            , div
                [ HA.style "max-width" "840px"
                , HA.style "margin" "0 auto"
                , HA.style "padding" "0 1.5rem"
                ]
                [ fileStatusSection
                , pdfConversionSection
                , errorSection
                ]
            ]
