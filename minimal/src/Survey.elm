module Survey exposing
    ( AnswerForm(..)
    , AnswerItem(..)
    , FormMsg(..)
    , NumericConstraints
    , ParsedPayload(..)
    , QuestionForm
    , QuestionType(..)
    , ResponseForm
    , ResponseFormMsg(..)
    , Role(..)
    , SurveyDefinition
    , SurveyForm
    , SurveyQuestion(..)
    , SurveyRef
    , SurveyResponse
    , WeightingMode(..)
    , buildCancellationMetadatum
    , buildResponseMetadatum
    , credentialToHex
    , emptyForm
    , emptyQuestion
    , formToDefinition
    , fromMetadatum
    , initResponseForm
    , metadataLabel
    , questionTypeToString
    , roleToString
    , toMetadatum
    , updateForm
    , updateResponseForm
    , viewResponseCard
    , viewResponseForm
    , viewSurvey
    , viewSurveyForm
    , weightingModeToString
    )

import Bytes.Comparable as Bytes exposing (Any, Bytes)
import Cardano.Address exposing (Credential(..), CredentialHash)
import Cardano.Metadatum exposing (Metadatum(..))
import Html exposing (Html, button, div, h3, h4, input, label, option, p, select, span, text, textarea)
import Html.Attributes as HA
import Html.Events as HE
import Integer exposing (Integer)
import List.Extra



-- ============================================================
-- CIP-179 CONSTANTS
-- ============================================================


{-| Metadata label for CIP-179 surveys.
Using 171717 instead of 17 to avoid collisions during experimentation.
-}
metadataLabel : Int
metadataLabel =
    171717



-- ============================================================
-- CIP-179 TYPES
-- ============================================================


type Role
    = DRep
    | SPO
    | CC
    | Stakeholder


type WeightingMode
    = CredentialBased
    | StakeBased
    | PledgeBased


type alias NumericConstraints =
    { minValue : Int
    , maxValue : Int
    , step : Maybe Int
    }


type SurveyQuestion
    = SingleChoice { prompt : String, options : List String }
    | MultiSelect { prompt : String, options : List String, maxSelections : Int }
    | Ranking { prompt : String, options : List String, maxRanked : Int }
    | NumericRange { prompt : String, constraints : NumericConstraints }
    | Custom { prompt : String, schemaUri : String, schemaHash : Bytes Any }


type alias SurveyDefinition =
    { specVersion : Int
    , owner : Credential
    , title : String
    , description : String
    , questions : List SurveyQuestion
    , roleWeighting : List ( Role, WeightingMode )
    , endEpoch : Int
    }


type alias SurveyRef =
    { txHash : String
    , index : Int
    }


type alias SurveyResponse =
    { surveyRef : SurveyRef
    , role : Role
    , responder : Credential
    , answers : List AnswerItem
    }


type AnswerItem
    = AnswerSingleChoice Int Int
    | AnswerMultiSelect Int (List Int)
    | AnswerRanking Int (List Int)
    | AnswerNumeric Int Int
    | AnswerCustom Int Metadatum


type ParsedPayload
    = ParsedDefinitions (List SurveyDefinition)
    | ParsedResponses (List SurveyResponse)
    | ParsedCancellations (List SurveyRef)



-- ============================================================
-- STRING HELPERS
-- ============================================================


allRoles : List Role
allRoles =
    [ DRep, SPO, CC, Stakeholder ]


roleToInt : Role -> Int
roleToInt role =
    case role of
        DRep ->
            0

        SPO ->
            1

        CC ->
            2

        Stakeholder ->
            3


intToRole : Int -> Maybe Role
intToRole n =
    case n of
        0 ->
            Just DRep

        1 ->
            Just SPO

        2 ->
            Just CC

        3 ->
            Just Stakeholder

        _ ->
            Nothing


roleToString : Role -> String
roleToString role =
    case role of
        DRep ->
            "DRep"

        SPO ->
            "SPO"

        CC ->
            "CC"

        Stakeholder ->
            "Stakeholder"


weightingModeToInt : WeightingMode -> Int
weightingModeToInt wm =
    case wm of
        CredentialBased ->
            0

        StakeBased ->
            1

        PledgeBased ->
            2


intToWeightingMode : Int -> Maybe WeightingMode
intToWeightingMode n =
    case n of
        0 ->
            Just CredentialBased

        1 ->
            Just StakeBased

        2 ->
            Just PledgeBased

        _ ->
            Nothing


weightingModeToString : WeightingMode -> String
weightingModeToString wm =
    case wm of
        CredentialBased ->
            "Credential-based"

        StakeBased ->
            "Stake-based"

        PledgeBased ->
            "Pledge-based"


questionTypeToString : QuestionType -> String
questionTypeToString qt =
    case qt of
        SingleChoiceType ->
            "Single choice"

        MultiSelectType ->
            "Multi-select"

        RankingType ->
            "Ranking"

        NumericRangeType ->
            "Numeric range"

        CustomType ->
            "Custom"


allowedWeightings : Role -> List WeightingMode
allowedWeightings role =
    case role of
        DRep ->
            [ CredentialBased, StakeBased ]

        SPO ->
            [ CredentialBased, StakeBased, PledgeBased ]

        CC ->
            [ CredentialBased ]

        Stakeholder ->
            [ StakeBased ]



-- ============================================================
-- ENCODING (SurveyDefinition -> Metadatum)
-- ============================================================


chunkText : String -> List String
chunkText s =
    if String.isEmpty s then
        [ "" ]

    else
        chunkTextByBytes (String.toList s) [] "" 0


{-| Split a string into chunks that each fit within 64 UTF-8 bytes.
Splits at character boundaries to avoid cutting multi-byte chars.
-}
chunkTextByBytes : List Char -> List String -> String -> Int -> List String
chunkTextByBytes chars acc currentChunk currentBytes =
    case chars of
        [] ->
            List.reverse (currentChunk :: acc)

        c :: rest ->
            let
                w =
                    charUtf8Width c
            in
            if currentBytes + w > 64 then
                chunkTextByBytes rest (currentChunk :: acc) (String.fromChar c) w

            else
                chunkTextByBytes rest acc (currentChunk ++ String.fromChar c) (currentBytes + w)


charUtf8Width : Char -> Int
charUtf8Width c =
    let
        code =
            Char.toCode c
    in
    if code <= 0x7F then
        1

    else if code <= 0x07FF then
        2

    else if code <= 0xFFFF then
        3

    else
        4


metaInt : Int -> Metadatum
metaInt n =
    Int (Integer.fromSafeInt n)


metaStr : String -> Metadatum
metaStr s =
    String s


metaBytes : Bytes Any -> Metadatum
metaBytes b =
    Bytes b


chunkedTextToMeta : String -> Metadatum
chunkedTextToMeta s =
    case chunkText s of
        [ single ] ->
            metaStr single

        chunks ->
            List (List.map metaStr chunks)


credentialToMeta : Credential -> Metadatum
credentialToMeta cred =
    case cred of
        VKeyHash hash ->
            List [ metaInt 0, metaBytes (Bytes.toAny hash) ]

        ScriptHash hash ->
            List [ metaInt 1, metaBytes (Bytes.toAny hash) ]


questionToMeta : SurveyQuestion -> Metadatum
questionToMeta q =
    case q of
        SingleChoice { prompt, options } ->
            List
                [ metaInt 0
                , chunkedTextToMeta prompt
                , List (List.map metaStr options)
                ]

        MultiSelect { prompt, options, maxSelections } ->
            List
                [ metaInt 1
                , chunkedTextToMeta prompt
                , List (List.map metaStr options)
                , metaInt maxSelections
                ]

        Ranking { prompt, options, maxRanked } ->
            List
                [ metaInt 2
                , chunkedTextToMeta prompt
                , List (List.map metaStr options)
                , metaInt maxRanked
                ]

        NumericRange { prompt, constraints } ->
            let
                constraintList =
                    [ metaInt constraints.minValue, metaInt constraints.maxValue ]
                        ++ (case constraints.step of
                                Just s ->
                                    [ metaInt s ]

                                Nothing ->
                                    []
                           )
            in
            List
                [ metaInt 3
                , chunkedTextToMeta prompt
                , List constraintList
                ]

        Custom { prompt, schemaUri, schemaHash } ->
            List
                [ metaInt 4
                , chunkedTextToMeta prompt
                , chunkedTextToMeta schemaUri
                , metaBytes schemaHash
                ]


roleWeightingToMeta : List ( Role, WeightingMode ) -> Metadatum
roleWeightingToMeta rws =
    Map
        (List.map
            (\( role, wm ) ->
                ( metaInt (roleToInt role), metaInt (weightingModeToInt wm) )
            )
            rws
        )


toMetadatum : SurveyDefinition -> Metadatum
toMetadatum def =
    List
        [ metaInt 0
        , List
            [ List
                [ metaInt def.specVersion
                , credentialToMeta def.owner
                , chunkedTextToMeta def.title
                , chunkedTextToMeta def.description
                , List (List.map questionToMeta def.questions)
                , roleWeightingToMeta def.roleWeighting
                , metaInt def.endEpoch
                ]
            ]
        ]



-- ============================================================
-- DECODING (Metadatum -> SurveyDefinition)
-- ============================================================


expectInt : Metadatum -> Result String Int
expectInt m =
    case m of
        Int i ->
            Ok (Integer.toInt i)

        _ ->
            Err "Expected integer"


expectStr : Metadatum -> Result String String
expectStr m =
    case m of
        String s ->
            Ok s

        _ ->
            Err "Expected string"


expectList : Metadatum -> Result String (List Metadatum)
expectList m =
    case m of
        List items ->
            Ok items

        _ ->
            Err "Expected list"


expectMap : Metadatum -> Result String (List ( Metadatum, Metadatum ))
expectMap m =
    case m of
        Map pairs ->
            Ok pairs

        _ ->
            Err "Expected map"


expectBytes : Metadatum -> Result String (Bytes Any)
expectBytes m =
    case m of
        Bytes b ->
            Ok b

        _ ->
            Err "Expected bytes"


decodeChunkedText : Metadatum -> Result String String
decodeChunkedText m =
    case m of
        String s ->
            Ok s

        List items ->
            traverseResults expectStr items
                |> Result.map String.concat

        _ ->
            Err "Expected string or list of strings"


traverseResults : (a -> Result e b) -> List a -> Result e (List b)
traverseResults f list =
    List.foldr
        (\item acc ->
            Result.map2 (::) (f item) acc
        )
        (Ok [])
        list


decodeCredential : Metadatum -> Result String Credential
decodeCredential m =
    expectList m
        |> Result.andThen
            (\items ->
                case items of
                    [ tagM, hashM ] ->
                        Result.map2
                            (\tag hash ->
                                case tag of
                                    0 ->
                                        Ok (VKeyHash (Bytes.fromHexUnchecked (Bytes.toHex hash)))

                                    1 ->
                                        Ok (ScriptHash (Bytes.fromHexUnchecked (Bytes.toHex hash)))

                                    _ ->
                                        Err ("Unknown credential tag: " ++ String.fromInt tag)
                            )
                            (expectInt tagM)
                            (expectBytes hashM)
                            |> Result.andThen identity

                    _ ->
                        Err "Credential must be a 2-element list"
            )


decodeQuestion : Metadatum -> Result String SurveyQuestion
decodeQuestion m =
    expectList m
        |> Result.andThen
            (\items ->
                case items of
                    tagM :: rest ->
                        expectInt tagM
                            |> Result.andThen
                                (\tag ->
                                    case tag of
                                        0 ->
                                            decodeSingleChoice rest

                                        1 ->
                                            decodeMultiSelect rest

                                        2 ->
                                            decodeRanking rest

                                        3 ->
                                            decodeNumericRange rest

                                        4 ->
                                            decodeCustom rest

                                        _ ->
                                            Err ("Unknown question type tag: " ++ String.fromInt tag)
                                )

                    [] ->
                        Err "Question array is empty"
            )


decodeSingleChoice : List Metadatum -> Result String SurveyQuestion
decodeSingleChoice items =
    case items of
        [ promptM, optionsM ] ->
            Result.map2
                (\prompt options ->
                    SingleChoice { prompt = prompt, options = options }
                )
                (decodeChunkedText promptM)
                (expectList optionsM |> Result.andThen (traverseResults expectStr))

        _ ->
            Err "SingleChoice: expected [prompt, options]"


decodeMultiSelect : List Metadatum -> Result String SurveyQuestion
decodeMultiSelect items =
    case items of
        [ promptM, optionsM, maxM ] ->
            Result.map3
                (\prompt options maxSel ->
                    MultiSelect { prompt = prompt, options = options, maxSelections = maxSel }
                )
                (decodeChunkedText promptM)
                (expectList optionsM |> Result.andThen (traverseResults expectStr))
                (expectInt maxM)

        _ ->
            Err "MultiSelect: expected [prompt, options, maxSelections]"


decodeRanking : List Metadatum -> Result String SurveyQuestion
decodeRanking items =
    case items of
        [ promptM, optionsM, maxM ] ->
            Result.map3
                (\prompt options maxR ->
                    Ranking { prompt = prompt, options = options, maxRanked = maxR }
                )
                (decodeChunkedText promptM)
                (expectList optionsM |> Result.andThen (traverseResults expectStr))
                (expectInt maxM)

        _ ->
            Err "Ranking: expected [prompt, options, maxRanked]"


decodeNumericRange : List Metadatum -> Result String SurveyQuestion
decodeNumericRange items =
    case items of
        [ promptM, constraintsM ] ->
            Result.map2
                (\prompt constraints ->
                    NumericRange { prompt = prompt, constraints = constraints }
                )
                (decodeChunkedText promptM)
                (decodeNumericConstraints constraintsM)

        _ ->
            Err "NumericRange: expected [prompt, constraints]"


decodeNumericConstraints : Metadatum -> Result String NumericConstraints
decodeNumericConstraints m =
    expectList m
        |> Result.andThen
            (\items ->
                case items of
                    [ minM, maxM ] ->
                        Result.map2
                            (\minV maxV ->
                                { minValue = minV, maxValue = maxV, step = Nothing }
                            )
                            (expectInt minM)
                            (expectInt maxM)

                    [ minM, maxM, stepM ] ->
                        Result.map3
                            (\minV maxV stepV ->
                                { minValue = minV, maxValue = maxV, step = Just stepV }
                            )
                            (expectInt minM)
                            (expectInt maxM)
                            (expectInt stepM)

                    _ ->
                        Err "NumericConstraints: expected 2 or 3 elements"
            )


decodeCustom : List Metadatum -> Result String SurveyQuestion
decodeCustom items =
    case items of
        [ promptM, uriM, hashM ] ->
            Result.map3
                (\prompt uri hash ->
                    Custom { prompt = prompt, schemaUri = uri, schemaHash = hash }
                )
                (decodeChunkedText promptM)
                (decodeChunkedText uriM)
                (expectBytes hashM)

        _ ->
            Err "Custom: expected [prompt, schemaUri, schemaHash]"


decodeRoleWeighting : Metadatum -> Result String (List ( Role, WeightingMode ))
decodeRoleWeighting m =
    expectMap m
        |> Result.andThen
            (traverseResults
                (\( keyM, valM ) ->
                    Result.map2 Tuple.pair
                        (expectInt keyM
                            |> Result.andThen
                                (\n ->
                                    intToRole n
                                        |> Result.fromMaybe ("Unknown role: " ++ String.fromInt n)
                                )
                        )
                        (expectInt valM
                            |> Result.andThen
                                (\n ->
                                    intToWeightingMode n
                                        |> Result.fromMaybe ("Unknown weighting mode: " ++ String.fromInt n)
                                )
                        )
                )
            )


decodeDefinition : Metadatum -> Result String SurveyDefinition
decodeDefinition m =
    expectList m
        |> Result.andThen
            (\items ->
                case items of
                    [ versionM, ownerM, titleM, descM, questionsM, rwM, epochM ] ->
                        Ok SurveyDefinition
                            |> resultApply (expectInt versionM)
                            |> resultApply (decodeCredential ownerM)
                            |> resultApply (decodeChunkedText titleM)
                            |> resultApply (decodeChunkedText descM)
                            |> resultApply (expectList questionsM |> Result.andThen (traverseResults decodeQuestion))
                            |> resultApply (decodeRoleWeighting rwM)
                            |> resultApply (expectInt epochM)

                    _ ->
                        Err ("Survey definition: expected 7-element array, got " ++ String.fromInt (List.length items))
            )


resultApply : Result e a -> Result e (a -> b) -> Result e b
resultApply ra rf =
    case ( rf, ra ) of
        ( Ok f, Ok a ) ->
            Ok (f a)

        ( Err e, _ ) ->
            Err e

        ( _, Err e ) ->
            Err e


fromMetadatum : Metadatum -> Result String ParsedPayload
fromMetadatum m =
    expectList m
        |> Result.andThen
            (\items ->
                case items of
                    [ tagM, contentM ] ->
                        expectInt tagM
                            |> Result.andThen
                                (\tag ->
                                    case tag of
                                        0 ->
                                            expectList contentM
                                                |> Result.andThen (traverseResults decodeDefinition)
                                                |> Result.map ParsedDefinitions

                                        1 ->
                                            expectList contentM
                                                |> Result.andThen (traverseResults decodeResponse)
                                                |> Result.map ParsedResponses

                                        2 ->
                                            expectList contentM
                                                |> Result.andThen (traverseResults decodeSurveyRef)
                                                |> Result.map ParsedCancellations

                                        _ ->
                                            Err ("Unknown CIP-179 tag: " ++ String.fromInt tag)
                                )

                    _ ->
                        Err "CIP-179 payload: expected [tag, content]"
            )


decodeResponse : Metadatum -> Result String SurveyResponse
decodeResponse m =
    expectList m
        |> Result.andThen
            (\items ->
                case items of
                    [ refM, roleM, credM, answersM ] ->
                        Ok SurveyResponse
                            |> resultApply (decodeSurveyRef refM)
                            |> resultApply
                                (expectInt roleM
                                    |> Result.andThen
                                        (\n ->
                                            intToRole n
                                                |> Result.fromMaybe ("Unknown role: " ++ String.fromInt n)
                                        )
                                )
                            |> resultApply (decodeCredential credM)
                            |> resultApply (expectList answersM |> Result.andThen (traverseResults decodeAnswerItem))

                    _ ->
                        Err ("Survey response: expected 4-element array, got " ++ String.fromInt (List.length items))
            )


decodeSurveyRef : Metadatum -> Result String SurveyRef
decodeSurveyRef m =
    expectList m
        |> Result.andThen
            (\items ->
                case items of
                    [ txIdM, indexM ] ->
                        Result.map2 SurveyRef
                            (expectBytes txIdM |> Result.map (\b -> Bytes.toHex b))
                            (expectInt indexM)

                    _ ->
                        Err "Survey ref: expected [tx_id, index]"
            )


decodeAnswerItem : Metadatum -> Result String AnswerItem
decodeAnswerItem m =
    expectList m
        |> Result.andThen
            (\items ->
                case items of
                    tagM :: rest ->
                        expectInt tagM
                            |> Result.andThen
                                (\tag ->
                                    case ( tag, rest ) of
                                        ( 0, [ qIdxM, optIdxM ] ) ->
                                            Result.map2 AnswerSingleChoice
                                                (expectInt qIdxM)
                                                (expectInt optIdxM)

                                        ( 1, [ qIdxM, selM ] ) ->
                                            Result.map2 AnswerMultiSelect
                                                (expectInt qIdxM)
                                                (expectList selM |> Result.andThen (traverseResults expectInt))

                                        ( 2, [ qIdxM, rankM ] ) ->
                                            Result.map2 AnswerRanking
                                                (expectInt qIdxM)
                                                (expectList rankM |> Result.andThen (traverseResults expectInt))

                                        ( 3, [ qIdxM, valM ] ) ->
                                            Result.map2 AnswerNumeric
                                                (expectInt qIdxM)
                                                (expectInt valM)

                                        ( 4, [ qIdxM, valM ] ) ->
                                            expectInt qIdxM
                                                |> Result.map (\qIdx -> AnswerCustom qIdx valM)

                                        _ ->
                                            Err ("Unknown answer tag/arity: " ++ String.fromInt tag)
                                )

                    [] ->
                        Err "Answer item array is empty"
            )



-- ============================================================
-- FORM TYPES
-- ============================================================


type QuestionType
    = SingleChoiceType
    | MultiSelectType
    | RankingType
    | NumericRangeType
    | CustomType


allQuestionTypes : List QuestionType
allQuestionTypes =
    [ SingleChoiceType, MultiSelectType, RankingType, NumericRangeType, CustomType ]


type alias QuestionForm =
    { questionType : QuestionType
    , prompt : String
    , options : List String
    , maxSelections : String
    , maxRanked : String
    , minValue : String
    , maxValue : String
    , step : String
    , schemaUri : String
    , schemaHash : String
    }


type alias RoleWeightingEntry =
    { role : Role
    , enabled : Bool
    , weightingMode : WeightingMode
    }


type alias SurveyForm =
    { title : String
    , description : String
    , questions : List QuestionForm
    , roleWeightings : List RoleWeightingEntry
    , endEpoch : String
    , ownerKeyHash : String
    }


type FormMsg
    = SetTitle String
    | SetDescription String
    | SetEndEpoch String
    | SetOwnerKeyHash String
    | AddQuestion
    | RemoveQuestion Int
    | SetQuestionType Int String
    | SetPrompt Int String
    | AddOption Int
    | RemoveOption Int Int
    | SetOption Int Int String
    | SetMaxSelections Int String
    | SetMaxRanked Int String
    | SetMinValue Int String
    | SetMaxValue Int String
    | SetStep Int String
    | SetSchemaUri Int String
    | SetSchemaHash Int String
    | ToggleRole Role
    | SetWeighting Role String
    | SubmitSurvey



-- ============================================================
-- FORM LOGIC
-- ============================================================


emptyQuestion : QuestionForm
emptyQuestion =
    { questionType = SingleChoiceType
    , prompt = ""
    , options = [ "", "" ]
    , maxSelections = "2"
    , maxRanked = "2"
    , minValue = "0"
    , maxValue = "100"
    , step = ""
    , schemaUri = ""
    , schemaHash = ""
    }


emptyForm : SurveyForm
emptyForm =
    { title = ""
    , description = ""
    , questions = [ emptyQuestion ]
    , roleWeightings =
        List.map
            (\role ->
                { role = role
                , enabled = role == DRep
                , weightingMode = List.head (allowedWeightings role) |> Maybe.withDefault CredentialBased
                }
            )
            allRoles
    , endEpoch = ""
    , ownerKeyHash = ""
    }


updateForm : FormMsg -> SurveyForm -> SurveyForm
updateForm msg form =
    case msg of
        SetTitle v ->
            { form | title = v }

        SetDescription v ->
            { form | description = v }

        SetEndEpoch v ->
            { form | endEpoch = v }

        SetOwnerKeyHash v ->
            { form | ownerKeyHash = v }

        AddQuestion ->
            { form | questions = form.questions ++ [ emptyQuestion ] }

        RemoveQuestion idx ->
            { form | questions = List.Extra.removeAt idx form.questions }

        SetQuestionType idx typeStr ->
            { form | questions = updateAt idx (setQuestionType typeStr) form.questions }

        SetPrompt idx v ->
            { form | questions = updateAt idx (\q -> { q | prompt = v }) form.questions }

        AddOption idx ->
            { form | questions = updateAt idx (\q -> { q | options = q.options ++ [ "" ] }) form.questions }

        RemoveOption qIdx oIdx ->
            { form
                | questions =
                    updateAt qIdx
                        (\q -> { q | options = List.Extra.removeAt oIdx q.options })
                        form.questions
            }

        SetOption qIdx oIdx v ->
            { form
                | questions =
                    updateAt qIdx
                        (\q -> { q | options = updateAt oIdx (\_ -> v) q.options })
                        form.questions
            }

        SetMaxSelections idx v ->
            { form | questions = updateAt idx (\q -> { q | maxSelections = v }) form.questions }

        SetMaxRanked idx v ->
            { form | questions = updateAt idx (\q -> { q | maxRanked = v }) form.questions }

        SetMinValue idx v ->
            { form | questions = updateAt idx (\q -> { q | minValue = v }) form.questions }

        SetMaxValue idx v ->
            { form | questions = updateAt idx (\q -> { q | maxValue = v }) form.questions }

        SetStep idx v ->
            { form | questions = updateAt idx (\q -> { q | step = v }) form.questions }

        SetSchemaUri idx v ->
            { form | questions = updateAt idx (\q -> { q | schemaUri = v }) form.questions }

        SetSchemaHash idx v ->
            { form | questions = updateAt idx (\q -> { q | schemaHash = v }) form.questions }

        ToggleRole role ->
            { form
                | roleWeightings =
                    List.map
                        (\rw ->
                            if rw.role == role then
                                { rw | enabled = not rw.enabled }

                            else
                                rw
                        )
                        form.roleWeightings
            }

        SetWeighting role wmStr ->
            { form
                | roleWeightings =
                    List.map
                        (\rw ->
                            if rw.role == role then
                                { rw | weightingMode = stringToWeightingMode wmStr }

                            else
                                rw
                        )
                        form.roleWeightings
            }

        SubmitSurvey ->
            form


updateAt : Int -> (a -> a) -> List a -> List a
updateAt idx f list =
    List.indexedMap
        (\i item ->
            if i == idx then
                f item

            else
                item
        )
        list


setQuestionType : String -> QuestionForm -> QuestionForm
setQuestionType typeStr q =
    let
        qt =
            stringToQuestionType typeStr
    in
    { q
        | questionType = qt
        , options =
            if needsOptions qt && List.length q.options < 2 then
                [ "", "" ]

            else
                q.options
    }


needsOptions : QuestionType -> Bool
needsOptions qt =
    case qt of
        SingleChoiceType ->
            True

        MultiSelectType ->
            True

        RankingType ->
            True

        NumericRangeType ->
            False

        CustomType ->
            False


stringToQuestionType : String -> QuestionType
stringToQuestionType s =
    case s of
        "multi-select" ->
            MultiSelectType

        "ranking" ->
            RankingType

        "numeric-range" ->
            NumericRangeType

        "custom" ->
            CustomType

        _ ->
            SingleChoiceType


questionTypeToValue : QuestionType -> String
questionTypeToValue qt =
    case qt of
        SingleChoiceType ->
            "single-choice"

        MultiSelectType ->
            "multi-select"

        RankingType ->
            "ranking"

        NumericRangeType ->
            "numeric-range"

        CustomType ->
            "custom"


stringToWeightingMode : String -> WeightingMode
stringToWeightingMode s =
    case s of
        "stake" ->
            StakeBased

        "pledge" ->
            PledgeBased

        _ ->
            CredentialBased


weightingModeToValue : WeightingMode -> String
weightingModeToValue wm =
    case wm of
        CredentialBased ->
            "credential"

        StakeBased ->
            "stake"

        PledgeBased ->
            "pledge"



-- ============================================================
-- FORM VALIDATION -> SurveyDefinition
-- ============================================================


formToDefinition : SurveyForm -> Result String SurveyDefinition
formToDefinition form =
    let
        validateTitle =
            if String.isEmpty (String.trim form.title) then
                Err "Title is required"

            else
                Ok (String.trim form.title)

        validateDescription =
            Ok (String.trim form.description)

        validateEndEpoch =
            String.toInt form.endEpoch
                |> Result.fromMaybe "End epoch must be a valid integer"

        validateOwner =
            let
                hex =
                    String.trim form.ownerKeyHash
            in
            if String.length hex /= 56 then
                Err "Owner key hash must be 56 hex characters (28 bytes)"

            else
                Ok (VKeyHash (Bytes.fromHexUnchecked hex))

        validateQuestions =
            if List.isEmpty form.questions then
                Err "At least one question is required"

            else
                traverseResults validateQuestion form.questions

        validateRoles =
            let
                enabled =
                    List.filter .enabled form.roleWeightings
            in
            if List.isEmpty enabled then
                Err "At least one role must be enabled"

            else
                Ok (List.map (\rw -> ( rw.role, rw.weightingMode )) enabled)
    in
    Ok SurveyDefinition
        |> resultApply (Ok 1)
        |> resultApply validateOwner
        |> resultApply validateTitle
        |> resultApply validateDescription
        |> resultApply validateQuestions
        |> resultApply validateRoles
        |> resultApply validateEndEpoch


validateQuestion : QuestionForm -> Result String SurveyQuestion
validateQuestion q =
    let
        prompt =
            String.trim q.prompt
    in
    if String.isEmpty prompt then
        Err "Question prompt is required"

    else
        case q.questionType of
            SingleChoiceType ->
                validateOptions q.options
                    |> Result.map (\opts -> SingleChoice { prompt = prompt, options = opts })

            MultiSelectType ->
                Result.map2
                    (\opts maxSel ->
                        MultiSelect { prompt = prompt, options = opts, maxSelections = maxSel }
                    )
                    (validateOptions q.options)
                    (String.toInt q.maxSelections
                        |> Result.fromMaybe "Max selections must be a valid integer"
                    )

            RankingType ->
                Result.map2
                    (\opts maxR ->
                        Ranking { prompt = prompt, options = opts, maxRanked = maxR }
                    )
                    (validateOptions q.options)
                    (String.toInt q.maxRanked
                        |> Result.fromMaybe "Max ranked must be a valid integer"
                    )

            NumericRangeType ->
                Result.map2
                    (\minV maxV ->
                        let
                            stepVal =
                                if String.isEmpty (String.trim q.step) then
                                    Nothing

                                else
                                    String.toInt q.step
                        in
                        NumericRange
                            { prompt = prompt
                            , constraints = { minValue = minV, maxValue = maxV, step = stepVal }
                            }
                    )
                    (String.toInt q.minValue
                        |> Result.fromMaybe "Min value must be a valid integer"
                    )
                    (String.toInt q.maxValue
                        |> Result.fromMaybe "Max value must be a valid integer"
                    )

            CustomType ->
                let
                    uri =
                        String.trim q.schemaUri

                    hash =
                        String.trim q.schemaHash
                in
                if String.isEmpty uri then
                    Err "Schema URI is required for custom questions"

                else if String.length hash /= 64 then
                    Err "Schema hash must be 64 hex characters (32 bytes)"

                else
                    Ok (Custom { prompt = prompt, schemaUri = uri, schemaHash = Bytes.fromHexUnchecked hash })


validateOptions : List String -> Result String (List String)
validateOptions options =
    let
        trimmed =
            List.map String.trim options
                |> List.filter (not << String.isEmpty)
    in
    if List.length trimmed < 2 then
        Err "At least 2 non-empty options are required"

    else
        Ok trimmed



-- ============================================================
-- VIEWS: SURVEY DISPLAY
-- ============================================================


viewSurvey : SurveyDefinition -> Html msg
viewSurvey def =
    div [ HA.class "survey-card" ]
        [ h3 [] [ text def.title ]
        , if not (String.isEmpty def.description) then
            p [ HA.class "survey-desc" ] [ text def.description ]

          else
            text ""
        , p [ HA.class "meta" ]
            [ text ("End epoch: " ++ String.fromInt def.endEpoch)
            , text " | "
            , text ("Version: " ++ String.fromInt def.specVersion)
            ]
        , p [ HA.class "meta" ]
            [ text
                ("Roles: "
                    ++ String.join ", "
                        (List.map
                            (\( r, w ) -> roleToString r ++ " (" ++ weightingModeToString w ++ ")")
                            def.roleWeighting
                        )
                )
            ]
        , p [ HA.class "meta" ]
            [ text ("Owner: " ++ credentialToHex def.owner) ]
        , div [ HA.class "survey-questions" ]
            (List.indexedMap viewQuestionDisplay def.questions)
        ]


credentialToHex : Credential -> String
credentialToHex cred =
    case cred of
        VKeyHash hash ->
            Bytes.toHex (Bytes.toAny hash)

        ScriptHash hash ->
            Bytes.toHex (Bytes.toAny hash)


viewQuestionDisplay : Int -> SurveyQuestion -> Html msg
viewQuestionDisplay idx question =
    div [ HA.class "question-display" ]
        (case question of
            SingleChoice { prompt, options } ->
                [ questionHeader idx "Single choice" prompt
                , viewOptionsDisplay options
                ]

            MultiSelect { prompt, options, maxSelections } ->
                [ questionHeader idx "Multi-select" prompt
                , p [ HA.class "meta" ] [ text ("Max selections: " ++ String.fromInt maxSelections) ]
                , viewOptionsDisplay options
                ]

            Ranking { prompt, options, maxRanked } ->
                [ questionHeader idx "Ranking" prompt
                , p [ HA.class "meta" ] [ text ("Max ranked: " ++ String.fromInt maxRanked) ]
                , viewOptionsDisplay options
                ]

            NumericRange { prompt, constraints } ->
                [ questionHeader idx "Numeric range" prompt
                , p [ HA.class "meta" ]
                    [ text
                        ("Range: "
                            ++ String.fromInt constraints.minValue
                            ++ " to "
                            ++ String.fromInt constraints.maxValue
                            ++ (case constraints.step of
                                    Just s ->
                                        ", step " ++ String.fromInt s

                                    Nothing ->
                                        ""
                               )
                        )
                    ]
                ]

            Custom { prompt, schemaUri } ->
                [ questionHeader idx "Custom" prompt
                , p [ HA.class "meta" ] [ text ("Schema: " ++ schemaUri) ]
                ]
        )


questionHeader : Int -> String -> String -> Html msg
questionHeader idx typeLabel prompt =
    div []
        [ span [ HA.class "badge" ] [ text typeLabel ]
        , span [ HA.class "meta", HA.style "margin-left" "0.5rem" ]
            [ text ("Q" ++ String.fromInt (idx + 1)) ]
        , p [ HA.style "margin" "0.25rem 0" ] [ text prompt ]
        ]


viewOptionsDisplay : List String -> Html msg
viewOptionsDisplay options =
    div [ HA.class "options-list" ]
        (List.indexedMap
            (\i opt ->
                div [ HA.class "option-item" ]
                    [ span [ HA.class "option-index" ] [ text (String.fromInt i ++ ".") ]
                    , text (" " ++ opt)
                    ]
            )
            options
        )



-- ============================================================
-- VIEWS: SURVEY CREATION FORM
-- ============================================================


viewSurveyForm : SurveyForm -> Maybe String -> String -> (FormMsg -> msg) -> Html msg
viewSurveyForm form validationError submitLabel toMsg =
    div [ HA.class "survey-form" ]
        [ h3 [] [ text "Create Survey" ]
        , div [ HA.class "form-group" ]
            [ label [] [ text "Title" ]
            , input
                [ HA.type_ "text"
                , HA.value form.title
                , HA.placeholder "Survey title"
                , HE.onInput (toMsg << SetTitle)
                ]
                []
            ]
        , div [ HA.class "form-group" ]
            [ label [] [ text "Description" ]
            , textarea
                [ HA.value form.description
                , HA.placeholder "Survey description or rationale"
                , HA.rows 3
                , HE.onInput (toMsg << SetDescription)
                ]
                []
            ]
        , div [ HA.class "form-group" ]
            [ label [] [ text "Questions" ]
            , div [ HA.class "questions-list" ]
                (List.indexedMap (viewQuestionForm toMsg) form.questions
                    ++ [ button
                            [ HA.class "btn btn-secondary"
                            , HE.onClick (toMsg AddQuestion)
                            ]
                            [ text "+ Add question" ]
                       ]
                )
            ]
        , div [ HA.class "form-group" ]
            [ label [] [ text "Eligible roles" ]
            , div [ HA.class "roles-list" ]
                (List.map (viewRoleWeighting toMsg) form.roleWeightings)
            ]
        , div [ HA.class "form-row" ]
            [ div [ HA.class "form-group" ]
                [ label [] [ text "End epoch" ]
                , input
                    [ HA.type_ "number"
                    , HA.value form.endEpoch
                    , HA.placeholder "e.g. 504"
                    , HE.onInput (toMsg << SetEndEpoch)
                    ]
                    []
                ]
            , div [ HA.class "form-group" ]
                [ label [] [ text "Owner credential (key hash, hex)" ]
                , input
                    [ HA.type_ "text"
                    , HA.value form.ownerKeyHash
                    , HA.placeholder "56 hex characters"
                    , HA.style "font-family" "monospace"
                    , HE.onInput (toMsg << SetOwnerKeyHash)
                    ]
                    []
                ]
            ]
        , case validationError of
            Just err ->
                p [ HA.class "error" ] [ text err ]

            Nothing ->
                text ""
        , button
            [ HA.class "btn btn-primary"
            , HE.onClick (toMsg SubmitSurvey)
            ]
            [ text submitLabel ]
        ]


viewQuestionForm : (FormMsg -> msg) -> Int -> QuestionForm -> Html msg
viewQuestionForm toMsg idx q =
    div [ HA.class "question-card" ]
        [ div [ HA.class "question-header" ]
            [ span [ HA.class "badge" ] [ text ("Q" ++ String.fromInt (idx + 1)) ]
            , select
                [ HA.value (questionTypeToValue q.questionType)
                , HE.onInput (toMsg << SetQuestionType idx)
                ]
                (List.map
                    (\qt ->
                        option
                            [ HA.value (questionTypeToValue qt) ]
                            [ text (questionTypeToString qt) ]
                    )
                    allQuestionTypes
                )
            , button
                [ HA.class "btn btn-danger btn-sm"
                , HE.onClick (toMsg (RemoveQuestion idx))
                ]
                [ text "Remove" ]
            ]
        , div [ HA.class "form-group" ]
            [ label [] [ text "Prompt" ]
            , textarea
                [ HA.value q.prompt
                , HA.placeholder "Question prompt"
                , HA.rows 2
                , HE.onInput (toMsg << SetPrompt idx)
                ]
                []
            ]
        , viewQuestionTypeFields toMsg idx q
        ]


viewQuestionTypeFields : (FormMsg -> msg) -> Int -> QuestionForm -> Html msg
viewQuestionTypeFields toMsg idx q =
    case q.questionType of
        SingleChoiceType ->
            viewOptionsEditor toMsg idx q.options

        MultiSelectType ->
            div []
                [ viewOptionsEditor toMsg idx q.options
                , div [ HA.class "form-group" ]
                    [ label [] [ text "Max selections" ]
                    , input
                        [ HA.type_ "number"
                        , HA.value q.maxSelections
                        , HA.min "1"
                        , HE.onInput (toMsg << SetMaxSelections idx)
                        ]
                        []
                    ]
                ]

        RankingType ->
            div []
                [ viewOptionsEditor toMsg idx q.options
                , div [ HA.class "form-group" ]
                    [ label [] [ text "Max ranked" ]
                    , input
                        [ HA.type_ "number"
                        , HA.value q.maxRanked
                        , HA.min "1"
                        , HE.onInput (toMsg << SetMaxRanked idx)
                        ]
                        []
                    ]
                ]

        NumericRangeType ->
            div [ HA.class "form-row" ]
                [ div [ HA.class "form-group" ]
                    [ label [] [ text "Min value" ]
                    , input
                        [ HA.type_ "number"
                        , HA.value q.minValue
                        , HE.onInput (toMsg << SetMinValue idx)
                        ]
                        []
                    ]
                , div [ HA.class "form-group" ]
                    [ label [] [ text "Max value" ]
                    , input
                        [ HA.type_ "number"
                        , HA.value q.maxValue
                        , HE.onInput (toMsg << SetMaxValue idx)
                        ]
                        []
                    ]
                , div [ HA.class "form-group" ]
                    [ label [] [ text "Step (optional)" ]
                    , input
                        [ HA.type_ "number"
                        , HA.value q.step
                        , HA.placeholder "e.g. 5"
                        , HE.onInput (toMsg << SetStep idx)
                        ]
                        []
                    ]
                ]

        CustomType ->
            div []
                [ div [ HA.class "form-group" ]
                    [ label [] [ text "Schema URI" ]
                    , input
                        [ HA.type_ "text"
                        , HA.value q.schemaUri
                        , HA.placeholder "https://..."
                        , HE.onInput (toMsg << SetSchemaUri idx)
                        ]
                        []
                    ]
                , div [ HA.class "form-group" ]
                    [ label [] [ text "Schema hash (blake2b-256, hex)" ]
                    , input
                        [ HA.type_ "text"
                        , HA.value q.schemaHash
                        , HA.placeholder "64 hex characters"
                        , HA.style "font-family" "monospace"
                        , HE.onInput (toMsg << SetSchemaHash idx)
                        ]
                        []
                    ]
                ]


viewOptionsEditor : (FormMsg -> msg) -> Int -> List String -> Html msg
viewOptionsEditor toMsg qIdx options =
    div [ HA.class "form-group" ]
        [ label [] [ text "Options" ]
        , div [ HA.class "options-editor" ]
            (List.indexedMap
                (\oIdx opt ->
                    div [ HA.class "option-row" ]
                        [ span [ HA.class "option-index" ] [ text (String.fromInt oIdx ++ ".") ]
                        , input
                            [ HA.type_ "text"
                            , HA.value opt
                            , HA.placeholder ("Option " ++ String.fromInt oIdx)
                            , HE.onInput (toMsg << SetOption qIdx oIdx)
                            ]
                            []
                        , if List.length options > 2 then
                            button
                                [ HA.class "btn btn-danger btn-sm"
                                , HE.onClick (toMsg (RemoveOption qIdx oIdx))
                                ]
                                [ text "x" ]

                          else
                            text ""
                        ]
                )
                options
                ++ [ button
                        [ HA.class "btn btn-secondary btn-sm"
                        , HE.onClick (toMsg (AddOption qIdx))
                        ]
                        [ text "+ Add option" ]
                   ]
            )
        ]


viewRoleWeighting : (FormMsg -> msg) -> RoleWeightingEntry -> Html msg
viewRoleWeighting toMsg rw =
    div [ HA.class "role-row" ]
        [ label [ HA.class "role-toggle" ]
            [ input
                [ HA.type_ "checkbox"
                , HA.checked rw.enabled
                , HE.onClick (toMsg (ToggleRole rw.role))
                ]
                []
            , text (" " ++ roleToString rw.role)
            ]
        , if rw.enabled then
            select
                [ HA.value (weightingModeToValue rw.weightingMode)
                , HE.onInput (toMsg << SetWeighting rw.role)
                ]
                (List.map
                    (\wm ->
                        option
                            [ HA.value (weightingModeToValue wm) ]
                            [ text (weightingModeToString wm) ]
                    )
                    (allowedWeightings rw.role)
                )

          else
            text ""
        ]



-- ============================================================
-- RESPONSE FORM TYPES
-- ============================================================


type AnswerForm
    = SingleChoiceForm (Maybe Int)
    | MultiSelectForm (List Int)
    | RankingForm (List Int)
    | NumericForm String
    | CustomForm String


type alias ResponseForm =
    { role : Maybe Role
    , answers : List AnswerForm
    }


type ResponseFormMsg
    = SetResponseRole String
    | SelectSingleChoice Int Int
    | ToggleMultiSelect Int Int
    | AddToRanking Int Int
    | RemoveFromRanking Int Int
    | SetNumericAnswer Int String
    | SetCustomAnswer Int String
    | SubmitResponse



-- ============================================================
-- RESPONSE FORM LOGIC
-- ============================================================


initResponseForm : SurveyDefinition -> ResponseForm
initResponseForm def =
    { role =
        case def.roleWeighting of
            ( r, _ ) :: _ ->
                Just r

            [] ->
                Nothing
    , answers = List.map initAnswerForm def.questions
    }


initAnswerForm : SurveyQuestion -> AnswerForm
initAnswerForm q =
    case q of
        SingleChoice _ ->
            SingleChoiceForm Nothing

        MultiSelect _ ->
            MultiSelectForm []

        Ranking _ ->
            RankingForm []

        NumericRange { constraints } ->
            NumericForm (String.fromInt constraints.minValue)

        Custom _ ->
            CustomForm ""


updateResponseForm : ResponseFormMsg -> ResponseForm -> ResponseForm
updateResponseForm msg form =
    case msg of
        SetResponseRole roleStr ->
            { form | role = stringToRole roleStr }

        SelectSingleChoice qIdx optIdx ->
            { form
                | answers =
                    updateAt qIdx (\_ -> SingleChoiceForm (Just optIdx)) form.answers
            }

        ToggleMultiSelect qIdx optIdx ->
            { form
                | answers =
                    updateAt qIdx
                        (\a ->
                            case a of
                                MultiSelectForm selected ->
                                    if List.member optIdx selected then
                                        MultiSelectForm (List.filter (\i -> i /= optIdx) selected)

                                    else
                                        MultiSelectForm (selected ++ [ optIdx ])

                                _ ->
                                    a
                        )
                        form.answers
            }

        AddToRanking qIdx optIdx ->
            { form
                | answers =
                    updateAt qIdx
                        (\a ->
                            case a of
                                RankingForm ranked ->
                                    if List.member optIdx ranked then
                                        RankingForm ranked

                                    else
                                        RankingForm (ranked ++ [ optIdx ])

                                _ ->
                                    a
                        )
                        form.answers
            }

        RemoveFromRanking qIdx position ->
            { form
                | answers =
                    updateAt qIdx
                        (\a ->
                            case a of
                                RankingForm ranked ->
                                    RankingForm (List.Extra.removeAt position ranked)

                                _ ->
                                    a
                        )
                        form.answers
            }

        SetNumericAnswer qIdx valStr ->
            { form
                | answers =
                    updateAt qIdx (\_ -> NumericForm valStr) form.answers
            }

        SetCustomAnswer qIdx valStr ->
            { form
                | answers =
                    updateAt qIdx (\_ -> CustomForm valStr) form.answers
            }

        SubmitResponse ->
            form


stringToRole : String -> Maybe Role
stringToRole s =
    case s of
        "DRep" ->
            Just DRep

        "SPO" ->
            Just SPO

        "CC" ->
            Just CC

        "Stakeholder" ->
            Just Stakeholder

        _ ->
            Nothing



-- ============================================================
-- RESPONSE ENCODING
-- ============================================================


buildResponseMetadatum :
    { txHash : String, index : Int }
    -> Credential
    -> SurveyDefinition
    -> ResponseForm
    -> Result String Metadatum
buildResponseMetadatum surveyRef responder def form =
    case form.role of
        Nothing ->
            Err "Please select a role"

        Just role ->
            let
                indexedAnswers =
                    List.indexedMap Tuple.pair form.answers

                encodedAnswers =
                    List.filterMap encodeAnswerForm indexedAnswers
            in
            if List.isEmpty encodedAnswers then
                Err "Please answer at least one question"

            else
                Ok
                    (List
                        [ metaInt 1
                        , List
                            [ List
                                [ List [ metaBytes (Bytes.fromHexUnchecked surveyRef.txHash), metaInt surveyRef.index ]
                                , metaInt (roleToInt role)
                                , credentialToMeta responder
                                , List encodedAnswers
                                ]
                            ]
                        ]
                    )


buildCancellationMetadatum : SurveyRef -> Metadatum
buildCancellationMetadatum ref =
    List
        [ metaInt 2
        , List
            [ List [ metaBytes (Bytes.fromHexUnchecked ref.txHash), metaInt ref.index ]
            ]
        ]


encodeAnswerForm : ( Int, AnswerForm ) -> Maybe Metadatum
encodeAnswerForm ( qIdx, answerForm ) =
    case answerForm of
        SingleChoiceForm (Just optIdx) ->
            Just (List [ metaInt 0, metaInt qIdx, metaInt optIdx ])

        SingleChoiceForm Nothing ->
            Nothing

        MultiSelectForm selected ->
            if List.isEmpty selected then
                Nothing

            else
                Just (List [ metaInt 1, metaInt qIdx, List (List.map metaInt selected) ])

        RankingForm ranked ->
            if List.isEmpty ranked then
                Nothing

            else
                Just (List [ metaInt 2, metaInt qIdx, List (List.map metaInt ranked) ])

        NumericForm valStr ->
            case String.toInt valStr of
                Just v ->
                    Just (List [ metaInt 3, metaInt qIdx, metaInt v ])

                Nothing ->
                    Nothing

        CustomForm s ->
            if String.isEmpty (String.trim s) then
                Nothing

            else
                Just (List [ metaInt 4, metaInt qIdx, metaStr s ])



-- ============================================================
-- VIEWS: RESPONSE FORM
-- ============================================================


viewResponseForm : SurveyDefinition -> ResponseForm -> Maybe String -> String -> (ResponseFormMsg -> msg) -> Html msg
viewResponseForm def form validationError submitLabel toMsg =
    div [ HA.class "survey-form" ]
        [ h3 [] [ text "Respond to Survey" ]
        , viewSurvey def
        , div [ HA.class "form-group" ]
            [ label [] [ text "Your role" ]
            , select
                [ HA.value (Maybe.map roleToString form.role |> Maybe.withDefault "")
                , HE.onInput (toMsg << SetResponseRole)
                ]
                (option [ HA.value "" ] [ text "-- Select role --" ]
                    :: List.map
                        (\( r, _ ) ->
                            option [ HA.value (roleToString r) ] [ text (roleToString r) ]
                        )
                        def.roleWeighting
                )
            ]
        , div [ HA.class "survey-questions" ]
            (List.indexedMap
                (\qIdx ( question, answer ) ->
                    viewAnswerInput toMsg qIdx question answer
                )
                (List.map2 Tuple.pair def.questions form.answers)
            )
        , case validationError of
            Just err ->
                p [ HA.class "error" ] [ text err ]

            Nothing ->
                text ""
        , button
            [ HA.class "btn btn-primary"
            , HE.onClick (toMsg SubmitResponse)
            ]
            [ text submitLabel ]
        ]


viewAnswerInput : (ResponseFormMsg -> msg) -> Int -> SurveyQuestion -> AnswerForm -> Html msg
viewAnswerInput toMsg qIdx question answer =
    div [ HA.class "question-card" ]
        (case ( question, answer ) of
            ( SingleChoice { prompt, options }, SingleChoiceForm selected ) ->
                [ questionHeader qIdx "Single choice" prompt
                , viewSingleChoiceInput toMsg qIdx selected options
                ]

            ( MultiSelect { prompt, options, maxSelections }, MultiSelectForm selected ) ->
                [ questionHeader qIdx "Multi-select" prompt
                , p [ HA.class "meta" ] [ text ("Select up to " ++ String.fromInt maxSelections) ]
                , viewMultiSelectInput toMsg qIdx selected options
                ]

            ( Ranking { prompt, options, maxRanked }, RankingForm ranked ) ->
                [ questionHeader qIdx "Ranking" prompt
                , p [ HA.class "meta" ] [ text ("Rank up to " ++ String.fromInt maxRanked) ]
                , viewRankingInput toMsg qIdx ranked options
                ]

            ( NumericRange { prompt, constraints }, NumericForm value ) ->
                [ questionHeader qIdx "Numeric range" prompt
                , p [ HA.class "meta" ]
                    [ text
                        ("Range: "
                            ++ String.fromInt constraints.minValue
                            ++ " to "
                            ++ String.fromInt constraints.maxValue
                            ++ (case constraints.step of
                                    Just s ->
                                        ", step " ++ String.fromInt s

                                    Nothing ->
                                        ""
                               )
                        )
                    ]
                , viewNumericInput toMsg qIdx value constraints
                ]

            ( Custom { prompt }, CustomForm value ) ->
                [ questionHeader qIdx "Custom" prompt
                , textarea
                    [ HA.value value
                    , HA.rows 3
                    , HA.placeholder "Your answer..."
                    , HE.onInput (toMsg << SetCustomAnswer qIdx)
                    ]
                    []
                ]

            _ ->
                [ text "" ]
        )


viewSingleChoiceInput : (ResponseFormMsg -> msg) -> Int -> Maybe Int -> List String -> Html msg
viewSingleChoiceInput toMsg qIdx selected options =
    div []
        (List.indexedMap
            (\oIdx opt ->
                label [ HA.style "display" "block", HA.style "cursor" "pointer" ]
                    [ input
                        [ HA.type_ "radio"
                        , HA.name ("q" ++ String.fromInt qIdx)
                        , HA.checked (selected == Just oIdx)
                        , HE.onClick (toMsg (SelectSingleChoice qIdx oIdx))
                        ]
                        []
                    , text (" " ++ opt)
                    ]
            )
            options
        )


viewMultiSelectInput : (ResponseFormMsg -> msg) -> Int -> List Int -> List String -> Html msg
viewMultiSelectInput toMsg qIdx selected options =
    div []
        (List.indexedMap
            (\oIdx opt ->
                label [ HA.style "display" "block", HA.style "cursor" "pointer" ]
                    [ input
                        [ HA.type_ "checkbox"
                        , HA.checked (List.member oIdx selected)
                        , HE.onClick (toMsg (ToggleMultiSelect qIdx oIdx))
                        ]
                        []
                    , text (" " ++ opt)
                    ]
            )
            options
        )


viewRankingInput : (ResponseFormMsg -> msg) -> Int -> List Int -> List String -> Html msg
viewRankingInput toMsg qIdx ranked options =
    div []
        [ if not (List.isEmpty ranked) then
            div []
                [ p [ HA.class "meta" ] [ text "Current ranking:" ]
                , div []
                    (List.indexedMap
                        (\pos optIdx ->
                            div [ HA.style "display" "flex", HA.style "align-items" "center", HA.style "gap" "0.5rem", HA.style "margin" "0.15rem 0" ]
                                [ span [] [ text (String.fromInt (pos + 1) ++ ". " ++ (List.Extra.getAt optIdx options |> Maybe.withDefault "?")) ]
                                , button
                                    [ HA.class "btn btn-danger btn-sm"
                                    , HE.onClick (toMsg (RemoveFromRanking qIdx pos))
                                    ]
                                    [ text "x" ]
                                ]
                        )
                        ranked
                    )
                ]

          else
            text ""
        , div [ HA.style "margin-top" "0.25rem" ]
            (List.indexedMap
                (\oIdx opt ->
                    if not (List.member oIdx ranked) then
                        button
                            [ HA.class "btn btn-sm"
                            , HA.style "margin" "0.15rem"
                            , HE.onClick (toMsg (AddToRanking qIdx oIdx))
                            ]
                            [ text ("+ " ++ opt) ]

                    else
                        text ""
                )
                options
            )
        ]


viewNumericInput : (ResponseFormMsg -> msg) -> Int -> String -> NumericConstraints -> Html msg
viewNumericInput toMsg qIdx value constraints =
    input
        [ HA.type_ "number"
        , HA.value value
        , HA.min (String.fromInt constraints.minValue)
        , HA.max (String.fromInt constraints.maxValue)
        , case constraints.step of
            Just s ->
                HA.step (String.fromInt s)

            Nothing ->
                HA.class ""
        , HE.onInput (toMsg << SetNumericAnswer qIdx)
        ]
        []



-- ============================================================
-- VIEWS: RESPONSE DISPLAY
-- ============================================================


viewResponseCard : Maybe SurveyDefinition -> SurveyResponse -> Html msg
viewResponseCard maybeDef response =
    div [ HA.class "survey-card" ]
        [ p [ HA.class "meta" ]
            [ text ("Responder: " ++ credentialToHex response.responder) ]
        , p [ HA.class "meta" ]
            [ text ("Role: " ++ roleToString response.role) ]
        , div [ HA.class "survey-questions" ]
            (List.map (viewAnswerItemDisplay maybeDef) response.answers)
        ]


viewAnswerItemDisplay : Maybe SurveyDefinition -> AnswerItem -> Html msg
viewAnswerItemDisplay maybeDef item =
    let
        getQuestion qIdx =
            maybeDef |> Maybe.andThen (\def -> List.Extra.getAt qIdx def.questions)

        getPrompt qIdx =
            getQuestion qIdx
                |> Maybe.map questionPrompt
                |> Maybe.withDefault ("Question " ++ String.fromInt qIdx)

        getOption qIdx optIdx =
            getQuestion qIdx
                |> Maybe.andThen (questionOptions >> List.Extra.getAt optIdx)
                |> Maybe.withDefault (String.fromInt optIdx)
    in
    case item of
        AnswerSingleChoice qIdx optIdx ->
            div [ HA.class "question-display" ]
                [ p [] [ text (getPrompt qIdx) ]
                , p [ HA.class "meta" ] [ text ("Answer: " ++ getOption qIdx optIdx) ]
                ]

        AnswerMultiSelect qIdx selected ->
            div [ HA.class "question-display" ]
                [ p [] [ text (getPrompt qIdx) ]
                , p [ HA.class "meta" ]
                    [ text ("Selected: " ++ String.join ", " (List.map (getOption qIdx) selected)) ]
                ]

        AnswerRanking qIdx ranked ->
            div [ HA.class "question-display" ]
                [ p [] [ text (getPrompt qIdx) ]
                , div [ HA.class "meta" ]
                    (List.indexedMap
                        (\pos optIdx ->
                            p [] [ text (String.fromInt (pos + 1) ++ ". " ++ getOption qIdx optIdx) ]
                        )
                        ranked
                    )
                ]

        AnswerNumeric qIdx value ->
            div [ HA.class "question-display" ]
                [ p [] [ text (getPrompt qIdx) ]
                , p [ HA.class "meta" ] [ text ("Value: " ++ String.fromInt value) ]
                ]

        AnswerCustom qIdx _ ->
            div [ HA.class "question-display" ]
                [ p [] [ text (getPrompt qIdx) ]
                , p [ HA.class "meta" ] [ text "(Custom answer)" ]
                ]


questionPrompt : SurveyQuestion -> String
questionPrompt q =
    case q of
        SingleChoice { prompt } ->
            prompt

        MultiSelect { prompt } ->
            prompt

        Ranking { prompt } ->
            prompt

        NumericRange { prompt } ->
            prompt

        Custom { prompt } ->
            prompt


questionOptions : SurveyQuestion -> List String
questionOptions q =
    case q of
        SingleChoice { options } ->
            options

        MultiSelect { options } ->
            options

        Ranking { options } ->
            options

        _ ->
            []
