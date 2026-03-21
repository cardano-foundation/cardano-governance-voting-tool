module Survey exposing
    ( BuildLinkedResponseResult
    , LinkedActionId
    , LinkValidation
    , NumericConstraints
    , ProposalSurveyPayload
    , SurveyAnswer
    , SurveyDetails
    , SurveyQuestion
    , SurveyResponse
    , buildLinkedResponseResultDecoder
    , encodeAnswer
    , encodeLinkedActionId
    , proposalSurveyPayloadDecoder
    , surveyResponseMetadata
    )

import Cardano.Metadatum as Metadatum
import Integer
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE


type alias LinkedActionId =
    { txId : String
    , govActionIx : Int
    }


type alias LinkValidation =
    { valid : Bool
    , errors : List String
    , actionEligibility : List String
    , linkedRoleWeighting : List ( String, String )
    , linkedActionId : Maybe LinkedActionId
    }


type alias NumericConstraints =
    { minValue : Int
    , maxValue : Int
    , step : Maybe Int
    }


type alias SurveyQuestion =
    { questionId : String
    , question : String
    , methodType : String
    , options : Maybe (List String)
    , maxSelections : Maybe Int
    , numericConstraints : Maybe NumericConstraints
    , methodSchemaUri : Maybe String
    , methodSchemaHash : Maybe String
    }


type alias SurveyDetails =
    { specVersion : String
    , title : String
    , description : String
    , questions : List SurveyQuestion
    , roleWeighting : List ( String, String )
    , endEpoch : Int
    }


type alias SurveyAnswer =
    { questionId : String
    , selection : Maybe (List Int)
    , numericValue : Maybe Int
    }


type alias SurveyResponse =
    { specVersion : String
    , surveyTxId : String
    , responderRole : String
    , answers : List SurveyAnswer
    }


type alias ProposalSurveyPayload =
    { linked : Bool
    , surveyTxId : Maybe String
    , linkValidation : LinkValidation
    , surveyDetails : Maybe SurveyDetails
    , surveyDetailsValidation : { valid : Bool, errors : List String }
    }


type alias BuildLinkedResponseResult =
    { valid : Bool
    , errors : List String
    , surveyResponse : Maybe SurveyResponse
    }


encodeLinkedActionId : LinkedActionId -> JE.Value
encodeLinkedActionId { txId, govActionIx } =
    JE.object
        [ ( "txId", JE.string txId )
        , ( "govActionIx", JE.int govActionIx )
        ]


encodeAnswer : SurveyAnswer -> JE.Value
encodeAnswer { questionId, selection, numericValue } =
    JE.object <|
        ( "questionId", JE.string questionId )
            :: (case selection of
                    Just indexes ->
                        [ ( "selection", JE.list JE.int indexes ) ]

                    Nothing ->
                        []
               )
            ++ (case numericValue of
                    Just value ->
                        [ ( "numericValue", JE.int value ) ]

                    Nothing ->
                        []
               )


surveyResponseMetadata : SurveyResponse -> Metadatum.Metadatum
surveyResponseMetadata surveyResponse =
    Metadatum.Map
        [ ( Metadatum.String "surveyResponse"
          , Metadatum.Map
                [ ( Metadatum.String "specVersion", Metadatum.String surveyResponse.specVersion )
                , ( Metadatum.String "surveyTxId", Metadatum.String surveyResponse.surveyTxId )
                , ( Metadatum.String "responderRole", Metadatum.String surveyResponse.responderRole )
                , ( Metadatum.String "answers"
                  , Metadatum.List (List.map answerMetadatum surveyResponse.answers)
                  )
                ]
          )
        ]


answerMetadatum : SurveyAnswer -> Metadatum.Metadatum
answerMetadatum answer =
    Metadatum.Map <|
        ( Metadatum.String "questionId", Metadatum.String answer.questionId )
            :: (case answer.selection of
                    Just indexes ->
                        [ ( Metadatum.String "selection"
                          , Metadatum.List
                                (List.map (Integer.fromSafeInt >> Metadatum.Int) indexes)
                          )
                        ]

                    Nothing ->
                        []
               )
            ++ (case answer.numericValue of
                    Just value ->
                        [ ( Metadatum.String "numericValue"
                          , Metadatum.Int (Integer.fromSafeInt value)
                          )
                        ]

                    Nothing ->
                        []
               )


buildLinkedResponseResultDecoder : Decoder BuildLinkedResponseResult
buildLinkedResponseResultDecoder =
    JD.map3 BuildLinkedResponseResult
        (JD.field "valid" JD.bool)
        (JD.field "errors" (JD.list JD.string))
        (JD.field "surveyResponse" (JD.nullable surveyResponseDecoder))


surveyPayloadDecoder : Decoder ProposalSurveyPayload
surveyPayloadDecoder =
    JD.map5 ProposalSurveyPayload
        (JD.field "linked" JD.bool)
        (JD.field "surveyTxId" (JD.nullable JD.string))
        (JD.field "linkValidation" linkValidationDecoder)
        (JD.field "surveyDetails" (JD.nullable surveyDetailsDecoder))
        (JD.field "surveyDetailsValidation" validationDecoder)


proposalSurveyPayloadDecoder : Decoder ProposalSurveyPayload
proposalSurveyPayloadDecoder =
    surveyPayloadDecoder


linkValidationDecoder : Decoder LinkValidation
linkValidationDecoder =
    JD.map5 LinkValidation
        (JD.field "valid" JD.bool)
        (JD.field "errors" (JD.list JD.string))
        (JD.oneOf
            [ JD.field "actionEligibility" (JD.list JD.string)
            , JD.succeed []
            ]
        )
        (JD.oneOf
            [ JD.field "linkedRoleWeighting" roleWeightingDecoder
            , JD.succeed []
            ]
        )
        (JD.field "linkedActionId" (JD.nullable linkedActionIdDecoder))


validationDecoder : Decoder { valid : Bool, errors : List String }
validationDecoder =
    JD.map2 (\valid errors -> { valid = valid, errors = errors })
        (JD.field "valid" JD.bool)
        (JD.field "errors" (JD.list JD.string))


linkedActionIdDecoder : Decoder LinkedActionId
linkedActionIdDecoder =
    JD.map2 LinkedActionId
        (JD.field "txId" JD.string)
        (JD.field "govActionIx" JD.int)


surveyDetailsDecoder : Decoder SurveyDetails
surveyDetailsDecoder =
    JD.map6 SurveyDetails
        (JD.field "specVersion" JD.string)
        (JD.field "title" JD.string)
        (JD.field "description" JD.string)
        (JD.field "questions" (JD.list surveyQuestionDecoder))
        (JD.field "roleWeighting" roleWeightingDecoder)
        (JD.field "endEpoch" JD.int)


surveyQuestionDecoder : Decoder SurveyQuestion
surveyQuestionDecoder =
    JD.map8 SurveyQuestion
        (JD.field "questionId" JD.string)
        (JD.field "question" JD.string)
        (JD.field "methodType" JD.string)
        (JD.maybe (JD.field "options" (JD.list JD.string)))
        (JD.maybe (JD.field "maxSelections" JD.int))
        (JD.maybe (JD.field "numericConstraints" numericConstraintsDecoder))
        (JD.maybe (JD.field "methodSchemaUri" JD.string))
        (JD.maybe (JD.field "methodSchemaHash" JD.string))


numericConstraintsDecoder : Decoder NumericConstraints
numericConstraintsDecoder =
    JD.map3 NumericConstraints
        (JD.field "minValue" JD.int)
        (JD.field "maxValue" JD.int)
        (JD.maybe (JD.field "step" JD.int))


surveyResponseDecoder : Decoder SurveyResponse
surveyResponseDecoder =
    JD.map4 SurveyResponse
        (JD.field "specVersion" JD.string)
        (JD.field "surveyTxId" JD.string)
        (JD.field "responderRole" JD.string)
        (JD.field "answers" (JD.list surveyAnswerDecoder))


surveyAnswerDecoder : Decoder SurveyAnswer
surveyAnswerDecoder =
    JD.map3 SurveyAnswer
        (JD.field "questionId" JD.string)
        (JD.maybe (JD.field "selection" (JD.list JD.int)))
        (JD.maybe (JD.field "numericValue" JD.int))


roleWeightingDecoder : Decoder (List ( String, String ))
roleWeightingDecoder =
    JD.keyValuePairs JD.string
