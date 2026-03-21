import json
import unittest
from pathlib import Path
from unittest.mock import AsyncMock, patch

import jsonschema
from fastapi.testclient import TestClient
from referencing import Registry, Resource

import server
import survey_metadata


FIXTURES_DIR = (
    Path(__file__).resolve().parents[3] / "CIP-0179" / "examples"
)
SCHEMAS_DIR = Path(__file__).resolve().parents[3] / "CIP-0179" / "schemas"


def load_example(name: str):
    with open(FIXTURES_DIR / name, "r", encoding="utf-8") as handle:
        return json.load(handle)


def load_schema(name: str):
    with open(SCHEMAS_DIR / name, "r", encoding="utf-8") as handle:
        return json.load(handle)


def validate_against_response_schema(instance):
    response_schema_path = SCHEMAS_DIR / "survey-response.schema.json"
    common_schema_path = SCHEMAS_DIR / "common.schema.json"
    response_schema = {
        **load_schema("survey-response.schema.json"),
        "$id": response_schema_path.as_uri(),
    }
    common_schema = {
        **load_schema("common.schema.json"),
        "$id": common_schema_path.as_uri(),
    }
    registry = (
        Registry()
        .with_resource(
            response_schema["$id"], Resource.from_contents(response_schema)
        )
        .with_resource(common_schema["$id"], Resource.from_contents(common_schema))
    )
    jsonschema.Draft202012Validator(
        response_schema,
        registry=registry,
    ).validate(instance)


class SurveyMetadataTests(unittest.TestCase):
    def test_parse_governance_survey_link_reads_top_level_survey_tx_id(self):
        payload = load_example("governance-action-anchor-survey-link.json")

        parsed = survey_metadata.parse_governance_survey_link(payload)

        self.assertEqual(parsed["kind"], "cardano-governance-survey-link")
        self.assertEqual(parsed["specVersion"], "1.0.0")
        self.assertEqual(
            parsed["surveyTxId"],
            "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
        )

    def test_validate_linked_survey_accepts_valid_example(self):
        survey_details = survey_metadata.extract_survey_details(
            load_example("survey-mixed-role-shared.json")
        )

        payload = survey_metadata.validate_linked_survey(
            spec_version="1.0.0",
            kind="cardano-governance-survey-link",
            survey_tx_id="1515151515151515151515151515151515151515151515151515151515151515",
            survey_details=survey_details,
            proposal_type="InfoAction",
            linked_action_id={
                "txId": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                "govActionIx": 0,
            },
            action_end_epoch=523,
        )

        self.assertTrue(payload["linkValidation"]["valid"])
        self.assertTrue(payload["surveyDetailsValidation"]["valid"])
        self.assertEqual(
            payload["linkValidation"]["linkedRoleWeighting"],
            {
                "DRep": "StakeBased",
                "SPO": "PledgeBased",
            },
        )

    def test_validate_linked_survey_rejects_end_epoch_mismatch(self):
        survey_details = survey_metadata.extract_survey_details(
            load_example("survey-single-choice.json")
        )

        payload = survey_metadata.validate_linked_survey(
            spec_version="1.0.0",
            kind="cardano-governance-survey-link",
            survey_tx_id="bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
            survey_details=survey_details,
            proposal_type="InfoAction",
            linked_action_id={
                "txId": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                "govActionIx": 0,
            },
            action_end_epoch=505,
        )

        self.assertFalse(payload["surveyDetailsValidation"]["valid"])
        self.assertIn(
            "surveyDetails.endEpoch must exactly match the governance action expiration epoch.",
            payload["surveyDetailsValidation"]["errors"],
        )

    def test_build_survey_response_normalizes_builtin_answers(self):
        survey_details = survey_metadata.extract_survey_details(
            load_example("survey-multi-question-mixed-type.json")
        )

        result = survey_metadata.build_survey_response(
            survey_details=survey_details,
            linked_role_weighting={"DRep": "StakeBased"},
            survey_tx_id="abababababababababababababababababababababababababababababababab",
            responder_role="DRep",
            answers=[
                {"questionId": "cip_shortlist", "selection": [0, 2]},
                {"questionId": "release_timing", "selection": [1]},
                {"questionId": "block_budget", "numericValue": 6},
            ],
        )

        self.assertTrue(result["valid"])
        self.assertEqual(
            result["surveyResponse"],
            {
                "specVersion": "1.0.0",
                "surveyTxId": "abababababababababababababababababababababababababababababababab",
                "responderRole": "DRep",
                "answers": [
                    {"questionId": "cip_shortlist", "selection": [0, 2]},
                    {"questionId": "release_timing", "selection": [1]},
                    {"questionId": "block_budget", "numericValue": 6},
                ],
            },
        )

    def test_build_survey_response_lowercases_tx_id_and_matches_schema(self):
        survey_details = survey_metadata.extract_survey_details(
            load_example("survey-single-choice.json")
        )

        result = survey_metadata.build_survey_response(
            survey_details=survey_details,
            linked_role_weighting={"DRep": "StakeBased"},
            survey_tx_id="ABABABABABABABABABABABABABABABABABABABABABABABABABABABABABABABAB",
            responder_role="DRep",
            answers=[{"questionId": "cip_0136_inclusion", "selection": [0]}],
        )

        self.assertTrue(result["valid"])

        metadata = {
            "17": survey_metadata.build_survey_response_metadata(result["surveyResponse"])
        }
        validate_against_response_schema(metadata)

        self.assertEqual(
            result["surveyResponse"]["surveyTxId"],
            "abababababababababababababababababababababababababababababababab",
        )

    def test_build_survey_response_rejects_custom_method_answers(self):
        survey_details = survey_metadata.extract_survey_details(
            load_example("survey-custom-method.json")
        )

        result = survey_metadata.build_survey_response(
            survey_details=survey_details,
            linked_role_weighting={"DRep": "StakeBased"},
            survey_tx_id="dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd",
            responder_role="DRep",
            answers=[
                {
                    "questionId": "roadmap_rank",
                    "customValue": {"focus": "budget"},
                }
            ],
        )

        self.assertFalse(result["valid"])
        self.assertIn("not yet supported", result["errors"][0])

    def test_build_survey_response_rejects_invalid_survey_tx_id_format(self):
        survey_details = survey_metadata.extract_survey_details(
            load_example("survey-single-choice.json")
        )

        result = survey_metadata.build_survey_response(
            survey_details=survey_details,
            linked_role_weighting={"DRep": "StakeBased"},
            survey_tx_id="not-a-tx-id",
            responder_role="DRep",
            answers=[{"questionId": "gov_priority", "selection": [0]}],
        )

        self.assertFalse(result["valid"])
        self.assertIn(
            "surveyResponse.surveyTxId does not match the linked survey.",
            result["errors"],
        )

    def test_build_survey_response_rejects_extra_answer_keys(self):
        survey_details = survey_metadata.extract_survey_details(
            load_example("survey-single-choice.json")
        )

        result = survey_metadata.build_survey_response(
            survey_details=survey_details,
            linked_role_weighting={"DRep": "StakeBased"},
            survey_tx_id="bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
            responder_role="DRep",
            answers=[
                {
                    "questionId": "gov_priority",
                    "selection": [0],
                    "unexpected": True,
                }
            ],
        )

        self.assertFalse(result["valid"])
        self.assertIn(
            "Answer 'gov_priority' contains unsupported keys: unexpected.",
            result["errors"],
        )


class SurveyEndpointTests(unittest.TestCase):
    def test_resolve_endpoint_returns_payload(self):
        mocked_payload = {
            "linked": True,
            "surveyTxId": "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
            "linkValidation": {"valid": True, "errors": []},
            "surveyDetails": {"title": "Example"},
            "surveyDetailsValidation": {"valid": True, "errors": []},
        }

        with TestClient(server.app) as client:
            with patch.object(
                server,
                "resolve_linked_proposal",
                new=AsyncMock(return_value=mocked_payload),
            ):
                response = client.post(
                    "/survey/resolve-linked-proposal",
                    json={
                        "networkId": "Testnet",
                        "proposalType": "InfoAction",
                        "linkedActionId": {
                            "txId": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                            "govActionIx": 0,
                        },
                        "actionEndEpoch": 504,
                        "anchorJson": load_example("governance-action-anchor-survey-link.json"),
                    },
                )

        self.assertEqual(response.status_code, 200)
        self.assertEqual(response.json()["surveyTxId"], mocked_payload["surveyTxId"])

    def test_build_endpoint_returns_normalized_survey_response(self):
        survey_details = survey_metadata.extract_survey_details(
            load_example("survey-mixed-role-shared.json")
        )

        with TestClient(server.app) as client:
            with patch.object(
                server,
                "resolve_linked_proposal",
                new=AsyncMock(
                    return_value={
                        "linked": True,
                        "surveyTxId": "1515151515151515151515151515151515151515151515151515151515151515",
                        "linkValidation": {
                            "valid": True,
                            "errors": [],
                            "linkedRoleWeighting": {
                                "DRep": "StakeBased",
                                "SPO": "PledgeBased",
                            },
                        },
                        "surveyDetails": survey_details,
                        "surveyDetailsValidation": {"valid": True, "errors": []},
                    }
                ),
            ):
                response = client.post(
                    "/survey/build-linked-response",
                    json={
                        "networkId": "Testnet",
                        "proposalType": "InfoAction",
                        "linkedActionId": {
                            "txId": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                            "govActionIx": 0,
                        },
                        "actionEndEpoch": 523,
                        "responderRole": "DRep",
                        "surveyTxId": "1515151515151515151515151515151515151515151515151515151515151515",
                        "answers": [{"questionId": "gov_priority", "selection": [0]}],
                        "anchorJson": load_example("governance-action-anchor-survey-link.json"),
                    },
                )

        self.assertEqual(response.status_code, 200)
        payload = response.json()
        self.assertTrue(payload["valid"])
        self.assertEqual(payload["surveyResponse"]["responderRole"], "DRep")
        self.assertEqual(payload["surveyResponse"]["answers"][0]["selection"], [0])


if __name__ == "__main__":
    unittest.main()
