import json
import re
from typing import Any, Dict, List, Optional

import httpx

SURVEY_SPEC_VERSION = "1.0.0"
SURVEY_LINK_KIND = "cardano-governance-survey-link"
SURVEY_METADATA_LABEL = "17"

BUILTIN_METHODS = {
    "singleChoice": "urn:cardano:poll-method:single-choice:v1",
    "multiSelect": "urn:cardano:poll-method:multi-select:v1",
    "numericRange": "urn:cardano:poll-method:numeric-range:v1",
}

ROLE_WEIGHTING_COMPATIBILITY = {
    "CC": ["CredentialBased"],
    "DRep": ["CredentialBased", "StakeBased"],
    "SPO": ["CredentialBased", "StakeBased", "PledgeBased"],
    "Stakeholder": ["StakeBased"],
}

PROPOSAL_ACTION_ELIGIBILITY = {
    "TreasuryWithdrawals": ["DRep", "CC"],
    "NewConstitution": ["DRep", "CC"],
    "NoConfidence": ["DRep", "SPO", "CC"],
    "NewCommittee": ["DRep", "SPO", "CC"],
    "HardForkInitiation": ["DRep", "SPO", "CC"],
    "ParameterChange": ["DRep", "SPO", "CC"],
    "InfoAction": ["DRep", "SPO", "CC"],
}

HEX_TX_ID_RE = re.compile(r"^[0-9a-fA-F]{64}$")
HEX_BLAKE2B_256_RE = re.compile(r"^[0-9a-fA-F]{64}$")
SURVEY_RESPONSE_ALLOWED_KEYS = {"questionId", "selection", "numericValue", "customValue"}


def empty_survey_payload() -> Dict[str, Any]:
    return {
        "linked": False,
        "surveyTxId": None,
        "linkValidation": {
            "valid": False,
            "errors": ["No survey link found for this proposal."],
        },
        "surveyDetails": None,
        "surveyDetailsValidation": {
            "valid": False,
            "errors": [],
        },
    }


def get_action_eligibility(proposal_type: Optional[str]) -> List[str]:
    if proposal_type in PROPOSAL_ACTION_ELIGIBILITY:
        return list(PROPOSAL_ACTION_ELIGIBILITY[proposal_type])
    return ["DRep", "SPO", "CC"]


def get_koios_base_url(network_id: Any) -> str:
    if network_id in ("Mainnet", "mainnet", 1, "1"):
        return "https://api.koios.rest/api/v1"
    return "https://preview.koios.rest/api/v1"


def parse_anchor_json(anchor_json: Any) -> Dict[str, Any]:
    if isinstance(anchor_json, dict):
        return anchor_json
    if isinstance(anchor_json, str):
        try:
            parsed = json.loads(anchor_json)
            if isinstance(parsed, dict):
                return parsed
        except json.JSONDecodeError:
            return {}
    return {}


def parse_governance_survey_link(anchor_json: Any) -> Dict[str, Optional[str]]:
    anchor = parse_anchor_json(anchor_json)
    survey_tx_id = anchor.get("surveyTxId")
    normalized_survey_tx_id = (
        survey_tx_id.lower()
        if isinstance(survey_tx_id, str) and HEX_TX_ID_RE.fullmatch(survey_tx_id)
        else None
    )
    return {
        "specVersion": anchor.get("specVersion")
        if isinstance(anchor.get("specVersion"), str)
        else None,
        "kind": anchor.get("kind") if isinstance(anchor.get("kind"), str) else None,
        "surveyTxId": normalized_survey_tx_id,
    }


def normalize_metadata_text(value: Any) -> Optional[str]:
    if isinstance(value, str):
        return value
    if isinstance(value, list) and all(isinstance(item, str) for item in value):
        return "".join(value)
    return None


def normalize_survey_question(question: Any) -> Optional[Dict[str, Any]]:
    if not isinstance(question, dict):
        return None

    question_id = normalize_metadata_text(question.get("questionId"))
    prompt = normalize_metadata_text(question.get("question"))
    method_type = normalize_metadata_text(question.get("methodType"))
    if not question_id or not prompt or not method_type:
        return None

    normalized_question: Dict[str, Any] = {
        "questionId": question_id,
        "question": prompt,
        "methodType": method_type,
    }

    options = question.get("options")
    if options is not None:
        if not isinstance(options, list):
            return None
        normalized_options: List[str] = []
        for option in options:
            normalized_option = normalize_metadata_text(option)
            if normalized_option is None:
                return None
            normalized_options.append(normalized_option)
        normalized_question["options"] = normalized_options

    max_selections = question.get("maxSelections")
    if isinstance(max_selections, int) and not isinstance(max_selections, bool):
        normalized_question["maxSelections"] = max_selections

    numeric_constraints = question.get("numericConstraints")
    if numeric_constraints is not None:
        if not isinstance(numeric_constraints, dict):
            return None
        min_value = numeric_constraints.get("minValue")
        max_value = numeric_constraints.get("maxValue")
        step = numeric_constraints.get("step")
        if (
            not isinstance(min_value, int)
            or isinstance(min_value, bool)
            or not isinstance(max_value, int)
            or isinstance(max_value, bool)
        ):
            return None
        if step is not None and (
            not isinstance(step, int) or isinstance(step, bool)
        ):
            return None

        normalized_question["numericConstraints"] = {
            "minValue": min_value,
            "maxValue": max_value,
            **({"step": step} if step is not None else {}),
        }

    method_schema_uri = normalize_metadata_text(question.get("methodSchemaUri"))
    if method_schema_uri:
        normalized_question["methodSchemaUri"] = method_schema_uri

    method_schema_hash = normalize_metadata_text(question.get("methodSchemaHash"))
    if method_schema_hash:
        normalized_question["methodSchemaHash"] = method_schema_hash

    return normalized_question


def normalize_survey_details(survey_details: Any) -> Optional[Dict[str, Any]]:
    if not isinstance(survey_details, dict):
        return None

    spec_version = normalize_metadata_text(survey_details.get("specVersion"))
    title = normalize_metadata_text(survey_details.get("title"))
    description = normalize_metadata_text(survey_details.get("description"))
    if not spec_version or not title or not description:
        return None

    raw_questions = survey_details.get("questions")
    if not isinstance(raw_questions, list):
        return None

    questions: List[Dict[str, Any]] = []
    for question in raw_questions:
        normalized_question = normalize_survey_question(question)
        if not normalized_question:
            return None
        questions.append(normalized_question)

    raw_role_weighting = survey_details.get("roleWeighting")
    if not isinstance(raw_role_weighting, dict):
        return None

    role_weighting: Dict[str, str] = {}
    for role, mode in raw_role_weighting.items():
        if (
            role in ROLE_WEIGHTING_COMPATIBILITY
            and isinstance(mode, str)
            and mode in ROLE_WEIGHTING_COMPATIBILITY[role]
        ):
            role_weighting[role] = mode
        else:
            return None

    end_epoch = survey_details.get("endEpoch")
    if not isinstance(end_epoch, int) or isinstance(end_epoch, bool):
        return None

    return {
        "specVersion": spec_version,
        "title": title,
        "description": description,
        "questions": questions,
        "roleWeighting": role_weighting,
        "endEpoch": end_epoch,
    }


def extract_label_17(metadata: Any) -> Any:
    if isinstance(metadata, list):
        for item in metadata:
            if not isinstance(item, dict):
                continue
            label = (
                item.get("label")
                or item.get("key")
                or item.get("metadata_label")
                or item.get("tx_metadata_label")
            )
            if str(label) != SURVEY_METADATA_LABEL:
                continue
            return (
                item.get("json")
                or item.get("json_metadata")
                or item.get("metadata")
                or item.get("value")
            )
        return None

    if isinstance(metadata, dict):
        if SURVEY_METADATA_LABEL in metadata:
            return metadata[SURVEY_METADATA_LABEL]
        int_key = 17
        if int_key in metadata:
            return metadata[int_key]

    return None


def extract_survey_details(metadata: Any) -> Optional[Dict[str, Any]]:
    label_17 = extract_label_17(metadata)
    if not isinstance(label_17, dict):
        return None
    return normalize_survey_details(label_17.get("surveyDetails"))


def validate_question(question: Dict[str, Any]) -> List[str]:
    errors: List[str] = []
    method_type = question["methodType"]
    is_single_choice = method_type == BUILTIN_METHODS["singleChoice"]
    is_multi_select = method_type == BUILTIN_METHODS["multiSelect"]
    is_numeric_range = method_type == BUILTIN_METHODS["numericRange"]
    is_builtin = is_single_choice or is_multi_select or is_numeric_range

    if not is_builtin:
        if not question.get("methodSchemaUri"):
            errors.append(
                f"Custom method question '{question['questionId']}' must include methodSchemaUri."
            )
        method_schema_hash = question.get("methodSchemaHash")
        if not isinstance(method_schema_hash, str) or not HEX_BLAKE2B_256_RE.fullmatch(
            method_schema_hash
        ):
            errors.append(
                f"Custom method question '{question['questionId']}' must include a 64-char hex methodSchemaHash."
            )
        return errors

    if is_single_choice or is_multi_select:
        options = question.get("options")
        if not isinstance(options, list) or len(options) < 2:
            errors.append(
                f"Question '{question['questionId']}' must include at least two options."
            )

    if is_single_choice:
        max_selections = question.get("maxSelections")
        if max_selections is not None and max_selections != 1:
            errors.append(
                f"Question '{question['questionId']}' single-choice maxSelections must be absent or 1."
            )

    if is_multi_select:
        max_selections = question.get("maxSelections")
        options = question.get("options", [])
        if (
            not isinstance(max_selections, int)
            or isinstance(max_selections, bool)
            or max_selections < 1
            or (isinstance(options, list) and max_selections > len(options))
        ):
            errors.append(
                f"Question '{question['questionId']}' multi-select maxSelections must be between 1 and the number of options."
            )

    if is_numeric_range:
        constraints = question.get("numericConstraints")
        if not isinstance(constraints, dict):
            errors.append(
                f"Question '{question['questionId']}' numeric-range requires numericConstraints."
            )
        else:
            min_value = constraints.get("minValue")
            max_value = constraints.get("maxValue")
            step = constraints.get("step")
            if (
                not isinstance(min_value, int)
                or isinstance(min_value, bool)
                or not isinstance(max_value, int)
                or isinstance(max_value, bool)
                or min_value > max_value
            ):
                errors.append(
                    f"Question '{question['questionId']}' numeric-range requires valid minValue and maxValue."
                )
            if step is not None and (
                not isinstance(step, int) or isinstance(step, bool) or step <= 0
            ):
                errors.append(
                    f"Question '{question['questionId']}' numeric-range step must be a positive integer."
                )

        if question.get("options") is not None or question.get("maxSelections") is not None:
            errors.append(
                f"Question '{question['questionId']}' numeric-range must not define options or maxSelections."
            )

    return errors


def filter_linked_role_weighting(
    role_weighting: Optional[Dict[str, str]], action_eligibility: List[str]
) -> Optional[Dict[str, str]]:
    if not role_weighting:
        return None
    return {
        role: mode
        for role, mode in role_weighting.items()
        if role in action_eligibility
    }


def validate_survey_details(
    survey_details: Optional[Dict[str, Any]],
    action_eligibility: Optional[List[str]] = None,
    action_end_epoch: Optional[int] = None,
) -> Dict[str, Any]:
    errors: List[str] = []
    normalized_survey_details = normalize_survey_details(survey_details)

    if not normalized_survey_details:
        return {
            "valid": False,
            "errors": ["Missing surveyDetails payload."],
        }

    if normalized_survey_details["specVersion"] != SURVEY_SPEC_VERSION:
        errors.append(f"surveyDetails.specVersion must be {SURVEY_SPEC_VERSION}.")

    if not normalized_survey_details["title"] or not normalized_survey_details["description"]:
        errors.append("surveyDetails.title and surveyDetails.description are required.")

    if not normalized_survey_details["questions"]:
        errors.append("surveyDetails.questions must be a non-empty array.")
    else:
        seen_question_ids = set()
        for question in normalized_survey_details["questions"]:
            question_id = question["questionId"]
            if question_id in seen_question_ids:
                errors.append(f"Duplicate questionId '{question_id}' in surveyDetails.questions.")
            seen_question_ids.add(question_id)
            errors.extend(validate_question(question))

    role_weighting = normalized_survey_details.get("roleWeighting")
    if not role_weighting:
        errors.append("surveyDetails.roleWeighting must be a non-empty object.")
    else:
        for role, mode in role_weighting.items():
            if role not in ROLE_WEIGHTING_COMPATIBILITY:
                errors.append(f"Unsupported responder role '{role}' in roleWeighting.")
                continue
            if mode not in ROLE_WEIGHTING_COMPATIBILITY[role]:
                errors.append(f"Role '{role}' cannot use weighting mode '{mode}'.")

    end_epoch = normalized_survey_details.get("endEpoch")
    if not isinstance(end_epoch, int) or isinstance(end_epoch, bool):
        errors.append("surveyDetails.endEpoch is required.")
    elif action_end_epoch is not None and end_epoch != action_end_epoch:
        errors.append(
            "surveyDetails.endEpoch must exactly match the governance action expiration epoch."
        )

    if action_eligibility:
        linked_role_weighting = filter_linked_role_weighting(
            role_weighting, action_eligibility
        )
        if not linked_role_weighting:
            errors.append(
                "Linked survey has no eligible roles after governance action filtering."
            )

    return {
        "valid": len(errors) == 0,
        "errors": errors,
    }


async def fetch_tx_metadata(
    async_client: httpx.AsyncClient, network_id: Any, tx_hash: str
) -> Any:
    response = await async_client.post(
        f"{get_koios_base_url(network_id)}/tx_metadata",
        headers={
            "accept": "application/json",
            "content-type": "application/json",
        },
        json={"_tx_hashes": [tx_hash]},
    )
    response.raise_for_status()
    rows = response.json()
    if not rows:
        return None

    row = rows[0]
    if not isinstance(row, dict):
        return None

    return row.get("json_metadata") or row.get("metadata")


async def fetch_linked_survey_details(
    async_client: httpx.AsyncClient,
    network_id: Any,
    survey_tx_id: str,
) -> Optional[Dict[str, Any]]:
    metadata = await fetch_tx_metadata(async_client, network_id, survey_tx_id)
    return extract_survey_details(metadata)


def validate_linked_survey(
    *,
    spec_version: Optional[str],
    kind: Optional[str],
    survey_tx_id: Optional[str],
    survey_details: Optional[Dict[str, Any]],
    proposal_type: Optional[str],
    linked_action_id: Optional[Dict[str, Any]],
    action_end_epoch: Optional[int],
) -> Dict[str, Any]:
    payload = empty_survey_payload()
    payload["linked"] = survey_tx_id is not None
    payload["surveyTxId"] = survey_tx_id

    action_eligibility = get_action_eligibility(proposal_type)
    link_errors: List[str] = []

    if kind != SURVEY_LINK_KIND:
        link_errors.append(
            f"Anchor metadata kind is not {SURVEY_LINK_KIND}."
        )

    if spec_version != SURVEY_SPEC_VERSION:
        link_errors.append(
            f"Anchor metadata specVersion must be {SURVEY_SPEC_VERSION}."
        )

    if not survey_tx_id:
        link_errors.append("Missing top-level surveyTxId in anchor metadata.")

    if survey_tx_id and not survey_details:
        link_errors.append(
            "Referenced surveyTxId has no label 17 surveyDetails payload."
        )

    survey_details_validation = validate_survey_details(
        survey_details, action_eligibility, action_end_epoch
    )
    payload["surveyDetails"] = survey_details
    payload["surveyDetailsValidation"] = survey_details_validation

    linked_role_weighting = filter_linked_role_weighting(
        survey_details.get("roleWeighting") if survey_details else None,
        action_eligibility,
    )
    if survey_details and not linked_role_weighting:
        link_errors.append(
            "Linked survey has no eligible roles after governance action filtering."
        )

    payload["linkValidation"] = {
        "valid": len(link_errors) == 0,
        "errors": link_errors,
        "actionEligibility": action_eligibility,
        "linkedRoleWeighting": linked_role_weighting,
        "linkedActionId": linked_action_id,
    }
    return payload


async def resolve_linked_proposal(
    async_client: httpx.AsyncClient,
    *,
    network_id: Any,
    proposal_type: Optional[str],
    linked_action_id: Optional[Dict[str, Any]],
    action_end_epoch: Optional[int],
    anchor_json: Any,
) -> Dict[str, Any]:
    parsed_link = parse_governance_survey_link(anchor_json)
    survey_tx_id = parsed_link["surveyTxId"]
    survey_details = None
    if survey_tx_id:
        survey_details = await fetch_linked_survey_details(
            async_client,
            network_id,
            survey_tx_id,
        )

    return validate_linked_survey(
        spec_version=parsed_link["specVersion"],
        kind=parsed_link["kind"],
        survey_tx_id=survey_tx_id,
        survey_details=survey_details,
        proposal_type=proposal_type,
        linked_action_id=linked_action_id,
        action_end_epoch=action_end_epoch,
    )


def validate_answer(
    question: Dict[str, Any], answer: Dict[str, Any]
) -> List[str]:
    errors: List[str] = []
    question_id = question["questionId"]
    method_type = question["methodType"]
    selection = answer.get("selection")
    numeric_value = answer.get("numericValue")

    if method_type == BUILTIN_METHODS["singleChoice"]:
        if (
            not isinstance(selection, list)
            or len(selection) != 1
            or any(not isinstance(entry, int) or isinstance(entry, bool) for entry in selection)
        ):
            errors.append(
                f"Answer '{question_id}' must contain exactly one selected option."
            )
        elif any(entry < 0 or entry >= len(question.get('options', [])) for entry in selection):
            errors.append(f"Answer '{question_id}' contains an invalid option index.")

    elif method_type == BUILTIN_METHODS["multiSelect"]:
        if not isinstance(selection, list) or any(
            not isinstance(entry, int) or isinstance(entry, bool) for entry in selection
        ):
            errors.append(f"Answer '{question_id}' must include selection[].")
        else:
            max_selections = question.get("maxSelections", 0)
            if len(selection) > max_selections:
                errors.append(f"Answer '{question_id}' exceeds maxSelections.")
            if any(entry < 0 or entry >= len(question.get("options", [])) for entry in selection):
                errors.append(f"Answer '{question_id}' contains an invalid option index.")

    elif method_type == BUILTIN_METHODS["numericRange"]:
        constraints = question.get("numericConstraints")
        if (
            not isinstance(constraints, dict)
            or not isinstance(numeric_value, int)
            or isinstance(numeric_value, bool)
        ):
            errors.append(f"Answer '{question_id}' must include numericValue.")
        else:
            min_value = constraints.get("minValue")
            max_value = constraints.get("maxValue")
            step = constraints.get("step")
            if numeric_value < min_value or numeric_value > max_value:
                errors.append(
                    f"Answer '{question_id}' numericValue is outside the allowed range."
                )
            if isinstance(step, int) and (numeric_value - min_value) % step != 0:
                errors.append(
                    f"Answer '{question_id}' numericValue does not satisfy the configured step."
                )

    return errors


def build_survey_response(
    *,
    survey_details: Optional[Dict[str, Any]],
    linked_role_weighting: Optional[Dict[str, str]],
    survey_tx_id: Optional[str],
    responder_role: Optional[str],
    answers: Any,
) -> Dict[str, Any]:
    errors: List[str] = []
    normalized_answers: List[Dict[str, Any]] = []
    normalized_survey_tx_id = (
        survey_tx_id.lower()
        if isinstance(survey_tx_id, str) and HEX_TX_ID_RE.fullmatch(survey_tx_id)
        else None
    )

    if responder_role not in ROLE_WEIGHTING_COMPATIBILITY:
        errors.append(
            "surveyResponse.responderRole must be one of DRep, SPO, CC, or Stakeholder."
        )
    elif not linked_role_weighting or responder_role not in linked_role_weighting:
        errors.append(
            f"surveyResponse.responderRole {responder_role} is not eligible for this linked survey."
        )

    if not normalized_survey_tx_id:
        errors.append("surveyResponse.surveyTxId does not match the linked survey.")

    if not survey_details:
        errors.append("Missing linked surveyDetails for surveyResponse validation.")
        return {
            "valid": False,
            "errors": errors,
            "surveyResponse": None,
        }

    if not isinstance(answers, list) or len(answers) == 0:
        errors.append("surveyResponse.answers must be a non-empty array.")
        return {
            "valid": False,
            "errors": errors,
            "surveyResponse": None,
        }

    question_map = {
        question["questionId"]: question for question in survey_details.get("questions", [])
    }
    seen_question_ids = set()

    for raw_answer in answers:
        if not isinstance(raw_answer, dict):
            errors.append("Each survey answer must be an object.")
            continue

        question_id = raw_answer.get("questionId")
        if not isinstance(question_id, str) or not question_id:
            errors.append("Every survey answer must include questionId.")
            continue

        unexpected_keys = sorted(set(raw_answer.keys()) - SURVEY_RESPONSE_ALLOWED_KEYS)
        if unexpected_keys:
            errors.append(
                f"Answer '{question_id}' contains unsupported keys: {', '.join(unexpected_keys)}."
            )
            continue

        if question_id in seen_question_ids:
            errors.append(f"Duplicate answer questionId '{question_id}'.")
            continue
        seen_question_ids.add(question_id)

        question = question_map.get(question_id)
        if not question:
            errors.append(f"Unknown answer questionId '{question_id}'.")
            continue

        has_selection = "selection" in raw_answer
        has_numeric = "numericValue" in raw_answer
        has_custom = "customValue" in raw_answer
        answer_key_count = len(
            [key for key in [has_selection, has_numeric, has_custom] if key]
        )
        if answer_key_count != 1:
            errors.append(
                f"Answer '{question_id}' must include exactly one of selection, numericValue, or customValue."
            )
            continue

        method_type = question["methodType"]
        if method_type not in BUILTIN_METHODS.values():
            if has_custom:
                errors.append(
                    f"Question '{question_id}' uses a custom survey method that is not yet supported in the Cardano Foundation voting tool."
                )
            else:
                errors.append(
                    f"Question '{question_id}' uses a custom survey method and requires customValue."
                )
            continue

        answer_errors = validate_answer(question, raw_answer)
        if answer_errors:
            errors.extend(answer_errors)
            continue

        normalized_answer: Dict[str, Any] = {"questionId": question_id}
        if has_selection:
            normalized_answer["selection"] = list(raw_answer["selection"])
        elif has_numeric:
            normalized_answer["numericValue"] = raw_answer["numericValue"]
        normalized_answers.append(normalized_answer)

    if errors:
        return {
            "valid": False,
            "errors": errors,
            "surveyResponse": None,
        }

    survey_response = {
        "specVersion": SURVEY_SPEC_VERSION,
        "surveyTxId": normalized_survey_tx_id,
        "responderRole": responder_role,
        "answers": normalized_answers,
    }
    return {
        "valid": True,
        "errors": [],
        "surveyResponse": survey_response,
    }


def build_survey_response_metadata(survey_response: Dict[str, Any]) -> Dict[str, Any]:
    return {"surveyResponse": survey_response}
