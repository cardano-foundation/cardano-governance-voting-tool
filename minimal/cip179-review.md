# CIP-179 Review: On-Chain Surveys and Polls

## Issues acknowledged by the CIP authors' design space

### 1. CBOR 64-byte text string limit unaddressed

Cardano transaction metadata constrains text strings to 64 bytes. The CDDL defines `title`, `description`, `question`, and `options` entries as plain `tstr`, but these will frequently exceed 64 bytes in practice. The `msg` field correctly uses `[+ tstr]` (chunked array, CIP-20 style), but no chunking mechanism is defined for the other text fields. The example survey title `"Select any number of candidate CIPs for potential inclusion in the Dijkstra hard fork."` (87 bytes) would be invalid on-chain as a single text metadatum.

### 2. No survey cancellation mechanism

Once a `surveyDetails` transaction is confirmed, the survey is immutable and live until `endEpoch`. If the creator discovers an error (wrong options, wrong `endEpoch`, typo in a question), there is no way to cancel or supersede it. A `cancelSurvey` payload signed by the same key and referencing the original `surveyTxId` would be simple to add and would prevent participants from wasting time on a broken survey.

### 3. CDDL `step: uint` includes zero

The prose says `step` must be a "positive integer," but the CDDL defines it as `uint`, which includes 0. A step of 0 creates a division-by-zero or infinite valid value set. Should be defined as a `pos_uint = uint .gt 0`.

### 4. Partial responses unspecified

The `answers` array must be "non-empty," but it is not specified whether a respondent must answer all questions or may answer a subset. This affects tally denominators per question. The CIP should explicitly state whether partial responses are valid or invalid.

### 5. Redundant `specVersion` in `surveyResponse`

Both `surveyDetails` and `surveyResponse` carry their own `specVersion`. Since a response is always bound to a specific survey via `surveyTxId`, the spec version is already determined by the survey definition. Having an independent `specVersion` on the response introduces an unnecessary compatibility question (what if they differ?) without adding value. Remove it from `surveyResponse`.

### 6. `metadataPosition` in ordering is useless

Duplicate resolution uses `(slot, txIndexInBlock, metadataPosition)`. Since the CIP mandates exactly one of `surveyDetails` or `surveyResponse` per label `17` per transaction, `metadataPosition` is always 0 and serves no purpose as a tiebreaker. It should be dropped.

## Structural and design-level issues

### 7. Strings everywhere instead of integers -- extremely wasteful on-chain

The CDDL uses text strings for nearly every field key and enumerated value. On-chain CBOR metadata is paid for per byte and permanently stored. Encoding overhead is substantial:

| Current (string) | CBOR cost | Integer alternative | CBOR cost |
|:--|:--|:--|:--|
| `"surveyDetails"` key | 14 bytes | e.g. `0` | 1 byte |
| `"surveyResponse"` key | 15 bytes | e.g. `1` | 1 byte |
| `"specVersion"` key | 12 bytes | e.g. `0` | 1 byte |
| `"questionId"` key | 11 bytes | e.g. `0` | 1 byte |
| `"methodType"` key | 11 bytes | e.g. `2` | 1 byte |
| `"urn:cardano:poll-method:single-choice:v1"` value | 43 bytes | e.g. `0` | 1 byte |
| `"CredentialBased"` value | 16 bytes | e.g. `0` | 1 byte |
| `"DRep"` value | 5 bytes | e.g. `0` | 1 byte |

A moderately sized survey with 3 questions and 4 options each could waste hundreds of bytes on string keys and enum labels that could be single-byte integers. Respondents also pay this cost on every response. This is unusual for Cardano metadata standards -- the ledger itself encodes analogous structures (certificates, voting procedures) with compact integer tags.

### 8. Product types with optional fields instead of sum types

The `survey_question` type is a single product (record) with optional fields whose presence depends on `methodType`:

```cddl
survey_question = {
  questionId: tstr,
  question: tstr,
  methodType: method_type,
  ? options: [+ tstr],
  ? maxSelections: uint,
  ? numericConstraints: numeric_constraints,
  ? methodSchemaUri: uri,
  ? methodSchemaHash: hex_blake2b_256
}
```

This requires extensive prose rules to state which fields are "required," "conditional," or "forbidden" for each method type. A sum type (tagged union / choice type) would make the valid shapes self-describing:

```cddl
; Sum type approach -- invalid combinations are unrepresentable
survey_question = single_choice_question
               / multi_select_question
               / numeric_range_question
               / custom_question

single_choice_question = {
  questionId: tstr,
  question: tstr,
  options: [2* tstr]          ; at least 2
}

multi_select_question = {
  questionId: tstr,
  question: tstr,
  options: [2* tstr],
  maxSelections: pos_uint
}

; etc.
```

Similarly, `answer_item` is already modeled as a sum type (three `/` alternatives), which is good -- but the question definition should follow the same pattern. The product-with-optionals approach pushes validation into prose instead of making illegal states unrepresentable in the schema.

### 9. Sybil resistance analysis misunderstands Cardano capabilities

The CIP's security section warns that `CredentialBased` "can be sybil attacked when governance-role validation is not applied" and treats sybil resistance as requiring elaborate chain-state analysis. This underestimates what Cardano already provides.

A response transaction can include the responder's credential in the `required_signers` field of the transaction body. This proves cryptographic ownership of the claimed credential -- the transaction is invalid at the ledger level without the corresponding signature. Combined with role-membership lookups (is this credential a registered DRep / SPO operator / CC member?), identity verification becomes straightforward:

- **required_signers** proves "this credential authorized this transaction"
- **Ledger state query** proves "this credential is a registered DRep/SPO/CC member"

The CIP's complex derivation rules (13 numbered sub-rules for responder identity) conflate what the ledger already guarantees with what tools need to check. The specification would be simpler and more secure if it required `required_signers` to contain the responder credential, rather than relying on indirect derivation from `voting_procedures`, witnesses, and "other chain evidence sources."

### 10. Survey identification should use TxId + index, not just TxId

Surveys are identified solely by `surveyTxId`. This is unusual for Cardano, where analogous on-chain artifacts use a (TxId, index) pair:

- UTxOs: `(TxId, output_index)`
- Governance actions: `(TxId, gov_action_index)`
- Certificates: referenced by `(TxId, cert_index)`

Using just TxId means a transaction can contain at most one survey definition, an arbitrary restriction that provides no benefit. A `(surveyTxId, surveyIndex)` identifier would:

- Follow established Cardano conventions
- Allow batching multiple survey definitions in one transaction (reduces fees)
- Future-proof the scheme without breaking changes

### 11. Restriction to one response per transaction is arbitrary

The CIP mandates exactly one `surveyResponse` per label `17` per transaction. A respondent who wants to answer 5 surveys must submit 5 separate transactions, paying fees 5 times. There is no technical reason for this -- the payload under label `17` could be an array of responses. Combined with issue 10 (TxId+index identification), this restriction unnecessarily inflates on-chain costs for active participants.

## Summary

| Severity | # | Issue |
|:--|:--|:--|
| **High** | 1 | 64-byte text limit makes examples invalid on-chain |
| **High** | 7 | String keys/enums waste hundreds of bytes per survey and per response |
| **High** | 8 | Product-with-optionals instead of sum types pushes validation entirely into prose |
| **Medium** | 9 | Overcomplicated identity derivation when `required_signers` already solves it |
| **Medium** | 10 | TxId-only identification breaks Cardano conventions, prevents batching |
| **Medium** | 11 | One response per Tx is needlessly costly |
| **Medium** | 2 | No cancellation mechanism |
| **Low** | 3 | `step: uint` should be `pos_uint` |
| **Low** | 4 | Partial responses unspecified |
| **Low** | 5 | Redundant `specVersion` in response |
| **Low** | 6 | Useless `metadataPosition` field |

The core concept is useful -- Cardano governance needs structured sentiment polling. But the on-chain encoding is far more expensive than necessary, the data model relies on prose-enforced invariants instead of type-level guarantees, and several design choices diverge from established Cardano idioms without clear justification.
