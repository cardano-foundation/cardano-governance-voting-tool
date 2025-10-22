// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

// https://typst.app/universe/package/cmarker
#import "@preview/cmarker:0.1.6"

// JSON data
#let data = json("metadata.json")

// Set page layout
#set page(
  paper: "a4",
  margin: (x: 2.2cm, y: 2cm),
)

// Title
#align(center, text(22pt)[
  *Vote Rationale*
])

// Authors (extracted from the 'authors' field of the JSON)
#if "authors" in data and data.authors.len() > 0 [
  #align(center, grid(
    columns: calc.min(2, data.authors.len()),
    row-gutter: 10pt,
    column-gutter: 35pt,
    ..data.authors.map(author => [
      #author.name
    ])
  ))
  #v(1em)
]

#v(1em) // add some vertical space

This document is automatically generated from the CIP-0136 JSON file attached to the vote.

// If the governance action ID is provided, display it.
#if "govActionId" in data.body [
  Governance Action ID: #data.body.govActionId
]

#set par(justify: true)

= Summary

#cmarker.render(data.body.summary)

= Rationale Statement

#cmarker.render(data.body.rationaleStatement)

// Optional Precedent Discussion
#if "precedentDiscussion" in data.body [
  = Precedent Discussion

  #cmarker.render(data.body.precedentDiscussion)
]

// Optional Counter-argument Discussion
#if "counterargumentDiscussion" in data.body [
  = Counter-argument Discussion

  #cmarker.render(data.body.counterargumentDiscussion)
]

// Optional Conclusion
#if "conclusion" in data.body [
  = Conclusion

  #cmarker.render(data.body.conclusion)
]

// Optional Internal Vote
#if "internalVote" in data.body [
  = Internal Vote

  #if "constitutional" in data.body.internalVote [
    - Constitutional: #data.body.internalVote.constitutional
  ]
  #if "unconstitutional" in data.body.internalVote [
    - Unconstitutional: #data.body.internalVote.unconstitutional
  ]
  #if "abstain" in data.body.internalVote [
    - Abstain: #data.body.internalVote.abstain
  ]
  #if "didNotVote" in data.body.internalVote [
    - Did not vote: #data.body.internalVote.didNotVote
  ]
  #if "againstVote" in data.body.internalVote [
    - Against vote: #data.body.internalVote.againstVote
  ]
]

// Optional References
#if "references" in data.body [
  = References

  #show link: underline

  #for reference in data.body.references [
    - #reference.at("@type"): #link(reference.uri)[#reference.label]
  ]
]

// Display author signatures if provided
#let hasWitness(author) = {
  if "witness" in author { true } else { false }
}
#if "authors" in data and data.authors.len() > 0 and data.authors.any(hasWitness) [
  = Author Signatures

  #for author in data.authors [

    === #author.name:

    #if hasWitness(author) [
      #set text(size: 8pt)
      - Witness algorithm: #author.witness.witnessAlgorithm
      - Public key: #author.witness.publicKey
      - Signature: #author.witness.signature
    ] else [
      Signature not provided.
    ]
  ]
]
