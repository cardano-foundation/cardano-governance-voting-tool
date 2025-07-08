# App Architecture

The main objective is to guide the voter through all the steps needed to submit their vote onchain.
To that aim, the app is split in stages:

1. Preparing a vote Tx with rationale
2. Gathering signatures and submit Tx

The signing stage is sufficiently independant that it should be possible to perform it by someone else than the preparation stage, granted the correct signatures are provided of course.
This way, we can easily envision a scenario where the vote Tx is prepared as a group.
But then each individual party can sign the prepared vote with their own setup.

## Preparing a vote Tx with rationale

This stage requires the following steps:

- Identify the voter type: CC | DRep | SPO
- Identify the vote credentials: Single stake key | Script
- Identify the fee provider: Connected wallet | ?
- Load governance proposals and select one
- Retrieve the proposals metadata
- Fill the vote rationale, following the relevant CIP standard JSON
- Pin the rationale in permanent storage (IPFS)
- Build the Tx
- Provide ways to export the Tx and rationale to share with signers

## Signing and submitting the Tx

Some people have a custom CLI setup, others can use this app to sign with their browser wallet, like Eternl.
The important part is being able to verify the Tx, meaning:

- Display the content of the Tx
- Check that the shared rationale hash corresponds to the one in the Tx
- Sign the Tx
- Gather other signature witnesses
- Submit the Tx

## Additional remarks

If in the preparation step, the voter is simply using their connected wallet and signing with it, there is no need to split the process in multiple stages.
The voter can directly submit their vote as soon as the Tx is built.
