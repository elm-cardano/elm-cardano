# proposal submission (one-shot)

This example was written for a single purpose: submit 5 governance
proposals on the Cardano **Preview** network in 3 transactions, in order
to populate on-chain data used to test the proposal-conflict-badges UI
of the CF voting app.

- Tx A: two competing ParameterChange proposals (#1 + #2)
- Tx B: one ParameterChange proposal (#3) chained on #1 via `latestEnacted`
- Tx C: one NoConfidence (#4) + one UpdateCommittee (#5), competing on the committee purpose

It was run once and is not meant to be re-run as-is (the anchors, the
guardrails script ref UTxO, the `latestEnacted` action IDs, and the
Koios API token are all hard-coded for that one run). It is kept in the
repository because the code may still be useful as a reference for
people who want to learn how to:

- build and submit Conway-era governance proposals with `elm-cardano`
- sign and submit a transaction through a CIP-30 wallet (no Ogmios /
  Koios submit endpoint involved)
- fetch protocol parameters and cost models from Koios via the Ogmios
  proxy
- retrieve a Plutus script reference UTxO by fetching the raw tx CBOR
  from Koios and decoding it with `Cardano.Transaction.deserialize`
- look up the most recently enacted action of a given purpose via the
  Koios `/proposal_list` endpoint, to fill the `latestEnacted` field
  required for chainable proposals
- witness the guardrails script by reference (`Witness.ByReference`)
- chain proposals across transactions by referencing a not-yet-enacted
  action ID as `latestEnacted`

## Run

```sh
cd frontend/elm-cardano/examples/proposal
elm-cardano make src/Main.elm --output main.js --debug && python -m http.server 3000
# then open index.html in a browser
```

## Notes

- Anchor URLs in `Main.elm` must be ≤ 128 bytes (Conway ledger limit on
  `Anchor.url`). The original run used long raw.githubusercontent.com
  URLs and was rejected by the wallet; they were replaced with short
  `ipfs://` URIs.
- The governance action deposit on Preview is 100k ADA per proposal, so
  each `buildTx` prepends an explicit `Spend (FromWallet ...)` intent
  covering `numberOfProposalsInTx × 100k ADA`; otherwise `TxIntent`'s
  pre-coin-selection balance check fails with `Missing lovelace input`.
