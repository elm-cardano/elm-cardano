# elm-cardano: Rewrite Analysis & Plan

> **Status:** Analysis document — captures the current state of elm-cardano,
> identifies its limitations, and proposes a direction for a from-scratch rewrite
> aimed at correctness, developer experience, and performance.
>
> **Audience:** Maintainers and contributors planning the next major version.

---

## 1. Executive Summary

elm-cardano successfully proved that a type-safe, productive Cardano offchain
library in Elm is viable. It supports Shelley through Conway eras, has 14
example applications, and powers real applications. However, the codebase has
accumulated fundamental issues that prevent it from being production-grade:

- **Cannot compile with `--optimize`** due to `Debug` module usage in library code.
- **Several incomplete implementations** crash at runtime via `Debug.todo`.
- **Inconsistent APIs** across modules (4 competing serialization naming schemes,
  3 competing error-handling patterns).
- **God modules** that mix unrelated concerns (`Cardano.TxIntent` is 2,546 lines,
  `Cardano.Transaction` is 1,635 lines).
- **Unenforced invariants** on key types like `Value` and `MultiAsset`.
- **Performance pain points** in CBOR encoding (hex-string round-trips for big
  numbers, O(n²) metadata duplicate detection).
- **Dependency on an unmaintained library** (`MartinSStewart/elm-uint64`).

The decision is to rewrite the library from scratch, with a comprehensive test
suite that locks in the desired behavior, while preserving the proven design
ideas: the intent-based builder, the `Source` abstraction, phantom-typed bytes,
and the per-address coin selection.

---

## 2. Project Snapshot (Current State)

| Metric              | Value                               |
| ------------------- | ----------------------------------- |
| Source Elm code     | ~14,313 lines / 35 files            |
| Test code           | ~6,650 lines / 20 files             |
| Exposed modules     | 36                                  |
| Largest module      | `Cardano.TxIntent` (2,546 lines)    |
| Second largest      | `Cardano.Transaction` (1,635 lines) |
| Examples            | 14 (11 active, 1 archived)          |
| Direct dependencies | 11                                  |
| Elm version         | 0.19.1                              |
| Custom CLI (Rust)   | v0.8.0 — required for UPLC patching |

---

## 3. Blocking Issues

### 3.1 `Debug` module usage in library code

The library currently cannot be compiled with `--optimize`. The elm-review
config has `NoDebug.Log` and `NoDebug.TodoOrToString` rules **intentionally
disabled** (`review/src/ReviewConfig.elm:51-53`).

**`Debug.todo` (incomplete implementations that crash at runtime):**

| Location            | Context                                                  | Severity |
| ------------------- | -------------------------------------------------------- | -------- |
| `TxIntent.elm:526`  | Byron addresses unsupported in `defaultEvalScriptsCosts` | High     |
| `TxIntent.elm:2419` | `AuthCommitteeHotCert` signature counting                | High     |
| `TxIntent.elm:2422` | `ResignCommitteeColdCert` signature counting             | High     |
| `Address.elm:494`   | Pointer credential encoding                              | High     |
| `Address.elm:498`   | Script pointer credential encoding                       | High     |
| `Address.elm:807`   | Unrecognized network ID                                  | Medium   |

**`Debug.log` (development logging left in library code):**

| Location                   | Context                          |
| -------------------------- | -------------------------------- |
| `Address.elm:673`          | Address decoding failure logging |
| `Cbor/Decode/Extra.elm:96` | CBOR decoding failure logging    |

**`Debug.toString` (used for error messages in library code):**

| Location                 | Context                       |
| ------------------------ | ----------------------------- |
| `TxIntent.elm:264, 1919` | `TxFinalizationError` display |
| `Witness.elm:215`        | `Witness.Error` display       |
| `Utxo.elm:272`           | `checkMinAda` error display   |

### 3.2 Resolution plan

- Replace each `Debug.todo` with a proper `Result` error variant, or if proven impossible, with infinite loop hacks.
- Remove `Debug.log` calls; rely on structured error returns.
- For each module that needs to display errors, define a per-type
  `<type>ToString` function. Build error display by composing those rather
  than calling `Debug.toString`.
- Re-enable both `NoDebug` rules in the elm-review configuration.

---

## 4. Type Safety & Invariant Enforcement

### 4.1 `Value` and `MultiAsset` zero-quantity invariants

Both `Value.elm:40` and `MultiAsset.elm:38` document an invariant ("never
contains zero quantities") that is not enforced by the type system. The
`subtract` function can produce zero entries, requiring manual `normalize`.

**Plan:**

- `Value` becomes an opaque type with smart constructors enforcing the invariant.
- `MultiAsset` becomes non-exposed, and is split into two opaque public types:
    - **`Assets`** wrapping `Natural` (for actual asset holdings, no zeros).
    - **`Mint`** wrapping `Integer` (for mint/burn, no zeros either —
      a zero mint is a no-op).
- Internally, the parametric `BytesMap PolicyId (BytesMap AssetName a)`
  representation is preserved as an implementation detail. Both opaque types
  delegate to shared internal helpers (`map2`, `split`, `normalize`, etc.).

### 4.2 `AuxiliaryData` label type is wrong

`AuxiliaryData.elm:31`: _"FIXME: Labels can't actually be arbitrarily sized
naturals but can only be u64."_

**Plan:** Keep `Natural` because it’s convenient and fine when decoded, but add checks to the cbor encoders.

### 4.3 `GovActionIndex` simplified to 1 byte

A comment in `Gov.elm` notes: _"will fail if a gov action index is >= 256"_.
The Cardano spec allows larger values.

**Plan:** Keep as-is and justify that more than 256 proposals in the same Tx is unlikely to happen, both because it’s quite close to the actual size limit of a Tx, and because it also implies more than 256 * 100k ada deposit (25M ada).

### 4.4 Hardcoded protocol parameters

`Utxo.elm` hardcodes `coinsPerUTxOByte = 4310` instead of accepting it as a
parameter. This breaks when protocol parameters change.

**Plan:** All protocol-parameter-sensitive functions must accept the relevant
parameters explicitly. A record should flow through the APIs that need it.

### 4.5 `TransactionBody` exposed record

The 22-field `TransactionBody` record is fully exposed, allowing construction
of invalid transactions. **Decision:** Keep it exposed (per user direction —
power users need it) but document the contract clearly and provide builder
helpers as the primary API.

---

## 5. API Design Inconsistencies

### 5.1 Naming convention for serialization

Currently four competing patterns are used: `to*/from*`, `encode/decode`,
`toCbor/fromCbor`, `serialize/deserialize`, plus one-offs like `encodeCborHex`.

### 5.2 Proposed convention

| Pattern                             | Purpose                                 | Returns / Takes                |
| ----------------------------------- | --------------------------------------- | ------------------------------ |
| `Type.toBytes` / `Type.fromBytes`   | Raw wire bytes (canonical form)         | `Bytes Type` ↔ `Maybe Type`    |
| `Type.toCbor` / `Type.fromCbor`     | CBOR codec combinators (composable)     | `E.Encoder` / `D.Decoder Type` |
| `Type.toBech32` / `Type.fromBech32` | Bech32 string                           | `String` ↔ `Maybe Type`        |
| `Type.toJson` / `Type.fromJson`     | JSON value                              | `JE.Value` / `JD.Decoder Type` |
| `Type.toData` / `Type.fromData`     | Plutus Data                             | `Data` ↔ `Maybe Type`          |
| `Type.toString`                     | Human display only (not roundtrippable) | `String`                       |
| `Type.toHex` / `Type.fromHex`       | Hex string of bytes                     | `String` ↔ `Maybe Type`        |

**Banned in public API:** `serialize`, `deserialize`, `encode`, `decode`,
`encodeXxx`, `decodeXxx`, `parseXxx`.

### 5.3 The "primary type" rule for module organization

**Constraint:** No submodule explosion. Splitting `Cardano.Address` into
`Cardano.Address.Credential`, `Cardano.Address.NetworkId`, etc. would force
verbose imports for every consumer.

**Rule:**

- The "primary" type of a module gets the bare names: `toCbor`, `fromCbor`,
  `toBytes`, etc.
- Secondary types get qualified names within the same module:
  `credentialToCbor`, `networkIdToCbor`, etc.

### 5.4 Renames required (high-confidence list)

| Module        | Current name                        | New name                                          |
| ------------- | ----------------------------------- | ------------------------------------------------- |
| Address       | `decode`                            | `fromCbor`                                        |
| Address       | `decodeCredential`                  | `credentialFromCbor`                              |
| Address       | `credentialToCbor`                  | (keep)                                            |
| Address       | `stakeAddressToBytes`               | (keep)                                            |
| Address       | `stakeAddressToCbor`                | (keep)                                            |
| Address       | `encodeNetworkId`                   | `networkIdToCbor`                                 |
| Address       | `fromString`                        | (keep)                                            |
| Transaction   | `serialize`                         | `toBytes`                                         |
| Transaction   | `deserialize`                       | `fromBytes`                                       |
| Transaction   | `encodeToCbor`                      | `toCbor`                                          |
| Transaction   | `decodeWitnessSet`                  | `witnessSetFromCbor`                              |
| Transaction   | `decodeVKeyWitness`                 | `vkeyWitnessFromCbor`                             |
| Transaction   | `encodeVKeyWitness`                 | `vkeyWitnessToCbor`                               |
| Script        | `encodeNativeScript`                | `nativeScriptToCbor`                              |
| Script        | `decodeNativeScript`                | `nativeScriptFromCbor`                            |
| Script        | `nativeScriptFromBytes`             | (keep)                                            |
| Script        | `nativeScriptBytes`                 | `nativeScriptToBytes`                             |
| Script        | `plutusScriptFromBytes`             | (keep, auto-detect format)                        |
| Script        | `cborWrappedBytes`                  | `plutusScriptToBytes`                             |
| Script        | `jsonDecodeNativeScript`            | `nativeScriptFromJson`                            |
| Script        | `jsonEncodeNativeScript`            | `nativeScriptToJson`                              |
| Script        | `refFromBytes`                      | `referenceFromBytes`                              |
| Script        | `refBytes`                          | `referenceToBytes`                                |
| Script        | `refScript`                         | `referenceScript`                                 |
| Script        | `refHash`                           | `referenceHash`                                   |
| Script        | `refFromScript`                     | `referenceFromScript`                             |
| Value         | `encode`                            | `toCbor`                                          |
| MultiAsset    | `coinsToCbor`                       | `assetsToCbor`                                    |
| MultiAsset    | `mintToCbor`                        | `mintToCbor` (keep)                               |
| MultiAsset    | `coinsFromCbor`                     | `assetsFromCbor`                                  |
| MultiAsset    | `mintFromCbor`                      | (keep)                                            |
| Gov           | `idFromBech32`                      | `idFromBech32` (keep — Id is secondary type)      |
| Gov           | `decodeDrep`, `encodeDrep`          | `drepFromCbor`, `drepToCbor`                      |
| Gov           | `decodeAction`, `encodeAction`      | `actionFromCbor`, `actionToCbor`                  |
| Gov           | `decodeAnchor`, `encodeAnchor`      | `anchorFromCbor`, `anchorToCbor`                  |
| Gov           | (all other `decodeX`/`encodeX`)     | `xFromCbor`/`xToCbor`                             |
| Pool          | `encodeParams`                      | `paramsToCbor`                                    |
| Pool          | `decodeParams`                      | `paramsFromCbor`                                  |
| Redeemer      | `encodeAsArray`                     | `toCbor`                                          |
| Redeemer      | `fromCborArray`                     | `fromCbor`                                        |
| Redeemer      | `encodeTag`                         | `tagToCbor`                                       |
| Redeemer      | `tagFromCbor`                       | (keep)                                            |
| Redeemer      | `encodeExUnits`                     | `exUnitsToCbor`                                   |
| Redeemer      | `exUnitsFromCbor`                   | (keep)                                            |
| Redeemer      | `encodeExUnitPrices`                | `exUnitPricesToCbor`                              |
| Redeemer      | `decodeExUnitPrices`                | `exUnitPricesFromCbor`                            |
| Utxo          | `encodeOutputReference`             | `outputReferenceToCbor`                           |
| Utxo          | `encodeOutput`                      | `outputToCbor`                                    |
| Utxo          | `encodeDatumOption`                 | `datumOptionToCbor`                               |
| Utxo          | `decodeOutputReference`             | `outputReferenceFromCbor`                         |
| Utxo          | `decodeOutput`                      | `outputFromCbor`                                  |
| Cip30         | `encodeRequest`                     | `requestToJson`                                   |
| Cip30         | `responseDecoder`                   | `responseFromJson`                                |
| Cip30         | `apiDecoder`                        | `apiResponseFromJson`                             |
| Cip30         | `utxoDecoder`                       | `utxoFromJson`                                    |
| Cip30         | `addressDecoder`                    | (move — create `Address.fromJson`)                |
| Cip30         | `hexCborDecoder`                    | (make internal)                                   |
| Cardano.Utils | `decodeRational`                    | merge into a `Rational` module or distribute      |
| Cardano.Utils | `encodeRationalNumber`              | same                                              |
| Cardano.Utils | `jsonEncodeCbor` / `jsonDecodeCbor` | move into the new CBOR library                    |

### 5.5 Error handling

- All fallible operations return `Result CustomError a`.
- Each module defines its own error type with an `errorToString`.
- `Maybe` only for genuine "not found" lookups, never for failures.
- Error types may carry contextual data so callers can render rich messages.

---

## 6. Module Organization

### 6.1 God modules

**`Cardano.Transaction`** (1,635 lines) handles too many responsibilities:

- Transaction/TransactionBody types
- 18 Certificate types and encoding
- Fee computation (3 components)
- VKeyWitness, BootstrapWitness
- WitnessSet encoding/decoding
- Transaction ID hashing
- Update procedures

**Plan:** Split into:

- `Cardano.Transaction` (core types + the high-level orchestration)
- `Cardano.Certificate` (all 18 certificate types — they are a distinct concept)
- `Cardano.Transaction.Fee` or similar internal module for fee math
- Keep witness types in `Cardano.Witness` (already a separate module)

**`Cardano.TxIntent`** (2,546 lines) is justified as a high-level facade but
needs internal decomposition (see §7).

### 6.2 Files to delete from src/

- `src/TempTxTest.elm` — 8-line temporary file in source directory.
- `src/Cardano/TxExamples.elm` (980 lines) — contains pretty string conversions that would be valuable to move elsewhere in appropriate places.

### 6.3 The umbrella `Cardano` module

A new top-level `Cardano.elm` module will provide a curated, opinionated API
covering 95% of use cases. Elm doesn't support re-export, so it will:

- Define `type alias`es pointing to the underlying types.
- Define wrapper functions that call into the detail modules.
- Define new helper functions for common boilerplate that doesn't already exist
  elsewhere.

**Risks to validate early:**

- How elm docs displays type aliases (readability).
- How elm-language-server displays them on hover.
- Type alias custom-type pattern matching propagation.

These should be prototyped on a small subset before committing to the full
umbrella module.

---

## 7. `Cardano.TxIntent` Deep Findings

### 7.1 Length distribution

- `buildTx` — 298 lines (lines 2067–2365)
- `preProcessIntents` — 240 lines (lines 1038–1278)
- `finalizeAdvanced` — 137 lines (lines 786–923)
- `computeCoinSelection` — 125 lines (lines 1931–2056)
- `containPlutusScripts` — 122 lines (lines 623–745)

### 7.2 Repeated patterns

1. **Redeemer construction is duplicated 6 times** (lines 2101–2186) for Spend,
   Mint, Cert, Withdraw, Vote, Proposal. The biggest single refactoring win.
2. **Witness validation** (`validateMints`, `validateVotes`, `validateWithdrawals`,
   `validateSpentOutputs`, lines 1553–1689) — same dedup-then-check-empties-then-combine
   pattern repeated 4 times.
3. **Credential extraction** (lines 2189–2209) — same `List.filterMap` pattern 4 times.

### 7.3 Fee convergence

The current 3-round loop has **no convergence guarantee or cycle detection**.
If a 1-lovelace fee change causes different coin selection, witness count, and
size, the loop could oscillate. Currently it errors on round 4 via
`checkInsufficientFee`.

**Plan:**

- Implement **convergence detection**: compare each round to the previous.
- Make the maximum iteration count **configurable** (with a sensible default).
- Document the convergence behavior so users know what to expect.

### 7.4 `defaultEvalScriptsCosts` issues

- `Debug.todo` for Byron addresses (line 526) → must become `Result.Err`.
- Only Mainnet/Preview supported (line 540) — **Preprod missing**.
- Hardcoded `conwayDefaultBudget` and `conwayDefaultCostModels` — not customizable.
  This is logical for a "by-default" function, but it probably doesn’t make sense with VM costs that may change regularly.
  We probably need to add a parameter to the `finalize` function, and remove the `defaultEvalScriptsCosts` function?

### 7.5 Missing error variants

Currently `TxFinalizationError` has 16 variants. Missing:

- `NotEnoughCollateral`
- `WitnessSetTooLarge` / `TxTooLarge`
- `RedeemerEvaluationTimeout`
- `InvalidTimeValidityRange` (start ≥ end is not currently structurally checked)

The user has indicated that **richer context for errors** is desirable as
long as it doesn't make the code too complex — to be evaluated during the
rewrite. The likely shape is errors that compose by wrapping other errors
with context: `InContext { stage, intentIndex, underlying }`.

### 7.6 `processOtherInfo` metadata duplicate detection

The inner `findDuplicate` (lines 1900–1913) is O(n²). Replace with hash-based
check.

### 7.7 Decomposition target

`TxIntent` becomes a thin orchestrator (~500 lines) that delegates to:

- `Cardano.Transaction.Fee` (extracted) — fee math + adjustment
- `Cardano.Transaction.Build` (extracted) — body+witness construction
- `Cardano.CoinSelection.Collateral` (expanded) — collateral logic
- Existing domain modules for intent processing helpers (mint, vote, cert, …)

---

## 8. The CBOR & Encoding Layer

### 8.1 Issues with the current `elm-toulouse/cbor` dependency

- Big number encoding/decoding goes through hex strings (acknowledged TODOs in
  `Cbor/Encode/Extra.elm:43` and `Cbor/Decode/Extra.elm:77`). This affects
  every transaction.
- Error reporting is opaque — failures collapse to `Maybe` with no context.
- Canonical map ordering must be implemented externally (and currently uses hex
  string sorting on every comparison).

### 8.2 Plan: New generic CBOR library

A new, generic, public **`elm-cardano/cbor`** package, usable outside of Cardano.

**Goals:**

- Performance-optimized big integer encoding (no hex round-trip).
- Structured error reporting (errors carry path + reason, not just `Maybe`).
- Canonical encoding available built-in.
- Fold-based decoders for record fields (already prototyped in `decodeBodyFold`).
- Explicit support for indefinite-length lists/bytes when needed (UPLC requires it).

### 8.3 `Data` (Plutus) encoding details to preserve

- Tag-based constructor encoding (121–127 for indices 0–6, 1280–1400 for 7–127,
  tag 102 for ≥128).
- Bytes chunking at 64 bytes for large byte strings.
- The dual `toCbor` / `toCborUplc` distinction (UPLC requires indefinite-length
  lists for non-empty lists, definite for empty). This is correct but error-prone
  — the new API should make the distinction obvious at the call site.

### 8.4 Plutus script wrapping layers

Plutus scripts have up to four wrapping forms:

1. Raw Flat bytes
2. `cbor_bytes(flat_bytes)`
3. `cbor_bytes(cbor_bytes(flat_bytes))` — used in transaction witness sets
4. `[tag, cbor_bytes(cbor_bytes(flat_bytes))]` — UTxO script reference

The current `plutusScriptFromBytes` heuristically detects which form it
received. **Per user direction, keep the auto-detect approach** — it works in
practice and avoids API churn — but document the layers carefully.

---

## 9. Dependency Strategy

### 9.1 Current dependencies

| Dependency                  | Version | Status           | Decision                              |
| --------------------------- | ------- | ---------------- | ------------------------------------- |
| `elm-toulouse/cbor`         | 4.0.1   | Maintained       | **Replace** with new in-house library |
| `elm-cardano/bech32`        | 1.0.0   | In-house         | Keep                                  |
| `dwayne/elm-integer`        | 1.0.0   | Stable           | Keep, contribute perf upstream        |
| `dwayne/elm-natural`        | 1.1.1   | Stable           | Keep, contribute perf upstream        |
| `MartinSStewart/elm-uint64` | 2.0.0   | **Unmaintained** | **Replace** (fold into Blake2b)       |
| `jxxcarlson/hex`            | 4.0.1   | Stable           | **Replace** (inline hand-optimized)   |
| `turboMaCk/any-dict`        | 2.6.0   | Maintained       | Keep                                  |
| `elmcraft/core-extra`       | —       | Maintained       | Audit usage, keep if needed           |

### 9.2 UInt64 audit results

`MartinSStewart/elm-uint64` has 137 calls across 5 files using 19 distinct
functions. Used in `Blake2b.elm`, `Blake2b/Int128.elm`, `Blake2b/UInt64.elm`,
`List/ExtraBis.elm`, and tests.

`List.ExtraBis` only uses `get64`, `take64`, `indexedMap64` (4 calls). The
64-bit indexing is not justified — Elm lists can never be that long. **Drop
`List.ExtraBis` entirely; revisit if Blake2b actually needs it during the
rewrite.**

After dropping it, UInt64 is 100% an internal Blake2b concern.

### 9.3 Hex audit results

`jxxcarlson/hex` has 9 calls across 7 files using 2 functions: `toString` and
`toBytes`. Usage:

- `Bytes.Comparable` (3 calls) — the canonical hex conversion point
- `Cardano.Cip30` (2 calls) — should delegate to `Bytes.Comparable`
- 3 example `External.elm` files — should delegate to `Bytes.Comparable`

**Plan:** Inline a hand-optimized hex codec into `Bytes.Comparable`, drop the
dependency. Hex conversion is performance-critical because it happens on every
byte comparison in canonical CBOR map ordering.

### 9.4 Blake2b

Full rewrite alongside the UInt64 fold-in. Performance-focused. Custom
internal `UInt64` representation tuned for the specific operations Blake2b
uses (the audit identified the exact 19 operations needed).

**Constraint per user direction:** Pure Elm only. No kernel JS.

---

## 10. CLI / UPLC Patching

### 10.1 How it works

Plain string replacement in compiled Elm JS. The Rust CLI:

1. Runs `elm make`.
2. Searches for two specific long error strings in the output JS.
3. Replaces them with `evalScriptsCostsKernel(_v0)` and
   `applyParamsToScriptKernel(_v0)`.
4. Prepends two JS files (`templates/eval-script-costs-kernel.js`,
   `templates/apply-params-to-script-kernel.js`) before `'use strict'`.
5. Copies WASM files to the output directory.

### 10.2 Decisions

- **Keep the current design.** No ports, no two-API approach.
- The library publishes to elm-packages once `Debug` usage is removed. Users
  who don't need UPLC evaluation work with vanilla `elm make`. Users who do
  need it run `elm-cardano make`.
- The kernel placeholder error messages are intentionally written in plain
  English so they make sense to developers who forgot to patch. This stays.
- **Robustness improvement:** Add uncommon, invisible Unicode markers to the
  placeholder strings so the patcher's anchor-search remains unique even
  through future refactors. The visible English message stays for human
  readers; the invisible markers are just sentinels for the patcher.
- The UPLC backend is allowed to change in the future (different WASM, pure
  JS, etc.). The kernel-function abstraction in `Cardano.Uplc` should make
  this swappable.

### 10.3 Async UPLC future

`ConcurrentTask` (or future Elm async primitives) is a possible future
direction, but **not now**. The challenge is that UPLC evaluation must happen
inside the fee convergence loop, not just at the end — so an async
implementation would propagate `Task` through the entire builder API.

**Plan:** Document the current synchronous design and its tradeoffs.

---

## 11. Test Strategy for the Rewrite

### 11.1 Current coverage

**Well tested:**

- Blake2b (3 modules with known vectors)
- Transaction serialization (34 real blockchain transactions across all eras)
- Coin selection (property-based + distribution analysis)
- `Data` (PlutusData) — fuzz-based round-trip
- CIP-67, CRC8 (spec test vectors)
- `TxIntent` building (1,928 lines of integration tests)

**Untested (no dedicated tests):**

- `Cardano.Cip30`, `Cardano.Cip95`
- `Cardano.Interval`, `Cardano.Redeemer`
- `Cardano.TxContext`, `Cardano.Uplc`
- `Cardano.AuxiliaryData`, `Cardano.Metadatum`
- `Bytes.Comparable`
- `Cbor.Decode.Extra`, `Cbor.Encode.Extra`
- `RationalNat`, `Word7`

**Minimal coverage:**

- `Cardano.Address` (2 tests)
- `Cardano.Pool` (2 tests)

### 11.2 Plan for the rewrite

The rewrite is from-scratch, so the test suite must be built up alongside the
new code with much higher coverage. Specifically:

- **Adopt all existing transaction test vectors** (the 34 real blockchain
  transactions) as a regression suite from day one.
- **Property-based tests** for every roundtrip (`toCbor` ∘ `fromCbor = id`,
  `toBytes` ∘ `fromBytes = id`, `toBech32` ∘ `fromBech32 = id`).
- **Property-based tests** for invariant-preserving operations on `Value`,
  `Assets`, `Mint`.
- **Convergence tests** for the fee loop: construct adversarial scenarios and
  verify the loop terminates within the configured iteration cap.
- **Tests against Cardano spec test vectors** wherever they are available.
- **Coverage targets:** every exposed module must have at least one test file.

---

## 12. Open Architectural Questions Resolved

This section records decisions made during the analysis phase so they don't
need to be re-litigated.

| Question                        | Decision                                                                              |
| ------------------------------- | ------------------------------------------------------------------------------------- |
| Rewrite approach                | Full rewrite from scratch                                                             |
| Test discipline                 | Build suite alongside; ≥1 test file per exposed module                                |
| `Value` opacity                 | Opaque, with smart constructors                                                       |
| `MultiAsset` opacity            | Two opaque types: `Assets`, `Mint`. Polymorphic internals shared.                   |
| `TransactionBody` opacity       | Stays exposed (power users need it)                                                   |
| Submodule explosion             | Avoid. Use the "primary type" naming rule.                                            |
| Umbrella `Cardano` module       | Yes — type aliases + wrapper functions + helpers                                      |
| Auto-detect Plutus script bytes | Keep                                                                                  |
| Naming convention               | `to*/from*` family, ban `encode/decode/serialize/deserialize`                         |
| Error handling                  | `Result` everywhere, `Maybe` only for "not found"                                     |
| UPLC patching                   | Keep current design (no ports, no two APIs)                                           |
| Patching anchor                 | Keep English messages, add invisible Unicode markers                                  |
| Async UPLC                      | Not now. Possibly `ConcurrentTask` in the future.                                     |
| Fee convergence                 | Convergence detection + configurable max iterations                                   |
| CBOR rewrite                    | Full rewrite, generic, performance + structured errors                                |
| Blake2b rewrite                 | Full rewrite, pure Elm, performance-focused                                           |
| UInt64                          | Folded into Blake2b internals                                                         |
| Hex                             | Inlined into `Bytes.Comparable`, drop external dep                                    |
| Witness duplications            | Keep — they are not actually duplicates; the differing names reflect different usages |
| `Cardano.Utils`                 | Distribute its contents to where they belong, or rename                               |
| `Cardano.TxExamples`            | Move out of the public API, distribute the pretty print functions                     |
| `src/TempTxTest.elm`            | Delete                                                                                |

---

## 13. Concrete Task List for the Rewrite

Tasks are loosely ordered. Many can be parallelized.

### Phase 0 — Foundations

1. **New CBOR library** (`elm-cardano/cbor`).
    - Generic, public package.
    - Performance-optimized big integer encoding.
    - Structured error reporting with path + reason.
    - Canonical map ordering built-in.
    - Indefinite-length list/bytes support.
    - Comprehensive test suite.

2. **New Bytes layer** (`Bytes.Comparable`-replacement).
    - Inlined hand-optimized hex codec.
    - Phantom-typed bytes wrapper.
    - Drop `jxxcarlson/hex` dependency.
    - Comprehensive tests.

3. **New Blake2b** (with internal UInt64).
    - Pure Elm, performance-focused.
    - Internal UInt64 type optimized for the 19 operations Blake2b needs.
    - Drop `MartinSStewart/elm-uint64` dependency.
    - Test against the existing known-vector suite.

### Phase 1 — Core types

4. **Address module** (rewrite).
    - Implement pointer credentials properly (no `Debug.todo`).
    - Handle unknown network IDs as `Result.Err`.
    - Adopt the new naming convention.
    - Comprehensive tests.

5. **Value module** (opaque rewrite).
    - Smart constructors enforcing no-zero invariant.
    - Property tests for invariant preservation.

6. **MultiAsset module** (opaque rewrite).
    - Public types: `Assets`, `Mint`.
    - Shared parametric internals.
    - Property tests.

7. **Utxo / Output module**.
    - `coinsPerUTxOByte` becomes a parameter, not a hardcoded constant.
    - Adopt the new naming convention.

8. **Script module**.
    - Native + Plutus.
    - Auto-detect Plutus script byte format (preserved).
    - Document the layered wrapping carefully.

9. **Data module** (Plutus Data).
    - Tail-recursive decoding (the current TODO).
    - Clear separation between standard CBOR encoding and UPLC encoding.

10. **AuxiliaryData / Metadatum**.
    - Tests for both types.

### Phase 2 — Transaction & governance

11. **Transaction module** (split + rewrite).
    - `Cardano.Transaction` — core types and high-level operations.
    - `Cardano.Certificate` — extracted from Transaction.
    - `Cardano.Transaction.Fee` (or internal equivalent) — fee math.
    - Adopt the new naming convention.
    - Test against the 34 real blockchain transaction vectors.

12. **Witness module**.
    - Preserve the `Source` abstraction.
    - Keep the per-domain types (Voter, Credential, Script as currently
      designed — they are not duplicates).
    - Better error context.
    - (MAYBE we could move Witness.Credential into a Credential.Witness type? Same for others?)

13. **Gov module**.
    - Implement `currentTreasuryValue` and `treasuryDonation`.
    - Add tests for governance voting flows.

14. **Pool module**.
    - More tests.

15. **Redeemer module**.
    - Tests for round-trip and invariants.

### Phase 3 — Builder

16. **TxIntent rewrite**.
    - Decompose into a thin orchestrator (~500 lines) plus extracted helpers.
    - Eliminate the 6× redeemer construction duplication.
    - Eliminate the 4× witness validation duplication.
    - Replace `processOtherInfo` O(n²) duplicate detection with hash-based.
    - Add convergence detection + configurable iteration cap.
    - Replace `Debug.todo` sites with proper errors.
    - Add Preprod support to default cost evaluation.
    - Make cost models / budget configurable.
    - Add missing error variants (NotEnoughCollateral, TxTooLarge, etc.).
    - Add richer error context where it doesn't hurt readability.

17. **CoinSelection module**.
    - Address the 8 existing TODOs.
    - Tests for adversarial cases (oscillation triggers).

### Phase 4 — Wallet & UPLC

18. **Cip30 module**.
    - Adopt naming convention.
    - First test suite for the module.
    - Helper for the common wallet state machine boilerplate.

19. **Cip95 module**.
    - First test suite for the module.

20. **Uplc module**.
    - Add invisible Unicode markers to the kernel placeholder strings.
    - Make the backend swappable (clean abstraction over the kernel functions).
    - Document the patching contract.
    - First tests (against the patched binary).

### Phase 5 — Public API

21. **Umbrella `Cardano` module**.
    - Prototype early to validate doc rendering.
    - Type aliases for the most common types.
    - Wrapper functions for the 20-ish core operations.
    - Helper functions for the 8 boilerplate patterns identified in examples.

22. **Cleanup**.
    - Delete `src/TempTxTest.elm`.
    - Move `src/Cardano/TxExamples.elm` functions to appropriate places.
    - Distribute `Cardano.Utils` contents.
    - Re-enable `NoDebug.*` rules in elm-review.

23. **Examples migration**.
    - Update all 11 active examples to the new API.
    - Identify remaining boilerplate that should be helpers.

24. **Documentation**.
    - Per-module overview.
    - The "easy 95%" guide using the umbrella module.
    - The "power user" guide using the detail modules.
    - Migration guide from the current version.

### Phase 6 — Polish

25. **Property-based test expansion** for every roundtrip and invariant.

26. **Publish to elm-packages.**

---

## 14. Boilerplate Patterns to Abstract in the Umbrella Module

Identified across 5+ examples each:

1. **Wallet state machine** (Startup → Discovered → Loading → Loaded).
2. **Wallet response decoder setup**:
   `Cip30.responseDecoder (Dict.singleton 30 Cip30.apiDecoder)`.
3. **UTxO state update boilerplate** after submission.
4. **Address credential extraction** with error fallback.
5. **Script address construction**:
   `Address.Shelley { networkId, ScriptHash, stakeCredential }`.
6. **Wallet → sign → merge sigs → submit** sequence.
7. **HTTP error decoding for backends** (Ogmios, Blockfrost).
8. **Local UTxO state filter / lookup** patterns.

---

## 15. Risks & Mitigations

| Risk                                         | Mitigation                                                                                                                         |
| -------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------- |
| Type alias documentation rendering is poor   | Prototype the umbrella module early on a subset; if rendering is unacceptable, fall back to a thin wrapper-functions-only umbrella |
| Fee convergence regressions                  | Adversarial property tests; cycle detection from day one                                                                           |
| Breaking changes overwhelm consumers         | Comprehensive migration guide; keep the old version available                                                                      |
| New CBOR library has bugs                    | Run both old and new in parallel during development; cross-check encodings on the 34 real transaction vectors                      |
| Blake2b performance regression               | Benchmark against the current implementation before replacing                                                                      |
| UPLC patching breaks silently after refactor | Add invisible Unicode markers; CI test that compiles a known script-using example and runs it                                      |
| Scope creep                                  | This document; defer non-blocking improvements                                                                                     |

---

## 16. Out of Scope (For This Rewrite)

To prevent scope creep, the following are explicitly **not** part of the
rewrite:

- Async UPLC evaluation via `ConcurrentTask`.
- Switching the UPLC WASM module (allowed in the future, but not now).
- Backend integrations beyond what already exists (Ogmios, Blockfrost, etc.).
- Hardware wallet integration beyond CIP-21 compliance testing.
- A new emulator.

---

## 17. Acknowledgements

This document synthesizes a multi-pass exploration of the elm-cardano codebase
across:

- Project structure and dependencies
- All `Debug` usage sites
- Core Cardano data types
- The CBOR / encoding layer
- Transaction building and finalization
- The `TxIntent` god module in detail
- The `Witness` module
- The CLI / UPLC patching mechanism
- The 6 major example applications
- API surface naming and consistency
- Test coverage gaps
