# Revision history for CreditCard

## 0.1.0.0 -- 2026-09-07

### Architecture & Internal Modularization
* Modularized internals under `Data.CreditCard.Internal.*`:
  * `Data.CreditCard.Internal.Types`: Domain definitions for `CreditCard`, `RawCreditCard`, `CardNumber`, `CardName`, `CCV`, `ValidityDate`, `CardMeta`, `CardSchema`, `CardType`, and `Env`.
  * `Data.CreditCard.Internal.Errors`: Granular `CardError` type with `Enum`, `Semigroup`, `Show`, and `Read` instances.
  * `Data.CreditCard.Internal.Helpers`: Pure conversion functions (`digits`, `luhnSum`, `checksumCardNumber`, `mbTpl`, `toNonEmptyTpl`).
  * `Data.CreditCard.Internal.Parser`: Token classification (`classifyToken`) and order-independent field extraction pipeline (`parseCardValidations`).
* Clean public re-exports through top-level `Data.CreditCard` module.

### Features & Enhancements
* **Order-Independent Field Parsing**: `create` / `createCreditCard` now accepts `[ByteString]` in arbitrary order, extracting `CardNumber`, `CardName`, `ValidityDate`, `CCV`, and retaining unparsed tokens as raw metadata.
* **Comprehensive Validation Accumulation**: Validation returns `Validation (NonEmpty (CardError ByteString)) CreditCard`, aggregating all validation errors simultaneously.
* **Serialization & Read Instances**: Implemented `Read` and `Show` instances for `CreditCard`, `RawCreditCard`, `CardNumber`, `CardName`, `CCV`, `CardSchema`, `CardType`, and `CardError`.
* **Security Masking**: `CardNumber` displays masked account digits and `CCV` displays `***` to prevent accidental credential leakage in logs and `Show` outputs.
* **BIN / IIN Lookup**: Fast in-memory lookup via Trie loaded from CSV range dataset (`ranges.csv`) using `setEnv` and `searchBinDb`.

### Testing & Verification
* Added 52 total automated tests across three tiers:
  * Golden file regression tests (`credit_card_test`).
  * Comprehensive public API unit tests (`tasty-hunit`) for smart constructors, helpers, error cases, and `Read` instances.
  * QuickCheck property tests (`tasty-quickcheck`) verifying permutation invariance for `create`.
