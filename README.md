# CreditCard

A robust Haskell library for credit card validation, data types, smart constructors, order-independent token parsing, privacy masking, and BIN/IIN range database lookups.

---

## Features

- **Order-Independent Token Parsing**: Parse card fields from input token lists (`[ByteString]`) regardless of the order they appear in (e.g. `[number, name, date, ccv]` or `[name, date, ccv, number]`).
- **Comprehensive Error Collection**: Powered by `Data.Validation` to capture and report all validation failures simultaneously via `NonEmpty (CardError ByteString)` rather than failing on the first error.
- **Smart Constructors**: Dedicated constructors (`mkCardNumber`, `mkCardName`, `mkValidDate`, `mkCCV`, `mkCreditCard`) ensuring valid domain data models.
- **Privacy & Security Masking**:
  - `CardNumber` displays masked account digits (e.g. `"426398******9299"`).
  - `CCV` displays `"***"` in its `Show` instance to prevent accidental leakage in logs.
- **BIN / IIN Lookup**: Fast in-memory prefix matching using a memoized `Trie` to resolve card issuer schema (Visa, MasterCard, AmEx, etc.), card type (Debit / Credit), and issuing country.
- **Luhn Checksum Verification**: Verification of card numbers using the standard Luhn (mod 10) algorithm.
- **Extensive Test Coverage**: Comprehensive suite including golden regression tests, public API unit tests (`tasty-hunit`), and QuickCheck permutation invariance property tests.

---

## Module Overview

The library exposes a clean public API through `Data.CreditCard`:

| Module | Description |
| :--- | :--- |
| [`Data.CreditCard`](src/Data/CreditCard.hs) | Main public entry point re-exporting types, smart constructors, parser, and BIN database lookup functions. |
| `Data.CreditCard.Internal.Types` | Core data models (`CreditCard`, `RawCreditCard`, `CardNumber`, `CardName`, `CCV`, `ValidityDate`, `CardMeta`, `Env`). |
| `Data.CreditCard.Internal.Errors` | Error types (`CardError`) and `Enum`, `Semigroup`, `Show`, `Read` instances. |
| `Data.CreditCard.Internal.Helpers` | Validation and transformation helpers (`digits`, `luhnSum`, `checksumCardNumber`, `mbTpl`). |
| `Data.CreditCard.Internal.Parser` | Order-independent token classification and validation pipeline (`parseCardValidations`, `classifyToken`). |

---

## Quickstart

### 1. High-Level Creation with Environment & BIN Lookup

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Reader
import Data.CreditCard
import Data.Validation

main :: IO ()
main = do
  -- Initialize environment with default BIN ranges database
  env <- setEnv def
  
  -- Order-independent field input: [number, name, expiration date, ccv]
  let inputs = ["4263982640269299", "John Doe", "12/2026", "837"]
  
  result <- runReaderT (create inputs) env
  case result of
    Success card -> do
      putStrLn $ "Successfully created card: " ++ show card
      putStrLn $ "Card Number (masked): " ++ show card.number
      putStrLn $ "Metadata: " ++ show card.metaData
    Failure errs ->
      putStrLn $ "Validation failed: " ++ show errs
```

### 2. Pure Smart Constructors

```haskell
import Data.CreditCard

-- Card Name (supports 2 or 3 part names)
let mbName = mkCardName "Jane M. Doe"

-- Expiration Date (MM/YYYY or MM/YY format)
let mbDate = mkValidDate "08/2028"

-- CCV Code (3 or 4 digits)
let mbCCV  = mkCCV "123"

-- Card Number (validates minimum length and separates 6-digit BIN + account ID)
let mbNum  = mkCardNumber "4263982640269299"
```

---

## Building and Testing

### Build Library
```bash
cabal build
```

### Run Test Suite
```bash
cabal test
```

The test suite runs:
1. **Golden Tests**: Verifies batch CSV decoding and formatting against golden expectation outputs.
2. **Unit Tests**: Verifies individual smart constructors (`mkCardName`, `mkCCV`, `mkValidDate`, `mkCardNumber`, `mkCreditCard`, `def`, `setEnv`, `digits`, `luhnSum`).
3. **Property Tests**: QuickCheck property verifying that `create` is invariant under any permutation of the input token list.

---

## License

BSD-3-Clause © Hristo Kochev