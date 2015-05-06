
# Notes

- basic principle (???.hs)
  - write general properties
  - generate test data
  - shrink counterexamples
- Gen
  - size
- Testable class
  - `(==>)`
  - `forAll`
  - `forAllShrink`
  - `(===)`
  - `counterexample`
  - `whenFail`
- `Arbitrary` class

- General Tips:
  - Don't use QuickCheck without something like hspec.
  - Switch back and forth between
    - writing properties,
    - writing production code,
    - writing proper Gens,
    - writing proper shrinks,
    - writing proper counterexamples,
    - don't loose your patience.
  - QuickCheck is a heavyweight tool, consider writing unit tests first.
