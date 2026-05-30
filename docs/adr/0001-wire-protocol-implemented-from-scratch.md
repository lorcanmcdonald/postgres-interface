# Wire protocol implemented from scratch in Haskell

The PostgreSQL Frontend/Backend Protocol is implemented directly in Haskell rather than delegating to an existing library. No server-side Haskell implementation exists on Hackage — `pg-wire` is client-only. The two viable alternatives were a Rust FFI boundary (using the `pgwire` crate, which is the clear Rust standard) or a separate Rust sidecar process; both split the project across two languages and runtimes, adding operational and build complexity that outweighs the protocol implementation cost. The protocol subset required for v1 — startup handshake, trust auth, Simple Query flow, `RowDescription`/`DataRow`/`CommandComplete` messages, and a handful of `pg_catalog` stubs — is finite and well-specified. The `pgwire` Rust crate and the Haskell `pg-wire` client package serve as implementation references.

## Considered Options

- **Rust `pgwire` crate via FFI** — full protocol coverage, but requires a Haskell/Rust FFI boundary and a mixed-language build.
- **Separate Rust sidecar using `pgwire`** — clean separation, but two deployable processes and an internal IPC protocol to maintain.
- **From scratch in Haskell** — chosen: keeps the project in a single language and runtime, and the required v1 subset is implementable.
