# From Elm to Elm-Cardano

This guide will help you integrate elm-cardano into your existing Elm project.
There are two components in elm-cardano:

1. The elm-cardano package
2. The elm-cardano command-line app

## The elm-cardano package

The package is described by the `elm.json` file at the root if this repository,
and consists of all the exposed modules inside the `src/` directory.
It is not published on the Elm package registry because it cannot satisfy
the "no Debug module call" requirement yet.
I expect it to reach that maturity level by the end of the year.
However, it’s still very usable today.
To add the elm-cardano package to your project today, you need the following things:

- Add this repository as a submodule to your project, and check out the commit for the latest release tag.
- Add the `path/to/elm-cardano/src` directory to the `source-directories` in your `elm.json` file.
- Add elm-cardano dependencies to the dependencies in your `elm.json` file.

To know which dependencies to add, you can look at the `templates/starter/elm.json` file in this repository.
Make sure to check this file at the same commit that the release tag you checked out.

For more details about what this package contains,
you can spin up a local docs server with [elm-doc-preview][elm-doc-preview].
Alternatively, these docs are regularly [published online][elm-cardano-doc] but may not be up to date.

[elm-doc-preview]: https://github.com/dmy/elm-doc-preview
[elm-cardano-doc]: https://elm-doc-preview.netlify.app/Cardano?repo=elm-cardano%2Felm-cardano&version=elm-doc-preview

## The elm-cardano command-line app

If you know Elm, you probably already know that direct FFI with JS/WASM is not allowed.
However, for some features like transaction building, FFI with WASM is necessary.
The reason is that building a valid transaction is a recursive process,
with multiple stages involving phase-2 smart contract evaluation.

This requirement to execute smart contracts in a VM isn’t something rewritable in Elm within any reasonable time frame.
So in the elm-cardano package, the `Cardano.Uplc` module contains the following function:

```elm
{-| Kernel function (needs patching by elm-cardano) to run phase 2 evaluation (WASM code).
-}
evalScriptsCostsKernel : JE.Value -> Result String (List String)
evalScriptsCostsKernel _ =
    Err "To build a Tx containing scripts, you need to use the elm-cardano binary instead of directly the elm binary. Details are in the elm-cardano GitHub repo."
```

Whenever you try to build a transaction with scripts,
or evaluate the cost of a transaction with scripts,
this function will be called and return and error.

Except if you compile your main with `elm-cardano make` instead of `elm make`.
In that case, the elm-cardano binary will patch this function
with an actual call to the WASM virtual machine able to run the scripts.

The elm-cardano binary is a minimal Rust CLI app available in the `cli/` folder of this repository.
It is available as a static binary for Linux, macOS, and Windows in the [latest release][release],
as well as on NPM, so you can have it locally installed in your `package.json` file.

In the future, this binary will also enable things like running scripts directly in the terminal,
for example to build one-off transactions or other similar tasks achievable with a `worker` program (no view).

[release]: https://github.com/elm-cardano/elm-cardano/releases/latest
