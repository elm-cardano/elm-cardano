# Frequently Asked Questions (FAQ)

I’ll try to answer the most frequently asked questions here.
If you have any question not covered here,
feel free to reach out via the #elm channel on TxPipe Discord.

## Why is there an elm-cardano binary?

The Elm language has many nice guarantees, such as no runtime errors,
when it compiles it usually work, enforced semantic versioning of dependencies, etc.
However, to ensure these guarantees, Elm has some very strict rules.
In particular, it is not possible to make direct FFI calls to JS/WASM code.

In the case of elm-cardano, we need a way to check phase-2 verifications of a transaction (Tx).
This means we need to evaluate UPLC smart contracts.
Rewriting from scratch a UPLC evaluator in pure Elm is not feasible in a reasonable time frame.
Especially implementing the cryptographic primitives,
and it would also have poor performances.

So we rely on a WebAssembly (WASM) version of Aiken’s UPLC virtual machine (VM).
And since building a Tx is a complex iterative process,
smart contract evaluation is needed multiple times in a loop.
Wiring this with ports would be too cumbersome to be worth the effort IMO.
So the elm-cardano binary instead patches the output of the Elm compiler
to inject the necessary code to evaluate smart contracts with the WASM UPLC VM
in a pure function `Cardano.Uplc.evalScriptsCosts`.

The elm-cardano binary also automatically adds some JS code to handle CIP-30 wallet interactions.
And in the future, it will additionally provide new capabilities,
such as running scripts in the command line.

The elm-cardano binary is rather minimal Rust CLI,
and you can find its code in the `cli/` directory of this repository.

## Why isn’t the elm-cardano package published?

To maintain its strong guarantees, Elm has limitations in place for published packages.
In particular, the `Debug` module is not allowed in published packages.
We currently have too many `Debug.todo`, `Debug.toString`, and `Debug.log` calls
in the code base to have no usage of the `Debug` module.

Being able to publish the elm-cardano package is our last item on the v1.0 roadmap.
I hope we can achieve it in 2025, but if I’m realistic, it’s probably going to be 2026.
Some important changes before being debug-free:

- Rewrite the CBOR library to be able to fail with error messages.
- Rework `Bytes` for safe usage.
- Write actual `toString` functions where relevant.
- Complete all the current `todo`s.

You can follow the progress to v1.0 in [issue #46 on GitHub][todos]

[todos]: https://github.com/elm-cardano/elm-cardano/issues/46

## Why isn’t it possible to compile with --optimize?

For the same reasons that the elm-cardano package is not published yet.
We still use the `Debug` module quite a lot.

If your objective is to ship small assets sizes, it’s still very much possible.
Elm even excels at it, by being able to only embed the functions in the call tree of the app.
So importing a new package and only using 1 function, will only add this function
and the ones it depends on to your compiled output.

I would suggest minimizing the compiled output with [esbuild][esbuild] with the `--minify` flag.
This, combined with gzip or brotli compression on the server,
will almost guarantee elm compiled asset transfer sizes under 200kB.

```sh
# Example usage of esbuild on a compiled output at static/main.js
esbuild static/main.js --minify --allow-overwrite --outfile=static/main.js
```

The esbuild `--bundle` flag is currently not supported due to difficulties in handling the dynamic WASM imports.
More info in the next question, but feel free to try and solve it, PR is welcome.

[esbuild]: https://esbuild.github.io/

## Why does elm-cardano have its own JS loader?

Loading the output of elm-cardano compilation (`main.js` here) is done as follows:

```js
<html>
<head>
    <meta charset="UTF-8" />
    <title>elm-cardano</title>
</head>

<body>
    <div id="myapp"></div>
    <script type="module">
        import * as ElmCardano from "./elm-cardano.js";
        // Load the Main.elm compiled file
        await ElmCardano.loadMain("./main.js");
        // Initialize the Elm app
        var app = Elm.Main.init({
            node: document.getElementById("myapp"),
            flags: null,
        });
        // Initialize default ports for the app
        ElmCardano.init({
            portFromElmToWallet: null, // app.ports.toWallet,
            portFromWalletToElm: null, // app.ports.fromWallet,
        });
    </script>
</body>
</html>
```

In contrast, loading a typical Elm application is done as follows:

```js
<html>
<head>
    <meta charset="UTF-8">
    <title>Main</title>
    <script src="main.js"></script>
</head>

<body>
    <div id="myapp"></div>
    <script>
        var app = Elm.Main.init({
            node: document.getElementById('myapp'),
            flags: null,
        });
    </script>
</body>
</html>
```

The main difference is the following.
The typical Elm app loads its JS compiled file with a `<script>` tag in the `<head>` section.
In contrast, the elm-cardano app loads its JS compiled file directly in the `<body>` section,
with the following 2-step process:

```js
import * as ElmCardano from "./elm-cardano.js"
await ElmCardano.loadMain("./main.js")
```

Short answer, importing both the compiled Elm and the WASM UPLC VM,
embedded directly into pure function calls in the elm-cardano package is a really hard thing to do well.
Especially if you want it to be compatible with both the browser, and Node (for tests and future CLI stuff).
So I spent a couple days figuring it out, and then made it standard.

Longer answer, the main difficulty resides in the loading order of the different scripts of the page.
The WASM initialization function is asynchronous, but the initialization of the Elm compiled output is synchronous,
and also not supporting ESM modules by default.
The difficulty revolves around constants in the Elm code.
When you have a constant in your Elm code, something like:

```elm
tx = finalize [] [intent1, intent2, ...]
```

This `tx` constant is evaluated immediately when the browser parses the compiled `main.js`.
However, at that time the WASM async initialization has not finished yet,
if you don’t write your imports in a very specific way.
So that’s why we have a special `loadMain(uri)` function inside the `elm-cardano.js` file,
which is compatible with the elm compiled IIFE file format,
and enables loading the Elm code after we are sure that the WASM UPLC VM has fully initialized.

## What is the interop story with other JS tools?

Same as any other Elm application.
There are two main ways to interop with JS.

1. Using ports
2. Using web components custom elements

Using ports is somewhat equivalent to calling a remote HTTP endpoint.
The request and the answer are completely asynchronous, so you need to handle that in your update function.

For web components custom elements, I’ll redirect you to this tutorial:
https://elmprogramming.com/interacting-with-web-components.html

To keep its guarantees, Elm doesn’t allow direct FFI calls to JavaScript.
However, you will probably find the [`elm-concurrent-task` library][elm-concurrent-task] useful.
It enables something akin to task ports, to improve the usability of chaining tasks with ports.

[elm-concurrent-task]: https://github.com/andrewMacmurray/elm-concurrent-task

## Does elm-cardano have an emulator?

No, But.
Elm-cardano doesn’t provide a full fledged emulator with phase-1 and phase-2 Tx validation.
However, its phase-2 smart contract evaluation is configurable to use custom network parameters.
So you could in theory set something up for an independent emulator.

## Is elm-cardano usable with a local testnet?

In theory, yes.
In practice, I haven’t tested it yet.
So if you try using elm-cardano on a custom local network, please let me know how it goes.
In particular, I’m thinking of things like the [Yaci DevKit][yaci].

[yaci]: https://github.com/bloxbean/yaci-devkit

## How to use an API provider with elm-cardano?

Elm-cardano has a very different philosophy than all other JS libraries for Cardano.
In particular, all elm-cardano code is currently pure synchronous functions.
Things like Tx building for example, make the assumption that the caller provides
some state containing all known and relevant UTxOs for the Tx builder.
This can be for example all UTxOs from the connected wallet,
and UTxOs relevant to the app, that were fetched from an API provider.

The way you fetch this data is completely up to you.
Eventually, it would be nice to have complementary libraries implementing common decoders and encoders
for the existing API providers like Blockfrost, Koios, Kupo+Ogmios, Maestro, etc.

For the time being, you can ask around on the #elm channel of the TxPipe Discord server,
and I can point you to example code from some projects using elm-cardano.

If you want to work on an API provider package, make sure to look at the following resources:

- [elm-open-api][elm-open-api] to help with code generation for REST APIs
- [elm-graph-ql][elm-graph-ql] to help with code generation for GraphQL APIs
- [elm-grpc][elm-grpc] to help with code generation for gRPC APIs
- [elm-codegen][elm-codegen] to help with code generation in general

[elm-open-api]: https://github.com/wolfadex/elm-open-api-cli
[elm-graph-ql]: https://github.com/dillonkearns/elm-graphql
[elm-grpc]: https://github.com/anmolitor/elm-grpc
[elm-codegen]: https://github.com/mdgriffith/elm-codegen

## Is elm-cardano usable in the Backend?

More or less.
Elm-cardano is built to be compatible with the backend.
For example, you can run tests (on Node) calling code with some Cardano Tx building in it.

```sh
elm-test --compiler elm-cardano
```

However, it’s on the roadmap but there isn’t yet an automated way to run Backend code written in Elm in general.
This isn’t an elm-cardano limitation, just an Elm one.
You can definitely do it, the proof being all the CLI tools written in Elm, for example [elm-open-api][elm-open-api].
If you want to do it, ask around on the #elm channel of the TxPipe Discord server,
or directly on the Elm slack in its #general channel.
People there are usually friendly and helpful.

That being said, being able to run some scripts directly in an automated way is definitely on the roadmap.
You might even spot the existence of an `elm-cardano run` subcommand, which does nothing yet!
