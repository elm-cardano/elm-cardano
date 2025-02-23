<p align="center">
  <img src="https://github.com/user-attachments/assets/0cd24e14-12e9-4274-bc16-a4ad8daa47e4" />
</p>


# Elm Cardano

Elm offchain package for Cardano. This project aims to be the friendliest and
most productive way of handling an offchain Cardano frontend. It should be a
perfect match to [Aiken][aiken] for onchain code.

For the time being, the package isn’t published on official elm packages.
So you will find the docs either locally by running elm-doc-preview,
or online at [its static preview page][docs-preview].

[aiken]: https://aiken-lang.org/
[docs-preview]: https://elm-doc-preview.netlify.app/Cardano?repo=elm-cardano%2Felm-cardano&version=elm-doc-preview

## Why elm-cardano?

If you don’t know Elm, you might be tempted to just ignore this,
why use something that isn’t Typescript in 2024?
Well, let me give you the 30s elevator pitch.

Your Elm code will have no runtime exception, so happy users,
and happy maintainers that can focus on building products, not fixing bugs.
Elm is statically typed, with a type system very similar to Aiken one,
and compiler error messages that are so friendly, even Rust was inspired by it.
When your code typechecks, it’s usually working, like in Rust, but even more true in Elm.
Elm packages are very well documented usually.
Every published package has a mandatory check that every exposed function must be documented.
Elm compiles to JavaScript, and has a few ways to interact with,
or be embedded in a regular HTML/JS app.
If you don’t know how, just ask around on the Elm slack, people there are very friendly.

## Quickstart

> Remark: this section aspirational as the elm package is not published yet.

Install the elm-cardano CLI and the Elm package.

```sh
# Install the elm compiler and the elm-cardano CLI
npm install -g elm elm-cardano

# (Temporary) clone the elm-cardano repo to expose its elm modules
# This step won’t be necessary when the elm package will be published
git clone --depth 1 --branch v0.2.0 https://github.com/elm-cardano/elm-cardano.git

# Initialize a template project in the elm-cardano-starter/ folder
mkdir elm-cardano-starter && cd elm-cardano-starter
npx elm-cardano init
```

This will generate the following template structure:
```sh
.gitignore   # some files to ignore
elm.json     # the elm app config
index.html   # the app web page
src/Main.elm # the elm app
```

Now you simply need to compile the elm app and start a static server.
```sh
# Compile the elm app. This will create some new files.
npx elm-cardano make src/Main.elm --output main.js
# Start a static web server then open your browser
python -m http.server
```

That’s it you are ready to build a Cardano offchain frontend with Elm!
More examples are available in the `examples/` dir of the repo.
More information about this project is also available
in the different documents in the `docs/` dir of the repo.

## Contributions

Contributions are very welcomed! For now things are moving fast so I suggest
discussing first over in the
[#elm][elm-cardano-channel] channel.

[elm-cardano-channel]: https://discord.gg/UgXYyy9dHg

The elm-cardano cli is built using the rust language.
To build successfully, it attempts to statically load the WASM files for Aiken UPLC virtual machine.
So we first need to download these files locally, then we can compile (in release).
```sh
# Download the uplc-wasm archive
curl -LO 'https://github.com/mpizenberg/uplc-wasm/releases/download/v0.2.0/my-artifact.zip'
unzip my-artifact.zip -d cli/pkg-uplc-wasm

# Build the elm-cardano cli
cargo build --release
```

Now you can put that `target/release/elm-cardano` binary somewhere in your path.
Or better, put a link to it somewhere in your path.
After doing that, we can install the elm tools.
The simplest way is via npm.
```sh
# Install all the tools:
# elm, elm-format, elm-test, elm-review, elm-watch
npm install
```

You can then run the tools you need by prefixing the command with `npx`.

```sh
# compile the elm package
npx elm make
# run the tests, using the elm-cardano binary for compilation
npx elm-test --compiler elm-cardano
# review the code
npx elm-review
# run the Tx builder example
cd examples/txbuild && npx elm-watch hot
```

## Credits

Many thanks to all people who contributed code and ideas to elm-cardano.
Many thanks to the person (they know who they are) who contributed the logo.

## Maintainer notes

Don’t forget to update the version tag in the quickstart instructions

Cargo-dist TLDR:
```sh
git commit -am "release: 0.2.0"
git tag "v0.2.0"
git push
git push --tags
```

Then download, extract and publish the npm package from the GitHub release page.
```sh
# Inside the extracted package/ dir
npm publish
```

Finally, don’t forget to update the docs published in the dedicated
```sh
cd frontend/
mkdir temp/
elm make --docs temp/docs.json
git co elm-doc-preview
cp temp/docs.json docs.json
git ci -a --amend
git push --force
git co main
rm -r temp/
```
