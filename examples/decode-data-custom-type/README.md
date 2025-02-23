This example shows how one can make a custom relatively simple `Decoder` API for decode from `Data` to custom types.
This is very similar to the `Json.Decoder` API provided by the elm/json package,
except here it is made extremely simple to purpose fit our use case.
But the idea is the same, we define a `type alias Decoder a = Data -> Result String a`
where `Data` plays the role that `Value` would play for JSON decoding.

You will find that alias, as well as a few basic decoder functions defined inside the `DataDecoder.elm` module.
Then in the `Main.elm`, we define some example `Data` value, taken from the Orcfax documentation https://docs.orcfax.io/consume#example-datum.
And we decode it into our custom datum type, looking like this:

```elm
type alias RationalDatum =
    { statement : RationalStatement
    , context : Data
    }


type alias RationalStatement =
    { feedId : String
    , createdAt : Integer
    , body : Rational
    }


type alias Rational =
    { num : Integer
    , denom : Integer
    }
```

As usual, you can compile this example as follows.

```sh
# npx elm-watch hot
elm-cardano make src/Main.elm --output main.js
python -m http.server
```
