# Smart Bucket

What I call "smart bucket" here, is basically a simple aiken contract,
that enables locking some utxos reusable by anyone, as long as they are put back in.
I call such a utxo a "bucket".
In this example, the bucket is limited to (testnet) ada and iUSD.
But the iUSD policy could be replaced by any policy really.
If you don’t replace the token policy ID by something you own,
the second Tx ("Reuse the bucket") will fail to perform coin selection.

```sh
# build the aiken contract
aiken build
# build the elm frontend
npx elm-cardano make src/Main.elm --output main.js && python -m http.server
```

https://github.com/user-attachments/assets/2b8c254e-3e43-4622-a984-b90a07d2d396
