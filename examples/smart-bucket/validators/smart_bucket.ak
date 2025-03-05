use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{VerificationKeyHash}
use cardano/assets
use cardano/transaction.{InlineDatum, OutputReference, Transaction}

const token_policy_id =
  #"2fe3c3364b443194b10954771c95819b8d6ed464033c21f03f8facb5"

const asset_name = "iUSD"

pub type Datum {
  owner: VerificationKeyHash,
  input_bucket_index: Int,
}

pub type Redeemer {
  input_bucket_index: Int,
  output_bucket_index: Int,
}

validator token_bucket {
  spend(
    datum: Option<Datum>,
    redeemer: Redeemer,
    own_ref: OutputReference,
    self: Transaction,
  ) {
    // If it’s the owner signing, they can do whatever
    // Otherwise, let’s check that all invariants are respected
    expect Some(Datum { owner, .. }) = datum
    if list.has(self.extra_signatories, owner) {
      True
    } else {
      // Check that the claimed input token bucket is indeed this UTxO
      expect Some(own_input) = list.at(self.inputs, redeemer.input_bucket_index)
      expect own_input.output_reference == own_ref

      // Check that the token bucket is created again in the outputs
      expect Some(output_bucket) =
        list.at(self.outputs, redeemer.output_bucket_index)

      // Check that the output bucket is at the same script address
      expect output_bucket.address == own_input.output.address

      // Check that the output token bucket has the same owner
      expect InlineDatum(output_datum_as_data) = output_bucket.datum
      expect output_datum: Datum = output_datum_as_data
      expect output_datum.owner == owner

      // Check that the output token bucket is referencing this one,
      // to avoid double satisfaction
      expect output_datum.input_bucket_index == redeemer.input_bucket_index

      // Check that the output bucket only has ada and token in it
      let output_value = output_bucket.value
      expect list.length(assets.policies(output_value)) == 2

      // Also check that there are no token imposters (with different asset names)
      // Maybe this check isn’t actually needed if the minting contract
      // prevent any other asset name?
      let token_assets = assets.tokens(output_value, token_policy_id)
      expect dict.size(token_assets) == 1

      // Check that the output ada and token amounts are >= than inputs
      let input_value = own_input.output.value
      expect assets.lovelace_of(output_value) >= assets.lovelace_of(input_value)
      let token_in =
        assets.quantity_of(input_value, token_policy_id, asset_name)
      let token_out =
        assets.quantity_of(output_value, token_policy_id, asset_name)
      token_out >= token_in
    }
  }

  else(_) {
    fail
  }
}
