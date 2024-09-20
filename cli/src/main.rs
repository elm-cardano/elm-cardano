use anyhow::Context;
use argh::FromArgs;
use std::fmt::Debug;
use std::fs;
use std::io::{self, Read, Write};
use std::process::Command as ProcessCommand;

#[derive(FromArgs, PartialEq, Debug)]
/// Top-level command.
struct Command {
    #[argh(subcommand)]
    command: SubCommand,
}

#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand)]
enum SubCommand {
    Make(MakeSubCommand),
    Postprocess(PostprocessSubCommand),
    Run(RunSubCommand),
}

#[derive(FromArgs, PartialEq, Debug)]
/// Make subcommand.
#[argh(subcommand, name = "make")]
struct MakeSubCommand {
    #[argh(positional)]
    /// the main Elm file to compile.
    source: String,

    #[argh(option)]
    /// specify the name of the resulting JS file.
    /// For example
    /// --output=assets/elm.js to generate the JS at assets/elm.js or
    /// --output=/dev/null to generate no output at all!
    output: String,

    #[argh(switch)]
    /// turn on the time-travelling debugger. It allows you to rewind and replay events.
    /// The events can be imported/exported into a file,
    /// which makes for very precise bug reports!
    debug: bool,

    #[argh(switch)]
    /// turn on optimizations to make code smaller and faster.
    /// For example, the compiler renames record fields to be as short as possible
    /// and unboxes values to reduce allocation.
    optimize: bool,
}

#[derive(FromArgs, PartialEq, Debug)]
/// Postprocess subcommand for elm-watch.
#[argh(subcommand, name = "postprocess")]
struct PostprocessSubCommand {
    #[argh(positional)]
    /// the target specified in elm-watch.json. Typically "main".
    target: String,
    #[argh(positional)]
    /// compilation mode. Either "debug", "standard" or "optimize".
    compilation_mode: String,
    #[argh(positional)]
    /// run mode. Either "make" or "hot".
    run_mode: String,
}

#[derive(FromArgs, PartialEq, Debug)]
/// Run subcommand.
#[argh(subcommand, name = "run")]
struct RunSubCommand {
    #[argh(switch)]
    /// whether to fooey
    fooey: bool,
}

fn main() -> anyhow::Result<()> {
    let Command { command }: Command = argh::from_env();
    match command {
        SubCommand::Make(make_args) => make_subcommand(make_args)?,
        SubCommand::Postprocess(make_args) => postprocess_subcommand(make_args)?,
        SubCommand::Run(run_args) => run_subcommand(run_args)?,
    }
    Ok(())
}

/// Validate the subcommand and call the `elm` binary with the same arguments if valid.
fn make_subcommand(make_args: MakeSubCommand) -> anyhow::Result<()> {
    // Validate output path
    if make_args.output.is_empty() {
        anyhow::bail!("Output path cannot be empty");
    }

    // Construct the elm command
    let mut cmd = ProcessCommand::new("elm");
    cmd.arg("make");

    // Add source file
    cmd.arg(&make_args.source);

    // Add output option
    cmd.arg("--output").arg(&make_args.output);

    // Add debug flag if set
    if make_args.debug {
        cmd.arg("--debug");
    }

    // Add optimize flag if set
    if make_args.optimize {
        cmd.arg("--optimize");
    }

    // Execute the command and stream output
    let status = cmd.status().context("Failed to execute elm make command")?;
    if !status.success() {
        anyhow::bail!("Compilation failed!")
    }

    // Read the elm compiled file
    let compiled_file = fs::read_to_string(&make_args.output)?;

    // Apply kernel patching to be able to call the uplc_wasm code synchronously.
    let patched_module = kernel_patching_uplc_wasm_iife(&compiled_file);

    // Overwrite the compiled output file
    fs::write(&make_args.output, &patched_module)?;

    Ok(())
}

/// Postprocess the file provided through stdin and output the result in stdout.
fn postprocess_subcommand(args: PostprocessSubCommand) -> anyhow::Result<()> {
    // Read the file given through stdin
    let mut compiled_file = String::new();
    io::stdin().read_to_string(&mut compiled_file)?;

    // Apply kernel patching to be able to call the uplc_wasm code synchronously.
    let patched_file = kernel_patching_uplc_wasm_iife(&compiled_file);

    // Write the patched file to stdout
    io::stdout().write_all(patched_file.as_bytes())?;

    Ok(())
}

fn run_subcommand(run_args: RunSubCommand) -> anyhow::Result<()> {
    todo!()
}

/// Kernel patching the elm compiler output with uplc_wasm
fn kernel_patching_uplc_wasm_iife(elm_js: &str) -> String {
    let eval_def = r#"
let uplc_wasm_module;
let evalScriptsCostsKernel = (elm_args) => {
  try {
    if (!uplc_wasm_module) {
      if (typeof window !== 'undefined' && window.document) {
        // Browser environment
        if (!("uplc_wasm" in window)) {
          throw new Error("Missing uplc_wasm module in window. Make sure to dynamically import the Elm app after window.uplc_wasm was populated.");
        }
        uplc_wasm_module = window.uplc_wasm;
      } else if (typeof process !== 'undefined' && process.versions && process.versions.node) {
        // Node.js environment
        uplc_wasm_module = require("./pkg-node/uplc_wasm.js");
      } else {
        throw new Error("Unknown environment");
      }
    }
    const args = elm_args.a; // elm uses a specific structure for its data
    const fromHexString = (hexString) =>
      Uint8Array.from(hexString.match(/.{1,2}/g).map((byte) => parseInt(byte, 16)));
    let redeemers = uplc_wasm_module.eval_phase_two_raw(
      fromHexString(args.tx_bytes), // tx_bytes: &[u8],
      args.utxos_refs_bytes.map(fromHexString), // utxos_refs_bytes: Vec<js_sys::Uint8Array>,
      args.utxos_outputs_bytes.map(fromHexString), // utxos_outputs_bytes: Vec<js_sys::Uint8Array>,
      fromHexString(args.cost_mdls_bytes), // cost_mdls_bytes: &[u8],
      BigInt(args.cpu_budget), // cpu_budget: u64,
      BigInt(args.mem_budget), // mem_budget: u64,
      BigInt(args.slot_config_zero_time), // slot_config_zero_time: u64,
      BigInt(args.slot_config_zero_slot), // slot_config_zero_slot: u64,
      args.slot_config_slot_length, // slot_config_slot_length: u32,
    );
    const uint8ArrayToHexString = (uint8Array) => {
      return Array.from(uint8Array).map((i) => i.toString(16).padStart(2, '0')).join('');
    };
    const redeemersAsHex = redeemers.map(uint8ArrayToHexString);
    return $elm$core$Result$Ok(_List_fromArray(redeemersAsHex));
  } catch (error) {
    return $elm$core$Result$Err('Script evaluation failed with error: ' + error);
  }
};
    "#;
    let old_body = "return $elm$core$Result$Err('evalScriptsCostsKernel');";
    let new_body = "return evalScriptsCostsKernel(_v0);";
    let use_strict_offset = elm_js.find("'use strict'").unwrap();
    [
        &elm_js[..use_strict_offset],
        &eval_def,
        &elm_js[use_strict_offset..].replacen(old_body, new_body, 1),
    ]
    .join("\n")
}

/// Convert a JS file resulting from an Elm compilation into an ES module.
fn into_es_module(elm_js: &str) -> String {
    // Remove the function wrapping at the beginning.
    // Remove everything thing before the "use strict".
    let use_strict_offset = elm_js.find("'use strict'").unwrap();
    // replace ';}(this));' by ';' at the end.
    let last_this_offset = elm_js.rfind("}(this").unwrap();
    [
        "const scope = {};",
        &elm_js[use_strict_offset..last_this_offset],
        "export const { Elm } = scope;",
    ]
    .join("\n")
}
