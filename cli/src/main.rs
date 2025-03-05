use anyhow::Context;
use argh::FromArgs;
use std::fmt::Debug;
use std::fs;
use std::io::{self, Read, Write};
use std::path::Path;
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
    Init(InitSubCommand),
    Make(MakeSubCommand),
    Postprocess(PostprocessSubCommand),
    Run(RunSubCommand),
}

#[derive(FromArgs, PartialEq, Debug)]
/// Initialize a project with a starter template
#[argh(subcommand, name = "init")]
struct InitSubCommand {}

#[derive(FromArgs, PartialEq, Debug)]
/// Call the elm compiler on a given elm file
#[argh(subcommand, name = "make")]
struct MakeSubCommand {
    #[argh(positional)]
    /// the main Elm file(s) to compile.
    source: Vec<String>,

    #[argh(option, default = "String::from(\"main.js\")")]
    /// specify the name of the resulting JS file.
    output: String,

    #[argh(option)]
    /// specify "--report json" to get error messages as JSON
    report: Option<String>,

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
/// Postprocess subcommand for elm-watch
#[argh(subcommand, name = "postprocess")]
struct PostprocessSubCommand {
    #[argh(positional)]
    /// the target specified in elm-watch.json (typically "main")
    target: String,

    #[argh(positional)]
    /// compilation mode (either "debug", "standard" or "optimize")
    compilation_mode: String,

    #[argh(positional)]
    /// run mode (either "make" or "hot")
    run_mode: String,
}

#[derive(FromArgs, PartialEq, Debug)]
/// TODO: Run subcommand.
#[argh(subcommand, name = "run")]
struct RunSubCommand {
    #[argh(positional)]
    /// the main Elm worker to run.
    source: String,
}

fn main() -> anyhow::Result<()> {
    let Command { command }: Command = argh::from_env();
    match command {
        SubCommand::Init(_) => init_subcommand()?,
        SubCommand::Make(make_args) => make_subcommand(make_args)?,
        SubCommand::Postprocess(make_args) => postprocess_subcommand(make_args)?,
        SubCommand::Run(run_args) => run_subcommand(run_args)?,
    }
    Ok(())
}

fn init_subcommand() -> anyhow::Result<()> {
    // Load template files
    let gitignore = include_str!("../../templates/starter/.gitignore");
    let readme = include_str!("../../templates/starter/README.md");
    let elmjson = include_str!("../../templates/starter/elm.json");
    let indexhtml = include_str!("../../templates/starter/index.html");
    let elmmain = include_str!("../../templates/starter/src/Main.elm");

    // Write the template files
    fs::create_dir_all("src")?;
    // Append to .gitignore
    let mut gitignore_file = fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(".gitignore")?;
    writeln!(gitignore_file, "\n{}", gitignore)?;
    // Append to README.md
    let mut readme_file = fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open("README.md")?;
    writeln!(readme_file, "\n{}", readme)?;
    // fs::write(".gitignore", gitignore)?;
    // fs::write("README.md", readme)?;
    fs::write("elm.json", elmjson)?;
    fs::write("index.html", indexhtml)?;
    fs::write(Path::new("src").join("Main.elm"), elmmain)?;

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
    cmd.args(&make_args.source);

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

    // Add report format if specified
    if let Some(report_value) = &make_args.report {
        cmd.arg("--report").arg(report_value);
    }

    // Execute the command and stream output
    let status = cmd.status().context("Failed to execute elm make command")?;
    if !status.success() {
        // The elm compiler failed, let’s just exit without any additional info
        // to not mess with potential json --report going to stderr.
        std::process::exit(status.code().unwrap_or(1));
    }

    // Read the elm compiled file
    let compiled_file = fs::read_to_string(&make_args.output)?;

    // Apply kernel patching to be able to call the uplc_wasm code synchronously.
    let patched_module = kernel_patching_uplc_wasm_iife(&compiled_file);

    // Overwrite the compiled output file
    fs::write(&make_args.output, &patched_module)?;

    // Load static files
    // These files are generated by the uplc-wasm crate:
    // https://github.com/mpizenberg/uplc-wasm
    let uplc_wasm_node_js = include_str!("../pkg-uplc-wasm/pkg-node/uplc_wasm.js");
    let uplc_wasm_node_bg = include_bytes!("../pkg-uplc-wasm/pkg-node/uplc_wasm_bg.wasm");
    let uplc_wasm_web_js = include_str!("../pkg-uplc-wasm/pkg-web/uplc_wasm.js");
    let uplc_wasm_web_bg = include_bytes!("../pkg-uplc-wasm/pkg-web/uplc_wasm_bg.wasm");
    // These files are templates for reuse
    let elm_cardano_js = include_str!("../../templates/elm-cardano.js");

    // Create the output directory structure
    let output_dir = Path::new(&make_args.output).parent().unwrap();
    let pkg_node_dir = output_dir.join("pkg-uplc-wasm").join("pkg-node");
    let pkg_web_dir = output_dir.join("pkg-uplc-wasm").join("pkg-web");
    fs::create_dir_all(&pkg_node_dir)?;
    fs::create_dir_all(&pkg_web_dir)?;

    // Write the static files to the output directory
    fs::write(output_dir.join("elm-cardano.js"), elm_cardano_js)?;
    fs::write(pkg_node_dir.join("uplc_wasm.js"), uplc_wasm_node_js)?;
    fs::write(pkg_node_dir.join("uplc_wasm_bg.wasm"), uplc_wasm_node_bg)?;
    fs::write(pkg_web_dir.join("uplc_wasm.js"), uplc_wasm_web_js)?;
    fs::write(pkg_web_dir.join("uplc_wasm_bg.wasm"), uplc_wasm_web_bg)?;

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
    let eval_def = include_str!("../../templates/eval-script-costs-kernel.js");
    let param_app_def = include_str!("../../templates/apply-params-to-script-kernel.js");
    let old_eval_body = "return $elm$core$Result$Err('To build a Tx containing scripts, you need to use the elm-cardano binary instead of directly the elm binary. Details are in the elm-cardano GitHub repo.');";
    let new_eval_body = "return evalScriptsCostsKernel(_v0);";
    let old_param_app_body = "return $elm$core$Result$Err('To apply parameters to a script, you need to use the elm-cardano binary instead of directly the elm binary. Details are in the elm-cardano GitHub repo.');";
    let new_param_app_body = "return applyParamsToScriptKernel(_v0);";
    let use_strict_offset = elm_js.find("'use strict'").unwrap();
    [
        &elm_js[..use_strict_offset],
        &eval_def,
        &param_app_def,
        &elm_js[use_strict_offset..]
            .replacen(old_eval_body, new_eval_body, 1)
            .replacen(old_param_app_body, new_param_app_body, 1),
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
