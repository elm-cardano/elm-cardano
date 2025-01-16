let applyParamsToScriptKernel = (elm_args) => {
  try {
    if (!uplc_wasm_module) {
      if (typeof window !== "undefined" && window.document) {
        // Browser environment
        if (!("uplc_wasm" in window)) {
          throw new Error(
            "Missing uplc_wasm module in window. Make sure to dynamically import the Elm app after window.uplc_wasm was populated.",
          );
        }
        uplc_wasm_module = window.uplc_wasm;
      } else if (
        typeof process !== "undefined" &&
        process.versions &&
        process.versions.node
      ) {
        // Node.js environment
        uplc_wasm_module = require("./pkg-uplc-wasm/pkg-node/uplc_wasm.js");
      } else {
        throw new Error("Unknown environment");
      }
    }
    const args = elm_args.a; // elm uses a specific structure for its data
    const fromHexString = (hexString) =>
      Uint8Array.from(
        hexString.match(/.{1,2}/g).map((byte) => parseInt(byte, 16)),
      );
    let paramsBytes = fromHexString(args.params); // paramsBytes: &[u8]
    let plutusScriptBytes = fromHexString(args.script); // plutusScriptBytes: &[u8]
    let appliedScript = uplc_wasm_module.apply_params_to_script(
      paramsBytes,
      plutusScriptBytes
    );
    const uint8ArrayToHexString = (uint8Array) => {
      return Array.from(uint8Array)
        .map((i) => i.toString(16).padStart(2, "0"))
        .join("");
    };
    return $elm$core$Result$Ok(uint8ArrayToHexString(appliedScript));
  } catch (error) {
    return $elm$core$Result$Err(
      "Parameter application failed with error: " + error,
    );
  }
};
